from __future__ import annotations

import unittest

from py_scripts.lib.cloudflare_talent_catalog_sync import (
    D1Talent,
    DuckDbTalent,
    build_catalog_sync_plan,
    display_name_from_duckdb,
    render_catalog_sync_sql,
)


class CloudflareTalentCatalogSyncTests(unittest.TestCase):
    def test_display_name_removes_catalog_suffixes(self) -> None:
        self.assertEqual(
            display_name_from_duckdb("Leia_Memoria_Variance_Project"),
            "Leia Memoria",
        )
        self.assertEqual(display_name_from_duckdb("Nova_Aokami_Ch"), "Nova Aokami")

    def test_exact_name_attaches_code_without_changing_id(self) -> None:
        plan = build_catalog_sync_plan(
            [DuckDbTalent("LEI3", "Leia Memoria", True)],
            [D1Talent("leia-memoria", "Leia Memoria", True)],
        )

        self.assertEqual(plan.errors, ())
        self.assertEqual(plan.updated_count, 1)
        self.assertEqual(plan.talents[0].id, "leia-memoria")
        self.assertEqual(plan.talents[0].talent_code, "LEI3")

    def test_new_talents_do_not_create_permissions(self) -> None:
        plan = build_catalog_sync_plan(
            [DuckDbTalent("AVA1", "Avaritia Hawthorne", True)],
            [],
        )
        sql = render_catalog_sync_sql(plan)

        self.assertEqual(plan.inserted_count, 1)
        self.assertIn("INSERT INTO talents", sql)
        self.assertNotIn("product_access", sql)
        self.assertNotIn("talent_access", sql)
        self.assertNotIn("permission_grants", sql)

    def test_unchanged_catalog_does_not_rewrite_talent_rows(self) -> None:
        plan = build_catalog_sync_plan(
            [DuckDbTalent("LEI3", "Leia Memoria", True)],
            [D1Talent("leia-memoria", "Leia Memoria", True, "LEI3", True)],
        )
        sql = render_catalog_sync_sql(plan)

        self.assertEqual(plan.unchanged_count, 1)
        self.assertNotIn("UPDATE talents", sql)
        self.assertNotIn("INSERT INTO talents", sql)
        self.assertIn("INSERT INTO talent_catalog_sync_runs", sql)

    def test_missing_d1_code_is_retained_as_warning(self) -> None:
        plan = build_catalog_sync_plan(
            [DuckDbTalent("AVA1", "Avaritia Hawthorne", True)],
            [D1Talent("old-talent", "Old Talent", True, "OLD9", True)],
        )

        self.assertEqual(plan.errors, ())
        self.assertTrue(any("OLD9" in warning for warning in plan.warnings))


if __name__ == "__main__":
    unittest.main()
