# Company dashboard membership

`company_talents.csv` defines which active unified-catalog talents appear in
company-level dashboards. The dashboard joins these codes to
`catalog.talents`; names and analytics remain sourced from DuckDB.

To add another company, append one row per talent using a stable lowercase
`company_code`, a display `company_name`, and the exact `talent_code` from
`catalog.talents`. A talent may appear in more than one company grouping when
that overlap is intentional, but each company/talent pair must be unique.

