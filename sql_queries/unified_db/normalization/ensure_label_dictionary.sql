CREATE SCHEMA IF NOT EXISTS normalization;

CREATE TABLE IF NOT EXISTS normalization.label_dictionaries (
  dictionary_id VARCHAR NOT NULL,
  dictionary_version VARCHAR NOT NULL,
  label_type VARCHAR NOT NULL,
  method VARCHAR NOT NULL,
  config_json VARCHAR NOT NULL,
  source_title_version_id VARCHAR,
  mapping_checksum VARCHAR NOT NULL,
  active BOOLEAN NOT NULL DEFAULT FALSE,
  pipeline_run_id VARCHAR,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (dictionary_id, dictionary_version)
);

CREATE TABLE IF NOT EXISTS normalization.label_mappings (
  dictionary_id VARCHAR NOT NULL,
  dictionary_version VARCHAR NOT NULL,
  raw_label VARCHAR NOT NULL,
  normalized_label VARCHAR NOT NULL,
  canonical_label VARCHAR NOT NULL,
  broader_group VARCHAR,
  normalization_method VARCHAR NOT NULL,
  observed_uses BIGINT NOT NULL,
  rapidfuzz_component VARCHAR,
  component_size BIGINT,
  PRIMARY KEY (dictionary_id, dictionary_version, raw_label)
);

CREATE OR REPLACE VIEW normalization.active_label_mappings AS
SELECT
  dictionary.dictionary_id,
  dictionary.dictionary_version,
  dictionary.label_type,
  mapping.raw_label,
  mapping.normalized_label,
  mapping.canonical_label,
  mapping.broader_group,
  mapping.normalization_method,
  mapping.observed_uses,
  mapping.rapidfuzz_component,
  mapping.component_size
FROM normalization.label_dictionaries AS dictionary
JOIN normalization.label_mappings AS mapping
  USING (dictionary_id, dictionary_version)
WHERE dictionary.active;
