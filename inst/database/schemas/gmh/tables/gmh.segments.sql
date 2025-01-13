DROP TABLE IF EXISTS gmh.segments CASCADE;

CREATE TABLE IF NOT EXISTS gmh.segments (
  segment_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  segment_name TEXT NOT NULL UNIQUE,
  segment_description TEXT,
  segment_url TEXT,
  segment_logo_url TEXT,
  segment_banner_url TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.segments IS 'Segments represent high-level GMH business units or divisions.';
COMMENT ON COLUMN gmh.segments.segment_id IS 'Unique identifier for the segment.';
COMMENT ON COLUMN gmh.segments.segment_name IS 'Name of the segment.';
COMMENT ON COLUMN gmh.segments.segment_description IS 'Description of the segment.';
COMMENT ON COLUMN gmh.segments.segment_url IS 'URL for the segment.';
COMMENT ON COLUMN gmh.segments.segment_logo_url IS 'Logo URL for the segment.';
COMMENT ON COLUMN gmh.segments.segment_banner_url IS 'Banner URL for the segment.';
COMMENT ON COLUMN gmh.segments.created_at IS 'Timestamp when the segment was created.';
