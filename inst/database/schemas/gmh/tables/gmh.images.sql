DROP TABLE IF EXISTS gmh.images CASCADE;

CREATE TABLE gmh.images (
  image_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  entity_type TEXT NOT NULL,
  entity_id INTEGER NOT NULL,
  image_type TEXT NOT NULL,
  image_url TEXT NOT NULL,
  image_description TEXT,
  image_content_type TEXT,
  image_color TEXT,
  image_size TEXT,
  gcs_bucket TEXT,
  gcs_path TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,

  UNIQUE (entity_type, entity_id, image_type, image_content_type, image_color)
);

COMMENT ON TABLE gmh.images IS 'Images: Stores images for various entities';
COMMENT ON COLUMN gmh.images.image_id IS 'Unique identifier for the image';
COMMENT ON COLUMN gmh.images.entity_type IS 'Type of entity the image is associated with';
COMMENT ON COLUMN gmh.images.entity_id IS 'Identifier for the entity the image is associated with';
COMMENT ON COLUMN gmh.images.image_type IS 'Type of image (e.g., logo, banner)';
COMMENT ON COLUMN gmh.images.image_url IS 'URL of the image';
COMMENT ON COLUMN gmh.images.image_description IS 'Description of the image';
COMMENT ON COLUMN gmh.images.image_content_type IS 'Content type of the image';
COMMENT ON COLUMN gmh.images.image_color IS 'Color of the image';
COMMENT ON COLUMN gmh.images.image_size IS 'Size of the image';
COMMENT ON COLUMN gmh.images.gcs_bucket IS 'Google Cloud Storage bucket name';
COMMENT ON COLUMN gmh.images.gcs_path IS 'Google Cloud Storage path';
COMMENT ON COLUMN gmh.images.created_at IS 'Timestamp when the image was created';
COMMENT ON COLUMN gmh.images.updated_at IS 'Timestamp when the image was last updated';
