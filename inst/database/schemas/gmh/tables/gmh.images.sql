DROP TABLE IF EXISTS gmh.images;

CREATE TABLE gmh.images (
  image_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  entity_type TEXT NOT NULL,
  entity_id INTEGER NOT NULL,
  image_type TEXT NOT NULL,
  image_url TEXT NOT NULL,
  image_description TEXT,
  is_primary BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (entity_type, entity_id, image_type)
);

COMMENT ON TABLE gmh.images IS 'Images: Stores images for various entities';
COMMENT ON COLUMN gmh.images.image_id IS 'Image ID';
COMMENT ON COLUMN gmh.images.entity_type IS 'Entity type';
COMMENT ON COLUMN gmh.images.entity_id IS 'Entity ID';
COMMENT ON COLUMN gmh.images.image_type IS 'Image type';
COMMENT ON COLUMN gmh.images.image_url IS 'Image URL';
COMMENT ON COLUMN gmh.images.image_description IS 'Image description';
COMMENT ON COLUMN gmh.images.is_primary IS 'Is primary image';
COMMENT ON COLUMN gmh.images.created_at IS 'Created at';
COMMENT ON COLUMN gmh.images.updated_at IS 'Updated at';
COMMENT ON CONSTRAINT gmh_images_entity_type_entity_id_image_type_key ON gmh.images IS 'Unique constraint for entity_type, entity_id, and image_type';
