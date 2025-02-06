DROP TABLE IF EXISTS survey.parking CASCADE;

CREATE TABLE survey.parking (
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    property_name TEXT NOT NULL,
    parking_type TEXT NOT NULL CHECK (parking_type IN ('Surface', 'Reserved', 'Covered', 'Garage', 'Other')),
    is_required BOOLEAN NOT NULL DEFAULT FALSE,
    is_included BOOLEAN NOT NULL DEFAULT FALSE,
    amount NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID REFERENCES survey.users(user_id),
    updated_by UUID REFERENCES survey.users(user_id),
    PRIMARY KEY (property_name, parking_type),
      CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);


