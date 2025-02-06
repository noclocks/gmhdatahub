DROP TABLE IF EXISTS survey.rents_by_floorplan;

CREATE TABLE survey.rents_by_floorplan (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
  property_name TEXT NOT NULL,
  floorplan_type TEXT NOT NULL CHECK (floorplan_type IN ('Studio', '1 Bedroom', '2 Bedroom', '3 Bedroom', '4 Bedroom', '5 Bedroom', '6 Bedroom')),
  floorplan_id TEXT NOT NULL,
  square_feet NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  number_of_beds INTEGER NOT NULL,
  number_of_baths INTEGER NOT NULL,
  total_units_count INTEGER NOT NULL DEFAULT 0,
  square_feet_per_bed INTEGER GENERATED ALWAYS AS (square_feet / NULLIF(number_of_beds, 0)) STORED,
  available BOOLEAN NOT NULL DEFAULT TRUE,
  market_rent_per_bed NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  market_rent_per_square_foot NUMERIC(10, 2) GENERATED ALWAYS AS (market_rent_per_bed / NULLIF(square_feet / NULLIF(number_of_beds, 0), 0)) STORED,
  concessions_gift_card NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  concessions_one_time_rent NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  concessions_monthly_rent NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  effective_rent_per_bed NUMERIC(10, 2) GENERATED ALWAYS AS (market_rent_per_bed - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent) STORED,
  effective_rent_per_square_foot NUMERIC(10, 2) GENERATED ALWAYS AS ((market_rent_per_bed - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent) / NULLIF(square_feet / NULLIF(number_of_beds, 0), 0)) STORED,
  expenses_furniture NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_tv NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_electricity_gas NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_water NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_cable_internet NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_trash_valet NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_parking NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  expenses_total NUMERIC(10, 2) GENERATED ALWAYS AS (expenses_furniture + expenses_tv + expenses_electricity_gas + expenses_water + expenses_cable_internet + expenses_trash + expenses_parking) STORED,
  bundled_rent_per_bed NUMERIC(10, 2) GENERATED ALWAYS AS ((market_rent_per_bed - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent) + (expenses_furniture + expenses_tv + expenses_electricity_gas + expenses_water + expenses_cable_internet + expenses_trash + expenses_parking)) STORED,
  bundled_rent_per_square_foot NUMERIC(10, 2) GENERATED ALWAYS AS (((market_rent_per_bed - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent) + (expenses_furniture + expenses_tv + expenses_electricity_gas + expenses_water + expenses_cable_internet + expenses_trash + expenses_parking)) / NULLIF(square_feet / NULLIF(number_of_beds, 0), 0)) STORED,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  PRIMARY KEY (property_name, leasing_week_id, floorplan_type, floorplan_id),
  CONSTRAINT chk_property_or_competitor CHECK (
      (property_id IS NOT NULL AND competitor_id IS NULL) OR
      (property_id IS NULL AND competitor_id IS NOT NULL)
  )
);
