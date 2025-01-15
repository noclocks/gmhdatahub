DROP TABLE IF EXISTS survey.leasing_summary CASCADE;

CREATE TABLE IF NOT EXISTS survey.leasing_summary (
    property_id INTEGER REFERENCES survey.properties(property_id),
    property_name TEXT NOT NULL,
    leasing_week DATE NOT NULL,
    reporting_cycle TEXT CHECK (reporting_cycle IN ('Monday-Sunday', 'Saturday-Friday', 'Sunday-Saturday')) DEFAULT 'Monday-Sunday',
    lease_launch_date DATE,
    renewal_launch_date DATE,
    current_occupancy DECIMAL(5,2),
    last_year_occupancy DECIMAL(5,2),
    current_pre_lease DECIMAL(5,2),
    last_year_pre_lease DECIMAL(5,2),
    total_renewals INT,
    total_new_leases INT,
    total_leases_weekly INT,
    traffic_weekly INT,
    current_incentive TEXT DEFAULT 'None',
    incentive_amount DECIMAL(5,2),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (property_id, leasing_week)
);
