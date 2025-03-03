DROP TABLE IF EXISTS gmh.portfolio_types CASCADE;

CREATE TABLE gmh.portfolio_types (
    portfolio_type_id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
