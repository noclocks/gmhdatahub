-- Choices (Enumerations for dropdowns, etc.)
CREATE TABLE app.choices (
    choice_id SERIAL PRIMARY KEY,
    category TEXT NOT NULL,
    value TEXT NOT NULL,
    description TEXT
);
