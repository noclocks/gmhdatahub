-- Roles: Defines roles for users
CREATE TABLE auth.roles (
    role_id SERIAL PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    description TEXT
);
