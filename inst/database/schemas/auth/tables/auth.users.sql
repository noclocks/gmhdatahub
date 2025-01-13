-- Users: Represents system users
CREATE TABLE auth.users (
    --user_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id SERIAL PRIMARY KEY,
    tenant_id INT NOT NULL REFERENCES auth.tenants(tenant_id) ON DELETE CASCADE,
    email TEXT UNIQUE NOT NULL,
    hashed_password TEXT NOT NULL,
    --role TEXT NOT NULL CHECK (roll IN ('Developer', 'GMH Admin', 'Partner Admin', 'Property Manager', 'User')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
