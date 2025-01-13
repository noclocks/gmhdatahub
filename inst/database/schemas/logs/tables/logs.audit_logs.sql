-- Audit Logs
CREATE TABLE logs.audit_logs (
    audit_log_id SERIAL PRIMARY KEY,
    table_name TEXT NOT NULL,
    operation TEXT NOT NULL,
    old_data JSONB,
    new_data JSONB,
    user_id INT REFERENCES auth.users(user_id),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
