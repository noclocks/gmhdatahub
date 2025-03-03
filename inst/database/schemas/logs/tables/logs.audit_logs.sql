-- Audit Logs
CREATE TABLE logs.audit_logs (
    audit_log_id SERIAL PRIMARY KEY,
    schema_name TEXT NOT NULL,
    table_name TEXT NOT NULL,
    operation TEXT NOT NULL,
    old_data JSONB,
    new_data JSONB,
    user_id UUID,
    session_id UUID,
    sql_statement TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
