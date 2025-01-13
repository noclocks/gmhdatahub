/* Schema */
CREATE SCHEMA IF NOT EXISTS logs;

/* Tables */

-- Table: audit_logs
CREATE TABLE logs.audit_logs (
    audit_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,  -- Unique identifier for each log entry
    schema_name TEXT NOT NULL,                                      -- Schema of the affected table
    table_name TEXT NOT NULL,                                       -- Name of the affected table
    operation TEXT NOT NULL,                                        -- Type of operation: INSERT, UPDATE, DELETE
    old_data JSONB,                                                 -- Data before the change (for UPDATE/DELETE)
    new_data JSONB,                                                 -- Data after the change (for INSERT/UPDATE)
    user_id INT,                                                    -- User ID responsible for the operation
    session_id UUID DEFAULT gen_random_uuid(),                      -- Unique session identifier
    query TEXT,                                                     -- Original query executed
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP                  -- Timestamp of the operation
);

-- Indexes to optimize querying
CREATE INDEX idx_audit_logs_table_name ON logs.audit_logs (table_name);
CREATE INDEX idx_audit_logs_operation ON logs.audit_logs (operation);
CREATE INDEX idx_audit_logs_user_id ON logs.audit_logs (user_id);
CREATE INDEX idx_audit_logs_created_at ON logs.audit_logs (created_at);

-- Table: error_logs
CREATE TABLE logs.error_logs (
    error_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,  -- Unique identifier for each error log
    error_message TEXT NOT NULL,                                    -- Error message details
    error_context JSONB,                                            -- Context of the error (e.g., inputs, state)
    user_id INT,                                                    -- User ID, if applicable
    session_id UUID DEFAULT gen_random_uuid(),                      -- Unique session identifier
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP                  -- Timestamp of the error occurrence
);

-- Indexes for error logs
CREATE INDEX idx_error_logs_user_id ON logs.error_logs (user_id);
CREATE INDEX idx_error_logs_created_at ON logs.error_logs (created_at);

-- Table: system_logs
CREATE TABLE logs.system_logs (
    system_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY, -- Unique identifier for each system log
    event_type TEXT NOT NULL,                                       -- Type of system event (e.g., startup, shutdown)
    event_details JSONB,                                            -- Additional details about the event
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP                  -- Timestamp of the event
);

-- Index for system logs
CREATE INDEX idx_system_logs_event_type ON logs.system_logs (event_type);
CREATE INDEX idx_system_logs_created_at ON logs.system_logs (created_at);

-- Entrata Logs
CREATE TABLE logs.entrata_logs (
    entrata_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    request_id TEXT,
    endpoint TEXT,
    operation TEXT,
    status_code INTEGER DEFAULT 200,
    request_payload JSONB,
    response_body JSONB,
    user_id INT REFERENCES auth.users(user_id),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_entrata_logs_endpoint ON logs.entrata_logs (endpoint);
CREATE INDEX idx_entrata_logs_created_at ON logs.entrata_logs (created_at);

-- Survey Logs
CREATE TABLE logs.market_survey_logs (
    response_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    survey_id INTEGER NOT NULL,                     -- Survey associated with the log
    section_id INTEGER,                             -- Section of the survey
    response_id INTEGER,                            -- Individual response identifier
    user_id INTEGER,                                -- User who submitted or changed the response
    property_id INTEGER,                            -- Property related to the survey
    leasing_week_id INTEGER,                        -- Leasing week associated with the survey
    response_data JSONB,                            -- The survey response data
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_market_survey_logs_survey_id ON logs.market_survey_logs (survey_id);
CREATE INDEX idx_market_survey_logs_created_at ON logs.market_survey_logs (created_at);


-- Pipeline Logs
CREATE TABLE logs.pipeline_logs (
    pipeline_log_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    pipeline_name TEXT NOT NULL,                    -- Name of the data pipeline
    pipeline_status TEXT NOT NULL,                  -- Status: success, failure, running
    data_lineage JSONB,                             -- Metadata about data lineage
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_pipeline_logs_created_at ON logs.pipeline_logs (created_at);
CREATE INDEX idx_pipeline_logs_pipeline_name ON logs.pipeline_logs (pipeline_name);


-- Functions

-- Main Audit Trigger Function
CREATE OR REPLACE FUNCTION logs.audit_trigger_func()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, new_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'INSERT', row_to_json(NEW), COALESCE(current_setting('gmhdatahub.user_id', true)::INT, 0));
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, old_data, new_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'UPDATE', row_to_json(OLD), row_to_json(NEW), COALESCE(current_setting('gmhdatahub.user_id', true)::INT, 0));
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, old_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'DELETE', row_to_json(OLD), COALESCE(current_setting('gmhdatahub.user_id', true)::INT, 0));
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Functions to Add/Remove Audit Triggers
CREATE OR REPLACE FUNCTION logs.add_audit_triggers_to_table(schema_name TEXT, table_name TEXT)
RETURNS VOID AS $$
BEGIN
    EXECUTE format(
        'CREATE TRIGGER audit_trigger_%I AFTER INSERT OR UPDATE OR DELETE ON %I.%I FOR EACH ROW EXECUTE FUNCTION logs.audit_trigger_func();',
        table_name, schema_name, table_name
    );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.add_audit_triggers_to_schema(schema_name TEXT)
RETURNS VOID AS $$
DECLARE
    tbl RECORD;
BEGIN
    FOR tbl IN SELECT tablename FROM pg_tables WHERE schemaname = schema_name LOOP
        PERFORM logs.add_audit_triggers_to_table(schema_name, tbl.tablename);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.remove_audit_triggers_from_table(schema_name TEXT, table_name TEXT)
RETURNS VOID AS $$
BEGIN
    EXECUTE format(
        'DROP TRIGGER IF EXISTS audit_trigger_%I ON %I.%I;',
        table_name, schema_name, table_name
    );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.remove_audit_triggers_from_schema(schema_name TEXT)
RETURNS VOID AS $$
DECLARE
    tbl RECORD;
BEGIN
    FOR tbl IN SELECT tablename FROM pg_tables WHERE schemaname = schema_name LOOP
        PERFORM logs.remove_audit_triggers_from_table(schema_name, tbl.tablename);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Views

CREATE VIEW logs.recent_audit_logs AS
SELECT *
FROM logs.audit_logs
WHERE created_at > NOW() - INTERVAL '7 days';

CREATE VIEW logs.api_errors AS
SELECT *
FROM logs.entrata_logs
WHERE status_code >= 400;


CREATE VIEW logs.pipeline_run_summary AS
SELECT pipeline_name, COUNT(*) AS run_count,
       COUNT(*) FILTER (WHERE pipeline_status = 'success') AS success_count,
       COUNT(*) FILTER (WHERE pipeline_status = 'failure') AS failure_count
FROM logs.pipeline_logs
GROUP BY pipeline_name;

CREATE OR REPLACE VIEW logs.recent_activity AS
SELECT
    a.created_at,
    'Audit Log' AS log_type,
    a.schema_name || '.' || a.table_name AS context,
    a.operation AS action,
    u.email AS user_email,
    COALESCE(a.old_data::TEXT, '') || ' -> ' || COALESCE(a.new_data::TEXT, '') AS details
FROM logs.audit_logs a
LEFT JOIN auth.users u ON a.user_id = u.user_id
WHERE a.created_at > NOW() - INTERVAL '7 days';

UNION ALL

SELECT
    e.created_at,
    'Error Log' AS log_type,
    NULL AS context,
    'Error' AS action,
    u.username AS user_name,
    e.error_message || ': ' || e.error_context::TEXT AS details
FROM logs.error_logs e
LEFT JOIN auth.users u ON e.user_id = u.user_id
WHERE e.created_at > NOW() - INTERVAL '7 days'

UNION ALL

SELECT
    s.created_at,
    'System Log' AS log_type,
    NULL AS context,
    s.event_type AS action,
    NULL AS user_name,
    s.event_details::TEXT AS details
FROM logs.system_logs s
WHERE s.created_at > NOW() - INTERVAL '7 days'

ORDER BY created_at DESC;


/*

CREATE OR REPLACE FUNCTION logs.audit_trigger_func()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, new_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'INSERT', row_to_json(NEW), current_setting('app.user_id')::INT);
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, old_data, new_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'UPDATE', row_to_json(OLD), row_to_json(NEW), current_setting('app.user_id')::INT);
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO logs.audit_logs (schema_name, table_name, operation, old_data, user_id)
        VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, 'DELETE', row_to_json(OLD), current_setting('app.user_id')::INT);
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Functions to Add/Remove Audit Triggers
CREATE OR REPLACE FUNCTION logs.add_audit_triggers_to_table(schema_name TEXT, table_name TEXT)
RETURNS VOID AS $$
BEGIN
    -- Check if the trigger already exists
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = format('audit_trigger_%I', table_name)
          AND tgrelid = format('%I.%I', schema_name, table_name)::regclass
    ) THEN
        -- Create a trigger for the specified table
        EXECUTE format(
            'CREATE TRIGGER audit_trigger_%I
             AFTER INSERT OR UPDATE OR DELETE ON %I.%I
             FOR EACH ROW EXECUTE FUNCTION logs.audit_trigger_func();',
             table_name, schema_name, table_name
        );
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.add_audit_triggers_to_schema(schema_name TEXT)
RETURNS VOID AS $$
DECLARE
    tbl RECORD;
BEGIN
    -- Loop through all tables in the specified schema
    FOR tbl IN
        SELECT schemaname, tablename
        FROM pg_tables
        WHERE schemaname = schema_name
    LOOP
        -- Check if the trigger already exists
        IF NOT EXISTS (
            SELECT 1
            FROM pg_trigger
            WHERE tgname = format('audit_trigger_%I', tbl.tablename)
              AND tgrelid = format('%I.%I', tbl.schemaname, tbl.tablename)::regclass
        ) THEN
            -- Create a trigger for the table
            EXECUTE format(
                'CREATE TRIGGER audit_trigger_%I
                 AFTER INSERT OR UPDATE OR DELETE ON %I.%I
                 FOR EACH ROW EXECUTE FUNCTION logs.audit_trigger_func();',
                 tbl.tablename, tbl.schemaname, tbl.tablename
            );
        END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.remove_audit_triggers_from_table(schema_name TEXT, table_name TEXT)
RETURNS VOID AS $$
BEGIN
    -- Check if the trigger exists
    IF EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = format('audit_trigger_%I', table_name)
          AND tgrelid = format('%I.%I', schema_name, table_name)::regclass
    ) THEN
        -- Drop the trigger
        EXECUTE format(
            'DROP TRIGGER IF EXISTS audit_trigger_%I ON %I.%I;',
            table_name, schema_name, table_name
        );
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION logs.remove_audit_triggers_from_schema(schema_name TEXT)
RETURNS VOID AS $$
DECLARE
    tbl RECORD;
BEGIN
    -- Loop through all tables in the specified schema
    FOR tbl IN
        SELECT schemaname, tablename
        FROM pg_tables
        WHERE schemaname = schema_name
    LOOP
        -- Check if the trigger exists
        IF EXISTS (
            SELECT 1
            FROM pg_trigger
            WHERE tgname = format('audit_trigger_%I', tbl.tablename)
              AND tgrelid = format('%I.%I', tbl.schemaname, tbl.tablename)::regclass
        ) THEN
            -- Drop the trigger
            EXECUTE format(
                'DROP TRIGGER IF EXISTS audit_trigger_%I ON %I.%I;',
                tbl.tablename, tbl.schemaname, tbl.tablename
            );
        END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

*/
