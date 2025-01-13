--usage: SELECT logs.remove_audit_triggers_from_schema('mkt');

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
