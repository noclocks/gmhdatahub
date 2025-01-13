-- usage: SELECT logs.add_audit_triggers_to_schema('mkt');

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
