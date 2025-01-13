--usage: SELECT logs.add_audit_triggers_to_table('mkt', 'property_summary');

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
