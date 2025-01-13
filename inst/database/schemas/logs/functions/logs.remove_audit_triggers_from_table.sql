--usage: SELECT logs.remove_audit_triggers_from_table('mkt', 'property_summary');

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
