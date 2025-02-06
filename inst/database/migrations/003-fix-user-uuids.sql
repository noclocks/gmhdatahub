DO $$
DECLARE
    fk_record RECORD;
BEGIN
    -- Query to fetch all foreign key constraints referencing survey.users
    FOR fk_record IN
        SELECT conname AS constraint_name,
               n.nspname AS schema_name,
               c.conrelid::regclass::text AS referencing_table,
               a.attname AS referencing_column
        FROM pg_constraint AS c
        JOIN pg_class AS t ON c.conrelid = t.oid
        JOIN pg_namespace AS n ON t.relnamespace = n.oid
        JOIN pg_attribute AS a ON a.attnum = ANY(c.conkey) AND a.attrelid = c.conrelid
        WHERE c.confrelid = 'survey.users'::regclass AND c.contype = 'f'
    LOOP
        -- Drop the existing foreign key constraint
        EXECUTE format(
            'ALTER TABLE %I.%I DROP CONSTRAINT %I;',
            fk_record.schema_name,
            fk_record.referencing_table,
            fk_record.constraint_name
        );

        -- Recreate the foreign key constraint with ON UPDATE CASCADE
        EXECUTE format(
            'ALTER TABLE %I.%I ADD CONSTRAINT %I FOREIGN KEY (%I) REFERENCES survey.users(user_id) ON UPDATE CASCADE;',
            fk_record.schema_name,
            fk_record.referencing_table,
            fk_record.constraint_name,
            fk_record.referencing_column
        );
    END LOOP;
END $$;

/*
SELECT conname AS constraint_name,
       confrelid::regclass AS referenced_table,
       conrelid::regclass AS referencing_table,
       confupdtype AS on_update_action
FROM pg_constraint
WHERE confrelid = 'survey.users'::regclass AND contype = 'f';

ALTER TABLE survey.property_summary DROP CONSTRAINT property_summary_created_by_fkey;
ALTER TABLE survey.property_summary DROP CONSTRAINT property_summary_updated_by_fkey;

ALTER TABLE survey.property_summary
ADD CONSTRAINT property_summary_created_by_fkey
FOREIGN KEY (created_by)
REFERENCES survey.users(user_id)
ON UPDATE CASCADE;

ALTER TABLE survey.property_summary
ADD CONSTRAINT property_summary_updated_by_fkey
FOREIGN KEY (updated_by)
REFERENCES survey.users(user_id)
ON UPDATE CASCADE;

ALTER TABLE survey.leasing_summary DROP CONSTRAINT leasing_summary_created_by_fkey;
ALTER TABLE survey.leasing_summary DROP CONSTRAINT leasing_summary_updated_by_fkey;

ALTER TABLE survey.utilities DROP CONSTRAINT utilities_created_by_fkey;
ALTER TABLE survey.utilities DROP CONSTRAINT utilities_updated_by_fkey;






-- Update user IDs in survey.users
UPDATE survey.users
SET user_id = '5d396b01-5fe3-4cb5-a58e-bc59f4435e9d'
WHERE user_email = 'jimmy.briggs@noclocks.dev';

UPDATE survey.users
SET user_id = '000bb29d-e521-48bc-b210-490415d4ed37'
WHERE user_email = 'patrick.howard@noclocks.dev';

UPDATE survey.utilities
SET created_by = '5d396b01-5fe3-4cb5-a58e-bc59f4435e9d'
*/
