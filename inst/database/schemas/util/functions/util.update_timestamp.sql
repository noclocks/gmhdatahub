CREATE OR REPLACE FUNCTION update_timestamp()
RETURNS TRIGGER AS $$
BEGIN
   NEW.updated_at = NOW();
   RETURN NEW;
END;
$$
 LANGUAGE plpgsql;

/* Usage:
CREATE TRIGGER update_timestamp
BEFORE UPDATE ON your_table
FOR EACH ROW
EXECUTE FUNCTION update_timestamp();
*/
