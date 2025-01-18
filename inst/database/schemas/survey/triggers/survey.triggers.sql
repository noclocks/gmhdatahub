-- Trigger Function
CREATE OR REPLACE FUNCTION set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Triggers for Tables
CREATE TRIGGER trigger_update_properties_updated_at
BEFORE UPDATE ON survey.properties
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

CREATE TRIGGER trigger_update_competitors_updated_at
BEFORE UPDATE ON survey.competitors
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

CREATE TRIGGER trigger_update_surveys_updated_at
BEFORE UPDATE ON survey.surveys
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

CREATE TRIGGER trigger_update_property_summary_updated_at
BEFORE UPDATE ON survey.property_summary
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

CREATE TRIGGER trigger_update_leasing_summary_updated_at
BEFORE UPDATE ON survey.leasing_summary
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();
