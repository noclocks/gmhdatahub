ALTER TABLE mkt.leasing_summary
    DROP CONSTRAINT leasing_summary_leasing_week_id_fkey,
    DROP COLUMN leasing_week_id;
