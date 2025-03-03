CREATE TABLE logs.pipeline_logs (
  pipeline_log_id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  pipeline_name TEXT NOT NULL,
  pipeline_status TEXT NOT NULL, -- Status: success, failure, running
  data_lineage JSONB, -- Metadata about data lineage
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
