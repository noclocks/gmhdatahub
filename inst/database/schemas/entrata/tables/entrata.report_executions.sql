DROP TABLE IF EXISTS entrata.report_executions;

CREATE TABLE entrata.report_executions (
  execution_id UUID NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
  request_id TEXT NOT NULL,
  report_name TEXT NOT NULL,
  report_date DATE NOT NULL DEFAULT CURRENT_DATE,
  report_version TEXT NOT NULL,
  queue_id TEXT NOT NULL,
  queue_start_time TIMESTAMPTZ,
  queue_end_time TIMESTAMPTZ,
  queue_duration INTEGER,
  report_filter_params JSONB,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID
);
