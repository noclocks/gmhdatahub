DROP TABLE IF EXISTS entrata.api_logs CASCADE;

CREATE UNLOGGED TABLE entrata.api_logs (
  log_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  log_date DATE NOT NULL DEFAULT CURRENT_DATE,
  entrata_endpoint TEXT NOT NULL,
  entrata_operation TEXT NOT NULL,
  entrata_request_id INTEGER,
  entrata_request_start TIMESTAMP,
  entrata_request_end TIMESTAMP,
  entrata_request_duration INTEGER,
  entrata_request_json JSONB,
  entrata_response_json JSONB,
  entrata_report_params_json JSONB,
  entrata_report_name TEXT,
  entrata_report_queue_id TEXT,
  log_comments TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
); --PARTITION BY RANGE (log_date);

CREATE INDEX idx_entrata_api_logs_log_date ON entrata.api_logs (log_date);
CREATE INDEX idx_entrata_api_logs_entrata_request_json ON entrata.api_logs USING GIN (entrata_request_json);
CREATE INDEX idx_entrata_api_logs_entrata_response_json ON entrata.api_logs USING GIN (entrata_response_json);
CREATE INDEX idx_entrata_api_logs_entrata_report_params_json ON entrata.api_logs USING GIN (entrata_report_params_json);
