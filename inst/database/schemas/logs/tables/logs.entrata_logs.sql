CREATE TABLE logs.entrata_logs (
  entrata_log_id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  request_id TEXT,
  entrata_endpoint TEXT,
  operation TEXT,
  status_code INTEGER DEFAULT 200,
  request_payload JSONB,
  response_body JSONB,
  user_id UUID,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
