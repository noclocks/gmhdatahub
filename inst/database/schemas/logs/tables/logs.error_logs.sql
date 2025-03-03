CREATE TABLE logs.error_logs (
  log_id UUID DEFAULT gen_random_uuid() PRIMARY KEY, -- Unique log identifier
  error_message TEXT NOT NULL, -- Error message details
  error_context JSONB, -- Context of the error (e.g., inputs, state)
  user_id UUID, -- User ID, if applicable
  session_id UUID, -- Unique session identifier
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP -- Timestamp of the error occurrence
);
