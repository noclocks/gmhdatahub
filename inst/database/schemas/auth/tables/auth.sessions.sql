-- Sessions: Tracks active user sessions
CREATE TABLE auth.sessions (
    session_id SERIAL PRIMARY KEY,
    user_id INT REFERENCES auth.users(user_id) ON DELETE CASCADE,
    token TEXT NOT NULL,
    expires_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
