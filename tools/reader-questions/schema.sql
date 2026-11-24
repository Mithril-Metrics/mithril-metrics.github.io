CREATE TABLE IF NOT EXISTS reader_questions (id TEXT PRIMARY KEY, raw_text TEXT NOT NULL, region TEXT NOT NULL DEFAULT '', status TEXT NOT NULL, title TEXT, reason TEXT, link TEXT, created_at INTEGER NOT NULL, listed_at INTEGER, closed_at INTEGER);
CREATE INDEX IF NOT EXISTS idx_rq_status ON reader_questions (status);
CREATE TABLE IF NOT EXISTS vote_totals (question_id TEXT PRIMARY KEY, votes INTEGER NOT NULL DEFAULT 0);
CREATE TABLE IF NOT EXISTS vote_log (question_id TEXT NOT NULL, voter TEXT NOT NULL, created_at INTEGER NOT NULL, PRIMARY KEY (question_id, voter));
CREATE TABLE IF NOT EXISTS rate_limits (key TEXT NOT NULL, kind TEXT NOT NULL, day TEXT NOT NULL, n INTEGER NOT NULL DEFAULT 0, PRIMARY KEY (key, kind));
