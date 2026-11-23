-- Reader questions: database schema (Cloudflare D1). Run once in the D1 console.

CREATE TABLE IF NOT EXISTS reader_questions (
  id          TEXT PRIMARY KEY,           -- e.g. R7K2QX
  raw_text    TEXT NOT NULL,              -- as submitted (cleaned); never shown publicly
  region      TEXT NOT NULL DEFAULT '',
  status      TEXT NOT NULL,              -- NEW, LISTED, DECLINED, PICKED, ANSWERED, EXPIRED
  title       TEXT,                       -- our neutral wording, the only text shown publicly
  reason      TEXT,                       -- decline code or public closing note
  link        TEXT,                       -- post URL once answered
  created_at  INTEGER NOT NULL,
  listed_at   INTEGER,
  closed_at   INTEGER
);
CREATE INDEX IF NOT EXISTS idx_rq_status ON reader_questions (status);

-- Vote totals for our questions (Q001...) and listed reader questions (R...).
CREATE TABLE IF NOT EXISTS vote_totals (
  question_id TEXT PRIMARY KEY,
  votes       INTEGER NOT NULL DEFAULT 0
);

-- One row per voter code and question, only to refuse repeat votes; deleted after 30 days.
CREATE TABLE IF NOT EXISTS vote_log (
  question_id TEXT NOT NULL,
  voter       TEXT NOT NULL,              -- HMAC of network address and question id
  created_at  INTEGER NOT NULL,
  PRIMARY KEY (question_id, voter)
);

-- Daily counters for rate limits; deleted after 2 days.
CREATE TABLE IF NOT EXISTS rate_limits (
  key   TEXT NOT NULL,                    -- HMAC of network address and day
  kind  TEXT NOT NULL,                    -- vote or submit
  day   TEXT NOT NULL,
  n     INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (key, kind)
);
