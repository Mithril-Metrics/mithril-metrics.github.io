# Reader questions: setup and go-live

Everything happens in the browser; nothing is installed on a laptop. About 45 minutes.
The Worker holds no GitHub credentials. Never paste a secret into a chat, a commit or this file.

## 1. Accounts
1. Shared Mithril Metrics Gmail account, two-factor login on.
2. Free Cloudflare account with that email (https://dash.cloudflare.com/sign-up), two-factor
   login on. Invite the other author: *Manage account → Members*.

## 2. Turnstile (spam check)
1. *Turnstile → Add widget*. Name `reader-questions`, hostname `mithril-metrics.github.io`,
   mode **Managed**.
2. Note the **site key** (public) and the **secret key** (keep it in your password manager).

## 3. Database
1. *Storage & Databases → D1 → Create*. Name `reader-questions`.
2. Open the database → *Console*, paste the contents of `schema.sql` (no comments: the console
   joins lines, so `--` comments would swallow the rest), click *Execute*. Check with
   `SELECT name FROM sqlite_master WHERE type = 'table';` → four tables plus `_cf_KV`.
   Columns: reader_questions holds raw text (never public), our title, status and dates;
   vote_totals the counts; vote_log one row per voter code and question (30 days);
   rate_limits daily counters (2 days).

## 4. Worker
1. *Workers & Pages → Create → Start with Hello World*. Name `reader-questions` → *Deploy*.
2. *Edit code*: replace everything with the contents of `worker.js` → *Deploy*.
3. *Settings → Bindings → Add → D1 database*: variable name `DB`, database `reader-questions`.
4. *Settings → Variables and Secrets*:
   - `ALLOWED_ORIGIN` (text) = `https://mithril-metrics.github.io`
   - `PAUSED` (text) = `0`
   - `TURNSTILE_SECRET` (secret) = the Turnstile secret key
   - `VOTER_SECRET` (secret) = a random string of 48+ characters (password manager generator)
   - `ADMIN_KEY` (secret) = another random string of 48+ characters
5. *Settings → Triggers → Cron Triggers → Add*: `17 3 * * *` (daily housekeeping).
6. *Settings → Observability*: switch **Workers Logs off** (no request logs kept).
7. Note the Worker address, e.g. `https://reader-questions.<account>.workers.dev`.

## 5. Protect the admin page (optional extra layer)
The admin routes already require the 48-character `ADMIN_KEY`, which is enough on its own.
For a second layer, put Cloudflare Access (Zero Trust, free plan) in front of the paths `/admin*`
and `/api/admin/*` only, allowing the two authors' emails. Do **not** protect the whole Worker:
the public routes (`/api/public`, `/api/vote`, `/api/submit`) must stay open. If the dashboard
only offers Access for the whole `workers.dev` address, skip this step. Zero Trust sign-up may
ask for a payment method even on the free plan.

## 6. Check
- `<worker address>/api/public` shows `{"votes":{},"readers":[],"closed":[]}`.
- `<worker address>/admin` shows the admin page; *Load* with the admin key lists nothing yet.

## 7. Go-live in the repository (Claude does this; send it the Worker address and site key)
1. `questions.qmd`: fill `CONFIG.api` and `CONFIG.sitekey`; uncomment the Turnstile script; add
   the Worker address to `connect-src`; remove the robots meta; `draft: false`.
2. `.github/external-allowlist.txt`: uncomment `challenges.cloudflare.com`.
3. `_quarto.yml`: add *Questions* to the navbar.
4. `impressum.qmd`, `privacy.qmd`: contact email in, `draft: false`, footer links; remove
   "This section applies once the page is live".
5. Test together on the live page: one vote, one submission, triage it in the admin page.

## Running it
- **Triage (P0b):** admin page → status NEW. List with our own neutral title, or decline with a
  code. Paste NEW items into a P0b chat if you want Claude's scoring first.
- **P0 PICK:** admin page → *Copy export for P0* → paste into the chat with the P0 prompt.
- **Emergency stop:** set `PAUSED` to `1` (votes and submissions stop at once).
- **Erasure request:** admin page → find the item → *Delete*.
