# Release trigger: setup

GitHub's own schedule is best-effort, so scheduled posts (`publish_at:` in `00_meta.yml`) could
stay in draft for hours. This Worker starts the publish workflow once an hour from Cloudflare;
GitHub's schedule (every 15 minutes) stays on as a backup. About 20 minutes, all in the browser.
Never paste the token into a chat, a commit or this file.

## 1. GitHub token
1. GitHub → *Settings → Developer settings → Personal access tokens → Fine-grained tokens →
   Generate new token*.
2. Name `mithril-release-trigger`. **Resource owner: Mithril-Metrics**. Expiration: 1 year
   (set a calendar reminder to renew it).
3. *Repository access → Only select repositories* → `mithril-metrics.github.io`.
4. *Permissions → Repository permissions → Actions: Read and write*. Nothing else
   (Metadata: Read-only is added automatically).
5. *Generate token*, copy it once into your password manager.
6. If the organisation requires approval for fine-grained tokens, approve it under
   *Mithril-Metrics → Settings → Personal access tokens → Pending requests*.

## 2. Worker
1. Cloudflare → *Workers & Pages → Create → Start with Hello World*. Name `release-trigger`
   → *Deploy*.
2. *Edit code*: replace everything with the contents of `worker.js` → *Deploy*.
3. *Settings → Variables and Secrets → Add*: type **Secret**, name `GITHUB_TOKEN`, value = the
   token → *Deploy*.
4. *Settings → Triggers → Cron Triggers → Add*: `2 * * * *` (minute 2 of every hour, UTC).
5. *Settings → Domains & Routes*: disable the `workers.dev` route (the Worker has no public use).
6. *Settings → Observability*: Workers Logs can stay on; they hold no personal data.

## 3. Check
1. Worker → *Settings → Triggers → Cron Triggers* → wait for the next run, or test it with the
   *Schedule* test in the code editor.
2. GitHub → *Actions → Publish blog*: a new `workflow_dispatch` run appears. With nothing due it
   finishes in under a minute and skips the build.

## Notes
- Running from both triggers is harmless: the release script skips posts that are already
  published.
- GitHub disables scheduled workflows in repositories with no commits for 60 days; the Worker
  keeps working, but check *Actions* if the blog has been quiet for two months.
- To publish a post right now, set `draft: false` in its `report.qmd` and push, or use
  *Actions → Publish blog → Run workflow*.
