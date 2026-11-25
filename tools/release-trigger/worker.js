// Release trigger for mithril-metrics.github.io (Cloudflare Worker, cron only).
// Once an hour it starts the "Publish blog" workflow in release-only mode, so posts with a
// passed `publish_at:` go live even when GitHub's own schedule is late or dropped.
// It has no public routes. Kept separate from reader-questions, which holds no GitHub token.
//
// Settings (Cloudflare dashboard > Worker > Settings):
//   GITHUB_TOKEN  secret: fine-grained token, this repository only, Actions: Read and write
//   Cron Trigger  2 * * * *

const OWNER = "Mithril-Metrics";
const REPO = "mithril-metrics.github.io";
const WORKFLOW = "publish.yml";
const BRANCH = "main";

async function dispatch(env) {
  const url = `https://api.github.com/repos/${OWNER}/${REPO}/actions/workflows/${WORKFLOW}/dispatches`;
  const res = await fetch(url, {
    method: "POST",
    headers: {
      "Authorization": `Bearer ${env.GITHUB_TOKEN}`,
      "Accept": "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
      "User-Agent": "mithril-metrics-release-trigger",
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ ref: BRANCH, inputs: { release_only: true } }),
  });
  if (res.status !== 204) {
    const text = await res.text();
    throw new Error(`workflow_dispatch failed: HTTP ${res.status} ${text.slice(0, 300)}`);
  }
}

export default {
  async scheduled(event, env, ctx) {
    await dispatch(env);
  },

  async fetch(request, env, ctx) {
    return new Response("Not found", { status: 404 });
  },
};
