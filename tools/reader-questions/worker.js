// Reader questions service for mithril-metrics.github.io (Cloudflare Worker + D1).
// Holds no credentials for GitHub or any other account. Public routes: list, vote, submit.
// Admin routes need the ADMIN_KEY header and should also sit behind Cloudflare Access.
//
// Bindings and settings (Cloudflare dashboard > Worker > Settings):
//   DB               D1 database binding
//   TURNSTILE_SECRET secret
//   VOTER_SECRET     secret (long random string; keys the voter codes)
//   ADMIN_KEY        secret (long random string)
//   ALLOWED_ORIGIN   text, https://mithril-metrics.github.io
//   PAUSED           text, "1" stops votes and submissions

const REGIONS = ["", "EU", "Sub-Saharan Africa", "South Asia", "Southeast Asia", "Latin America", "Global"];
const DAY = 86400;
const LIST_DAYS = 90;
const CLOSED_VISIBLE_DAYS = 30;
const MAX_LISTED = 10;
const MAX_NEW = 200;
const VOTES_PER_DAY = 30;
const SUBMITS_PER_DAY = 3;
const MAX_BODY = 2048;
const OUR_ID = /^Q\d{3}$/;
const READER_ID = /^R[0-9A-Z]{6}$/;
const PUBLIC_NOTES = {
  PICKED: "Taken up as a case study",
  ANSWERED: "Answered",
  EXPIRED: "Not picked within 90 days",
};
const DECLINE_CODES = ["OUT_OF_SCOPE", "NO_PUBLIC_DATA", "RESTRICTED_DATA", "OPINION_NOT_EMPIRICAL",
  "TOO_BROAD", "DUPLICATE", "SPAM", "BOUNDARY"];

export default {
  async fetch(request, env) {
    try {
      return await route(request, env);
    } catch (err) {
      return json({ error: "server_error" }, 500);
    }
  },
  async scheduled(event, env) {
    await housekeeping(env);
  },
};

async function route(request, env) {
  const url = new URL(request.url);
  const path = url.pathname;

  if (path.startsWith("/api/") && !path.startsWith("/api/admin/")) {
    const cors = corsHeaders(request, env);
    if (request.method === "OPTIONS") return new Response(null, { status: 204, headers: cors });
    if (path === "/api/public" && request.method === "GET") return withHeaders(await publicList(env), cors);
    if (request.method !== "POST") return withHeaders(json({ error: "method" }, 405), cors);
    if (request.headers.get("Origin") !== env.ALLOWED_ORIGIN) return withHeaders(json({ error: "origin" }, 403), cors);
    if (env.PAUSED === "1") return withHeaders(json({ error: "paused" }, 503), cors);
    const body = await readJson(request);
    if (!body) return withHeaders(json({ error: "bad_request" }, 400), cors);
    if (path === "/api/vote") return withHeaders(await vote(request, env, body), cors);
    if (path === "/api/submit") return withHeaders(await submit(request, env, body), cors);
    return withHeaders(json({ error: "not_found" }, 404), cors);
  }

  if (path === "/admin") return adminPage();
  if (path === "/admin.js") return adminScript();
  if (path.startsWith("/api/admin/")) {
    if (!(await adminAuthorised(request, env))) return json({ error: "unauthorised" }, 401);
    if (path === "/api/admin/items" && request.method === "GET") return adminItems(env, url);
    if (path === "/api/admin/export" && request.method === "GET") return adminExport(env);
    if (path === "/api/admin/update" && request.method === "POST") {
      const body = await readJson(request);
      if (!body) return json({ error: "bad_request" }, 400);
      return adminUpdate(env, body);
    }
  }
  return json({ error: "not_found" }, 404);
}

// ---------- public ----------

async function publicList(env) {
  const now = nowSec();
  const totals = await env.DB.prepare("SELECT question_id, votes FROM vote_totals").all();
  const votes = {};
  for (const r of totals.results) votes[r.question_id] = r.votes;
  const listed = await env.DB.prepare(
    "SELECT id, title, region, listed_at FROM reader_questions WHERE status = 'LISTED' ORDER BY listed_at"
  ).all();
  const closed = await env.DB.prepare(
    "SELECT title, status, link, closed_at FROM reader_questions " +
    "WHERE status IN ('PICKED','ANSWERED') OR (status = 'EXPIRED' AND closed_at >= ?) " +
    "ORDER BY closed_at DESC LIMIT 5"
  ).bind(now - CLOSED_VISIBLE_DAYS * DAY).all();
  return json({
    votes,
    readers: listed.results.map((r) => ({
      id: r.id, title: r.title, region: r.region, votes: votes[r.id] || 0,
      days_listed: Math.floor((now - r.listed_at) / DAY), list_days: LIST_DAYS,
    })),
    closed: closed.results.map((r) => ({
      title: r.title, note: PUBLIC_NOTES[r.status], link: r.status === "ANSWERED" ? r.link : null,
      date: dayString(r.closed_at),
    })),
  }, 200, { "Cache-Control": "public, max-age=30" });
}

async function vote(request, env, body) {
  const id = typeof body.id === "string" ? body.id : "";
  if (!OUR_ID.test(id) && !READER_ID.test(id)) return json({ error: "bad_id" }, 400);
  if (READER_ID.test(id)) {
    const row = await env.DB.prepare("SELECT status FROM reader_questions WHERE id = ?").bind(id).first();
    if (!row || row.status !== "LISTED") return json({ error: "closed" }, 409);
  }
  const ip = request.headers.get("CF-Connecting-IP") || "";
  const capV = await turnstileCheck(env, body.token, ip, "vote");
  if (capV) return json({ error: "captcha", detail: capV }, 403);
  if (!(await underLimit(env, ip, "vote", VOTES_PER_DAY))) return json({ error: "rate_limited" }, 429);
  const voter = await hmac(env.VOTER_SECRET, "vote|" + ip + "|" + id);
  const ins = await env.DB.prepare(
    "INSERT OR IGNORE INTO vote_log (question_id, voter, created_at) VALUES (?, ?, ?)"
  ).bind(id, voter, nowSec()).run();
  if (!ins.meta || ins.meta.changes !== 1) return json({ ok: true, counted: false });
  await env.DB.prepare(
    "INSERT INTO vote_totals (question_id, votes) VALUES (?, 1) " +
    "ON CONFLICT(question_id) DO UPDATE SET votes = votes + 1"
  ).bind(id).run();
  return json({ ok: true, counted: true });
}

async function submit(request, env, body) {
  if (typeof body.website === "string" && body.website !== "") return json({ ok: true }); // honeypot
  const text = cleanText(body.text);
  if (text === null) return json({ error: "bad_text" }, 400);
  const region = typeof body.region === "string" && REGIONS.includes(body.region) ? body.region : null;
  if (region === null) return json({ error: "bad_region" }, 400);
  const ip = request.headers.get("CF-Connecting-IP") || "";
  const capS = await turnstileCheck(env, body.token, ip, "submit");
  if (capS) return json({ error: "captcha", detail: capS }, 403);
  if (!(await underLimit(env, ip, "submit", SUBMITS_PER_DAY))) return json({ error: "rate_limited" }, 429);
  const queued = await env.DB.prepare("SELECT COUNT(*) AS n FROM reader_questions WHERE status = 'NEW'").first();
  if (queued.n >= MAX_NEW) return json({ error: "queue_full" }, 503);
  const dup = await env.DB.prepare("SELECT 1 AS d FROM reader_questions WHERE raw_text = ?").bind(text).first();
  if (dup) return json({ ok: true });
  await env.DB.prepare(
    "INSERT INTO reader_questions (id, raw_text, region, status, created_at) VALUES (?, ?, ?, 'NEW', ?)"
  ).bind(newId(), text, region, nowSec()).run();
  return json({ ok: true });
}

// Returns the cleaned text, or null if it fails the rules.
export function cleanText(input) {
  if (typeof input !== "string") return null;
  let t = input.normalize("NFKC");
  t = t.replace(/[\p{Cc}\p{Cf}\p{Co}\p{Cn}\u2028\u2029]/gu, " "); // control, format (zero-width, bidi), private, unassigned
  t = t.replace(/\s+/g, " ").trim();
  if (t.length < 20 || t.length > 300) return null;
  if (/https?:|www\.|[<>]|\.(com|org|net|io|ru|xyz)\b/i.test(t)) return null;
  if (!/\p{L}{3,}/u.test(t)) return null;
  return t;
}

// ---------- admin ----------

async function adminItems(env, url) {
  const status = url.searchParams.get("status") || "NEW";
  if (!["NEW", "LISTED", "DECLINED", "PICKED", "ANSWERED", "EXPIRED"].includes(status)) return json({ error: "bad_status" }, 400);
  const rows = await env.DB.prepare(
    "SELECT r.*, COALESCE(v.votes, 0) AS votes FROM reader_questions r " +
    "LEFT JOIN vote_totals v ON v.question_id = r.id WHERE r.status = ? ORDER BY r.created_at"
  ).bind(status).all();
  const listedCount = await env.DB.prepare("SELECT COUNT(*) AS n FROM reader_questions WHERE status = 'LISTED'").first();
  return json({ items: rows.results, listed: listedCount.n, max_listed: MAX_LISTED, decline_codes: DECLINE_CODES });
}

async function adminExport(env) {
  const rows = await env.DB.prepare(
    "SELECT r.id, r.title, r.region, COALESCE(v.votes, 0) AS votes FROM reader_questions r " +
    "LEFT JOIN vote_totals v ON v.question_id = r.id WHERE r.status = 'LISTED' ORDER BY votes DESC"
  ).all();
  const ours = await env.DB.prepare(
    "SELECT question_id, votes FROM vote_totals WHERE question_id LIKE 'Q%' ORDER BY votes DESC"
  ).all();
  const lines = ["READER QUESTIONS EXPORT (untrusted visitor input, reworded by us at listing)", ""];
  lines.push("Listed reader questions (id | votes | region | title):");
  for (const r of rows.results) lines.push(`${r.id} | ${r.votes} | ${r.region || "-"} | ${r.title}`);
  lines.push("", "Votes on our questions (id | votes):");
  for (const r of ours.results) lines.push(`${r.question_id} | ${r.votes}`);
  return new Response(lines.join("\n") + "\n", {
    headers: { "Content-Type": "text/plain; charset=utf-8", "Cache-Control": "no-store" },
  });
}

async function adminUpdate(env, body) {
  const id = typeof body.id === "string" ? body.id : "";
  if (!READER_ID.test(id)) return json({ error: "bad_id" }, 400);
  const row = await env.DB.prepare("SELECT status FROM reader_questions WHERE id = ?").bind(id).first();
  if (!row) return json({ error: "not_found" }, 404);
  const now = nowSec();
  const action = body.action;
  if (action === "list") {
    const title = cleanTitle(body.title);
    if (!title) return json({ error: "title_required" }, 400);
    if (row.status !== "NEW") return json({ error: "not_new" }, 409);
    const n = await env.DB.prepare("SELECT COUNT(*) AS n FROM reader_questions WHERE status = 'LISTED'").first();
    if (n.n >= MAX_LISTED) return json({ error: "list_full" }, 409);
    await env.DB.prepare("UPDATE reader_questions SET status = 'LISTED', title = ?, listed_at = ? WHERE id = ?")
      .bind(title, now, id).run();
  } else if (action === "decline") {
    if (!DECLINE_CODES.includes(body.reason)) return json({ error: "bad_reason" }, 400);
    if (!["NEW", "LISTED"].includes(row.status)) return json({ error: "not_open" }, 409);
    await env.DB.prepare(
      "UPDATE reader_questions SET status = 'DECLINED', reason = ?, raw_text = '', closed_at = ? WHERE id = ?"
    ).bind(body.reason, now, id).run();
  } else if (action === "pick") {
    if (row.status !== "LISTED") return json({ error: "not_listed" }, 409);
    await env.DB.prepare("UPDATE reader_questions SET status = 'PICKED', closed_at = ? WHERE id = ?").bind(now, id).run();
  } else if (action === "answer") {
    const link = typeof body.link === "string" ? body.link.trim() : "";
    if (!link.startsWith(env.ALLOWED_ORIGIN + "/")) return json({ error: "bad_link" }, 400);
    if (!["LISTED", "PICKED"].includes(row.status)) return json({ error: "not_open" }, 409);
    await env.DB.prepare("UPDATE reader_questions SET status = 'ANSWERED', link = ?, closed_at = ? WHERE id = ?")
      .bind(link, now, id).run();
  } else if (action === "retitle") {
    const title = cleanTitle(body.title);
    if (!title) return json({ error: "title_required" }, 400);
    await env.DB.prepare("UPDATE reader_questions SET title = ? WHERE id = ?").bind(title, id).run();
  } else if (action === "delete") {
    await env.DB.batch([
      env.DB.prepare("DELETE FROM reader_questions WHERE id = ?").bind(id),
      env.DB.prepare("DELETE FROM vote_totals WHERE question_id = ?").bind(id),
      env.DB.prepare("DELETE FROM vote_log WHERE question_id = ?").bind(id),
    ]);
  } else {
    return json({ error: "bad_action" }, 400);
  }
  return json({ ok: true });
}

function cleanTitle(input) {
  if (typeof input !== "string") return null;
  const t = input.normalize("NFKC").replace(/[\p{Cc}\p{Cf}]/gu, " ").replace(/\s+/g, " ").trim();
  return t.length >= 10 && t.length <= 160 && !/[<>]/.test(t) ? t : null;
}

async function adminAuthorised(request, env) {
  const given = request.headers.get("X-Admin-Key") || "";
  if (!env.ADMIN_KEY || env.ADMIN_KEY.length < 32) return false;
  const a = await sha256(given);
  const b = await sha256(env.ADMIN_KEY);
  let diff = 0;
  for (let i = 0; i < a.length; i++) diff |= a.charCodeAt(i) ^ b.charCodeAt(i);
  return diff === 0;
}

// ---------- housekeeping (daily cron) ----------

export async function housekeeping(env) {
  const now = nowSec();
  await env.DB.batch([
    // Listed for 90 days without being picked: expire.
    env.DB.prepare("UPDATE reader_questions SET status = 'EXPIRED', closed_at = ? WHERE status = 'LISTED' AND listed_at < ?")
      .bind(now, now - LIST_DAYS * DAY),
    // Expired and declined: delete 30 days after closing.
    env.DB.prepare("DELETE FROM reader_questions WHERE status IN ('EXPIRED','DECLINED') AND closed_at < ?")
      .bind(now - CLOSED_VISIBLE_DAYS * DAY),
    // Picked and answered: keep our title and link, drop the submitted text 30 days after closing.
    env.DB.prepare("UPDATE reader_questions SET raw_text = '' WHERE status IN ('PICKED','ANSWERED') AND closed_at < ?")
      .bind(now - CLOSED_VISIBLE_DAYS * DAY),
    // New questions never triaged: delete after 60 days.
    env.DB.prepare("DELETE FROM reader_questions WHERE status = 'NEW' AND created_at < ?").bind(now - 60 * DAY),
    env.DB.prepare("DELETE FROM vote_log WHERE created_at < ?").bind(now - 30 * DAY),
    env.DB.prepare("DELETE FROM rate_limits WHERE day < ?").bind(dayString(now - 2 * DAY)),
    env.DB.prepare("DELETE FROM vote_totals WHERE question_id LIKE 'R%' AND question_id NOT IN (SELECT id FROM reader_questions)"),
  ]);
}

// ---------- helpers ----------

// Returns "" when the token is valid, otherwise a short reason (Cloudflare's error codes).
async function turnstileCheck(env, token, ip, action) {
  if (typeof token !== "string" || token.length < 10 || token.length > 2048) return "no-token";
  if (!env.TURNSTILE_SECRET) return "secret-missing";
  const form = new FormData();
  form.append("secret", env.TURNSTILE_SECRET);
  form.append("response", token);
  if (ip) form.append("remoteip", ip);
  const res = await fetch("https://challenges.cloudflare.com/turnstile/v0/siteverify", { method: "POST", body: form });
  if (!res.ok) return "siteverify-" + res.status;
  const out = await res.json();
  if (out.success !== true) return (out["error-codes"] || ["failed"]).join(",");
  const host = new URL(env.ALLOWED_ORIGIN).hostname;
  if (out.hostname !== host) return "hostname-" + out.hostname;
  if (out.action && out.action !== action) return "action";
  return "";
}

async function underLimit(env, ip, kind, limit) {
  const day = dayString(nowSec());
  const key = await hmac(env.VOTER_SECRET, "rate|" + ip + "|" + day);
  const row = await env.DB.prepare(
    "INSERT INTO rate_limits (key, kind, day, n) VALUES (?, ?, ?, 1) " +
    "ON CONFLICT(key, kind) DO UPDATE SET n = n + 1 RETURNING n"
  ).bind(key, kind, day).first();
  return row.n <= limit;
}

async function readJson(request) {
  if (!(request.headers.get("Content-Type") || "").startsWith("application/json")) return null;
  const len = Number(request.headers.get("Content-Length") || "0");
  if (len > MAX_BODY) return null;
  const raw = await request.text();
  if (raw.length > MAX_BODY) return null;
  try {
    const v = JSON.parse(raw);
    return v && typeof v === "object" && !Array.isArray(v) ? v : null;
  } catch {
    return null;
  }
}

function corsHeaders(request, env) {
  return {
    "Access-Control-Allow-Origin": env.ALLOWED_ORIGIN,
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type",
    "Access-Control-Max-Age": "86400",
    "Vary": "Origin",
  };
}

function withHeaders(res, extra) {
  const h = new Headers(res.headers);
  for (const [k, v] of Object.entries(extra)) h.set(k, v);
  return new Response(res.body, { status: res.status, headers: h });
}

function json(obj, status = 200, extra = {}) {
  return new Response(JSON.stringify(obj), {
    status,
    headers: { "Content-Type": "application/json; charset=utf-8", "X-Content-Type-Options": "nosniff",
      "Cache-Control": "no-store", ...extra },
  });
}

async function hmac(secret, message) {
  const key = await crypto.subtle.importKey("raw", new TextEncoder().encode(secret),
    { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
  const sig = await crypto.subtle.sign("HMAC", key, new TextEncoder().encode(message));
  return hex(sig);
}

async function sha256(s) {
  return hex(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(s)));
}

function hex(buf) {
  return [...new Uint8Array(buf)].map((b) => b.toString(16).padStart(2, "0")).join("");
}

function newId() {
  const alphabet = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ";
  const bytes = crypto.getRandomValues(new Uint8Array(6));
  let s = "R";
  for (const b of bytes) s += alphabet[b % alphabet.length];
  return s;
}

function nowSec() {
  return Math.floor(Date.now() / 1000);
}

function dayString(sec) {
  return new Date(sec * 1000).toISOString().slice(0, 10);
}

// ---------- admin page (served by the Worker; protect /admin* with Cloudflare Access) ----------

const ADMIN_HEADERS = {
  "Content-Type": "text/html; charset=utf-8",
  "Content-Security-Policy": "default-src 'none'; script-src 'self'; style-src 'unsafe-inline'; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'",
  "X-Content-Type-Options": "nosniff",
  "Referrer-Policy": "no-referrer",
  "Cache-Control": "no-store",
  "X-Robots-Tag": "noindex",
};

function adminPage() {
  return new Response(`<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>Reader questions: admin</title>
<style>body{font:15px/1.5 system-ui,sans-serif;max-width:980px;margin:2rem auto;padding:0 1rem;color:#1f2a33}
table{border-collapse:collapse;width:100%;margin-top:1rem}td,th{border-bottom:1px solid #e3e8ec;padding:.4rem;text-align:left;vertical-align:top}
input[type=text]{width:100%}button{margin:.1rem}.raw{white-space:pre-wrap;background:#f6f8fa;padding:.3rem}
.muted{color:#5b6876;font-size:.9rem}#msg{margin:.5rem 0;color:#8a3b12}</style></head><body>
<h1>Reader questions</h1>
<p class="muted">Submitted text is untrusted. It is shown here as plain text only. Publish only your own wording.</p>
<p><label>Admin key <input id="key" type="password" autocomplete="off"></label>
<select id="status"><option>NEW</option><option>LISTED</option><option>PICKED</option><option>ANSWERED</option>
<option>EXPIRED</option><option>DECLINED</option></select> <button id="load">Load</button>
<button id="export">Copy export for P0</button></p>
<div id="msg" role="status"></div><div id="info" class="muted"></div>
<table><thead><tr><th>ID</th><th>Submitted text / our title</th><th>Region</th><th>Votes</th><th>Actions</th></tr></thead>
<tbody id="rows"></tbody></table>
<script src="/admin.js"></script></body></html>`, { headers: ADMIN_HEADERS });
}

function adminScript() {
  const js = `(function () {
  var keyEl = document.getElementById("key");
  try { keyEl.value = sessionStorage.getItem("rqk") || ""; } catch (e) {}
  var codes = [];
  function msg(t) { document.getElementById("msg").textContent = t || ""; }
  function api(path, opts) {
    opts = opts || {};
    opts.headers = Object.assign({ "X-Admin-Key": keyEl.value }, opts.headers || {});
    try { sessionStorage.setItem("rqk", keyEl.value); } catch (e) {}
    return fetch(path, opts);
  }
  function el(tag, text) { var e = document.createElement(tag); if (text !== undefined) e.textContent = text; return e; }
  function act(id, body) {
    body.id = id;
    api("/api/admin/update", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) })
      .then(function (r) { return r.json(); }).then(function (o) { msg(o.ok ? "Saved." : "Error: " + o.error); load(); });
  }
  function load() {
    var status = document.getElementById("status").value;
    api("/api/admin/items?status=" + encodeURIComponent(status)).then(function (r) {
      if (r.status === 401) { msg("Wrong admin key."); return null; } return r.json();
    }).then(function (o) {
      if (!o) return;
      codes = o.decline_codes;
      document.getElementById("info").textContent = "Listed now: " + o.listed + " of " + o.max_listed + ".";
      var tbody = document.getElementById("rows"); tbody.replaceChildren();
      o.items.forEach(function (it) {
        var tr = el("tr");
        tr.appendChild(el("td", it.id));
        var td = el("td");
        if (it.raw_text) { var raw = el("div", it.raw_text); raw.className = "raw"; td.appendChild(raw); }
        var title = el("input"); title.type = "text"; title.placeholder = "Our neutral title (10-160 characters)";
        title.value = it.title || ""; td.appendChild(title);
        if (it.reason) td.appendChild(el("div", "Reason: " + it.reason));
        if (it.link) td.appendChild(el("div", "Link: " + it.link));
        tr.appendChild(td);
        tr.appendChild(el("td", it.region || "-"));
        tr.appendChild(el("td", String(it.votes)));
        var a = el("td");
        function btn(label, fn) { var b = el("button", label); b.type = "button"; b.addEventListener("click", fn); a.appendChild(b); }
        if (it.status === "NEW") {
          btn("List", function () { act(it.id, { action: "list", title: title.value }); });
          var sel = el("select"); codes.forEach(function (c) { sel.appendChild(el("option", c)); }); a.appendChild(sel);
          btn("Decline", function () { act(it.id, { action: "decline", reason: sel.value }); });
        }
        if (it.status === "LISTED") {
          btn("Save title", function () { act(it.id, { action: "retitle", title: title.value }); });
          btn("Picked", function () { act(it.id, { action: "pick" }); });
        }
        if (it.status === "LISTED" || it.status === "PICKED") {
          btn("Answered", function () {
            var link = prompt("Post URL (https://mithril-metrics.github.io/...)");
            if (link) act(it.id, { action: "answer", link: link });
          });
        }
        btn("Delete", function () { if (confirm("Delete " + it.id + " and its votes?")) act(it.id, { action: "delete" }); });
        tr.appendChild(a);
        tbody.appendChild(tr);
      });
      if (!o.items.length) msg("No items with status " + status + ".");
    });
  }
  document.getElementById("load").addEventListener("click", function () { msg(""); load(); });
  document.getElementById("export").addEventListener("click", function () {
    api("/api/admin/export").then(function (r) { return r.ok ? r.text() : null; }).then(function (t) {
      if (t === null) { msg("Wrong admin key."); return; }
      navigator.clipboard.writeText(t).then(function () { msg("Export copied. Paste it into the P0 chat."); },
        function () { msg("Copy failed; export printed below."); var pre = el("pre", t); document.body.appendChild(pre); });
    });
  });
})();`;
  return new Response(js, { headers: { "Content-Type": "text/javascript; charset=utf-8", "Cache-Control": "no-store", "X-Content-Type-Options": "nosniff" } });
}
