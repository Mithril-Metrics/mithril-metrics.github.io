"""Fail the build if a rendered page loads anything from another site.

Scans every .html and .css file in the rendered site (default: _site/) for resources the browser
fetches when the page opens: scripts, stylesheets, fonts, images, frames, media and CSS url()/
@import. Plain links (<a href>) are not loaded and are ignored. Hosts listed in
.github/external-allowlist.txt are accepted; anything else stops the build, so a new outside
service cannot reach visitors without a privacy check and an update of privacy.qmd.

Usage: python3 -I .github/scripts/check_external.py [site_dir]
"""
import re
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlparse

OWN_HOSTS = {"mithril-metrics.github.io"}
LOADED_ATTRS = {
    "script": ["src"],
    "img": ["src", "srcset"],
    "iframe": ["src"],
    "frame": ["src"],
    "embed": ["src"],
    "object": ["data"],
    "source": ["src", "srcset"],
    "video": ["src", "poster"],
    "audio": ["src"],
    "track": ["src"],
    "input": ["src"],
}
LOADED_LINK_RELS = {"stylesheet", "preload", "modulepreload", "prefetch", "preconnect",
                    "dns-prefetch", "icon", "shortcut", "apple-touch-icon", "manifest"}
CSS_URL = re.compile(r"""(?:url\(\s*['"]?|@import\s+['"])([^'")\s]+)""", re.I)


def external_host(url):
    url = url.strip()
    if url.startswith("//"):
        url = "https:" + url
    parsed = urlparse(url)
    if parsed.scheme not in ("http", "https"):
        return None
    host = (parsed.hostname or "").lower()
    if not host or host in OWN_HOSTS:
        return None
    return host


class Collector(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.urls = []
        self.in_style = False

    def handle_starttag(self, tag, attrs):
        a = {k.lower(): (v or "") for k, v in attrs}
        if tag == "link":
            rels = set(a.get("rel", "").lower().split())
            if rels & LOADED_LINK_RELS:
                self.urls.append(a.get("href", ""))
        for attr in LOADED_ATTRS.get(tag, []):
            value = a.get(attr, "")
            if attr == "srcset":
                self.urls.extend(part.strip().split(" ")[0] for part in value.split(",") if part.strip())
            elif value:
                self.urls.append(value)
        if "style" in a:
            self.urls.extend(CSS_URL.findall(a["style"]))
        if tag == "style":
            self.in_style = True

    def handle_endtag(self, tag):
        if tag == "style":
            self.in_style = False

    def handle_data(self, data):
        if self.in_style:
            self.urls.extend(CSS_URL.findall(data))


def main():
    site = Path(sys.argv[1] if len(sys.argv) > 1 else "_site")
    allow_file = Path(".github/external-allowlist.txt")
    allowed = set()
    if allow_file.exists():
        for line in allow_file.read_text(encoding="utf-8").splitlines():
            line = line.split("#")[0].strip().lower()
            if line:
                allowed.add(line)
    problems = {}
    for path in sorted(site.rglob("*")):
        if path.suffix not in (".html", ".css"):
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        if path.suffix == ".css":
            urls = CSS_URL.findall(text)
        else:
            collector = Collector()
            collector.feed(text)
            urls = collector.urls
        for url in urls:
            host = external_host(url)
            if host and host not in allowed:
                problems.setdefault(host, set()).add(str(path.relative_to(site)))
    if problems:
        print("Pages load resources from hosts that are not in .github/external-allowlist.txt:")
        for host, files in sorted(problems.items()):
            print(f"  {host}: {', '.join(sorted(files)[:5])}" + (" ..." if len(files) > 5 else ""))
        print("Host the resource on the site, or add the host to the allow-list and to privacy.qmd.")
        sys.exit(1)
    print(f"No unapproved outside resources in {site}/.")


if __name__ == "__main__":
    main()
