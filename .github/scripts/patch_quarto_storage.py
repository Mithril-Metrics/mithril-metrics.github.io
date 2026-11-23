"""Stop Quarto's tabset helper from writing an empty entry to localStorage on every page load.

Quarto's tabsets.js stores "{}" under quarto-persistent-tabsets-data whenever a page opens, even
though the site has no tabsets. This removes that write; clicking a tab (if tabsets are ever
used) still stores the choice, which the visitor then asked for. Runs after rendering, before
publishing. Fails if a Quarto update changes the file so the patch no longer applies.

Usage: python3 -I .github/scripts/patch_quarto_storage.py [site_dir]
"""
import sys
from pathlib import Path

WRITE = 'localStorage.setItem("quarto-persistent-tabsets-data", "{}");'


def main():
    site = Path(sys.argv[1] if len(sys.argv) > 1 else "_site")
    files = list(site.rglob("tabsets.js"))
    if not files:
        print("No tabsets.js found; nothing to patch.")
        return
    for path in files:
        text = path.read_text(encoding="utf-8")
        if WRITE in text:
            path.write_text(text.replace(WRITE, ""), encoding="utf-8")
            print(f"Patched {path}")
        elif '"quarto-persistent-tabsets-data", "{}"' in text.replace("\n", " ").replace("  ", " "):
            print(f"{path}: the empty-entry write has a new form; update this script.")
            sys.exit(1)
        else:
            print(f"{path}: no empty-entry write found.")


if __name__ == "__main__":
    main()
