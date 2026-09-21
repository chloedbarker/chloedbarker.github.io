"""Check local page links, assets, fragment targets, and document landmarks."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit, unquote

ROOT = Path(__file__).resolve().parents[1]
class Document(HTMLParser):
    def __init__(self, path):
        super().__init__()
        self.refs, self.ids, self.errors = [], set(), []
        self.main_count = 0
        self.feed(path.read_text())
    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if 'id' in attrs:
            if attrs['id'] in self.ids: self.errors.append('duplicate ID: ' + attrs['id'])
            self.ids.add(attrs['id'])
        if tag == 'main': self.main_count += 1
        if tag == 'img' and 'alt' not in attrs: self.errors.append('image missing alt')
        for key in ('href', 'src'):
            if key in attrs: self.refs.append(attrs[key])

pages = {p: Document(p) for p in ROOT.rglob('index.html') if '.git' not in p.parts}
errors = []
count = 0
for path, doc in pages.items():
    for error in doc.errors: errors.append(f'{path.relative_to(ROOT)}: {error}')
    if doc.main_count != 1: errors.append(f'{path}: expected one main landmark')
    for ref in doc.refs:
        url = urlsplit(ref)
        if url.scheme or url.netloc: continue
        target = (ROOT / unquote(url.path.lstrip('/')) if url.path.startswith('/') else path.parent / unquote(url.path)) if url.path else path
        target = target.resolve()
        if target.is_dir(): target /= 'index.html'
        count += 1
        if not target.exists(): errors.append(f'{path.relative_to(ROOT)}: missing {ref}')
        elif url.fragment and target in pages and unquote(url.fragment) not in pages[target].ids:
            errors.append(f'{path.relative_to(ROOT)}: missing fragment {ref}')
if errors: raise SystemExit('\n'.join(errors))
print(f'PASS: {len(pages)} pages, {count} local references, landmarks, image alt attributes, and fragment targets.')
