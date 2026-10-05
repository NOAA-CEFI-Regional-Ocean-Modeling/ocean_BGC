"""mdx_bib citations for FORD, rendered author-year.

FORD passes ``md_extensions`` by name only (no extension configs), so this
wrapper points mdx_bib at ``references.bib`` next to this file. It also clears
citations on ``reset()``, which FORD calls before each entity's docstring;
otherwise every entity's bibliography would include all earlier citations.

Citations use pandoc syntax:

    [@garcia1992]                -> (Garcia and Gordon, 1992)
    [e.g., @waite1992; @smayda1971] -> (e.g., Waite et al., 1992; Smayda, 1971)
    Garcia and Gordon [-@garcia1992] -> Garcia and Gordon (1992)
"""

import re
import xml.etree.ElementTree as etree
from pathlib import Path
from typing import Any

from markdown import Markdown
from markdown.inlinepatterns import InlineProcessor
from mdx_bib import Bibliography, CitationsExtension
from pybtex.database import Entry, Person

BIBTEX_FILE = Path(__file__).with_name('references.bib')

# A bracketed group containing at least one @key, e.g. "[see @a; -@b]"
CITATION_GROUP_RE = r'\[([^\[\]]*@\w+[^\[\]]*)\]'
CITE_KEY_RE = re.compile(r'(-?)@(\w+)')


def last_name(person: Person) -> str:
    return ' '.join(person.prelast_names + person.last_names)


class AuthorYearBibliography(Bibliography):
    """mdx_bib's bibliography with author-year labels and DOI links."""

    def names_and_year(self, citekey: str) -> tuple[str, str] | None:
        """("Garcia and Gordon", "1992"), or None if the key isn't in the bib file."""
        entry = self.bibsource.get(citekey)
        if entry is None:
            return None
        authors = entry.persons.get('author', [])
        if not authors:
            return None
        if len(authors) == 1:
            names = last_name(authors[0])
        elif len(authors) == 2:
            names = f'{last_name(authors[0])} and {last_name(authors[1])}'
        else:
            names = f'{last_name(authors[0])} et al.'
        return names, entry.fields.get('year', 'n.d.')

    def label(self, citekey: str, suppress_author: bool = False) -> str:
        found = self.names_and_year(citekey)
        if found is None:
            return citekey
        names, year = found
        return year if suppress_author else f'{names}, {year}'

    def formatAuthor(self, author: Person) -> str:
        # mdx_bib uses only the first word of the last name
        # ("Boyer" for "de Boyer Montégut")
        initials = ''.join(
            f'{name[0]}.' for name in author.first_names + author.middle_names
        )
        return f'{last_name(author)} {initials}'

    def formatReference(self, ref: Entry) -> str:
        html = super().formatReference(ref)
        doi = ref.fields.get('doi')
        if doi:
            html = html.replace(
                '</p>', f' <a href="https://doi.org/{doi}">doi:{doi}</a></p>'
            )
        return html

    def makeBibliography(self, root: etree.Element) -> etree.Element:
        div = super().makeBibliography(root)
        for row in div.iter('tr'):
            found = self.names_and_year(row[0].text)
            if found:
                row[0].text = '{} ({})'.format(*found)
        return div


class AuthorYearCitationPattern(InlineProcessor):
    """Replace a whole "[... @key ...]" group with "(... Author, Year ...)"."""

    def __init__(self, pattern: str, bibliography: AuthorYearBibliography) -> None:
        super().__init__(pattern)
        self.bib = bibliography

    def handleMatch(
        self, m: re.Match[str], data: str
    ) -> tuple[etree.Element, int, int]:
        text = m.group(1)
        keys = list(CITE_KEY_RE.finditer(text))
        span = etree.Element('span')
        span.set('class', 'citation')
        span.text = '(' + text[: keys[0].start()]
        for i, key in enumerate(keys):
            link = etree.SubElement(span, 'a')
            link.set('href', '#' + self.bib.referenceID(key.group(2)))
            link.text = self.bib.label(key.group(2), suppress_author=bool(key.group(1)))
            is_last = i + 1 == len(keys)
            link.tail = text[
                key.end() : len(text) if is_last else keys[i + 1].start()
            ] + (')' if is_last else '')
        return span, m.start(0), m.end(0)


class FordCitationsExtension(CitationsExtension):
    bib: AuthorYearBibliography

    def __init__(self, **kwargs: Any) -> None:
        kwargs.setdefault('bibtex_file', str(BIBTEX_FILE))
        super().__init__(**kwargs)
        self.bib = AuthorYearBibliography(
            self, self.getConfig('bibtex_file'), self.getConfig('order')
        )

    def extendMarkdown(self, md: Markdown) -> None:
        super().extendMarkdown(md)
        # Replaces mdx_bib's key-only pattern;
        # must outrank "reference" (170) and "link" (160)
        md.inlinePatterns.register(
            AuthorYearCitationPattern(CITATION_GROUP_RE, self.bib), 'mdx_bib', 175
        )

    def reset(self) -> None:
        self.bib.citations.clear()
        self.bib.references.clear()


def makeExtension(**kwargs: Any) -> FordCitationsExtension:
    return FordCitationsExtension(**kwargs)


if __name__ == '__main__':
    import markdown

    html = markdown.markdown(
        'A [@garcia1992]. Dunne et al. [-@dunne2007]. B [e.g., @waite1992; @smayda1971]. '
        'C [@local1]. Not a cite [x].\n\n[@local1]: Manual ref.',
        extensions=[makeExtension()],
    )
    assert '(<a href="#ref-garcia1992">Garcia and Gordon, 1992</a>)' in html, html
    assert (
        'Dunne et al. <span class="citation">(<a href="#ref-dunne2007">2007</a>)</span>'
        in html
    ), html
    assert (
        '(e.g., <a href="#ref-waite1992">Waite et al., 1992</a>; <a href="#ref-smayda1971">Smayda, 1971</a>)'
        in html
    ), html
    assert '(<a href="#ref-local1">local1</a>)' in html and 'Not a cite [x]' in html, (
        html
    )
    assert '<td>Garcia and Gordon (1992)</td>' in html and 'Montégut' not in html, html
    print('ok')
