
from bibtex_summary.bib2org import convert_bibtex_to_org
from bibtex_summary.resource_download import download_pdf_in_bibtex


# special symbols
CLOVER_SYMBOL = '♣'  # or club symbol
STAR_SYMBOL = '✸'
DIAMOND_SYMBOL = '⯁'

# symbol mapping
SYMBOL_KWARGS = dict(
    bib_source_symbol=CLOVER_SYMBOL,
    code_link_symbol=STAR_SYMBOL,
    note_link_symbol=DIAMOND_SYMBOL,
)

ORG_PREAMBLE = r'''
#+TITLE: Paper collection
#+SUBTITLE: /since 2020/
#+AUTHOR: Daehwan Nam
'''

ORG_BIBLIOGRAPHY = r'''
\bibliographystyle{apalike}
\bibliography{paper-bibliography}
'''

NOTATION_SECTION = f'''
* Notation
- {SYMBOL_KWARGS['bib_source_symbol']}: The link to .bib file
- {SYMBOL_KWARGS['code_link_symbol']}: The link to code
- {SYMBOL_KWARGS['note_link_symbol']}: The link to note
'''


if __name__ == '__main__':
    bib_file_path = './paper-bibliography.bib'
    note_dir_path = './note'
    convert_bibtex_to_org(
        bib_file_path=bib_file_path,
        org_preamble=ORG_PREAMBLE,
        org_bibliography=ORG_BIBLIOGRAPHY,
        org_local_variable_code=None,
        notation_section=NOTATION_SECTION,
        **SYMBOL_KWARGS,
        note_dir_path=note_dir_path,
    )

    pdf_dir_path = './pdf'
    pdf_url_tag = 'pdfurl'
    download_pdf_in_bibtex(
        bib_file_path=bib_file_path,
        pdf_dir_path=pdf_dir_path,
        pdf_url_tag=pdf_url_tag)
