
from bibtex_summary.bib2org import convert_bibtex_to_org
from bibtex_summary.resource_download import download_pdf_in_bibtex
from bibtex_summary.bib_merge import bib_merge, bib_filter


# special symbols
CLOVER_SYMBOL = '♣'  # or club symbol
STAR_SYMBOL = '✸'
DIAMOND_SYMBOL = '■'

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
\bibliography{bibliography}
'''

NOTATION_SECTION = f'''
* Notation
- {SYMBOL_KWARGS['bib_source_symbol']}: The link to .bib file
- {SYMBOL_KWARGS['code_link_symbol']}: The link to code
- {SYMBOL_KWARGS['note_link_symbol']}: The link to note
'''

MERGED_FULL_BIB_FILE_PATH = './bibliography-full.bib'
MERGED_SIMPLE_BIB_FILE_PATH = './bibliography.bib'
PRIORITY_FILE_PATH = './priority.txt'
BIB_DIR_PATH = './bib'
NOTE_DIR_PATH = './note'
SUMMARY_FILE_PATH = './bibliography.org'
PDF_DIR_PATH = './pdf'
PDF_URL_TAG = 'pdfurl'
PDF_URL_EXTRA_TAG = 'pdfurl-extra'



if __name__ == '__main__':
    bib_merge(MERGED_FULL_BIB_FILE_PATH, BIB_DIR_PATH)

    convert_bibtex_to_org(
        bib_file_path=MERGED_FULL_BIB_FILE_PATH,
        priority_file_path=PRIORITY_FILE_PATH,
        org_preamble=ORG_PREAMBLE,
        org_bibliography=ORG_BIBLIOGRAPHY,
        org_local_variable_code=None,
        notation_section=NOTATION_SECTION,
        **SYMBOL_KWARGS,
        note_dir_path=NOTE_DIR_PATH,
        summary_file_path=SUMMARY_FILE_PATH
    )

    download_pdf_in_bibtex(
        bib_file_path=MERGED_FULL_BIB_FILE_PATH,
        pdf_dir_path=PDF_DIR_PATH,
        pdf_url_tag=PDF_URL_TAG,
        pdf_url_extra_tag=PDF_URL_EXTRA_TAG
    )

    bib_filter(MERGED_FULL_BIB_FILE_PATH, MERGED_SIMPLE_BIB_FILE_PATH)
