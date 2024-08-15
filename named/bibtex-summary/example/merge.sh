#!/usr/bin/sh

MERGED_BIB_FILE_PATH='./bibliography.bib'
BIB_DIR_PATH='./bib'

python -m bibtex_summary.bib_merge $MERGED_BIB_FILE_PATH $BIB_DIR_PATH
