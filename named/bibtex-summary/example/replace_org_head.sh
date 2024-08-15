#!/usr/bin/sh

BIB_DIR_PATH='./bib'
PRIORITY_FILE_PATH='./priority.txt'

OLD_ORG_HEADING_PATH=$1
NEW_ORG_HEADING_PATH=$2

python -m bibtex_summary.replace_org_head $BIB_DIR_PATH $PRIORITY_FILE_PATH "$OLD_ORG_HEADING_PATH" "$NEW_ORG_HEADING_PATH"
