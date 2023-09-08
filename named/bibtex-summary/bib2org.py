
import argparse
import os
import re
from collections import OrderedDict
from itertools import chain

import bibtexparser

from .utility import entry_id_to_file_name


HARD_INDENTATION = False
INDENT_CHAR = ' ' if HARD_INDENTATION else ''


def get_org_structure(entries):
    org_structure = OrderedDict(type='root')
    for entry in entries:
        org_heading_tuples = [tuple(head_str.strip() for head_str in head_list_str.split('->'))
                              for head_list_str in entry['org-head'].split('|')]
        for org_heading_tuple in org_heading_tuples:
            org_substructure = org_structure
            for org_heading in org_heading_tuple:
                org_substructure = org_substructure.setdefault(org_heading, OrderedDict(type='heading'))
            assert entry['ID'] not in org_substructure
            org_substructure[entry['ID']] = OrderedDict(entry=entry, type='entry')
    return org_structure


def save_org_structure(
        org_structure, *,
        org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
        bib_source_symbol, code_link_symbol, note_link_symbol,
        note_dir_path):
    output_list = []

    def append_new_line():
        if not (len(output_list) > 1 and output_list[-2] == output_list[-1] == '\n'):
            output_list.append('\n')

    white_space_regex = re.compile(r'\s+')

    def normalize_str(s):
        return white_space_regex.sub(' ', s)

    def make_info_unit(text, org_mark=''):
        return '{org_mark}{content}{org_mark}'.format(
            org_mark=org_mark, content=normalize_str(text))

    def is_empty_content(content):
        return len(content.strip()) == 0

    def has_content(entry, key):
        if key in entry:
            return not is_empty_content(entry[key])
        else:
            return False

    def get_entry_info(entry):
        return " | ".join(chain(
            [make_info_unit(entry['org-cmt'], '~')] if has_content(entry, 'org-cmt') else [],
            [make_info_unit(entry['title'].replace('{', '').replace('}', ''))]))

    def get_entry_bib_link(entry):
        return r'[[bibs-bib-id:{id}][{bib_source_symbol}]]'.format(
            id=entry['ID'], bib_source_symbol=bib_source_symbol)

    def get_entry_code_link(entry):
        if has_content(entry, 'code-url'):
            return (''.join(f'[[bibs-code-url:{url}][{code_link_symbol}]]'
                            for url in entry['code-url'].split()))
        else:
            return None

    def get_entry_note_link(entry):
        entry_note_path = os.path.join(note_dir_path, entry_id_to_file_name(entry['ID']) + '.org')
        if os.path.isfile(entry_note_path):
            entry_bib_link = f'[[bibs-note-file:{entry_note_path}][{note_link_symbol}]]'
            return entry_bib_link
        else:
            return None

    def update_output(org_structure, heading_level):
        if org_structure['type'] == 'entry':
            entry = org_structure['entry']
            output_list.append(INDENT_CHAR * (heading_level - 1) + '- ')
            # output_list.append(r'\cite{{{id}}}: {info}'.format(
            #     id=entry['ID'],
            #     info=" | ".join(chain(
            #         ([make_info_unit(entry['org-cmt'], '~')] if entry['org-cmt'] else []),            #         [make_info_unit(entry['title'])]
            #     ))))

            entry_items = [get_entry_info(entry),
                           get_entry_bib_link(entry),
                           get_entry_code_link(entry),
                           get_entry_note_link(entry)]

            output_list.append(' '.join(s for s in entry_items if s != None))
        else:
            for heading, body in org_structure.items():
                if heading == 'type':
                    continue
                if body['type'] == 'heading':
                    output_list.append('*' * (heading_level + 1) + ' ')
                    output_list.append(heading)
                    append_new_line()
                update_output(body, heading_level + 1)
                append_new_line()

    output_list.append(org_preamble)
    if notation_section:
        append_new_line()
        output_list.append(notation_section)
    append_new_line()
    update_output(org_structure, 0)
    append_new_line()
    output_list.append(org_bibliography)
    if org_local_variable_code is not None:
        append_new_line()
        output_list.append(org_local_variable_code)

    with open('paper-collection.org', 'w') as f:
        for output in output_list:
            f.write(output)


def convert_bibtex_to_org(
        bib_file_path, *,
        org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
        bib_source_symbol, code_link_symbol, note_link_symbol,
        note_dir_path,
):
    # bib_file_path = 'paper-bibliography.bib'
    with open(bib_file_path) as bibtex_file:
        bibtex_str = bibtex_file.read()

    bibtex_database = bibtexparser.loads(bibtex_str)

    org_structure = get_org_structure(bibtex_database.entries)
    save_org_structure(org_structure,
                       org_preamble=org_preamble,
                       org_bibliography=org_bibliography,
                       org_local_variable_code=org_local_variable_code,
                       notation_section=notation_section,
                       bib_source_symbol=bib_source_symbol,
                       code_link_symbol=code_link_symbol,
                       note_link_symbol=note_link_symbol,
                       note_dir_path=note_dir_path)
