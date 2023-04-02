
import argparse
import os
import re
from collections import OrderedDict
from itertools import chain

import bibtexparser


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


def save_org_structure(org_structure, *,
                       org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
                       bib_source_symbol, code_link_symbol):
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

    def update_output(org_structure, heading_level):
        if org_structure['type'] == 'entry':
            entry = org_structure['entry']
            output_list.append(INDENT_CHAR * (heading_level - 1) + '- ')
            # output_list.append(r'\cite{{{id}}}: {info}'.format(
            #     id=entry['ID'],
            #     info=" | ".join(chain(
            #         ([make_info_unit(entry['org-cmt'], '~')] if entry['org-cmt'] else []),
            #         [make_info_unit(entry['title'])]
            #     ))))

            entry_info = " | ".join(chain(
                [make_info_unit(entry['org-cmt'], '~')] if entry['org-cmt'] else [],
                [make_info_unit(entry['title'].replace('{', '').replace('}', ''))]))
            entry_bib_link = r'[[bib-id:{id}][{clover}]]'.format(id=entry['ID'], clover=bib_source_symbol)
            entry_code_links = (''.join(f'[[code-url:{url}][{code_link_symbol}]]' for url in entry['code-url'].split())
                                if 'code-url' in entry else '')
            output_list.append(' '.join(s for s in [entry_info, entry_bib_link, entry_code_links] if s != ''))
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


def convert_bibtex_to_org(bib_file_path, *,
                          org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
                          bib_source_symbol, code_link_symbol):
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
                       code_link_symbol=code_link_symbol)
