
import argparse
import os
import re
from collections import OrderedDict
from itertools import chain

import bibtexparser

from .utility import entry_id_to_file_name


HARD_INDENTATION = False
INDENT_CHAR = ' ' if HARD_INDENTATION else ''

UNKNOWN_YEAR = -1


class OrgStructure:
    def __init__(self, heading):
        self.heading = heading

        self.substructure_dict = {}

        self.entries = []
        self.entry_ids = set()

    def add_sub_heading(self, sub_heading):
        if sub_heading not in self.substructure_dict:
            self.substructure_dict[sub_heading] = OrgStructure(sub_heading)
        return self.substructure_dict[sub_heading]

    def iter_substructures(self):
        return self.substructure_dict.values()

    def add_entry(self, entry):
        assert entry['ID'] not in self.entry_ids
        self.entry_ids.add(entry['ID'])
        self.entries.append(entry)
        return entry

    def iter_entries(self):
        yield from self.entries

    def sorted_entries(self):
        def key(entry):
            return int(entry.get('year', UNKNOWN_YEAR))

        return sorted(self.entries, key=key)


def get_root_org_structure(entries):
    org_structure = OrgStructure('root')
    for entry in entries:
        org_heading_tuples = [tuple(head_str.strip() for head_str in head_list_str.split('->'))
                              for head_list_str in entry['org-head'].split('|')]
        for org_heading_tuple in org_heading_tuples:
            org_substructure = org_structure
            for org_heading in org_heading_tuple:
                org_substructure = org_substructure.add_sub_heading(org_heading)
            org_substructure.add_entry(entry)
    return org_structure


def save_org_structure(
        root_org_structure, *,
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

    def update_output_with_entry(entry, heading_level):
        output_list.append(INDENT_CHAR * heading_level + '- ')
        entry_items = [get_entry_info(entry),
                       get_entry_bib_link(entry),
                       get_entry_code_link(entry),
                       get_entry_note_link(entry)]

        output_list.append(' '.join(s for s in entry_items if s != None))

    def update_output_with_org_structure(org_structure, heading_level):
        # add heading info
        output_list.append('*' * (heading_level + 1) + ' ')
        output_list.append(org_structure.heading)
        append_new_line()

        # entries
        for entry in org_structure.sorted_entries():
            update_output_with_entry(entry, heading_level)
            append_new_line()

        # substructures
        for org_substructure in org_structure.iter_substructures():
            update_output_with_org_structure(org_substructure, heading_level + 1)
        append_new_line()

    def update_output_with_root_org_structure(org_structure):
        # substructures
        for org_substructure in org_structure.iter_substructures():
            update_output_with_org_structure(org_substructure, 0)
        append_new_line()

    output_list.append(org_preamble)
    if notation_section:
        append_new_line()
        output_list.append(notation_section)
    append_new_line()
    update_output_with_root_org_structure(root_org_structure)
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

    root_org_structure = get_root_org_structure(bibtex_database.entries)
    save_org_structure(root_org_structure,
                       org_preamble=org_preamble,
                       org_bibliography=org_bibliography,
                       org_local_variable_code=org_local_variable_code,
                       notation_section=notation_section,
                       bib_source_symbol=bib_source_symbol,
                       code_link_symbol=code_link_symbol,
                       note_link_symbol=note_link_symbol,
                       note_dir_path=note_dir_path)
