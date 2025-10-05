
import argparse
import os
import re
from collections import OrderedDict, defaultdict
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

    def sorted_substructures(self, priority_structure):
        substructures = list(self.iter_substructures())

        def key(substructure):
            return priority_structure.get_substructure_priority(substructure.heading)

        return sorted(substructures, key=key)

    def sorted_substructures_if_possible(self, priority_structure):
        if priority_structure is None:
            return self.iter_substructures()
        else:
            return self.sorted_substructures(priority_structure)

    def add_entry(self, entry):
        assert entry['ID'] not in self.entry_ids, f"Duplicate bibtex files or duplicate org-head values exist for {entry['ID']}"
        self.entry_ids.add(entry['ID'])
        self.entries.append(entry)
        return entry

    def iter_entries(self):
        yield from self.entries

    def sorted_entries(self):
        def key(entry):
            return int(entry.get('year', UNKNOWN_YEAR))

        return sorted(self.entries, key=key)


class PriorityStructure:
    def __init__(self, heading, priority):
        self.heading = heading
        self.priority = priority
        self.substructure_dict = {}

    def add_sub_heading(self, sub_heading):
        if sub_heading not in self.substructure_dict:
            self.substructure_dict[sub_heading] = PriorityStructure(sub_heading, len(self.substructure_dict))
        return self.substructure_dict[sub_heading]

    def get_substructure_priority(self, sub_heading):
        if sub_heading in self.substructure_dict:
            return self.substructure_dict[sub_heading].priority
        else:
            return float('inf')

    def get_substructure(self, sub_heading):
        return self.substructure_dict.get(sub_heading)

def get_sub_priority_structure(priority_structure, sub_heading):
    if priority_structure is None:
        return None
    else:
        return priority_structure.get_substructure(sub_heading)


def heading_seq_repr_to_tuple(heading_arrow_repr):
    return tuple(heading_str.strip() for heading_str in heading_arrow_repr.split('->'))


def get_root_org_structure(entries):
    org_structure = OrgStructure('<root>')
    for entry in entries:
        if ('org-head' not in entry) or (entry['org-head'].strip() == ''):
            print('Warning: "{}" does not have any "org-head" value.'.format(entry['ID']))
            org_heading_tuples = [('Unclassified',)]
        else:
            org_heading_tuples = [heading_seq_repr_to_tuple(heading_arrow_repr)
                                  for heading_arrow_repr in entry['org-head'].split('|')]
        for org_heading_tuple in org_heading_tuples:
            org_substructure = org_structure
            for org_heading in org_heading_tuple:
                org_substructure = org_substructure.add_sub_heading(org_heading)
            org_substructure.add_entry(entry)
    return org_structure


def get_root_priority_structure(priority_file_path):
    org_heading_tuples = []
    if os.path.isfile(priority_file_path):
        with open(priority_file_path) as f:
            for line in f:
                if line.strip():
                    org_heading_tuples.append(heading_seq_repr_to_tuple(line))

    priority_structure = PriorityStructure('<root>', 0)
    for org_heading_tuple in org_heading_tuples:
        priority_substructure = priority_structure
        for org_heading in org_heading_tuple:
            priority_substructure = priority_substructure.add_sub_heading(org_heading)

    return priority_structure


def save_org_structure(
        root_org_structure,
        root_priority_structure,
        *,
        org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
        bib_source_symbol, code_link_symbol, note_link_symbol,
        note_dir_path, summary_file_path):
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

    def update_output_with_org_structure(heading_level, org_structure, priority_structure):
        # add heading info
        output_list.append('*' * (heading_level + 1) + ' ')
        output_list.append(org_structure.heading)
        append_new_line()

        # entries
        sorted_entries = org_structure.sorted_entries()
        for entry in sorted_entries:
            update_output_with_entry(entry, heading_level)
            append_new_line()

        # substructures
        for org_substructure_idx, org_substructure in enumerate(
                org_structure.sorted_substructures_if_possible(priority_structure)
        ):
            if org_substructure_idx == 0 and len(sorted_entries) > 0:
                append_new_line()
            update_output_with_org_structure(
                heading_level + 1, org_substructure,
                get_sub_priority_structure(priority_structure, org_substructure.heading))
        append_new_line()

    def update_output_with_root_org_structure(org_structure, priority_structure):
        # substructures
        for org_substructure in org_structure.sorted_substructures_if_possible(priority_structure):
            update_output_with_org_structure(
                0, org_substructure,
                get_sub_priority_structure(priority_structure, org_substructure.heading))
        append_new_line()

    output_list.append(org_preamble)
    if notation_section:
        append_new_line()
        output_list.append(notation_section)
    append_new_line()
    update_output_with_root_org_structure(root_org_structure, root_priority_structure)
    append_new_line()
    output_list.append(org_bibliography)
    if org_local_variable_code is not None:
        append_new_line()
        output_list.append(org_local_variable_code)

    with open(summary_file_path, 'w') as f:
        for output in output_list:
            f.write(output)


def convert_bibtex_to_org(
        bib_file_path,
        priority_file_path,
        *,
        org_preamble, org_bibliography, org_local_variable_code=None, notation_section=None,
        bib_source_symbol, code_link_symbol, note_link_symbol,
        note_dir_path, summary_file_path
):
    # bib_file_path = 'paper-bibliography.bib'
    with open(bib_file_path) as bibtex_file:
        bibtex_str = bibtex_file.read()

    # Fix month field error
    # https://github.com/sciunto-org/python-bibtexparser/issues/204#issuecomment-397662093
    #
    # e.g. month = jul
    bibtex_parser = bibtexparser.bparser.BibTexParser(common_strings=True)
    bibtex_database = bibtexparser.loads(bibtex_str, parser=bibtex_parser)

    root_org_structure = get_root_org_structure(bibtex_database.entries)
    root_priority_structure = get_root_priority_structure(priority_file_path)
    save_org_structure(root_org_structure,
                       root_priority_structure,
                       org_preamble=org_preamble,
                       org_bibliography=org_bibliography,
                       org_local_variable_code=org_local_variable_code,
                       notation_section=notation_section,
                       bib_source_symbol=bib_source_symbol,
                       code_link_symbol=code_link_symbol,
                       note_link_symbol=note_link_symbol,
                       note_dir_path=note_dir_path,
                       summary_file_path=summary_file_path)
