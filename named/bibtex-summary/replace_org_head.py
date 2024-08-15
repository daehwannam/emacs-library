
import os
import re
# import shutil
# import time
import glob
import argparse

# EXAMPLE_BIB = '''
# @inproceedings{DBLP:conf/emnlp/WolfDSCDMCRLFDS20,
#   author       = {Thomas Wolf and
#                   Lysandre Debut and
#                   Victor Sanh and
#                   Julien Chaumond and
#                   Clement Delangue and
#                   Anthony Moi and
#                   Pierric Cistac and
#                   Tim Rault and
#                   R{\'{e}}mi Louf and
#                   Morgan Funtowicz and
#                   Joe Davison and
#                   Sam Shleifer and
#                   Patrick von Platen and
#                   Clara Ma and
#                   Yacine Jernite and
#                   Julien Plu and
#                   Canwen Xu and
#                   Teven Le Scao and
#                   Sylvain Gugger and
#                   Mariama Drame and
#                   Quentin Lhoest and
#                   Alexander M. Rush},
#   editor       = {Qun Liu and
#                   David Schlangen},
#   title        = {Transformers: State-of-the-Art Natural Language Processing},
#   booktitle    = {Proceedings of the 2020 Conference on Empirical Methods in Natural
#                   Language Processing: System Demonstrations, {EMNLP} 2020 - Demos,
#                   Online, November 16-20, 2020},
#   pages        = {38--45},
#   publisher    = {Association for Computational Linguistics},
#   year         = {2020},
#   url          = {https://doi.org/10.18653/v1/2020.emnlp-demos.6},
#   doi          = {10.18653/v1/2020.emnlp-demos.6},
#   timestamp    = {Mon, 28 Aug 2023 21:17:11 +0200},
#   biburl       = {https://dblp.org/rec/conf/emnlp/WolfDSCDMCRLFDS20.bib},
#   bibsource    = {dblp computer science bibliography, https://dblp.org},
#   org-head  = {  Software|
#                Software->AI|
#                System},
#   org-cmt   = {Hugging Face's transformers},
#   pdfurl    = {},
#   code-url  = {https://github.com/huggingface/transformers},
# }
# '''


ORG_HEAD_EXPR_REGEX = re.compile(r'(.*)(org-head *= *{[^{}]*})(.*)', flags=re.DOTALL)
ORG_HEAD_SPLIT_REGEX = re.compile(r'(\s*\|\s*)')
# ORG_HEAD_FIRST_SPLIT_REGEX = re.compile(r'(\s+)')

def replace_heading_path(bib, old_org_heading_path, new_org_heading_path):
    # By default, '.' doesn't match with a new line, but it can match any character with re.DOTALL.
    match_obj = ORG_HEAD_EXPR_REGEX.match(bib)

    group_1 = match_obj.group(1)
    group_2 = match_obj.group(2)
    group_3 = match_obj.group(3)

    if old_org_heading_path not in group_2:
        replaced = False
        return replaced, bib

    start_idx = group_2.index('{') + 1
    end_idx = len(group_2) - 1
    assert group_2[end_idx] == '}'

    def update(full_org_heading_path):
        return re.sub(r'^{}'.format(old_org_heading_path), new_org_heading_path, full_org_heading_path)

    org_head_splits = ORG_HEAD_SPLIT_REGEX.split(group_2[start_idx: end_idx])
    updated_org_head_splits = []
    org_head_first_match_obj = re.match(r'\S.*', org_head_splits[0])
    org_head_first_split_prefix = org_head_splits[0][:org_head_first_match_obj.span()[0]]
    # org_head_first_sub_splits = ORG_HEAD_FIRST_SPLIT_REGEX.split(org_head_splits[0])
    # if len(org_head_first_sub_splits) > 1:
    #     assert len(org_head_first_sub_splits) == 3
    #     org_head_first_split_prefix = org_head_first_sub_splits[0] + org_head_first_sub_splits[1]
    # else:
    #     org_head_first_split_prefix = ''
    new_org_head_first_split = org_head_first_split_prefix + \
        update(org_head_first_match_obj.group())
        # org_head_first_sub_splits[-1].replace(old_org_heading_path, new_org_heading_path)
    updated_org_head_splits.append(new_org_head_first_split)

    if len(org_head_splits) > 2:
        for idx, org_head_split in enumerate(org_head_splits[1:]):
            if idx % 2 == 0:
                updated_org_head_split = org_head_split
            else:
                # updated_org_head_split = org_head_split.replace(old_org_heading_path, new_org_heading_path)
                updated_org_head_split = update(org_head_split)
            updated_org_head_splits.append(updated_org_head_split)

    updated_org_head = ''.join(updated_org_head_splits)

    new_group_2 = group_2[:start_idx] + updated_org_head + group_2[end_idx:]

    replaced = True
    new_bib = group_1 + new_group_2 + group_3

    return replaced, new_bib


def update_bib_files(bib_dir_path, old_org_heading_path, new_org_heading_path):
    num_replaces = 0
    bib_paths = glob.glob(os.path.join(bib_dir_path, '*.bib'))
    for bib_path in bib_paths:
        with open(bib_path) as f:
            bib = f.read()
        replaced, new_bib = replace_heading_path(bib, old_org_heading_path, new_org_heading_path)
        if replaced:
            num_replaces += 1
            with open(bib_path, 'w') as f:
                f.write(new_bib)
    print(f'{num_replaces} files are updated.')


def update_priority_file(file_path, old_org_heading_path, new_org_heading_path):
    new_lines = []

    def update(full_org_heading_path):
        return re.sub(r'^{}'.format(old_org_heading_path), new_org_heading_path, full_org_heading_path)

    with open(file_path) as f:
        for line in f:
            new_lines.append(update(line))

    # num_duplicates = 0
    # backup_file_path = file_path + '.' + str(num_duplicates)
    # while os.path.exist(backup_file_path):
    #     num_duplicates += 1
    #     backup_file_path = file_path + '.' + str(num_duplicates)
    # shutil.copyfile(file_path, backup_file_path)

    with open(file_path, 'w') as f:
        f.write(''.join(new_lines))


# print(replace_heading_path(EXAMPLE_BIB, 'Software', 'Application')[1])


def update_for_org_head(
        bib_dir_path,
        priority_file_path,
        old_org_heading_path,
        new_org_heading_path
):
    update_bib_files(bib_dir_path, old_org_heading_path, new_org_heading_path)
    update_priority_file(priority_file_path, old_org_heading_path, new_org_heading_path)


if __name__ == '__main__':
    # BIB_DIR_PATH = './bib'
    # PRIORITY_FILE_PATH = './priority.txt'

    parser = argparse.ArgumentParser()
    parser.add_argument('bib_dir_path')
    parser.add_argument('priority_file_path')
    parser.add_argument('old_org_heading_path')
    parser.add_argument('new_org_heading_path')
    args = parser.parse_args()

    update_for_org_head(
        args.bib_dir_path, args.priority_file_path,
        args.old_org_heading_path, args.new_org_heading_path)
