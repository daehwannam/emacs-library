import os
import re
import glob
import argparse

from .utility import entry_id_to_file_name


def bib_merge(merged_bib_file_path, bib_dir_path):
    lines = []
    paths = glob.glob(os.path.join(bib_dir_path, '*.bib'))
    for idx, path in enumerate(paths):
        with open(path) as f:
            lines.extend(f)
            if idx + 1 < len(paths):
                lines.append('\n\n')

    with open(merged_bib_file_path, 'w') as f:
        for line in lines:
            f.write(line)


entry_start_re = re.compile(r'@\s*([a-zA-Z])+\s*{+\s*(\S+)+\s*,')

def bib_split(merged_bib_file_path, bib_dir_path):
    bib_ids = []
    bib_texts = []
    bib_lines = []

    def lines2text():
        text = ''.join(bib_lines).strip()
        bib_texts.append(text)

    with open(merged_bib_file_path) as f:
        for line in f:
            match = entry_start_re.match(line)
            if (match is not None) and (match.group(1) != 'Comment'):
                if len(bib_ids) > 0:
                    lines2text()
                bib_lines = [line]

                bib_ids.append(match.group(2))
            else:
                bib_lines.append(line)

        assert len(bib_lines) > 0
        lines2text()

    assert len(bib_ids) == len(bib_texts)

    for bib_id, bib_text in zip(bib_ids, bib_texts):
        if not os.path.isdir(bib_dir_path):
            os.makedirs(bib_dir_path)
        bib_file_path = os.path.join(bib_dir_path, entry_id_to_file_name(bib_id) + '.bib')
        if os.path.isfile(bib_file_path):
            raise Exception(f'The file already exists: "{bib_file_path}"')
        with open(bib_file_path, 'w') as f:
            f.write(bib_text)


if __name__ == '__main__':
    # MERGED_BIB_FILE_PATH = './bibliography.bib'
    # BIB_DIR_PATH = './bib'

    parser = argparse.ArgumentParser()
    parser.add_argument('merged_bib_file_path')
    parser.add_argument('bib_dir_path')
    args = parser.parse_args()

    bib_merge(args.merged_bib_file_path, args.bib_dir_path)
