
import os

import bibtexparser
# import urllib.request
import requests
from tqdm.contrib.concurrent import process_map

from .utility import entry_id_to_file_name


def download_url(url, file_path=None, dir_path=None):
    # https://www.quickprogrammingtips.com/python/how-to-download-multiple-files-concurrently-in-python.html
    if file_path is None:
        # assumes that the last segment after the / represents the file name
        # if the url is http://abc.com/xyz/file.txt, the file name will be file.txt
        file_name_start_pos = url.rfind("/") + 1
        file_name = url[file_name_start_pos:]

        if dir_path is None:
            file_path = file_name
        else:
            file_path = os.path.join(dir_path, file_name)
    else:
        assert dir_path is None

    r = requests.get(url, stream=True)
    if r.status_code == requests.codes.ok:
        with open(file_path, 'wb') as f:
            for data in r:
                f.write(data)


def download_pdf_of_entry(kwargs):
    def func(entry, pdf_dir_path, pdf_url_tag, pdf_url_extra_tag):
        pdf_filename = entry_id_to_file_name(entry['ID']) + ".pdf"
        pdf_file_path = os.path.join(pdf_dir_path, pdf_filename)
        if not os.path.isfile(pdf_file_path) and (entry[pdf_url_tag] or entry.get(pdf_url_extra_tag)):
            pdf_url = entry[pdf_url_tag] or entry.get(pdf_url_extra_tag)
            try:
                download_url(pdf_url, file_path=pdf_file_path)
            except requests.exceptions.ConnectionError:
                print(f'''Error: Connection aborted while downloading entry {entry['ID']} from {pdf_url}''')
    func(**kwargs)


def download_pdf_in_bibtex(
        bib_file_path,
        pdf_dir_path,
        pdf_url_tag='pdfurl',
        pdf_url_extra_tag='pdfurl-extra'
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

    if not os.path.exists(pdf_dir_path):
        os.makedirs(pdf_dir_path)

    entries = bibtex_database.entries
    num_processes = 8
    process_map(download_pdf_of_entry,
                [dict(entry=entry, pdf_dir_path=pdf_dir_path,
                      pdf_url_tag=pdf_url_tag, pdf_url_extra_tag=pdf_url_extra_tag)
                 for entry in entries],
                max_workers=num_processes)
