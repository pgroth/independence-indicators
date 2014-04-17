import itertools
import os
import csv
import re

INPUT_FILE = 'publications-nov-2013.csv'
OUTPUT = 'processed_pubmed_'

def byAuthor(author):
	author = re.sub(r'[^\x00-\x7F]+',' ', author)
	author_file_name = "".join(author.split())
	if os.path.exists(OUTPUT + author_file_name + '.csv'):
		return
	with open('publications-nov-2013.csv','r') as f:
		csv_in = csv.DictReader(f)
		with open(OUTPUT + author_file_name + '.csv', 'w') as csv_out:
			for d in csv_in:
					try:
							year = int(d['pub_year'])
					except:
							continue
					authors = d['authors'].split('|')
					if author in authors:
						for a1, a2 in itertools.combinations(authors,2):
							csv_out.write(str(a1) + '\t' + str(a2) + '\t' + str(1) + '\t' + str(year) + '\n')