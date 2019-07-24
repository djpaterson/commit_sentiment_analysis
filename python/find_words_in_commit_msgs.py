import csv
import re
from collections import Counter
import sys

args = sys.argv
if len(args) != 3:
  raise Exception("Usage: python3 find_words_in_commit_msgs.py <word> <bug_fix_commit>")

word = args[1]
bug_fix_req = args[2]
word_regex = re.compile(r'\w+')
with open("../data/all_project_sentiment_analysis.csv") as infile:
  csv_reader = csv.reader(infile, delimiter=',')
  line_count = 0
  commits_analysed = 0
  commit_hash=""
  commits_with_word=0
  word_occurrences = 0
  for row in csv_reader:
    if commit_hash == row[1]:
      continue
    commit_hash = row[1]
    line_count += 1
    if line_count == 1:
      continue
    bug_fix = row[6] == bug_fix_req
    if not bug_fix:
      continue
    commits_analysed += 1
    sentence = row[2].replace("\\n", "").lower()
    if word in word_regex.findall(sentence):
      commits_with_word += 1
      word_occurrences += Counter(word_regex.findall(sentence))[word]

print("Number of commits that contain the word '{}': {}".format(word, commits_with_word))
print("Number of occurrences of the word '{}': {}".format(word,word_occurrences))
