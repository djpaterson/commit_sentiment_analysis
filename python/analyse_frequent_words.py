import csv
import re

words = {}
stopwords = []
with open("stopwords.txt") as stopfile:
  for line in stopfile:
    stopwords.append(line.strip())

text = ""
word_regex = re.compile(r'\w+')
with open("../data/all_project_sentiment_analysis.csv") as infile:
  csv_reader = csv.reader(infile, delimiter=',')
  line_count = 0
  for row in csv_reader:
    line_count += 1
    if line_count == 1:
      continue
    bug_fix = row[6] == 'True'
    if not bug_fix:
      continue
    sentence = row[2].replace("\\n", "").lower()
    for word in word_regex.findall(sentence):
      if not word in stopwords:
        if not word in words:
          words[word] = 0
        words[word] += 1

sorted_words = sorted(words.items(), key = lambda kv: kv[1])
print(sorted_words)