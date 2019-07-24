import csv
import re
from textblob import TextBlob

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
  commits_analysed = 0
  commit_hash=""
  for row in csv_reader:
    if commit_hash == row[1]:
        continue
    commit_hash = row[1]
    line_count += 1
    if line_count == 1:
      continue
    bug_fix = row[6] == 'False'
    if not bug_fix:
      continue
    commits_analysed += 1
    sentence = row[2].replace("\\n", "").lower()
    for word in word_regex.findall(sentence):
      if not word in stopwords:
        if not word in words:
          words[word] = 0
        words[word] += 1

sorted_words = sorted(words.items(), key = lambda kv: kv[1])
for word in sorted_words:
    blob = TextBlob(word[0])
    print("Word: {}, Count: {}, Sentiment: {}, Subjectivity: {}".format(word[0], word[1], blob.sentiment.polarity, blob.sentiment.subjectivity))
print("Commits analysed: {}".format(commits_analysed))


# cat ../data/all_project_sentiment_analysis.csv | grep -E "[^,]+,[^,]+,\".*fixed.*+\",[^,]+,[^,]+,[^,]+,True" | cut -d',' -f3 | uniq | wc -l

# cat ../data/all_project_sentiment_analysis.csv | grep -E "[^,]+,[^,]+,\".*fixed.*+\",[^,]+,[^,]+,[^,]+,False" | cut -d',' -f3 | uniq | wc -l
