import tempfile
import os
import subprocess
import sys
from pydriller import RepositoryMining
from textblob import TextBlob
from pathlib import Path

if len(sys.argv) != 2:
    raise Exception("Usage: python3 commit_sentiment_analysis.py <working_dir> <commit_hash>")

working_dir = sys.argv[1]

sentiments = {}

for commit in RepositoryMining(working_dir).traverse_commits():
    files_changed = [ x.filename for x in commit.modifications ]
    blob = TextBlob(commit.msg)
    sentiment = blob.sentences[0].polarity
    for file in files_changed:
        if not file in sentiments:
            sentiments[file] = []
        sentiments[file].extend([sentiment])

print(sentiments)
