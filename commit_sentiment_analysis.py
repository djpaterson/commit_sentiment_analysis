import tempfile
import os
import subprocess
import sys
from pydriller import RepositoryMining
from textblob import TextBlob
from pathlib import Path

if len(sys.argv) != 5:
    raise Exception("Usage: python3 commit_sentiment_analysis.py <working_dir> <commit_hash>")

project = sys.argv[1]
working_dir = sys.argv[2]
fix_commits = sys.argv[3].split(":")
output_file=sys.argv[4]

with open(output_file, 'w+') as outfile:
    outfile.write("project,commit_hash,commit_msg,file_changed,sentiment_score,bug_fix_commit\n")
    for commit in RepositoryMining(working_dir).traverse_commits():
        files_changed = [ (x.new_path if x.new_path is not None else x.old_path) for x in commit.modifications ]
        blob = TextBlob(commit.msg)
        sentiment = blob.sentences[0].polarity
        for file in files_changed:
            outfile.write("{},{},{},{},{},{}\n".format(project,commit.hash,commit.msg.replace("\n","\\n").replace(",","."),file,sentiment,commit.hash in fix_commits))
    outfile.close()
