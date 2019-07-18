import tempfile
import os
import subprocess
import sys
import re
from pydriller import RepositoryMining
from textblob import TextBlob
from pathlib import Path

if len(sys.argv) != 5:
    raise Exception("Usage: python3 commit_sentiment_analysis.py <working_dir> <commit_hash>")

project = sys.argv[1]
working_dir = sys.argv[2]
fix_commits = sys.argv[3].split(":")
output_file=sys.argv[4]
line_rex = re.compile(".*\n")
with open(output_file, 'w+') as outfile:
    outfile.write("project,commit_hash,commit_msg,file_changed,sentiment_score,subjectivity_score,bug_fix_commit\n")
    for commit in RepositoryMining(working_dir).traverse_commits():
        files_changed = [ (x.new_path if x.new_path is not None else x.old_path) for x in commit.modifications ]
        commit_filter = ""
        for line in line_rex.findall(commit.msg):
            if line.strip() != '' and not line.startswith("git-svn-id"):
                if commit_filter != '':
                    commit_filter = commit_filter + "\n"
                commit_filter = commit_filter + line.strip()

        blob = TextBlob(commit_filter)
        sentiment = blob.sentiment.polarity
        subjectivity = blob.sentiment.subjectivity
        for file in files_changed:
            outfile.write("{},{},{},{},{},{},{}\n".format(project,commit.hash,commit_filter.replace("\n","\\n").replace(",",".").replace("\"","\\\""),file,sentiment,subjectivity,commit.hash in fix_commits))
    outfile.close()
