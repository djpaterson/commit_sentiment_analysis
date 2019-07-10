#!/usr/bin/env bash

die(){
  echo $1
  exit 1
}


if [[ ! -d "tools/defects4j" ]]; then
  die "Defects4J not found inside tools folder. Did you run setup.sh?"
fi
D4J="tools/defects4j/framework/bin/defects4j"

for project in Chart; do
  last_commit=$(tail -n 1 "tools/defects4j/framework/projects/$project/commit-db")
  max_bug_id=$(echo $last_commit | cut -d',' -f1)
  max_bug_commit_hash=$(echo $last_commit | cut -d',' -f3)
  tmp_dir="/tmp/commitanalysis-$project-$max_bug_id-$$"
  mkdir -p $tmp_dir
  $D4J checkout -p $project -v ${max_bug_id}f -w $tmp_dir
  pushd $tmp_dir > /dev/null 2>&1
    git checkout -b sentiment_analysis $max_bug_commit_hash
  popd > /dev/null 2>&1
  python3 commit_sentiment_analysis.py $tmp_dir
  rm -rf $tmp_dir
done
