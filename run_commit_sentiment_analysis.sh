#!/usr/bin/env bash

die(){
  echo $1
  exit 1
}

trap SIGINT "die \"Caught signal CTRL+C. Shutting Down\""

if [[ ! -d "tools/defects4j" ]]; then
  die "Defects4J not found inside tools folder. Did you run setup.sh?"
fi
D4J="tools/defects4j/framework/bin/defects4j"

for project in Chart Lang Math Time Mockito Closure; do
  last_commit=$(tail -n 1 "tools/defects4j/framework/projects/$project/commit-db")
  max_bug_id=$(echo $last_commit | cut -d',' -f1)
  max_bug_commit_hash=$(echo $last_commit | cut -d',' -f3)
  all_fix_commits=$(cut -d',' -f3 "tools/defects4j/framework/projects/$project/commit-db" | tr '\n' ':' | perl -p -e "s/(.*):$/\1/g")
  tmp_dir="/tmp/commitanalysis-$project-$max_bug_id-$$"
  mkdir -p $tmp_dir
  $D4J checkout -p $project -v ${max_bug_id}f -w $tmp_dir
  pushd $tmp_dir > /dev/null 2>&1
    if [[ $project == "Math" ]] || [[ $project == "Lang" ]]; then
      git checkout trunk
    else
      git checkout master
    fi
  popd > /dev/null 2>&1
  python3 commit_sentiment_analysis.py $project $tmp_dir $all_fix_commits "data/$project-sentiment-analysis.csv"
  rm -rf $tmp_dir
done

combined_data_file="data/all_project_sentiment_analysis.csv"
rm $combined_data_file
echo "project,commit_hash,commit_msg,file_changed,sentiment_score,subjectivity_score,bug_fix_commit" > $combined_data_file
for project in CHart Lang Math Time Mockito Closure; do
  tail -n +2 "data/$project-sentiment-analysis.csv" >> $combined_data_file
done
