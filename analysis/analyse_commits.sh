cat "commits_to_analyse.csv" | while read -r commit; do
  commit_hash=$(echo $commit | cut -d',' -f1);
  contains_faulty_class=$(echo $commit | cut -d',' -f4);
  if [[ "$contains_faulty_class" == "FALSE" ]]; then
    continue;
  fi
  IFS=":"
  read -ra FILES <<< $(echo $commit | cut -d',' -f5);
  for i in "${FILES[@]}"; do
    files=$(echo $i | xargs)
    IFS=' '
    for file in "${files[@]}"; do
      echo $file
    done
  done
done
