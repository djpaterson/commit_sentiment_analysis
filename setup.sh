die(){
  echo $1
  exit 1
}

D4J_PATH="https://github.com/rjust/defects4j"
if [ ! -d "tools" ]; then
  mkdir "tools"
fi
git clone $D4J_PATH tools/defects4j || die "Could not checkout Defects4J!"
pushd "tools/defects4j" > /dev/null 2>&1
  bash init.sh
  pushd project_repos > /dev/null 2>&1
    wget "https://github.com/jon-bell/defects4j/blob/master/project_repos/jfreechart.git.tgz?raw=true"
    tar -xzf "jfreechart.git.tgz?raw=true"
  popd > /dev/null 2>&1
  wget "https://github.com/jon-bell/defects4j/commit/c8b3d3792331bd989d512bda893e23b21b0aae6e.patch"

  # move to a previous version so that patch can be applied
  git checkout "ea5730c522db8baa997da5389a2b1bf51ce9f2d5" || die "Could not checkout old code"

  # apply patch that makes Chart a git project
  git apply "c8b3d3792331bd989d512bda893e23b21b0aae6e.patch" || die "Could not apply git patch!"
popd > /dev/null 2>&1
