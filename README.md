# commit_sentiment_analysis

This repository contains the scripts for running and analysing data from the final research chapter of my thesis.
In order to run the experiments, first run `bash setup.sh` which will download the Defects4J tool and apply the required patch to make the JFreeChart subject use Git instead of SVN
Following this, run `bash run_commit_sentiment_analysis.sh` from a terminal in order to complete the experiments.

The data processing for this paper was done in R, of which the scripts can be found in the analysis directory. The `analyse.R' script has two prerequisites:

1) It must be run from the analysis directory
2) There must be an environment variable called THESIS_DIR present when running the script. This is used as an output directory for plots, tables and macros, which are then used in the text.

When these pre-requisites are met, run `rscript analyse.R` to produce the information as displayed in the thesis chapter.
