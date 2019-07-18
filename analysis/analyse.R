library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(ggthemes)
library(effsize)

source("write.R")

data <- read_csv("../data/all_project_sentiment_analysis.csv")

if(Sys.getenv("THESIS_DIR") == ""){
  stop("Please ensure THESIS_DIR environment variable is set for macros, plots and tables")
}

macro.file <- paste(Sys.getenv("THESIS_DIR"), "rmacros.tex", sep="/")
macro.file.bak <- paste(macro.file, ".bak", sep="")
# backup macro file
file.copy(macro.file, macro.file.bak, overwrite=T)
# delete macro file
unlink(macro.file)

squash.files <- function(df){
  return(df %>% 
           group_by(project, commit_hash, commit_msg) %>% 
           summarise(num_files_changed = n(), 
                     sentiment_score=first(sentiment_score), 
                     subjectivity_score=first(subjectivity_score), 
                     bug_fix_commit=first(bug_fix_commit)) %>% 
           ungroup()
         )
}

plot.output.dir <- paste0(Sys.getenv("THESIS_DIR"), "/plots/")

save.plot <- function(plot, filename){
  ggsave(filename = paste0(plot.output.dir, filename), plot = plot)
}

readable.plot <- function(plot, y.label = "", x.label = ""){
  plot <- plot + theme_few() + theme(axis.title.x = element_text(), axis.title.y = element_text()) + theme(legend.position = "bottom")
  if(nchar(y.label) > 0){
    plot <- plot + ylab(y.label)
  }
  if(nchar(x.label) > 0){
    plot <- plot + xlab(x.label)
  }
  return(plot)
}


data %<>% squash.files()
# rq 1 macros
write.macro("numCommitsAnalysed", data %>% summarise(n=n()) %>% select(n), min.dp = 0, round.dp = 0)
write.macro("numZeroSentiment", data %>% filter(sentiment_score == 0) %>% count(), min.dp = 0, round.dp = 0)
write.macro("numZeroSubjectivity", data %>% filter(subjectivity_score == 0) %>% count(), min.dp = 0, round.dp = 0)
write.macro("percZeroSentiment", data %>% summarise(total = n(), s = sum(sentiment_score == 0)) %>% mutate(perc = s / total * 100) %>% select(perc), xspace=F)
write.macro("percZeroSubjectivity", data %>% summarise(total = n(), s = sum(subjectivity_score == 0)) %>% mutate(perc = s / total * 100) %>% select(perc), xspace=F)
write.macro("numPositiveSentiment", data %>% filter(sentiment_score > 0) %>% count(), min.dp=0, round.dp=0)
write.macro("numNegativeSentiment", data %>% filter(sentiment_score < 0) %>% count(), min.dp=0, round.dp=0)
table_data <- data %>%
  group_by(project) %>% 
  summarise(Positive = sum(sentiment_score > 0), 
            Negative = sum(sentiment_score < 0), 
            Neutral = sum(sentiment_score == 0), 
            Total = n()) %>% 
  rename("Project" = project)
perc_sentimental <- table_data %>% ungroup() %>% 
  mutate(
    perc_positive = Positive / Total * 100, 
    perc_negative = Negative / Total * 100, 
    perc_neutral = Neutral / Total * 100)
perc_sentimental_sum <- perc_sentimental %>%
  summarise(max_positive = max(perc_positive), max_negative = max(perc_negative), max_neutral = max(perc_neutral))

write.macro("maxPositiveNum", perc_sentimental[perc_sentimental$perc_positive == max(perc_sentimental$perc_positive),]$Positive, min.dp = 0, round.dp=0)
write.macro("maxPositivePerc", perc_sentimental_sum %>% select(max_positive),xspace=F)
write.macro("maxPositiveProj", perc_sentimental[perc_sentimental$perc_positive == max(perc_sentimental$perc_positive),]$Project)

write.macro("maxNegativeNum", perc_sentimental[perc_sentimental$perc_negative == max(perc_sentimental$perc_negative),]$Negative, min.dp=0, round.dp=0)
write.macro("maxNegativePerc", perc_sentimental_sum %>% select(max_negative), xspace=F)
write.macro("maxNegativeProj", perc_sentimental[perc_sentimental$perc_negative == max(perc_sentimental$perc_negative),]$Project)

write.macro("maxNeutralNum", perc_sentimental[perc_sentimental$perc_neutral == max(perc_sentimental$perc_neutral),]$Neutral, min.dp=0, round.dp=0)
write.macro("maxNeutralPerc", perc_sentimental_sum %>% select(max_neutral),xspace=F)
write.macro("maxNeutralProj", perc_sentimental[perc_sentimental$perc_neutral == max(perc_sentimental$perc_neutral),]$Project)
table_data %>%
  write_table(max.dp=0, round.dp = 0, filename = "sentiment_per_project.tex")

words <- c("great", "good", "cool", "handy", "nice", "tidy", "nasty", "bad", "stupid", "terrible", "crash", "missing")
counts <- c()
average_sentiments <- c()
for(word in words){
  counts <- c(counts, nrow(data[grepl(word, data$commit_msg, ignore.case=T),]))
  average_sentiments <- c(average_sentiments, (data %>% filter(grepl(word, commit_msg, ignore.case=T)) %>% summarise(sentiment = mean(sentiment_score)))[[1]][1])
}
words.df <- data.frame(words,counts,average_sentiments)
words.df %>% rename("Word" = words, "Count" = counts, "Average Sentiment" = average_sentiments) %>% arrange(-average_sentiments) %>%
  write_table(filename = "sentimental_words.tex", round.dp=0)

write.macro("fixCommits", data %>% filter(grepl("fix", commit_msg)) %>% count(), min.dp=0,round.dp=0)
write.macro("fixSentiment", data %>% filter(grepl("fix", commit_msg)) %>% summarise(sentiment = mean(sentiment_score)))

plot <- data %>% ggplot(aes(x=subjectivity_score, y=sentiment_score)) + geom_jitter()
readable.plot(plot, x.label = "Subjectivity", y.label = "Sentiment") %>% save.plot(filename="SUBJECITIVITY_VS_SENTIMENT.pdf")

sentiment_no_subj <- data %>% filter(subjectivity_score == 0, sentiment_score != 0)

write.macro("positiveNoSubj", sentiment_no_subj %>% filter(sentiment_score > 0) %>% count(), round.dp=0, min.dp=0)
write.macro("positiveNoSubjUseful", sentiment_no_subj %>% filter(sentiment_score > 0, grepl("useful", commit_msg, ignore.case=T)) %>% count(), round.dp=0, min.dp=0)

write.macro("negativeNoSubj", sentiment_no_subj %>% filter(sentiment_score < 0) %>% count(), round.dp=0, min.dp=0)
write.macro("negativeNoSubjUseful", sentiment_no_subj %>% filter(sentiment_score < 0, grepl("useful", commit_msg, ignore.case=T)) %>% count(), round.dp=0, min.dp=0)
write.macro("negativeNoSubjHarder", sentiment_no_subj %>% filter(sentiment_score < 0, grepl("harder", commit_msg, ignore.case=T)) %>% count(), round.dp=0, min.dp=0)

# rq 2 macros
fault_fix_commits <- data %>% filter(bug_fix_commit==T)
plot <- data %>% ggplot(aes(y=sentiment_score,x=project)) + geom_boxplot(aes(group=interaction(project,bug_fix_commit),fill=bug_fix_commit)) + theme(legend.position="bottom")
readable.plot(plot, x.label = "Project", y.label = "Sentiment Score") %>% save.plot(filename = "SENTIMENT_VS_BUG_FIX.pdf")

write.macro("sentPValue", wilcox.test(data[data$bug_fix_commit ==T,]$sentiment_score, data[data$bug_fix_commit==F,]$sentiment_score)$p.value)
write.macro("sentA12", VD.A(data$sentiment_score, data$bug_fix_commit)$estimate)

plot <- data %>% ggplot(aes(y=subjectivity_score,x=project)) + geom_boxplot(aes(group=interaction(project,bug_fix_commit),fill=bug_fix_commit)) + theme(legend.position="bottom")
readable.plot(plot, x.label = "Project", y.label = "Subjectivity Score") %>% save.plot(filename = "SUBJECTIVITY_VS_BUG_FIX.pdf")

write.macro("subjPValue", wilcox.test(data[data$bug_fix_commit ==T,]$subjectivity_score, data[data$bug_fix_commit==F,]$subjectivity_score)$p.value)
write.macro("subjA12", VD.A(data$subjectivity_score, data$bug_fix_commit)$estimate)


