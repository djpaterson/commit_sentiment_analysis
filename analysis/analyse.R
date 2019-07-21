library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(ggthemes)
library(effsize)
library(stringr)

source("write.R")

data <- read_delim("../data/all_project_sentiment_analysis.csv", escape_double = F, delim=",")

if(Sys.getenv("THESIS_DIR") == ""){
  stop("Please ensure THESIS_DIR environment variable is set for macros, plots and tables")
}

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
  plot <- plot + theme_few() +
    theme(axis.title.x = element_text(size = 30), 
          axis.title.y = element_text(size = 30), 
          axis.text = element_text(size = 20),
          legend.position = "bottom")
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

write.macro("subjNoSent", data %>% filter(subjectivity_score == 1, sentiment_score == 0) %>% count(), round.dp=0, min.dp=0)
write.macro("subjNoSentFinal", data %>% filter(subjectivity_score == 1, sentiment_score == 0, grepl("final", commit_msg, ignore.case=T)) %>% count(), round.dp=0, min.dp=0)

# rq 2 macros
fault_fix_commits <- data %>% filter(bug_fix_commit==T)

stats_data <- data.frame(project = character(), sent.p.value = numeric(), sent.a12 = numeric(), subj.p.value = numeric(), subj.a12 = numeric(), stringsAsFactors = F)
for(project in c("Chart", "Closure", "Lang", "Math", "Mockito", "Time")){
  data.mask <- data[data$project==project,]
  sent.p.value <- wilcox.test(data.mask[data.mask$bug_fix_commit ==T,]$sentiment_score, data.mask[data.mask$bug_fix_commit==F,]$sentiment_score)$p.value
  subj.p.value <- wilcox.test(data.mask[data.mask$bug_fix_commit ==T,]$subjectivity_score, data.mask[data.mask$bug_fix_commit==F,]$subjectivity_score)$p.value
  sent.a12 <- VD.A(data.mask$sentiment_score, data.mask$bug_fix_commit)$estimate
  subj.a12 <- VD.A(data.mask$subjectivity_score, data.mask$bug_fix_commit)$estimate
  stats_data[nrow(stats_data)+1,] = list(project, sent.p.value, sent.a12, subj.p.value, subj.a12)
  write.macro(paste0("subjPValue",project), subj.p.value)
  write.macro(paste0("subjAhat",project), subj.a12)
  write.macro(paste0("sentPValue",project), sent.p.value)
  write.macro(paste0("sentAhat",project), sent.a12)
}

write.macro("bestSentP", (stats_data %>% top_n(1,-sent.p.value))$sent.p.value)
write.macro("bestSentPProject", (stats_data %>% top_n(1,-sent.p.value))$project)
write.macro("bestSentAhat", (stats_data %>% top_n(1,sent.a12))$sent.a12)
write.macro("bestSentAhatProject", (stats_data %>% top_n(1,sent.a12))$project)

write.macro("worstSentP", (stats_data %>% top_n(1,sent.p.value))$sent.p.value)
write.macro("worstSentPProject", (stats_data %>% top_n(1,sent.p.value))$project)
write.macro("worstSentAhat", (stats_data %>% top_n(1,-sent.a12))$sent.a12)
write.macro("worstSentAhatProject", (stats_data %>% top_n(1,-sent.a12))$project)

write.macro("bestSubjP", (stats_data %>% top_n(1,-subj.p.value))$subj.p.value)
write.macro("bestSubjPProject", (stats_data %>% top_n(1,-subj.p.value))$project)
write.macro("bestSubjAhat", (stats_data %>% top_n(1,subj.a12))$subj.a12)
write.macro("bestSubjAhatProject", (stats_data %>% top_n(1,subj.a12))$project)

write.macro("worstSubjP", (stats_data %>% top_n(1,subj.p.value))$subj.p.value)
write.macro("worstSubjPProject", (stats_data %>% top_n(1,subj.p.value))$project)
write.macro("worstSubjAhat", (stats_data %>% top_n(1,-subj.a12))$subj.a12)
write.macro("worstSubjAhatProject", (stats_data %>% top_n(1,-subj.a12))$project)


plot <- data %>% 
  ggplot(aes(y=sentiment_score,x=project)) + 
  geom_boxplot(aes(group=interaction(project,bug_fix_commit),fill=bug_fix_commit)) + 
  geom_text(data = stats_data, aes(y = 1.25,x=project, label = paste0("p = ",format(round(sent.p.value,digits = 2),nsmall = 2))), size = 6) +
  geom_text(data = stats_data, aes(y = 1.1,x=project, label = paste0("hat(A)  == ",format(round(sent.a12, digits=2), snamell = 2))), parse = T, size = 6) +
  scale_y_continuous(limits=c(-1,1.25), breaks=c(-1,-0.6,-0.2,0.2,0.6,1))
readable.plot(plot, x.label = "Project", y.label = "Sentiment Score") %>% save.plot(filename = "SENTIMENT_VS_BUG_FIX.pdf")

plot <- data %>% 
  ggplot(aes(y=subjectivity_score,x=project)) + 
  geom_boxplot(aes(group=interaction(project,bug_fix_commit),fill=bug_fix_commit)) + 
  geom_text(data = stats_data, aes(y = 1.125,x=project, label = paste0("p = ",format(round(subj.p.value,digits = 2),nsmall = 2))), size = 6) +
  geom_text(data = stats_data, aes(y = 1.05,x=project, label = paste0("hat(A)  == ",format(round(subj.a12, digits=2), snamell = 2))), parse = T, size = 6) +
  scale_y_continuous(limits=c(0,1.125), breaks=c(0,0.2,0.4,0.6,0.8,1))
readable.plot(plot, x.label = "Project", y.label = "Subjectivity Score") %>% save.plot(filename = "SUBJECTIVITY_VS_BUG_FIX.pdf")



