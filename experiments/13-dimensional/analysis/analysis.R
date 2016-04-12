library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

source("../analysis/helpers.R")

setwd("~/Documents/git/cocolab/collective/experiments/11-long-square/Submiterator-master")

num_round_dirs = 10 # problem with round 5
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/long-square.csv', sep='')) %>%
    #'round', i, '/long-square-trials.csv', sep='')) %>%
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","predicate","response","context","sentence_type","language"))
unique(d$language)
d = d[d$language!="Chinese"&d$language!="Spanish, English"&d$language!="Spanish"&d$language!="Hindi"&d$language!="spanish"&d$language!=""&d$language!="SPANISH"&d$language!="vietnamese, english"&d$language!="English and Kreyol"&d$language!="Vietnamese",]
unique(d$language)

#d = subset(df, select=c("workerid","predicate","response","context","sentence_type"))

length(unique(d$workerid)) # n=82

#write.csv(d,"../results/long-square_results.csv")

d_raw <- d #un-transformed data
#d_raw <- na.omit(d) #un-transformed data

## plots

#summary(d_raw)

raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","predicate"))

raw_plot <- ggplot(raw_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(~ predicate) + theme_bw()
raw_plot

#ggsave("../analysis/long-square.pdf",width=6,height=2.7)

d = dcast(data=d, workerid + predicate + context ~ sentence_type, value.var="response",mean)

centered = cbind(d, myCenter(d[,c("predicate","context")]))
centered$diff = centered$dist - centered$coll

tall = centered[centered$predicate=="tall",]
t_m = glm(diff~ ccontext, data=tall)
summary(t_m)

square = centered[centered$predicate=="square",]
s_m = glm(diff~ ccontext, data=square)
summary(s_m)

long = centered[centered$predicate=="long",]
l_m = glm(diff~ ccontext, data=long)
summary(l_m)
