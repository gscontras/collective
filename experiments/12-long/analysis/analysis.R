library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

source("../analysis/helpers.R")

setwd("~/Documents/git/cocolab/collective/experiments/12-long/Submiterator-master")

num_round_dirs = 5 # problem with round 5
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/long.csv', sep='')) %>%
    #'round', i, '/long-trials.csv', sep='')) %>%
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","utterance","response","context","sentence_type","language","slide_number"))
unique(d$language)
d = d[d$language!="Spanish"&d$language!="Gujarati"&d$language!="",]
unique(d$language)

summary(lmer(response~context*sentence_type*utterance+(1|workerid),data=d))

length(unique(d$workerid)) # n=41

#write.csv(d,"../results/long-square_results.csv")

#d_raw <- d #un-transformed data
d_raw <- na.omit(d) #un-transformed data

## plots

#summary(d_raw)

raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","utterance"))

raw_plot <- ggplot(raw_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(~ utterance) + theme_bw()
raw_plot

#ggsave("../analysis/long.pdf",width=6,height=2.7)

d = dcast(data=d, workerid + utterance + context + slide_number ~ sentence_type, value.var="response",mean)

centered = cbind(d, myCenter(d[,c("utterance","context")]))
centered$diff = centered$dist - centered$coll

big = centered[centered$utterance=="big",]
b_m = glm(diff~ ccontext, data=big)
summary(b_m)

heavy = centered[centered$utterance=="heavy",]
h_m = glm(diff~ ccontext, data=heavy)
summary(h_m)

long = centered[centered$utterance=="long",]
l_m = lm(diff~ ccontext+workerid, data=long)
summary(l_m)
