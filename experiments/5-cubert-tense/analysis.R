library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(grid)
library(reshape2)

d = read.csv("~/Documents/git/CoCoLab/collective/experiments/5-cubert-tense/Submiterator-master/cubert-tense-trials.tsv",sep="\t",header=T)
head(d)

# counts and raw values

table(d$sceanrio,d$context,d$tense,d$utterance)

aggregate(response~sceanrio*sentence_type*context*tense*utterance,d,mean)

# reshape data

d = dcast(data=d, workerid + utterance + sceanrio + context + tense ~ sentence_type, value.var="response",mean)

# nomarlized collectivity score

d$collective = (d$coll / (d$coll + d$dist))
d$collective_ratio = (d$coll / d$dist)
d$coll_diff = (d$coll - d$dist)

# tests

aggregate(coll~context*utterance,data=d,mean)

#aggregate(coll~context*tense*utterance,data=d,mean)

# plots

setwd("~/Documents/git/CoCoLab/collective/experiments/5-cubert-tense/Submiterator-master/")

d_s = bootsSummary(data=d, measurevar="coll", groupvars=c("context","utterance","sceanrio","tense"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("high","low")),y=coll,fill=sceanrio)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(title="collective (95% bootsci)")+
  scale_fill_manual(values=c("blue","red"))
plot <- plot  +facet_grid(utterance ~ tense) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

#ggsave("expt2collectivebootsci.pdf")
