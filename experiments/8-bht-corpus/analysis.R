library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)
library(gridExtra)

setwd("~/Documents/git/CoCoLab/collective/experiments/7-bht-corpus/")

sub = read.table("Submiterator-master/bht-corpus-subject_information.tsv",sep="\t",header=T)
sub

d = read.table("Submiterator-master/bht-corpus-trials.tsv",sep="\t",header=T)

#only native English
d <- d[d$workerid!=16&d$workerid!=17&d$workerid!=18&d$workerid!=27,]

# sense rates
d$sentence = paste(d$noun,d$predicate,sep=" ")
sense = data.frame(table(d$sentence,d$sense))
colnames(sense) <- c("sentence","sense","count")
head(sense)
s = dcast(data=sense, sentence ~ sense, value.var="count",mean,na.rm=T)
head(s)
s$sense = (s$No/s$Yes)
head(s)
summary(s)

d$sense_rate <- s$sense[match(d$sentence,s$sentence)]
head(d)
# trim on the basis of sense rate
d <- d[d$sense_rate<0.05,]

# counts and raw values

head(d)

table(d$predicate,d$noun)

# trim to just those sentences that make sense

d = d[!is.na(d$sense)&d$sense=="Yes",]

summary(d)

# just attested sentences

#a = d[d$attested=="True",]

# all sentences

a <- d

## Attested sentence analysis (collapsing over animacy)

a_sent_casted = dcast(data=a, animate + sentence + predicate + noun + workerid ~ sentence_type, value.var="response",mean,na.rm=T)
a_sent_casted$collective = (a_sent_casted$coll/a_sent_casted$dist)
a_sent_casted$coll_diff = (a_sent_casted$coll-a_sent_casted$dist)

a_sent_casted <- na.omit(a_sent_casted)
head(a_sent_casted)

### coll plots

## big plot

big = a_sent_casted[a_sent_casted$predicate=="big",]

noun_s = bootsSummary(data=big, measurevar="coll", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
big_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("") +
  xlab("")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
big_plot

## heavy plot

heavy = a_sent_casted[a_sent_casted$predicate=="heavy",]

noun_s = bootsSummary(data=heavy, measurevar="coll", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
heavy_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement") +
  xlab("")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
heavy_plot

## tall plot

tall = a_sent_casted[a_sent_casted$predicate=="tall",]

noun_s = bootsSummary(data=tall, measurevar="coll", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
tall_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("") +
  xlab("noun")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
tall_plot

pdf("results/all.pdf")
grid.arrange(big_plot, heavy_plot, tall_plot)
dev.off()





### coll_diff plots

## big plot

big = a_sent_casted[a_sent_casted$predicate=="big",]

noun_s = bootsSummary(data=big, measurevar="coll_diff", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
big_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll_diff,mean),y=coll_diff)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll_diff,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("") +
  xlab("")+
  #ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
big_plot

## heavy plot

heavy = a_sent_casted[a_sent_casted$predicate=="heavy",]

noun_s = bootsSummary(data=heavy, measurevar="coll_diff", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
heavy_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll_diff,mean),y=coll_diff)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll_diff,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("coll - diff") +
  xlab("")+
  #ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
heavy_plot

## tall plot

tall = a_sent_casted[a_sent_casted$predicate=="tall",]

noun_s = bootsSummary(data=tall, measurevar="coll_diff", groupvars=c("noun"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
tall_plot <- ggplot(noun_s, aes(x=reorder(noun,-coll_diff,mean),y=coll_diff)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,-coll_diff,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("") +
  xlab("noun")+
  #ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
tall_plot

pdf("results/all_diff.pdf")
grid.arrange(big_plot, heavy_plot, tall_plot)
dev.off()


