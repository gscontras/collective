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
d = d[!is.na(d$sense),]

table(d$sense,d$attested)
# unattested: 87% make sense
# attested: 98% make sense
d$madesense = 0
d[d$sense=="Yes",]$madesense = 1
s_agg = aggregate(madesense~attested+workerid,data=d,mean)
head(s_agg)
aggregate(madesense~attested,data=s_agg,mean)



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
#d <- d[d$sense_rate<0.05,]

# counts and raw values

head(d)

table(d$predicate,d$noun)

# trim to just those sentences that make sense

d = d[!is.na(d$sense)&d$sense=="Yes",]

summary(d)

# just attested sentences

a = d[d$attested=="True",]

# all sentences

#a <- d

## Attested sentence analysis (collapsing over animacy)

a_sent_casted = dcast(data=a, animate + sentence + predicate + noun + workerid ~ sentence_type, value.var="response",mean,na.rm=T)
a_sent_casted$collective = (a_sent_casted$coll/a_sent_casted$dist)
a_sent_casted$coll_diff = (a_sent_casted$coll-a_sent_casted$dist)

a_sent_casted <- na.omit(a_sent_casted)
head(a_sent_casted)

### coll plots


## all plot

all <- a_sent_casted 
all_s = bootsSummary(data=all, measurevar="coll", groupvars=c("noun",'predicate'))
all_s$noun <- factor(all_s$noun,ordered=is.ordered(all_s$noun))
all_plot <- ggplot(all_s, aes(x=reorder(noun,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  facet_wrap(~predicate,ncol=3,scale="free_x")
all_plot
ggsave("results/bht_plot2.pdf",width=6,height=2.7)

##test

aggregate(coll~predicate,data=a_sent_casted,mean)
contrasts(a_sent_casted$noun)
big <- a_sent_casted[a_sent_casted$predicate=="big",]
aggregate(coll~noun,data=big,mean)
big$noun <- factor(big$noun,levels=c("waves","rooms","boys","children","houses"))
#contrasts(big$noun) <- "contr.sum"
heavy <- a_sent_casted[a_sent_casted$predicate=="heavy",]
heavy$noun <- factor(heavy$noun,levels=c("bags","lids","trees","loads","men"))
aggregate(coll~noun,data=heavy,mean)
#contrasts(heavy$noun) <- "contr.sum"
tall <- a_sent_casted[a_sent_casted$predicate=="tall",]
tall$noun <- factor(tall$noun,levels=c("trees","plants","offspring","buildings","windows"))
aggregate(coll~noun,data=tall,mean)
#contrasts(tall$noun) <- "contr.sum"

b = lmer(coll~noun+(1|workerid),data=big)
summary(b)
h = lmer(coll~noun+(1|workerid),data=heavy)
summary(h)
t = lmer(coll~noun+(1|workerid),data=tall)
summary(t)

d <- a_sent_casted
d$predicate <- factor(d$predicate,levels=c("heavy","big","tall"))
contrasts(d$predicate)
m = lmer(coll~predicate+(1+predicate|workerid)+(1|noun),data=d)
summary(m)
