library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)

setwd("~/Documents/git/CoCoLab/collective/experiments/6-corpus-tense/")

sub = read.table("~/Documents/git/CoCoLab/collective/experiments/6-corpus-tense/Submiterator-master/corpus-tense-subject_information.tsv",sep="\t",header=T)

d = read.table("~/Documents/git/CoCoLab/collective/experiments/6-corpus-tense/Submiterator-master/corpus-tense-trials.tsv",sep="\t",header=T)
d$sentence = paste(d$noun,d$predicate,sep=" ")

c_score = read.csv("~/Documents/git/CoCoLab/collective/experiments/6-corpus-tense/Submiterator-master/sentence-comp-score.csv",header=T)
head(c_score)

d$c_score = c_score$c_ratio[match(d$sentence,c_score$X)]

# trim to just those sentences that make sense

d = d[d$sense=="Yes",]

summary(d)

head(d)

## Attested sentence analysis (collapsing over animacy)


d_sent_casted = dcast(data=d, animate + sentence + tense + noun + predicate + workerid +slide_number + c_score ~ sentence_type, value.var="response",mean)
d_sent_casted <- na.omit(d_sent_casted)
d_sent_casted$coll_ratio = (d_sent_casted$coll/(d_sent_casted$coll+d_sent_casted$dist))
d_sent_casted$coll_diff = (d_sent_casted$coll-d_sent_casted$dist)

table(d_sent_casted$sentence)
aggregate(coll~tense,data=d_sent_casted,mean)
aggregate(coll~tense*c_score,data=d_sent_casted,mean)

plot <- ggplot(d_sent_casted, aes(x=coll,y=dist,color=tense)) +
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.5,aes(label=sentence),angle=45) +
  #ylab("faultless?") +
  #xlab("collective?")+
  ylim(0,1) +
  xlim(0,1)
plot

plot <- ggplot(d_sent_casted, aes(x=tense,y=coll_ratio)) +
  #geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.5,aes(label=sentence),angle=45) +
  #ylab("faultless?") +
  #xlab("collective?")+
  ylim(0,1)
plot

coll_s = bootsSummary(data=d_sent_casted, measurevar="coll", groupvars=c("tense"))
coll_plot <- ggplot(coll_s, aes(x=tense,y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=tense, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\n")+
  #ylim(0,1)+
  xlab("\ntense") 
coll_plot
ratio_s = bootsSummary(data=d_sent_casted, measurevar="coll_ratio", groupvars=c("tense"))
ratio_plot <- ggplot(ratio_s, aes(x=tense,y=coll_ratio)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=tense, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement ratio\n")+
  #ylim(0,1)+
  xlab("\ntense") 
ratio_plot
diff_s = bootsSummary(data=d_sent_casted, measurevar="coll_diff", groupvars=c("tense"))
diff_plot <- ggplot(diff_s, aes(x=tense,y=coll_diff)) +
  geom_point(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=tense, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement difference\n")+
  #ylim(0,1)+
  xlab("\ntense") 
diff_plot

## Just looking at "small"

s = d_sent_casted[d_sent_casted$predicate=="small",]
head(s)
aggregate(coll~noun,data=s,mean)
aggregate(coll~tense*noun,data=s,mean)
s_s = bootsSummary(data=s, measurevar="coll", groupvars=c("tense","noun"))
small_plot <- ggplot(s_s, aes(x=noun,y=coll,fill=tense)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=noun, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\n")
  #ylim(0,1)+
  #xlab("\nt") 
small_plot

s_dist_s = bootsSummary(data=s, measurevar="dist", groupvars=c("tense","noun"))
small_dist_plot <- ggplot(s_dist_s, aes(x=noun,y=dist,fill=tense)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=noun, width=0.1),position=position_dodge(width=0.9))+
  ylab("distributive endorsement\n")
#ylim(0,1)+
#xlab("\nt") 
small_dist_plot


centered = cbind(d_sent_casted, myCenter(d_sent_casted[,c("tense","slide_number","c_score")]))

all_m = lmer(coll_diff~ctense+cslide_number+(1+ctense+cslide_number|sentence)+(1+ctense+cslide_number|workerid),data=centered)
summary(all_m)

coll_m = lmer(coll~ctense+cslide_number+(1+ctense+cslide_number|sentence)+(1+ctense+cslide_number|workerid),data=centered)
summary(coll_m)

c_s = bootsSummary(data=centered,measurevar="coll",groupvars = c("slide_number","tense"))

c_plot <- ggplot(c_s, aes(x=slide_number,y=coll,color=tense)) +
  geom_smooth()+
  geom_point()
  #geom_abline(intercept=0,slope=1) +
  #geom_text(size=2,alpha=.5,aes(label=sentence),angle=45)
  #ylab("faultless?") +
  #xlab("collective?")+
c_plot
