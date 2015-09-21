library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)

setwd("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/")

sub = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-subject_information.tsv",sep="\t",header=T)
sub

d = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-trials.tsv",sep="\t",header=T)

#only native English
d <- d[d$workerid!=8&d$workerid!=20&d$workerid!=30&d$workerid!=47&d$workerid!=73,]

s = read.csv("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/bnc.sentences.csv",header=T)

# counts and raw values

head(d)

table(d$predicate,d$noun)

d = d[!is.na(d$sense),]

table(d$sense,d$attested)
# unattested: 67% make sense
# attested: 95% make sense
d$madesense = 0
d[d$sense=="Yes",]$madesense = 1
s_agg = aggregate(madesense~attested+workerid,data=d,mean)
head(s_agg)
aggregate(madesense~attested,data=s_agg,mean)

c = dcast(data=d, predicate + noun + workerid ~ sentence_type, value.var="response",mean,na.rm=T)

gof(c$coll,c$dist)

ggplot(c, aes(x=dist,y=coll)) +
  geom_point()

# trim to just those sentences that make sense

d = d[!is.na(d$sense)&d$sense=="Yes",]

summary(d)


# just attested sentences

a = d[d$attested=="True",]

## Attested sentence analysis (collapsing over animacy)

a$sentence = paste(a$noun,a$predicate,sep=" ")

a_sent_casted = dcast(data=a, animate + sentence + predicate + noun + workerid ~ sentence_type, value.var="response",mean,na.rm=T)
a_sent_casted$collective = (a_sent_casted$coll/a_sent_casted$dist)
a_sent_casted$coll_diff = (a_sent_casted$coll-a_sent_casted$dist)

a_sent_corr = dcast(data=a, sentence ~ sentence_type, value.var="response",mean,na.rm=T)
gof(a_sent_corr$coll,a_sent_corr$dist)# r=-.78,r2=0.61

a_sent_casted <- na.omit(a_sent_casted)

ggplot(a_sent_corr, aes(x=dist,y=coll)) +
  geom_point()+
  geom_smooth()


#small analysis

small = a_sent_casted[a_sent_casted$predicate=="small",]
head(small)
small$noun <- factor(small$noun,levels=c("numbers","children","rooms","classes"))
contrasts(small$noun) <- 'contr.sum'
sm = lmer(coll~noun+(1|workerid),data=small)
summary(sm)

# noun plot

noun_s = bootsSummary(data=a_sent_casted, measurevar="coll", groupvars=c("noun","animate"))
noun_s$noun <- factor(noun_s$noun,ordered=is.ordered(noun_s$noun))
noun_plot <- ggplot(noun_s, aes(x=reorder(noun,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
noun_plot

ggsave('results/noun_plot.pdf',width=6.3,height=3)

# sentence plot

sentence_s = bootsSummary(data=a_sent_casted, measurevar="coll", groupvars=c("sentence","animate"))
sentence_s$sentence <- factor(sentencen_s$sentence,ordered=is.ordered(sentence_s$sentence))
sentence_plot <- ggplot(sentence_s, aes(x=reorder(sentence,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(sentence,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsentence")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=7))
sentence_plot

ggsave('results/sentence_plot2.pdf',width=6.3,height=3.5)



# noun by pred plot

pred_s = bootsSummary(data=a_sent_casted, measurevar="coll", groupvars=c("noun","predicate"))
pred_s$noun <- factor(pred_s$noun,ordered=is.ordered(pred_s$noun))
pred_plot <- ggplot(pred_s, aes(x=reorder(noun,-coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement") +
  xlab("")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
  facet_wrap(~predicate,scales="free_x")
pred_plot

ggsave('results/pred_plot.pdf',width=6,height=6.5)

## just predicates with multiple nouns

head(a_sent_casted)
p_n <- a_sent_casted[a_sent_casted$predicate=="bright"|
                       a_sent_casted$predicate=="closed"|
                       a_sent_casted$predicate=="friendly"|
                       a_sent_casted$predicate=="full"|
                       a_sent_casted$predicate=="guilty"|
                       a_sent_casted$predicate=="open"|
                       a_sent_casted$predicate=="quiet"|
                       a_sent_casted$predicate=="small",]
summary(p_n)
n_pred_s = bootsSummary(data=p_n, measurevar="coll", groupvars=c("noun","predicate"))
n_pred_s$noun <- factor(n_pred_s$noun,ordered=is.ordered(n_pred_s$noun))
n_pred_plot <- ggplot(n_pred_s, aes(x=reorder(noun,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
  facet_wrap(~predicate,nrow=2,scales="free_x")
n_pred_plot

ggsave('results/noun_pred_plot2.pdf',width=5.9,height=4)






## Attested sentence collectivity ranking

a_sent_casted$dist_coll_ratio = a_sent_casted$dist/a_sent_casted$coll

a_sent_ranking_plot = ggplot(a_sent_casted,aes(x=0,y=dist_coll_ratio)) +
  geom_text(size=2,alpha=.5,aes(label=sentence))  +
  ylab("distirbutive endorsement / collective endorsement") +
  xlab("sentence") +
  scale_x_discrete(breaks=NULL) +
  ylim(.4,1.8)

ggsave(filename='attested_sentence_ranking_plot.png',plot=a_sent_ranking_plot,width=3, height=12)


## Attested Predicate analysis (collapsing over animacy)

a_pred_casted = dcast(data=a, predicate~ sentence_type, value.var="response",mean)
gof(a_pred_casted$coll,a_pred_casted$dist) # r=-.83, r2=.69
a_pred_casted$CI.YMin.coll = a_pred_casted$coll - dcast(data=a, predicate~ sentence_type, value.var="response",ci.low)$coll
a_pred_casted$CI.YMin.dist = a_pred_casted$dist - dcast(data=a, predicate~ sentence_type, value.var="response",ci.low)$dist
a_pred_casted$CI.YMax.coll = a_pred_casted$coll + dcast(data=a, predicate~ sentence_type, value.var="response",ci.high)$coll
a_pred_casted$CI.YMax.dist = a_pred_casted$dist + dcast(data=a, predicate~ sentence_type, value.var="response",ci.high)$dist

a_pred_word_plot <- ggplot(a_pred_casted, aes(x=coll,y=dist)) +
  #  geom_point() +
    geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_pred_word_plot.png',plot=a_pred_word_plot,width=8, height=8)

## Attested predicate collectivity ranking

a_pred_casted$coll_dist_ratio = a_pred_casted$coll/a_pred_casted$dist

a_pred_ranking_plot = ggplot(a_pred_casted,aes(x=0,y=coll_dist_ratio)) +
  geom_text(size=2,alpha=.75,aes(label=predicate),angle=45,position=position_dodge(width=.9))  +
  ylab("collective endorsement / distirbutive endorsement") +
  xlab("predicate") +
  scale_x_discrete(breaks=NULL) +
  ylim(.4,1.8)

ggsave(filename='attested_pred_ranking_plot.png',plot=a_pred_ranking_plot,width=3, height=8)

## Attested Predicate analysis (looking at animacy)

a_pred_animacy_casted = dcast(data=a, predicate+animate~ sentence_type, value.var="response",mean)
a_pred_animacy_casted$CI.YMin.coll = a_pred_animacy_casted$coll - dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.low)$coll
a_pred_animacy_casted$CI.YMin.dist = a_pred_animacy_casted$dist - dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.low)$dist
a_pred_animacy_casted$CI.YMax.coll = a_pred_animacy_casted$coll + dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.high)$coll
a_pred_animacy_casted$CI.YMax.dist = a_pred_animacy_casted$dist + dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.high)$dist

a_pred_animacy_plot <- ggplot(a_pred_animacy_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_pred_animacy_plot.png',plot=a_pred_animacy_plot,width=9, height=8)


## Attested Noun analysis (looking at animacy)

a_noun_casted = dcast(data=a, noun+animate~ sentence_type, value.var="response",mean)
a_noun_casted$CI.YMin.coll = a_noun_casted$coll - dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.low)$coll
a_noun_casted$CI.YMin.dist = a_noun_casted$dist - dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.low)$dist
a_noun_casted$CI.YMax.coll = a_noun_casted$coll + dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.high)$coll
a_noun_casted$CI.YMax.dist = a_noun_casted$dist + dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.high)$dist

a_noun_word_plot <- ggplot(a_noun_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_noun_word_plot.png',plot=a_noun_word_plot,width=9, height=8)









# just unattested sentences

u = d[d$attested=="False",]

## Unattested Predicate analysis (collapsing over animacy)

u_pred_casted = dcast(data=u, predicate~ sentence_type, value.var="response",mean)
u_pred_casted$CI.YMin.coll = u_pred_casted$coll - dcast(data=u, predicate~ sentence_type, value.var="response",ci.low)$coll
u_pred_casted$CI.YMin.dist = u_pred_casted$dist - dcast(data=u, predicate~ sentence_type, value.var="response",ci.low)$dist
u_pred_casted$CI.YMax.coll = u_pred_casted$coll + dcast(data=u, predicate~ sentence_type, value.var="response",ci.high)$coll
u_pred_casted$CI.YMax.dist = u_pred_casted$dist + dcast(data=u, predicate~ sentence_type, value.var="response",ci.high)$dist

u_pred_word_plot <- ggplot(u_pred_casted, aes(x=coll,y=dist)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_pred_word_plot.png',plot=u_pred_word_plot,width=8, height=8)

## Unattested Predicate analysis (looking at animacy)

u_pred_animacy_casted = dcast(data=u, predicate+animate~ sentence_type, value.var="response",mean)
u_pred_animacy_casted$CI.YMin.coll = u_pred_animacy_casted$coll - dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.low)$coll
u_pred_animacy_casted$CI.YMin.dist = u_pred_animacy_casted$dist - dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.low)$dist
u_pred_animacy_casted$CI.YMax.coll = u_pred_animacy_casted$coll + dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.high)$coll
u_pred_animacy_casted$CI.YMax.dist = u_pred_animacy_casted$dist + dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.high)$dist

u_pred_animacy_plot <- ggplot(u_pred_animacy_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_pred_animacy_plot.png',plot=u_pred_animacy_plot,width=9, height=8)

## Unattested Noun analysis (looking at animacy)

u_noun_casted = dcast(data=u, noun+animate~ sentence_type, value.var="response",mean)
u_noun_casted$CI.YMin.coll = u_noun_casted$coll - dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.low)$coll
u_noun_casted$CI.YMin.dist = u_noun_casted$dist - dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.low)$dist
u_noun_casted$CI.YMax.coll = u_noun_casted$coll + dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.high)$coll
u_noun_casted$CI.YMax.dist = u_noun_casted$dist + dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.high)$dist

u_noun_word_plot <- ggplot(u_noun_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_noun_word_plot.png',plot=u_noun_word_plot,width=9, height=8)

