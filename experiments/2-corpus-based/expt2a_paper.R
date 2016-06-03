library(lme4)
#library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)
library(hydroGOF)

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
  geom_smooth(method=lm)


# sentence plot

sentence_s = bootsSummary(data=a_sent_casted, measurevar="coll", groupvars=c("sentence","animate"))
sentence_s$sentence <- factor(sentence_s$sentence,ordered=is.ordered(sentence_s$sentence))
sentence_plot <- ggplot(sentence_s, aes(x=reorder(sentence,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge(),fill="gray40") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(sentence,coll,mean), width=0.1),position=position_dodge(width=0.9),color="black")+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsentence")+
  ylim(0,1) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=7))
sentence_plot

#ggsave('results/sentence_plot2.png',width=6.3,height=3.5)

a_sent_casted$sentence <- factor(a_sent_casted$sentence,ordered=is.ordered(a_sent_casted$sentence))
violin_plot <- ggplot(a_sent_casted, aes(x=reorder(sentence,coll,mean),y=coll)) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(sentence,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  geom_violin(fill="black")+
  stat_summary(fun.y=mean, geom="point", size=4, color="red")+
  #geom_boxplot(width=0.1)+
  ylab("collective endorsement\n") +
  xlab("\nsentence")+
  ylim(0,1) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=7))
violin_plot

#ggsave('results/sentence_plot_violin.png',width=6.3,height=3.5)

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

#ggsave('results/noun_pred_plot2.pdf',width=5.9,height=4)

p_n$noun <- factor(p_n$noun,ordered=is.ordered(p_n$noun))
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
n_pred_violin <- ggplot(p_n, aes(x=reorder(noun,coll,mean),y=coll)) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  geom_violin(fill="black")+
  #geom_boxplot(width=0.1,color="white")+
  #stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="white")+
  stat_summary(fun.data=data_summary,color="white",fill="red",shape=18,size=.5)+
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  #ylim(0,1) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
  facet_wrap(~predicate,nrow=2,scales="free_x")
n_pred_violin

#ggsave('results/noun_pred_plot_violin.png',width=5.9,height=4)


######################################################
###### BERKELEY SLIDES PLOT
######################################################

n_pred_s = bootsSummary(data=p_n, measurevar="coll", groupvars=c("noun","predicate"))
n_pred_s$noun <- factor(n_pred_s$noun,ordered=is.ordered(n_pred_s$noun))
n_pred_plot <- ggplot(n_pred_s, aes(x=reorder(noun,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge(),fill="white") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9),color="gray")+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("subject noun")+
  ylim(0,1) +
  facet_wrap(~predicate,nrow=2,scales="free_x")+
  theme_blackDisplay()+
  theme(axis.text.x=element_text(angle=45,vjust=1.1,hjust=1))
n_pred_plot

ggsave("/Users/Greg/Documents/git/CoCoLab/collective/presentations/2015_Berkeley/expt2a.pdf",width=13,height=8)



#small analysis

small = a_sent_casted[a_sent_casted$predicate=="small",]
head(small)
small$noun <- factor(small$noun,levels=c("numbers","children","rooms","classes"))
#contrasts(small$noun) <- 'contr.sum'
sm = lmer(coll~noun+(1|workerid),data=small)
summary(sm)
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)    0.57190    0.05925 135.84000   9.652  < 2e-16 ***
#  nounchildren  -0.16202    0.06918  96.86000  -2.342  0.02123 *  
#  nounrooms     -0.19773    0.07103  94.99000  -2.784  0.00648 ** 
#  nounclasses   -0.28333    0.07310  91.23000  -3.876  0.00020 ***

# bright
bright = p_n[p_n$predicate=="bright",]
bright$noun <- factor(bright$noun,levels=c("eyes","colors"))
bm = lmer(coll~noun+(1|workerid),data=bright)
summary(bm)
#            Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.75407    0.06582 56.77000  11.457 2.22e-16 ***
#  nouncolors  -0.18599    0.08240 41.84000  -2.257   0.0293 *  

# closed
closed = p_n[p_n$predicate=="closed",]
closed$noun <- factor(closed$noun,levels=c("doors","shops"))
cm = lmer(coll~noun+(1|workerid),data=closed)
summary(cm)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.59518    0.06910 60.04000   8.613 4.41e-12 ***
#  nounshops   -0.16046    0.08571 36.09000  -1.872   0.0693 .   

# friendly
friendly = p_n[p_n$predicate=="friendly",]
friendly$noun <- factor(friendly$noun,levels=c("natives","people"))
fm = lmer(coll~noun+(1|workerid),data=friendly)
summary(fm)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.73277    0.05463 45.17000  13.412   <2e-16 ***
#  nounpeople  -0.05181    0.05563 11.27000  -0.931    0.371    

# full
full = p_n[p_n$predicate=="full",]
full$noun <- factor(full$noun,levels=c("streets","papers","pubs"))
fum = lmer(coll~noun+(1|workerid),data=full)
summary(fum)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.68004    0.05897 72.96000  11.533   <2e-16 ***
#  nounpapers  -0.11367    0.09581 39.52000  -1.186   0.2425    
#nounpubs    -0.23446    0.08065 53.36000  -2.907   0.0053 ** 

# guilty
guilty = p_n[p_n$predicate=="guilty",]
guilty$noun <- factor(guilty$noun,levels=c("defendants","men"))
gm = lmer(coll~noun+(1|workerid),data=guilty)
summary(gm)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.67682    0.05411 52.87000  12.508   <2e-16 ***
#  nounmen     -0.01641    0.08024 27.69000  -0.205    0.839    

# open
open = p_n[p_n$predicate=="open",]
open$noun <- factor(open$noun,levels=c("eyes","gates","doors","curtains","pubs","windows"))
om = lmer(coll~noun+(1|workerid),data=open)
summary(om)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)    0.645859   0.066373 173.590000   9.731   <2e-16 ***
#  noungates      0.006575   0.085160 134.750000   0.077   0.9386    
#noundoors     -0.032949   0.082482 128.930000  -0.399   0.6902    
#nouncurtains  -0.002674   0.085480 135.970000  -0.031   0.9751    
#nounpubs      -0.113868   0.088188 137.830000  -1.291   0.1988    
#nounwindows   -0.146230   0.082504 135.220000  -1.772   0.0786 .  

# quiet
quiet = p_n[p_n$predicate=="quiet",]
quiet$noun <- factor(quiet$noun,levels=c("dogs","streets"))
qm = lmer(coll~noun+(1|workerid),data=quiet)
summary(qm)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.70667    0.06593 58.00000  10.718 2.22e-15 ***
#  nounstreets -0.05788    0.08891 58.00000  -0.651    0.518   


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



### analyze subject information
s = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-subject_information.tsv",sep="\t",header=T)
s <- s[s$workerid!=8&s$workerid!=20&s$workerid!=30&s$workerid!=47&s$workerid!=73,]
length(unique(s$workerid)) # n=85
summary(s)
# mean age: 36; median age: 31
table(s$education)
# -1      0       1       2         3         4 
# 2 (2%)  3 (4%)  4 (5%) 24 (28%)  36 (42%)  16 (19%)
table(s$enjoyment)
table(s$asses)
table(s$gender)
# female: 42 (49%); male: 43 (51%)
# reward: $0.3