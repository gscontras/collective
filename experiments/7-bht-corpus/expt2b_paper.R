library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(hydroGOF)

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

a_sent_corr = dcast(data=a, sentence ~ sentence_type, value.var="response",mean,na.rm=T)
gof(a_sent_corr$coll,a_sent_corr$dist)# r=-.98,r2=0.96


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
#ggsave("results/bht_plot2.pdf",width=6,height=2.7)

#a_sent_casted$noun <- factor(a_sent_casted$noun,ordered=is.ordered(a_sent_casted$noun))
#a_sent_casted$noun <- factor(a_sent_casted$noun,levels=a_sent_casted[order(a_sent_casted$coll),"noun"])
#mtcars3$carb <-factor(mtcars2$carb, levels=ag_mtcars[order(ag_mtcars$mpg), "carb"])
a_sent_casted$noun <- factor(a_sent_casted$noun,levels=c("houses","children","boys","rooms","waves","windows","buildings","offspring","plants","men","loads","trees","lids","bags"))
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
#violin_plot <- ggplot(a_sent_casted, aes(x=reorder(noun,coll,mean),y=coll)) +
violin_plot <- ggplot(a_sent_casted, aes(x=noun,y=coll)) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9))+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  geom_violin(fill="black")+
  stat_summary(fun.data=data_summary,color="white",fill="red",shape=18,size=.5)+
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  #ylim(0,1) +  
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  facet_wrap(~predicate,scale="free_x")
  #facet_grid(.~predicate,scale="free")
violin_plot
#ggsave("results/bht_plot_violin.png",width=6,height=2.7)



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
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   0.41962    0.06234 58.78000   6.731 7.84e-09 ***
#  nounrooms    -0.18000    0.06065 97.99000  -2.968 0.003766 ** 
#  nounboys     -0.21677    0.06140 98.26000  -3.530 0.000634 ***
#  nounchildren -0.21462    0.06065 97.99000  -3.539 0.000616 ***
#  nounhouses   -0.22597    0.06140 98.26000  -3.680 0.000381 ***
h = lmer(coll~noun+(1|workerid),data=heavy)
summary(h)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  0.63000    0.06543 83.66000   9.629 3.33e-15 ***
#  nounlids    -0.08001    0.07670 94.34000  -1.043  0.29955    
#nountrees   -0.15205    0.07868 95.10000  -1.933  0.05626 .  
#nounloads   -0.24153    0.07673 94.47000  -3.148  0.00220 ** 
#  nounmen     -0.25462    0.07582 94.03000  -3.358  0.00113 ** 
t = lmer(coll~noun+(1|workerid),data=tall)
summary(t)
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)    0.301538   0.064802 50.680000   4.653 2.38e-05 ***
#  nounplants    -0.003846   0.057850 96.070000  -0.066    0.947    
#nounoffspring -0.032781   0.060098 96.470000  -0.545    0.587    
#nounbuildings -0.058077   0.057850 96.070000  -1.004    0.318    
#nounwindows   -0.088686   0.058548 96.200000  -1.515    0.133   

# pred comparison
d <- a_sent_casted
d$predicate <- factor(d$predicate,levels=c("heavy","big","tall"))
contrasts(d$predicate)
m = lmer(coll~predicate+(1+predicate|workerid)+(1|noun),data=d)
summary(m)

## big waves vs. heavy men
bw = big[big$noun=="waves",]
hm = heavy[heavy$noun=="men",]
bwhm = rbind(bw,hm)
head(bwhm)
aggregate(coll~predicate,data=bwhm,mean)
bhm = lmer(coll~predicate+(1|workerid),data=bwhm)
summary(bhm)

tt = tall[tall$noun=="trees",]
tthm = rbind(tt,hm)
aggregate(coll~predicate,data=tthm,mean)
thm = lmer(coll~predicate+(1|workerid),data=tthm)
summary(thm)




### analyze subject information
s = read.table("~/Documents/git/CoCoLab/collective/experiments/7-bht-corpus/Submiterator-master/bht-corpus-subject_information.tsv",sep="\t",header=T)
#s <- s[s$workerid!=16&s$workerid!=17&s$workerid!=18&s$workerid!=27,]
length(unique(s$workerid)) # n=30
summary(s)
# mean age: 38; median age: 36
table(s$education)
# 1       2         3         4 
# 1 (4%)  7 (23%)  19 (62%)  3 (12%)
table(s$enjoyment)
table(s$asses)
table(s$gender)
# female: 25 (83%); male: 5 (17%)
# reward: $0.3