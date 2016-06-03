library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")



d = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v14.manipulationcheck4/experiment_persistence.v14-master/Submiterator-master/persistence.v14-trials.tsv",sep="\t",header=T)

s = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v14.manipulationcheck4/experiment_persistence.v14-master/Submiterator-master/persistence.v14-subject_information.tsv",sep="\t",header=T)
summary(s)

d$coll = 0
d[d$choice =="coll_list",]$coll = 1

summary(d)

d$coll = as.factor(as.character(d$coll))
contrasts(d$coll)
contrasts(d$utterance)
contrasts(d$disambiguator)
table(d$utterance, d$disambiguator, d$scenario)

et = droplevels(subset(d, disambiguator != ""))
centered = cbind(et, myCenter(et[,c("disambiguator","scenario","trial")]))
nrow(centered)
head(centered)
str(centered)
#m = glmer(coll ~ cdisambiguator + ctrial + (1+cdisambiguator|workerid) + (1|utterance) + (1|scenario), data = centered, family="binomial")
#summary(m)

#if we include predicate as a predictor
m = glmer(coll ~ cdisambiguator + cdisambiguator:utterance + ctrial + (1|workerid) + (1|scenario), data = centered, family="binomial")
summary(m)

bare = droplevels(subset(d, disambiguator == ""))
centered = cbind(bare, myCenter(bare[,c("scenario","trial","utterance")]))
nrow(centered)
head(centered)
str(centered)
table(centered$utterance,centered$cscenario,centered$coll)
m = glmer(coll ~ utterance + cscenario + ctrial + (1+ctrial|workerid), data = centered, family="binomial")
summary(m)

m =glm(choice ~ utterance * cscenario, data = centered, family="binomial")
summary(m)

heavy = droplevels(subset(bare, utterance == "heavy" & disambiguator == ""))
heavy = cbind(heavy, myCenter(heavy[,c("scenario","trial")]))
nrow(heavy)
m = glm(coll ~ cscenario, data=heavy, family="binomial")
summary(m)

t.test(as.numeric(coll) ~ scenario,data=heavy)


# counts and raw values

table(d$scenario,d$utterance)

# plots

d_s = bootsSummary(data=d, measurevar="coll", groupvars=c("utterance","disambiguator","scenario"))

plot <- ggplot(d_s, aes(x=factor(disambiguator,labels=c("bare","each","tog.")),y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(disambiguator,labels=c("bare","each","tog.")), width=0.1),position=position_dodge(width=0.9))+
  ylab("collective choice rate \n")+
  xlab("\n utterance") +
  scale_fill_manual(values=c("red","blue"))
  #labs(title="collective choice (95% bootsci)")
plot <- plot  +facet_grid( ~ utterance)
  #+ theme_blackDisplay()
plot

ggsave(filename='expt1bootsci2.pdf',width=7.15, height=2.15)

d_agg = aggregate(coll~utterance*disambiguator*scenario,data=d,mean)
d_agg$ci_low = d_agg$coll - aggregate(coll~utterance*disambiguator*scenario,data=d,se)$coll
d_agg$ci_high = d_agg$coll + aggregate(coll~utterance*disambiguator*scenario,data=d,se)$coll

plot <- ggplot(d_agg, aes(x=factor(disambiguator,labels=c("bare","each","tog.")),y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high, x=factor(disambiguator,labels=c("bare","each","tog.")), width=0.1),position=position_dodge(width=0.9))+
  ylab("collective choice rate \n")+
  xlab("\n disambiguator") +
  scale_fill_manual(values=c("red","blue"))+
  labs(title="collective choice (se)")
plot <- plot  +facet_grid( ~ utterance)
#+ theme_blackDisplay()
plot

ggsave(filename='expt1se.png',plot=plot,width=7.15, height=2.15)


## looking only at first trials


t1 = d[d$trial==1,]

table(t1$disambiguator,t1$utterance)
aggregate(coll~utterance*disambiguator*scenario,data=t1,mean)

t1_s = bootsSummary(data=t1, measurevar="coll", groupvars=c("scenario","utterance","disambiguator"))

plot <- ggplot(t1_s, aes(x=factor(disambiguator,labels=c("bare","each","together")),y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(disambiguator,labels=c("bare","each","together")), width=0.1),position=position_dodge(width=0.9))+
  ylab("collective choice \n")+
  xlab("\n utterance")
#+ labs(fill="utterance")
plot <- plot  +facet_grid( ~ utterance)
#+ theme_blackDisplay()
plot



### analyze subject information
s = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v14.manipulationcheck4/experiment_persistence.v14-master/Submiterator-master/persistence.v14-subject_information.tsv",sep="\t",header=T)
summary(s)
# mean age: 31; median age: 29
table(s$education)
# 1       2         3         4 
# 2 (4%) 15 (30%)  29 (58%)  4 (8%)
table(s$enjoyment)
table(s$asses)
table(s$gender)
# female: 21 (42%); male: 29 (58%)
# reward: $0.25