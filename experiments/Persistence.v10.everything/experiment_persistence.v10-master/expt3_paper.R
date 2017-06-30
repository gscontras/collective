library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(grid)
library(hydroGOF)

setwd("~/git/collective/experiments/Persistence.v10.everything/experiment_persistence.v10-master/")
source("analysis/helpers.R")

d = read.csv("~/git/collective/experiments/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

s = read.table("~/git/collective/experiments/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-subject_information.tsv",sep="\t",header=T)
head(s)
summary(s)
d$language = s$language[match(d$workerid,s$workerid)]
summary(d)
d = d[d$language!="Spanish "&d$language!="spanish"&d$language!="Russian",]
summary(d)
unique(d$workerid) ## 77 participants

#save for archiving
#write.csv(d,"~/git/collective/writing/Cubert/revision2/data/expt3.csv")

  
# counts and raw values

table(d$sceanrio,d$context)

aggregate(response~sceanrio*sentence_type*context*utterance,d,mean)

# reshape data

d = dcast(data=d, workerid + trial + utterance + sceanrio + context ~ sentence_type, value.var="response",mean)

# nomarlized collectivity score

d$collective = (d$coll / (d$coll + d$dist))
d$collective_ratio = (d$coll / d$dist)
d$coll_diff = (d$coll - d$dist)

# tests

centered = cbind(d, myCenter(d[,c("sceanrio","trial","utterance","context")]))

summary(d)

# collective difference score

coll_m = lmer(coll_diff~
           utterance*ccontext + utterance:csceanrio + csceanrio + ctrial +
           (1+ctrial|workerid),data=centered)
summary(coll_m)

#raw collective

raw_m = lmer(coll~
            utterance*ccontext + utterance:csceanrio + csceanrio + ctrial +
                (1+ctrial|workerid),data=centered)
summary(raw_m)

#raw interaction collective

int_m = lmer(coll~
               utterance:ccontext + utterance:csceanrio + ctrial +
               (1+ctrial|workerid),data=centered)
summary(int_m)

gof(centered$coll,centered$dist) # r = -.62, r2=.38

big = centered[centered$utterance=="big",]
b_m = glm(coll~ ccontext * csceanrio + ctrial, data=big)
#b_m = lmer(coll~ ccontext + csceanrio + ctrial + (1|workerid), data=big)
summary(b_m)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         0.572511   0.036483  15.692   <2e-16 ***
#  ccontext            0.183252   0.072818   2.517   0.0141 *  
#  csceanrio           0.002504   0.073810   0.034   0.9730    
#ctrial             -0.027741   0.041526  -0.668   0.5063    
#ccontext:csceanrio  0.209416   0.146415   1.430   0.1570  

heavy = centered[centered$utterance=="heavy",]
h_m = glm(coll~ ccontext * csceanrio + ctrial, data=heavy)
#h_m = lmer(coll~ ccontext + csceanrio + ctrial + (1|workerid), data=heavy)
summary(h_m)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         0.60603    0.03681  16.465   <2e-16 ***
#  ccontext            0.10685    0.07477   1.429   0.1573    
#csceanrio           0.19286    0.07593   2.540   0.0132 *  
#  ctrial              0.02121    0.05004   0.424   0.6729    
#ccontext:csceanrio -0.12233    0.14792  -0.827   0.4110 

tall = centered[centered$utterance=="tall",]
t_m = glm(coll_diff~ ccontext * csceanrio + ctrial, data=tall)
#t_m = lmer(coll~ ccontext + csceanrio + ctrial + (1|workerid), data=tall)
summary(t_m)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         0.65551    0.03279  19.991  < 2e-16 ***
#  ccontext            0.30347    0.06631   4.577 1.93e-05 ***
#  csceanrio           0.07334    0.06604   1.110    0.271    
#ctrial             -0.01029    0.04088  -0.252    0.802    
#ccontext:csceanrio  0.02300    0.13227   0.174    0.862   


## get mean collective endorsement for heavy for model plot

h_s = bootsSummary(data=heavy, measurevar="coll", groupvars=c("sceanrio"))
h_s


# plots

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots")

# raw sentence_type ratings plot

d_raw = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)
s = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-subject_information.tsv",sep="\t",header=T)
head(s)
summary(s)
d_raw$language = s$language[match(d_raw$workerid,s$workerid)]
summary(d_raw)
d_raw = d_raw[d_raw$language!="Spanish "&d_raw$language!="spanish"&d_raw$language!="Russian",]
summary(d_raw)
unique(d_raw$workerid) ## 77 participants

raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","utterance","sceanrio"))

raw_plot <- ggplot(raw_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))
raw_plot <- raw_plot  +facet_grid(sceanrio ~ utterance) + theme_bw()
raw_plot
#ggsave("expt3.png",width=6,height=2.7)

data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
violin_plot <- ggplot(d_raw, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  geom_violin()+
  stat_summary(fun.data=data_summary,color="white",shape=18,size=.5,position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))
violin_plot <- violin_plot  +facet_grid(sceanrio ~ utterance) + theme_bw()
violin_plot
#ggsave("expt3-violin.png",width=6,height=2.7)



# get model results and combine with expt results

h_m = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/heavy.csv",header=T)
head(h_m)
head(h_s)
h_s$data = "human"
h_s$N = NULL
h_m$data = "model"
h_m$scenario = "move"
h_m[h_m$k=="full",]$scenario = "inspect"
h_m$k = NULL
h_m$noise = NULL
h_m$X = NULL
colnames(h_s) = c("scenario","coll","bootsci_high","bootsci_low","data")
colnames(h_m) = c("coll","data","scenario")
h_m$bootsci_high = NA
h_m$bootsci_low = NA
heavy = rbind(h_m,h_s)
head(heavy)

heavy_plot <- ggplot(heavy, aes(x=data,y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\n source of data") +
  labs(fill="scenario")+
  scale_fill_manual(values=c("red", "blue"))+
  theme_bw()
  #facet_grid(.~context)
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
heavy_plot
ggsave("heavymodel.pdf",height=3,width=5)


#tall_big_s = bootsSummary(data=d_casted[d_casted$utterance!="heavy",], measurevar="coll", groupvars=c("context","utterance"))

bigtall = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/big-tall.csv",header=T)

bt_plot <- ggplot(bigtall, aes(x=data,y=coll,fill=context)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\n source of data") +
  labs(fill="context")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~predicate) +
  theme_bw()
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
bt_plot
ggsave("bigtallmodel.pdf",height=3)



### analyze subject information
s = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-subject_information.tsv",sep="\t",header=T)
#s = s[s$language!="Spanish "&s$language!="spanish"&s$language!="Russian",]
summary(s)
# mean age: 32; median age: 29
table(s$education)
# 1       2         3         4 
# 8 (9%) 34 (42%)  31 (40%)  7 (9%)
table(s$enjoyment)
table(s$asses)
table(s$gender)
# female: 26 (31%); male: 54 (69%)
# reward: $0.25