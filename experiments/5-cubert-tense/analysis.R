library(gridExtra)
library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(grid)
library(reshape2)

setwd("~/Documents/git/CoCoLab/collective/experiments/5-cubert-tense/Submiterator-master/")

all_d = read.csv("~/Documents/git/CoCoLab/collective/experiments/5-cubert-tense/Submiterator-master/cubert-tense-trials.tsv",sep="\t",header=T)
head(all_d)

# counts and raw values

table(all_d$sceanrio,all_d$context,all_d$tense,all_d$utterance)

aggregate(response~sceanrio*sentence_type*context*tense*utterance,all_d,mean)

# reshape data

d = dcast(data=all_d, workerid + utterance + sceanrio + context + tense + trial ~ sentence_type, value.var="response",mean)

# nomarlized collectivity score

d$collective = (d$coll / (d$coll + d$dist))
d$collective_ratio = (d$coll / d$dist)
d$coll_diff = (d$coll - d$dist)

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
plot <- plot  +facet_grid(tense ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

#ggsave("expt2collectivebootsci.pdf")


## test

centered = cbind(d, myCenter(d[,c("sceanrio","tense","utterance","context","trial")]))

summary(centered)


#raw collective

raw_m = lmer(coll~
               utterance*ccontext + utterance:csceanrio + csceanrio  + utterance + csceanrio : ctense + ccontext : ctense  + ctrial +
               (1+ctrial|workerid),data=centered)
summary(raw_m)




### extra plots

all_d_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("context","utterance","sceanrio","sentence_type"))

all_plot <- ggplot(all_d_s, aes(x=factor(context,labels=c("high","low")),y=response,fill=sentence_type)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(title="collapsing over tense\n",fill="paraphrase")+
  scale_fill_manual(values=c("red","blue"))+
  facet_grid(sceanrio ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
all_plot

#ggsave("all_results.pdf",height=3.4)

pres_s = bootsSummary(data=all_d[all_d$tense=="present",], measurevar="response", groupvars=c("context","utterance","sceanrio","sentence_type"))

pres_plot <- ggplot(pres_s, aes(x=factor(context,labels=c("high","low")),y=response,fill=sentence_type)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("") +
  labs(title="present tense\n",fill="paraphrase")+
  scale_fill_manual(values=c("red","blue"))+
  facet_grid(sceanrio ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
pres_plot

past_s = bootsSummary(data=all_d[all_d$tense=="past",], measurevar="response", groupvars=c("context","utterance","sceanrio","sentence_type"))

past_plot <- ggplot(past_s, aes(x=factor(context,labels=c("high","low")),y=response,fill=sentence_type)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(title="past tense\n",fill="paraphrase")+
  scale_fill_manual(values=c("red","blue"))+
  facet_grid(sceanrio ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
past_plot

#pdf("tense_plots.pdf")
#grid.arrange(pres_plot, past_plot)
#dev.off()

grid.arrange(pres_plot, past_plot)

tense_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("tense","sentence_type","utterance","context"))

tense_plot <- ggplot(tense_s, aes(x=tense,y=response,fill=sentence_type)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=tense, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n tense") +
 # labs(title="past tense\n",fill="paraphrase")+
  scale_fill_manual(values=c("red","blue"))+
  facet_grid(context ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
tense_plot
