setwd("~/Documents/git/CoCoLab/collective/presentations/2015_SemFest/")



# expt 1 plots

d = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v14.manipulationcheck4/experiment_persistence.v14-master/Submiterator-master/persistence.v14-trials.tsv",sep="\t",header=T)

d$coll = 0
d[d$choice =="coll_list",]$coll = 1

d = d[d$scenario=="move",]

d$utterance <- factor(d$utterance,levels=c("big","tall","heavy"))

# each predicate

d_s = bootsSummary(data=d, measurevar="coll", groupvars=c("utterance","disambiguator"))

plot <- ggplot(d_s, aes(y=coll,x=utterance,fill=factor(disambiguator,labels=c("bare","each","together")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=utterance, width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("collective choice \n")+
  xlab("\n predicate") +
  labs(fill="utterance") + 
  scale_fill_manual(values=c("white", "red","blue"))
#theme(legend.position="bottom", legend.margin=unit(-.7,"cm") )
plot <- plot   + theme_blackDisplay()
plot

ggsave("expt1.pdf",width=11, height=5.5)

# collapse predicates

d_s = bootsSummary(data=d, measurevar="coll", groupvars=c("disambiguator"))

plot <- ggplot(d_s, aes(y=coll,x=factor(disambiguator,labels=c("bare","each","together")),fill=factor(disambiguator,labels=c("bare","each","together")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(disambiguator,labels=c("bare","each","together")), width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("collective choice \n")+
  xlab("\n utterance") +
  labs(fill="utterance") + 
  scale_fill_manual(values=c("white", "red","blue"))
#theme(legend.position="bottom", legend.margin=unit(-.7,"cm") )
plot <- plot   + theme_blackDisplay()
plot

ggsave("expt1_1.pdf",width=11, height=5.5)




# expt 2 plots

d = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

d = d[d$sceanrio=="move",]

d$utterance <-factor(d$utterance,levels=c("big",'tall','heavy'))

# each predicate results

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("sentence_type","context","utterance"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("variable","predictable")),y=response,fill=factor(sentence_type,labels=c("together","each")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("variable","predictable")), width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("endorsement \n")+
  ylim(0,1)+
  xlab("\n context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("blue","red"))
plot <- plot  +facet_grid(. ~ utterance) + theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt2.pdf",width=18, height=7)

# all predicates results

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("sentence_type","context"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("variable","predictable")),y=response,fill=factor(sentence_type,labels=c("together","each")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("variable","predictable")), width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("endorsement \n")+
  ylim(0,1)+
  xlab("\n context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("blue","red"))
plot <- plot  + theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt2_1.pdf",width=12, height=7)



#;;((high ((#f #t) (0.7596204935730887 0.24037950642691144))) 
#  ;; (mid ((#f #t) (0.7266653433219182 0.2733346566780817))) 
#    ;; (low ((#f #t) (0.7124395971249051 0.28756040287509493))))


# model plots

d = read.csv('model_results.csv',header=T)

d$noise<-factor(d$noise,levels=c("low",'mid','high'))


p <- ggplot(d, aes(y=coll,x=noise,fill=noise)) +
  geom_bar(alpha=1/2,stat='identity',position=position_dodge()) +
  #	xlab("distributive threshold") +
  ylab("likelihood of collective interpretation\n") +
  xlab("\n noise")+
  ylim(0,.4) +
  #scale_color_continuous(breaks=c(-1,0,1)) +
  #labs(color="collective difficulty",title="State")+ 
  #facet_wrap( ~ dist, ncol=2) + 
  theme_blackDisplay() +guides(fill=FALSE)
p

ggsave('plot.pdf',width=11, height=9)


# KL model plots

d = read.csv('kl_model_results.csv',header=T)

d$noise<-factor(d$noise,levels=c("low",'mid','high'))

agg = aggregate(coll~scenario,d,mean)


p <- ggplot(d, aes(y=coll,x=noise,fill=scenario)) +
  geom_bar(alpha=1/2,stat='identity',position=position_dodge()) +
  #  xlab("distributive threshold") +
  ylab("collective?\n") +
  xlab("\n noise")+
  #ylim(0,.4) +
  #scale_color_continuous(breaks=c(-1,0,1)) +
  #labs(color="collective difficulty",title="State")+ 
  #facet_wrap( ~ dist, ncol=2) + 
  theme_blackDisplay() +
  #guides(fill=FALSE) +
  scale_fill_manual(values=c("#F45E5B", "#5187FF"))
p

ggsave('scenario-plot.pdf',width=11, height=9)



#noise plots

low <- ggplot(data.frame(y=c(.1, .8, .1),
                         x=c(0.5, 1, 1.5) ),
              aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#F45E5B",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()

ggsave(filename='low.png',plot=low,width=5, height=3)

mid <- ggplot(data.frame(y=c(.2, .6, .2),
                         x=c(0.5, 1, 1.5) ),
              aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#17B32B",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()+
  

ggsave(filename='mid.png',plot=mid,width=5, height=3)

high <- ggplot(data.frame(y=c(.333, .333, .333),
                          x=c(0.5, 1, 1.5) ),
               aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#5187FF",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()

ggsave(filename='high.png',plot=high,width=5, height=3)
