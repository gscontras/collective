setwd("~/Documents/git/CoCoLab/collective/presentations/2015_UCSC/")



# expt 1 plots

d = read.table("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v14.manipulationcheck4/experiment_persistence.v14-master/Submiterator-master/persistence.v14-trials.tsv",sep="\t",header=T)
d$coll = 0
d[d$choice =="coll_list",]$coll = 1
#d = d[d$scenario=="move",]
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

# look at scenario for bare utterances

s = d[d$disambiguator=="",]

d_s = bootsSummary(data=s, measurevar="coll", groupvars=c("utterance","scenario"))

plot <- ggplot(d_s, aes(y=coll,x=utterance,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=utterance, width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("collective choice \n")+
  xlab("\n predicate") +
  labs(fill="scenario") + 
  scale_fill_manual(values=c("cyan", "orange","blue"))
#theme(legend.position="bottom", legend.margin=unit(-.7,"cm") )
plot <- plot   + theme_blackDisplay()
plot

ggsave("expt1_all_XPRAG.pdf",width=11, height=5.5)


# expt 2 plots

d = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

#d = d[d$sceanrio=="move",]

d$utterance <-factor(d$utterance,levels=c("big",'heavy','tall'))

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

ggsave("expt3-no-scenario.pdf",width=18, height=7)


## predicate with all data

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("sentence_type","context","utterance","sceanrio"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("variable","predictable")),y=response,fill=factor(sentence_type,labels=c("together","each")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("variable","predictable")), width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("endorsement \n")+
  ylim(0,1)+
  xlab("\n context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("blue","red"))
plot <- plot  +facet_grid(sceanrio ~ utterance) + theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt3-ALL_XPRAG.pdf",width=18, height=9)


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

ggsave("expt3-context.pdf",width=12, height=7)


# all predicates results

h_s = bootsSummary(data=d[d$utterance=="heavy",], measurevar="response", groupvars=c("sentence_type","context","sceanrio"))

plot <- ggplot(h_s, aes(x=factor(context,labels=c("variable","predictable")),y=response,fill=factor(sentence_type,labels=c("together","each")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("variable","predictable")), width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("endorsement \n")+
  ylim(0,1)+
  xlab("\n context") +
  labs(fill="paraphrase")+
  facet_wrap(~sceanrio) +
  scale_fill_manual(values=c("blue","red"))
plot <- plot  + theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt3-heavy.pdf",width=14, height=7)



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

low <- ggplot(data.frame(y=c(.125, .75, .125),
                         x=c(0.9, 1, 1.1) ),
              aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#F45E5B",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()+
  theme(axis.text.y = element_blank())
low
ggsave(filename='low.png',plot=low,width=4.5, height=3)

mid <- ggplot(data.frame(y=c(.2, .6, .2),
                         x=c(0.7, 1, 1.3) ),
              aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#17B32B",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()+
  theme(axis.text.y = element_blank())
mid
ggsave(filename='mid.png',plot=mid,width=4.5, height=3)

high <- ggplot(data.frame(y=c(.333, .333, .333),
                          x=c(0.5, 1, 1.5) ),
               aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#5187FF",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  theme_blackDisplay()+
  theme(axis.text.y = element_blank())
high
ggsave(filename='high.png',plot=high,width=4.5,height=3)

no <- ggplot(data.frame(y=c(0,1,0),
                          x=c(0.94, 1, 1.06) ),
               aes(x=x, y=y)) +
  geom_bar(alpha=.5,fill="#9fb317",stat='identity',position=position_dodge())+
  ylab("") +
  xlab("")+
  ylim(0,1)+
  #xlim(c(0.5,1.5))+
  theme_blackDisplay()
no
ggsave(filename='no.png',plot=no,width=5.5,height=3)


### Expt 2a corpus sentence plot

d = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-trials.tsv",sep="\t",header=T)
#only native English
d <- d[d$workerid!=8&d$workerid!=20&d$workerid!=30&d$workerid!=47&d$workerid!=73,]
s = read.csv("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/bnc.sentences.csv",header=T)
# counts and raw values
head(d)
table(d$predicate,d$noun)
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
a_sent_casted <- na.omit(a_sent_casted)

sentence_s = bootsSummary(data=a_sent_casted, measurevar="coll", groupvars=c("sentence","animate"))
sentence_s$sentence <- factor(sentence_s$sentence,ordered=is.ordered(sentence_s$sentence))
sentence_plot <- ggplot(sentence_s, aes(x=reorder(sentence,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge(),fill="white") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(sentence,coll,mean), width=0.1),position=position_dodge(width=0.9),color="grey")+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsentence")+
  ylim(0,1) + 
  scale_fill_manual(values=c("grey","white")) +
  theme_blackDisplay() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=14))
sentence_plot

ggsave('sentence_plot.pdf',width=11,height=7)

## Expt 2a small plot

head(a_sent_casted)
p_n <- a_sent_casted[a_sent_casted$predicate=="small",]
summary(p_n)
n_pred_s = bootsSummary(data=p_n, measurevar="coll", groupvars=c("noun"))
n_pred_s$noun <- factor(n_pred_s$noun,ordered=is.ordered(n_pred_s$noun))
n_pred_plot <- ggplot(n_pred_s, aes(x=reorder(noun,coll,mean),y=coll)) +
  geom_bar(stat="identity",position=position_dodge(),fill="white") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9),color="grey")+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nnoun")+
  ylim(0,1) + 
  scale_fill_manual(values=c("grey","white")) +
  theme_blackDisplay() #+
  #theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
n_pred_plot

ggsave('small_plot.pdf',height=6,width=9.5)


## expt 2b big-heavy-tall plot

d = read.table("~/Documents/git/CoCoLab/collective/experiments/7-bht-corpus/Submiterator-master/bht-corpus-trials.tsv",sep="\t",header=T)
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
  geom_bar(stat="identity",position=position_dodge(),fill="white") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(noun,coll,mean), width=0.1),position=position_dodge(width=0.9),color="grey")+
  #geom_text(size=2,alpha=.5,aes(label=noun),angle=45) +
  ylab("collective endorsement\n") +
  xlab("noun")+
  ylim(0,1) + 
  scale_fill_manual(values=c("grey","white")) +
  theme_blackDisplay() +
theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  facet_wrap(~predicate,ncol=3,scale="free_x")
all_plot
ggsave("bht_corpus.pdf",width=10,height=6.5)



### model results

d = read.csv("~/Documents/git/cocolab/collective/model/plural-predication.csv",header=F)
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)
d$noise <-factor(d$noise,levels=c("no","low",'mid','high'))
d$numobjs <- factor(d$numobjs)
#d$state = paste(d$obj1,d$obj2)
#d$state = paste(d$obj1,d$obj2,d$obj3)
d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "partial"
d[d$knowledge=="true",]$k = "full"
d$k <-factor(d$k,levels=c("partial","full"))
head(d)
# check effect direction for inferred thetas
agg <- aggregate(p~noise*k,d[d$collective=="true",],sum)
p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  scale_fill_manual(values=c("orange", "cyan"))+
  theme(axis.text.x = element_text(size=10,angle=0))+
  theme_blackDisplay()#+
#facet_grid(dist_theta~coll_theta)
p

ggsave("model-results_XPRAG.pdf",width=10,height=6)


#### heavy plot

heavy = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/heavy2.csv",header=T)

head(heavy)
heavy$context <- factor(heavy$context,labels=c("variable","predictable"))

heavy_plot <- ggplot(heavy, aes(x=data,y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9),color="gray")+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("") +
  labs(fill="scenario")+
  scale_fill_manual(values=c("cyan", "orange"))+
  facet_grid(.~context)+
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
theme_blackDisplay()
heavy_plot
ggsave("heavymodel_XPRAG.pdf",width=12,height=6)


## big-tall plot

bigtall = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/big-tall.csv",header=T)

bt_plot <- ggplot(bigtall, aes(x=data,y=coll,fill=factor(context,labels=c("variable","predictable")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9),color="grey")+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("") +
  labs(fill="context")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~predicate) +
  theme_blackDisplay()
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
bt_plot
ggsave("bigtallmodel_XPRAG.pdf",width=12,height=6)
