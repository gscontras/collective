library(ggplot2)

setwd("~/Documents/git/cocolab/collective/model/")


###################################


## INFERRED THETAS


#4 objects infer thetas
a = read.csv("plural-predication-KL.csv",header=F)
a$KL = 1
b = read.csv("plural-predication.csv",header=F)
#b$KL = 0
#d = rbind(a,b)
d <- b
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)


d$noise <-factor(d$noise,levels=c("no","low",'mid','high'))
d$numobjs <- factor(d$numobjs)
d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
#d$state = paste(d$obj1,d$obj2)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))

# check effect direction for inferred thetas

agg <- aggregate(p~noise*knowledge,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=knowledge)) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=10,angle=0))#+
  #facet_grid(dist_theta~coll_theta)
p
ggsave("plots/plural-predication.pdf",height=3,width=8)

# check state distribution for inferred thetas

t <- d[d$collective=="true",]

state_agg <- aggregate(p~noise*knowledge*state*collective,d,sum)

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=15,angle=90))+
  facet_grid(knowledge~.)
state
ggsave("plots/plural-predication-state.pdf",height=5,width=40,limitsize=FALSE)







### unfit plots
setwd("~/Documents/git/cocolab/collective/model/")

d = read.csv("plural-predication-sum.csv",header=F)
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","p")
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)


d$noise <-factor(d$noise,levels=c("high","mid",'low','no'))
#d$k <-factor(d$knowledge,labels=c("partial","full"))
d$numobjs <- factor(d$numobjs)
#d$state = paste(d$obj1,d$obj2)
d$state = paste(d$obj1,d$obj2,d$obj3)
#d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "sum"
d[d$knowledge=="true",]$k = "full"
head(d)
d$noise <-factor(d$noise,levels=c("no","low",'mid','high'))
d$noise <-factor(d$noise,labels=c("no\n(\u03c3=0.01)","low\n(\u03c3=0.75)",'mid\n(\u03c3=1)','high\n(\u03c3=1.25)'))

# check effect direction for inferred thetas

agg <- aggregate(p~noise*k,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  scale_fill_manual(values=c("red", "blue"))+
  theme(axis.text.x = element_text(size=10,angle=0))+
  theme_bw()
#facet_grid(dist_theta~coll_theta)
p

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-results-sum.png",height=3)


## general plots

setwd("~/Documents/git/cocolab/collective/model/")

d = read.csv("plural-predication-add.csv",header=F)
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","p")
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)


d$noise <-factor(d$noise,levels=c("no","low",'mid','high'))
d$noise <-factor(d$noise,labels=c("no\n(\u03c3=0.02)","low\n(\u03c3=1)",'mid\n(\u03c3=2)','high\n(\u03c3=3)'))
#d$pred = as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 1))
#d$variability= as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 2))
#d$k <-factor(d$knowledge,labels=c("partial","full"))
d$numobjs <- factor(d$numobjs)
d$state = paste(d$obj1,d$obj2)
#d$state = paste(d$obj1,d$obj2,d$obj3)
#d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "sum"
d[d$knowledge=="true",]$k = "full"
head(d)

# check effect direction for inferred thetas

agg <- aggregate(p~k*noise,d[d$collective=="true",],sum)

#agg$interpretation = factor(agg$interpretation,levels=c("distributive","collective"))

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  scale_fill_manual(values=c("red", "blue"))+
  theme(axis.text.x = element_text(size=10,angle=0))+
  theme_bw()
#facet_grid(dist_theta~coll_theta)
p

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-results-add.png",width=6,height=2.7)



##### big-tall plots

## results from experiment 3

d_raw = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

bt = d_raw[d_raw$utterance!= "heavy",]

bt_s = bootsSummary(data=bt, measurevar="response", groupvars=c("sentence_type","context","utterance"))

setwd("~/Documents/git/cocolab/collective/model/")

d = read.csv("plural-predication-big-tall.csv",header=F)
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
head(d)
d$pred = as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 1))
d$variability= as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 2))
d$numobjs <- factor(d$numobjs)
d$state = paste(d$obj1,d$obj2)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "sum"
d[d$knowledge=="true",]$k = "full"
head(d)
agg <- aggregate(p~k*pred*variability,d[d$collective=="true",],sum)
agg$interpretation = "coll"
head(agg)
#agg = aggregate(p~pred*variability*interpretation,agg,mean)
colnames(agg) = c("sceanrio","utterance","context","response","sentence_type")
agg$context = factor(agg$context,labels=c("random","regular"))
#agg$sceanrio = factor(agg$sceanrio,labels=c("inspect","move"))
agg$N = NA
agg$bootsci_high = NA
agg$bootsci_low = NA
agg$sceanrio = NULL
agg$data = "model"
head(bt_s)
bt_s$data = "human"
bt_coll = bt_s[bt_s$sentence_type=="coll",]
all_agg = rbind(bt_coll,agg)
head(all_agg)

all_plot <- ggplot(all_agg, aes(x=context,y=response,fill=data)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="source\nof data")+
  scale_fill_manual(values=c("red", "blue"))+
  #facet_grid(data~utterance) + 
  facet_grid(. ~ utterance) + 
  theme_bw()
all_plot

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-big-tall.pdf",height=2.7)



##### heavy plots

## results from experiment 3

d_raw = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

h = d_raw[d_raw$utterance== "heavy",]

h_s = bootsSummary(data=h, measurevar="response", groupvars=c("sentence_type","sceanrio","utterance"))

setwd("~/Documents/git/cocolab/collective/model/")

d = read.csv("plural-predication-heavy.csv",header=F)
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
head(d)
d$pred = as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 1))
d$variability= as.character(lapply(strsplit(as.character(d$noise), split="-"), "[", 2))
d$numobjs <- factor(d$numobjs)
d$state = paste(d$obj1,d$obj2)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "sum"
d[d$knowledge=="true",]$k = "full"
head(d)
agg <- aggregate(p~k*pred,d[d$collective=="true",],sum)
agg$interpretation = "coll"
head(agg)
#agg = aggregate(p~pred*variability*interpretation,agg,mean)
colnames(agg) = c("sceanrio","utterance","response","sentence_type")
#agg$context = factor(agg$context,labels=c("random","regular"))
agg$sceanrio = factor(agg$sceanrio,labels=c("inspect","move"))
agg$N = NA
agg$bootsci_high = NA
agg$bootsci_low = NA
#agg$sceanrio = NULL
agg$data = "model"
head(h_s)
h_s$data = "human"
h_coll = h_s[h_s$sentence_type=="coll",]
all_agg = rbind(h_coll,agg)
head(all_agg)

all_plot <- ggplot(all_agg, aes(x=sceanrio,y=response,fill=data)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=sceanrio, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective\n")+
  ylim(0,1)+
  xlab("\ncontext scenario") +
  labs(fill="source\nof data")+
  scale_fill_manual(values=c("red", "blue"))+
  #facet_grid(data~utterance) + 
  facet_grid(. ~ utterance) + 
  theme_bw()
all_plot

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-heavy.pdf",height=2.7,width=4)







### new plot for all predicates

d_raw = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)
raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","sceanrio","utterance","context"))
raw_s = raw_s[raw_s$sentence_type == "coll",]
raw_s$data = "human"

m = read.csv("new_plot_model.csv",header=T)
all = rbind(raw_s,m)

all_plot <- ggplot(all, aes(x=context,y=response,fill=data)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="source\nof data")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(sceanrio ~ utterance) + 
  theme_bw()

all_plot

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")
ggsave("model_all_new.pdf",width=6,height=2.7)
