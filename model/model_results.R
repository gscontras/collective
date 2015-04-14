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







### test plots


d = read.csv("plural-predication-1.csv",header=F)
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","p")
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)


d$noise <-factor(d$noise,levels=c("high","mid",'low','no'))
#d$k <-factor(d$k,levels=c("partial","full"))
d$numobjs <- factor(d$numobjs)
#d$state = paste(d$obj1,d$obj2)
d$state = paste(d$obj1,d$obj2,d$obj3)
#d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
d$k = "partial"
d[d$knowledge=="true",]$k = "full"
head(d)

# check effect direction for inferred thetas

agg <- aggregate(p~noise*k,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  scale_fill_manual(values=c("red", "blue"))+
  theme(axis.text.x = element_text(size=10,angle=0))#+
#facet_grid(dist_theta~coll_theta)
p

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-results.pdf")

# check state distribution for inferred thetas

t <- d[d$collective=="true",]

state_agg <- aggregate(p~noise*knowledge*state*collective,d,sum)

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=15,angle=90))+
  facet_grid(knowledge~.)
state
ggsave("plots/plural-predication-20-state.pdf",height=5,width=40,limitsize=FALSE)



### heavy plots

setwd("~/Documents/git/CoCoLab/collective/model/")

d = read.csv("plural-predication-big.csv",header=F)
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","p")
#colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","p")
colnames(d) <- c("noise","numobjs","knowledge","collective","obj1","obj2","obj3","obj4","p")
head(d)


#d$noise <-factor(d$noise,levels=c("low","mid","high"))
#d$k <-factor(d$k,levels=c("partial","full"))
d$numobjs <- factor(d$numobjs)
#d$state = paste(d$obj1,d$obj2)
#d$state = paste(d$obj1,d$obj2,d$obj3)
d$state = paste(d$obj1,d$obj2,d$obj3,d$obj4)
d$state <- factor(d$state)
#d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))
#d$k = "partial"
#d[d$knowledge=="true",]$k = "full"
head(d)

# check effect direction for inferred thetas

agg <- aggregate(p~noise,d[d$collective=="true",],sum)
agg


# a=2     .1414
# a=1.8   .1451
# a=1.75  .1462
# a=1.6   .1488
# a=1.5   .1501
# a-1.45  .1505
# a=1.4   .1507
# a=1.375 .1507
# a=1.35  .1507
# a=1.3   .1504

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  scale_fill_manual(values=c("red", "blue"))+
  theme(axis.text.x = element_text(size=10,angle=0))#+
#facet_grid(dist_theta~coll_theta)
p

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")

ggsave("model-results.pdf")





