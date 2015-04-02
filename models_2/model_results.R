library(ggplot2)

setwd("~/Documents/git/cocolab/collective/models_2/")



###################################




#4 objects fixed thetas
a = read.csv("plural-predication-KL-4-23.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-4-23.csv",header=F)
b$KL = 0
d = rbind(a,b)
#d <- b
#colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","p","KL")
colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","obj3","p","KL")
head(d)

d$noise <-factor(d$noise,levels=c("low",'mid','high'))
d$numobjs <- factor(d$numobjs)
d$dist_theta <- factor(d$dist_theta)
d$coll_theta <- factor(d$coll_theta)
#d$state = paste(d$obj1,d$obj2)
d$state = paste(d$obj1,d$obj2,d$obj3)
d$state <- factor(d$state)
d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))

#all <- d

## no neg plots

#d <- d[d$neg==0,]

# check effect direction for fixed thetas

agg <- aggregate(p~noise*dist_theta*coll_theta*KL,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=as.factor(KL))) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=10,angle=90))+
  facet_grid(dist_theta~coll_theta)
p
ggsave("plots/plural-predication-4-23.pdf",height=10,width=20)

# check state distribution

t <- d[d$collective=="true",]

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=5,angle=90))+
  facet_grid(KL~dist_theta+coll_theta)
state
ggsave("plots/plural-predication-state-4-23.pdf",height=5,width=150,limitsize=FALSE)



###################################


## INFERRED THETAS


#4 objects infer thetas
a = read.csv("plural-predication-noah-KL.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-noah.csv",header=F)
b$KL = 0
d = rbind(a,b)
#d <- b
colnames(d) <- c("noise","numobjs","collective","obj1","obj2","p","KL")
#colnames(d) <- c("noise","numobjs","collective","obj1","obj2","obj3","p","KL")
head(d)


d$noise <-factor(d$noise,levels=c("low",'mid','high'))
d$numobjs <- factor(d$numobjs)
#d$state = paste(d$obj1,d$obj2,d$obj3)
d$state = paste(d$obj1,d$obj2)
d$state <- factor(d$state)
d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))

# check effect direction for inferred thetas

agg <- aggregate(p~noise*KL,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=as.factor(KL))) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=10,angle=90))#+
  #facet_grid(dist_theta~coll_theta)
p
ggsave("plots/plural-predication-noah.pdf",height=3,width=8)

# check state distribution for inferred thetas

t <- d[d$collective=="true",]

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=15,angle=90))+
  facet_grid(KL~.)
state
ggsave("plots/plural-predication-state-noah.pdf",height=5,width=10,limitsize=FALSE)







########################


## KL FLIP

#4 objects infer thetas
a = read.csv("plural-predication-KL-4-23.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-4-23.csv",header=F)
b$KL = 0
d = rbind(a,b)
#d <- a
colnames(d) <- c("noise","numobjs","collective","obj1","obj2","obj3","p","KL")
#colnames(d) <- c("noise","numobjs","collective","obj1","obj2","obj3","p","KL")
head(d)


d$noise <-factor(d$noise,levels=c("low",'mid','high'))
d$numobjs <- factor(d$numobjs)
d$state = paste(d$obj1,d$obj2,d$obj3)
#d$state = paste(d$obj1,d$obj2)
d$state <- factor(d$state)
d$KL <- factor(d$KL)
d$p <- as.numeric(as.character(d$p))

# check effect direction for inferred thetas

agg <- aggregate(p~noise*KL,d[d$collective=="true",],sum)

p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=as.factor(KL))) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=10,angle=90))#+
#facet_grid(dist_theta~coll_theta)
p
ggsave("plots/plural-predication-4-23.pdf",height=3,width=8)

# check state distribution for inferred thetas

t <- d[d$collective=="true",]

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=15,angle=90))+
  facet_grid(KL~.)
state
ggsave("plots/plural-predication-4-23-state.pdf",height=5,width=10,limitsize=FALSE)



