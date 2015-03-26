library(ggplot2)

setwd("~/Documents/git/cocolab/collective/models/")

#no alternatives comparison
a = read.csv("plural-predication-KL-no-alt.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-no-alt.csv",header=F)
b$KL = 0
d = rbind(a,b)
colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","p","KL")
head(d)



## KL
a = read.csv("plural-predication.csv",header=F)
a$neg = 0
a$KL = 0
b = read.csv("plural-predication-neg.csv",header=F)
b$neg = 1
b$KL = 0
c = read.csv("plural-predication-KL.csv",header=F)
c$neg = 0
c$KL = 1
d = read.csv("plural-predication-KL-neg.csv",header=F)
d$neg = 1
d$KL = 1
head(d)

d = rbind(a,b,c,d)
colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","p","neg","KL")
head(d)




###################################




#4 objects fixed thetas
a = read.csv("plural-predication-KL-1-5.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-12.csv",header=F)
b$KL = 0
#d = rbind(a,b)
d <- b
#colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","p","KL")
colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","obj3","p","KL")
head(d)




## check
# (high 2 1 2 ((#f #t) (0.9491058411023046 0.050894158897695496)))
#t <- d[d$noise=="high"&d$numobjs==2&d$dist_theta==1&d$coll_theta==2,]
#t$p <- as.numeric(as.character(t$p))
#aggregate(p~collective,t,sum)

d$noise <-factor(d$noise,levels=c("low",'mid','high'))
d$numobjs <- factor(d$numobjs)
d$dist_theta <- factor(d$dist_theta)
d$coll_theta <- factor(d$coll_theta)
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
ggsave("plots/plural-predication-12.pdf",height=10,width=20)

# check state distribution

t <- d[d$collective=="true",]

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=5,angle=90))+
  facet_grid(KL~dist_theta+coll_theta)
state
ggsave("plots/plural-predication-state-12.pdf",height=5,width=70,limitsize=FALSE)



###################################


## INFERRED THETAS


#4 objects infer thetas
a = read.csv("plural-predication-KL-1-5.csv",header=F)
a$KL = 1
b = read.csv("plural-predication-3-5.csv",header=F)
b$KL = 0
#d = rbind(a,b)
d <- b
#colnames(d) <- c("noise","numobjs","dist_theta","coll_theta","collective","obj1","obj2","p","KL")
colnames(d) <- c("noise","numobjs","collective","obj1","obj2","p","KL")
head(d)


d$noise <-factor(d$noise,levels=c("low",'mid','high'))
d$numobjs <- factor(d$numobjs)
d$dist_theta <- factor(d$dist_theta)
d$coll_theta <- factor(d$coll_theta)
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
ggsave("plots/plural-predication-3-5.pdf",height=10,width=20)

# check state distribution for inferred thetas

t <- d[d$collective=="true",]

state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=15,angle=90))+
  facet_wrap(~KL)
state
ggsave("plots/plural-predication-state-3-5.pdf",height=5,width=25,limitsize=FALSE)




## no neg plots

n <- all[all$neg==1,]

# check effect direction

agg <- aggregate(p~noise*dist_theta*coll_theta*KL,n[n$collective=="true",],sum)

neg <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=as.factor(KL))) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=10,angle=90))+
  facet_grid(dist_theta~coll_theta)
neg
ggsave("plots/plural-predication-neg.pdf",height=10,width=20)

# check state distribution

t <- n[n$collective=="true",]

neg_state <- ggplot(t,aes(x=state,y=p,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  theme(axis.text.x = element_text(size=5,angle=90))+
  facet_grid(KL~dist_theta+coll_theta)
neg_state
ggsave("plots/plural-predication-neg-state.pdf",height=5,width=70,limitsize=FALSE)


