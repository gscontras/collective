## plots
library(ggplot2)

setwd("/Users/Greg/Documents/git/CoCoLab/collective/presentations/2015_Berkeley/")

x1.25 <- seq(-4,4,length=100)*1.25 + 0
hx1.25 <- dnorm(x1.25,0,1.25)
h = cbind(x1.25,hx1.25)
h <- as.data.frame(h)
h$noise = "high"
colnames(h) = c("x","hx","noise")

x1 <- seq(-4,4,length=100)*1 + 0
hx1 <- dnorm(x1,0,1)
m = cbind(x1,hx1)
m <- as.data.frame(m)
m$noise = "mid"
colnames(m) = c("x","hx","noise")

x.75 <- seq(-4,4,length=100)*.75 + 0
hx.75 <- dnorm(x.75,0,.75)
l = cbind(x.75,hx.75)
l <- as.data.frame(l)
l$noise = "low"
colnames(l) = c("x","hx","noise")

x.01 <- seq(-4,4,length=100)*.01 + 0
hx.01 <- dnorm(x.01,0,.01)
n = cbind(x.01,hx.01)
n <- as.data.frame(n)
n$noise = "no"
colnames(n) = c("x","hx","noise")

d = rbind(h,m,l,n)

d$noise = factor(d$noise,levels=c("no","low","mid","high"))
d$noise = factor(d$noise,labels=c("no (\u03c3=0.01)","low (\u03c3=0.75)","mid (\u03c3=1.00)","high (\u03c3=1.25)"))

ggplot(d[d$noise=="no (\u03c3=0.01)",], aes(x=x,y=hx)) +
  geom_line(aes(color=noise,fill=noise),size=2.5)+
  xlab("\nnoise")+
  labs(color="")+
  ylab("")+
  theme_blackDisplay()+
  scale_color_manual(values=c("#F45E5B", "#5187FF","#17B32B","#9fb317"))+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  theme(legend.key.height=unit(2,"line"))
ggsave("noise-no.png",height=6,width=10)

ggplot(d[d$noise=="no (\u03c3=0.01)"|d$noise=="low (\u03c3=0.75)",], aes(x=x,y=hx)) +
  geom_line(aes(color=noise,fill=noise),size=2.5)+
  xlab("\nnoise")+
  ylab("")+
  ylim(0,.6)+
  labs(color="")+
  theme_blackDisplay()+
  scale_color_manual(values=c("#F45E5B", "#5187FF","#17B32B","#9fb317"))+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  theme(legend.key.height=unit(2,"line"))
ggsave("noise-low.png",height=6,width=10)

ggplot(d[d$noise!="high (\u03c3=1.25)",], aes(x=x,y=hx)) +
  geom_line(aes(color=noise,fill=noise),size=2.5)+
  xlab("\nnoise")+
  ylab("")+
  ylim(0,.6)+
  labs(color="")+
  theme_blackDisplay()+
  scale_color_manual(values=c("#F45E5B", "#5187FF","#17B32B","#9fb317"))+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  theme(legend.key.height=unit(2,"line"))
ggsave("noise-mid.png",height=6,width=10)

ggplot(d, aes(x=x,y=hx)) +
  geom_line(aes(color=noise,fill=noise),size=2.5)+
  xlab("\nnoise")+
  ylab("")+
  ylim(0,.6)+
  labs(color="")+
  theme_blackDisplay()+
  scale_color_manual(values=c("#F45E5B", "#5187FF","#17B32B","#9fb317"))+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  theme(legend.key.height=unit(2,"line"))
ggsave("noise-all.png",height=6,width=10)



########################################
##### MODEL RESULTS
########################################

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
d$noise <-factor(d$noise,labels=c("no\n(\u03c3=0.01)","low\n(\u03c3=0.75)",'mid\n(\u03c3=1.00)','high\n(\u03c3=1.25)'))
agg <- aggregate(p~noise*k,d[d$collective=="true",],sum)
p <- ggplot(agg,aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  #scale_fill_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("cyan", "orange"))+
  theme(axis.text.x = element_text(size=10,angle=0))+
  theme_blackDisplay()
p

setwd("~/Documents/git/CoCoLab/collective/presentations/2015_Berkeley/")

ggsave("model-results-sum.png",width=12.5,height=6.5)
