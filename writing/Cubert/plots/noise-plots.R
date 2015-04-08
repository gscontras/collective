## plots
library(ggplot2)


# noise plots

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")
n = read.csv("noise.csv",header=T)

n$level <- factor(n$level,levels=c("no","low","medium","high"))

ggplot(n,aes(x=value,y=p,color=level)) +
  geom_point(alpha=1) +
  geom_smooth(alpha=.25) +
  xlab("noise value")+
  ylab("probability")+
  labs(fill="noise level")

low <- ggplot(n, aes(x=value,y=p,fill=level)) +
  #geom_bar(alpha=.5,stat='identity',position='stack')
  geom_bar(stat="identity")
  #geom_density(aes(alpha=0.5,size=1))
  #ylab("") +
  #xlab("")+
  #ylim(0,1)
low


qplot(value, ..count.., data=n, geom="density", fill=level, position="stack")

#ggsave(filename='low.png',plot=low,width=5, height=3)

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
