## plots
library(ggplot2)


# noise plots

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/")
n = read.csv("noise.csv",header=T)

n$level <- factor(n$level,levels=c("no","low","medium","high"))
n$level <- factor(n$level,labels=c("no noise","low noise","medium noise","high noise"))

ggplot(n, aes(x=value,y=p)) +
  geom_bar(stat="identity",width=.05) +
  #scale_x_continuous(lim=c(0.38,1.62),breaks=c(0.5,0.7,1,1.3,1.5)) +
  scale_x_continuous(breaks=round(seq(from=0,to=round(max(n$value),1),by=.2),1))+
  scale_y_continuous(breaks=seq(0,1.24,by=.25)) +
  geom_text(aes(label=value,y=p+.075),size=2.6) +
  ylab("p(noise value)\n")+
  xlab("\nnoise value")+
  facet_wrap(~level,nrow=1)
ggsave("noise_plots.pdf",height=2.25,width=6.5)


s = read.csv("states.csv",header=T)

s$level <- factor(s$level,labels=c("full knowledge","partial knowledge"))

ggplot(s, aes(x=state,y=p)) +
  geom_bar(stat="identity") +
  #scale_x_continuous(lim=c(0.38,1.62),breaks=c(0.5,0.7,1,1.3,1.5)) +
  #scale_x_continuous(breaks=round(seq(from=0,to=round(max(n$value),1),by=.2),1))+
  #scale_y_continuous(breaks=seq(0,1.24,by=.25)) +
  #geom_text(aes(label=value,y=p+.075),size=2.6) +
  ylab("p(state)\n")+
  xlab("\nstate")+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  facet_wrap(~level,nrow=1)
ggsave("state_plots.pdf",height=2.25,width=6.5)
