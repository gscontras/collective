library(ggplot2)

setwd("~/Documents/git/cocolab/collective/models/")

d = read.csv("QUD_results.csv",header=T)

d$noise<-factor(d$noise,levels=c("low",'mid','high'))

d$QUD<-factor(d$QUD,levels=c('null','flat','max'))

p <- ggplot(d,aes(x=QUD,y=pcollective,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)")

ggsave("QUD_results.png",plot=p,width=5, height=3)
