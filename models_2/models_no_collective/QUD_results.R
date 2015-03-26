library(ggplot2)

setwd("~/Documents/git/cocolab/collective/models/")

d = read.csv("QUD_results.csv",header=T)

d$noise<-factor(d$noise,levels=c("low",'mid','high'))

d$QUD<-factor(d$QUD,levels=c('null','flat','max','min','sum'))

p <- ggplot(d,aes(x=QUD,y=pcollective,fill=noise)) +
  geom_bar(stat='identity',position=position_dodge()) +
  ylab("P(collective)") +
  facet_grid(.~QUDs,scales = "free_x")

ggsave("QUD_results.png",plot=p,width=7, height=3)
