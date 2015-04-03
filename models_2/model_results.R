library(ggplot2)

setwd("~/Documents/git/cocolab/collective/models_2/")


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







