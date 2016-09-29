library(rwebppl)
library(ggplot2)
setwd("~/Documents/git/CoCoLab/collective/model/")

my_params = data.frame(
  noise = c(0.99, 0.75, 0.5, 0.25),
  #noise = c("no","low","mid","high"),
  #nobjects = c(2,2,2,2),
  nobjects = c(3,3,3,3),
  utterance = c("ambiguous-pos","ambiguous-pos","ambiguous-pos","ambiguous-pos")
  #utterance = c("ambiguous-neg","ambiguous-neg","ambiguous-neg","ambiguous-neg")
  #disttheta = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3)
)

model.results <- data.frame()
for (i in 1:nrow(my_params)){
  my_data = my_params[i,]
  print(my_data)
rs<-webppl(program_file = "~/Documents/git/CoCoLab/collective/model/plural-predication-arrangement-9.wppl",
#rs<-webppl(program_file = "~/Documents/git/CoCoLab/collective/model/plural-predication.wppl",
       data = my_data,
       data_var = "my_data")

  rs.pk<-data.frame(rs$`partial knowledge`$support)
  rs.pk$probs <- rs$`partial knowledge`$probs
  rs.pk$noise <- my_data$noise
  rs.pk$nobjects <- my_data$nobjects
  rs.pk$knowledge <- "partial"
  
  rs.fk<-data.frame(rs$`full knowledge`$support)
  rs.fk$probs <- rs$`full knowledge`$probs
  rs.fk$noise <- my_data$noise
  rs.fk$nobjects <- my_data$nobjects
  rs.fk$knowledge <- "full"

  model.results <- bind_rows(model.results, rs.pk, rs.fk)
}

df<-model.results[,order(colnames(model.results),decreasing=FALSE)]
colnames(df) = c("isCollective","collTheta","distTheta","knowledge","nObjects","noise","probs","state1","state2")
#colnames(df) = c("isCollective","collTheta","distTheta","knowledge","nObjects","noise","probs","state1","state2","state3")
#colnames(df) = c("isCollective","collTheta","distTheta","knowledge","nObjects","noise","probs","state1","state2","state3","state4")
r <- df[df$isCollective==TRUE,]
r$state = paste(r$state1,r$state2) 
#r$state = paste(r$state1,r$state2,r$state3) 
#r$noise = factor(r$noise,levels=c("high","mid","low","no"))

c = aggregate(probs~noise*nObjects*knowledge,data=r,sum)
ggplot(c, aes(x = noise,y = probs, fill = knowledge, group = knowledge,label=round(probs,3)))+
  geom_bar(stat='identity', position = "dodge")+
  geom_text(position=position_dodge(0.2))+
  facet_wrap(~nObjects)+
  scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()
#ggsave("2-obj-c.pdf")
#ggsave("2-obj-c_unsorted.pdf")

c_agg = aggregate(probs~knowledge*distTheta*collTheta*noise,data=r,sum)
ggplot(c_agg,aes(x=noise,y=probs,color=knowledge))+
  geom_line(stat="identity")+
  geom_point(stat="identity",aes(shape=knowledge))+
  facet_grid(collTheta~distTheta)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()
#ggsave("2-obj-c_agg.pdf",width=15,height=15)
#ggsave("2-obj-c_agg_unsorted.pdf",width=15,height=15)
#ggsave("3-obj-c_agg.pdf",width=15,height=15)
  
ct = aggregate(probs~collTheta*knowledge*noise,data=r,sum)
ggplot(ct,aes(y=probs,x=collTheta,fill=knowledge))+
  #geom_line(stat="identity")+
  #geom_point(stat="identity",aes(shape=knowledge))+
  geom_bar(stat="identity")+
  facet_grid(noise~knowledge)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()
#ggsave("2-obj-ct.pdf")
#ggsave("2-obj-ct_unsorted.pdf")
#ggsave("3-obj-ct.pdf")

dt = aggregate(probs~distTheta*knowledge*noise,data=r,sum)
ggplot(dt,aes(y=probs,x=distTheta,fill=knowledge))+
  #geom_line(stat="identity")+
  #geom_point(stat="identity",aes(shape=knowledge))+
  geom_bar(stat="identity")+
  facet_grid(noise~knowledge)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()
#ggsave("2-obj-dt.pdf")
#ggsave("2-obj-dt_unsorted.pdf")
#ggsave("3-obj-dt.pdf")

dt_m <- r[r$distTheta==max(r$distTheta),]
dt_m_agg = aggregate(probs~state*knowledge*distTheta*noise,data=dt_m,sum)
ggplot(dt_m_agg,aes(x=state,y=probs,color=knowledge))+
  geom_line(stat="identity")+
  geom_point(stat="identity",aes(shape=knowledge))+
  facet_grid(noise~distTheta)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
#ggsave("2-obj-dt_m.pdf")
#ggsave("2-obj-dt_m_unsorted.pdf")
#ggsave("3-obj-dt_m.pdf")

ct_m <- r[r$collTheta==max(r$collTheta),]
ct_m_agg = aggregate(probs~state*knowledge*collTheta*noise,data=ct_m,sum)
ggplot(ct_m_agg,aes(x=state,y=probs,color=knowledge))+
  geom_line(stat="identity")+
  geom_point(stat="identity",aes(shape=knowledge))+
  facet_grid(noise~collTheta)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
#ggsave("2-obj-ct_m.pdf")
#ggsave("2-obj-ct_m_unsorted.pdf")





agg = aggregate(probs~state*knowledge*distTheta*collTheta*noise,data=r,sum)
ggplot(agg,aes(x=state,y=probs,color=knowledge))+
  geom_line(stat="identity")+
  geom_point(stat="identity",aes(shape=knowledge))+
  facet_grid(noise~distTheta)+
  #scale_x_continuous(breaks=c(0.25,0.5,0.75,0.99))+
  theme_bw()



r[r$noise==0.99&r$knowledge=="partial",]$probs - r[r$noise==0.75&r$knowledge=="partial",]$probs
r[r$noise==0.75&r$knowledge=="partial",]$probs - r[r$noise==0.50&r$knowledge=="partial",]$probs
r[r$noise==0.50&r$knowledge=="partial",]$probs - r[r$noise==0.25&r$knowledge=="partial",]$probs
r[r$noise==0.99&r$knowledge=="partial",]$probs - r[r$noise==0.25&r$knowledge=="partial",]$probs

r[r$noise==0.99&r$knowledge=="full",]$probs - r[r$noise==0.75&r$knowledge=="full",]$probs
r[r$noise==0.75&r$knowledge=="full",]$probs - r[r$noise==0.50&r$knowledge=="full",]$probs
r[r$noise==0.50&r$knowledge=="full",]$probs - r[r$noise==0.25&r$knowledge=="full",]$probs
r[r$noise==0.99&r$knowledge=="full",]$probs - r[r$noise==0.25&r$knowledge=="full",]$probs

r[r$noise==0.99&r$knowledge=="partial",]$probs - r[r$noise==0.99&r$knowledge=="full",]$probs
r[r$noise==0.75&r$knowledge=="partial",]$probs - r[r$noise==0.75&r$knowledge=="full",]$probs
r[r$noise==0.50&r$knowledge=="partial",]$probs - r[r$noise==0.50&r$knowledge=="full",]$probs
r[r$noise==0.25&r$knowledge=="partial",]$probs - r[r$noise==0.25&r$knowledge=="full",]$probs

