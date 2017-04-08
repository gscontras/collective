## black display

theme_blackDisplay <- function(base_size = 12, base_family = "Helvetica") {
  require(grid)
  theme(
    line =               element_line(colour = "white", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "black", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      colour = "white", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                      margin = margin(), debug = FALSE),
    axis.text =          element_text(size = 30, colour = "white"),
    strip.text =         element_text(size = 30, colour = "white"),
    
    axis.line =          element_blank(),
    axis.text.x =        element_text(vjust = 1),
    axis.text.y =        element_text(hjust = 1),
    axis.ticks =         element_line(colour = "white", size = 0.2),
    axis.title =         element_text(size=32,colour = "white"),
    axis.title.x =       element_text(vjust = 1),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.3, "lines"),
    #axis.ticks.margin =  unit(0.5, "lines"),
    
    legend.background =  element_rect(colour = "black"),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "black", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = 26, colour = "white"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = 26, face = "bold", hjust = 0, colour = "white"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = "black", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "white"),
    panel.grid.major =   element_line(colour = "grey20", size = 0.2),
    panel.grid.minor =   element_line(colour = "grey5", size = 0.5),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "grey30", colour = "grey10"),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),
    
    plot.background =    element_rect(colour = "black", fill = "black"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}


## plots
library(ggplot2)

x1.25 <- seq(-4,4,length=100)*3 + 0
hx1.25 <- dnorm(x1.25,0,3)
h = cbind(x1.25,hx1.25)
h <- as.data.frame(h)
h$noise = "high"
colnames(h) = c("x","hx","noise")

x1 <- seq(-4,4,length=100)*2 + 0
hx1 <- dnorm(x1,0,2)
m = cbind(x1,hx1)
m <- as.data.frame(m)
m$noise = "mid"
colnames(m) = c("x","hx","noise")

x.75 <- seq(-4,4,length=100)*1 + 0
hx.75 <- dnorm(x.75,0,1)
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
d$noise = factor(d$noise,labels=c("no (\u03c3=0.01)","low (\u03c3=1)","mid (\u03c3=2)","high (\u03c3=3)"))

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
#ggsave("~/Dropbox/Presentations/2017/CSUF/noise-all.png",height=6,width=10)



########################################
##### MODEL RESULTS
########################################

### unfit plots
setwd("~/Documents/git/cocolab/collective/model/")

d = read.csv("model_results-revised.csv",header=T)
head(d)
d = na.omit(d)

d$noise <-factor(d$noise,levels=c("high","mid",'low','no'))
#d$k <-factor(d$knowledge,labels=c("partial","full"))
d$numobjs <- factor(d$numobjs)
d$p <- as.numeric(as.character(d$p))
d$k = "sum"
d[d$knowledge=="TRUE",]$k = "full"
head(d)
d$noise <-factor(d$noise,levels=c("no","low",'mid','high'))
d$noise <-factor(d$noise,labels=c("no\n(\u03c3=0.01)","low\n(\u03c3=1)",'mid\n(\u03c3=2)','high\n(\u03c3=3)'))

# check effect direction for inferred thetas

p <- ggplot(d[d$collective=="TRUE",],aes(x=noise,y=p)) +
  geom_bar(stat='identity',position=position_dodge(),aes(fill=k)) +
  ylab("probability of\ncollective interpretation\n") +
  xlab("\ncollective interpretation noise")+
  labs(fill="speaker\nknowledge\naccess")+
  #scale_fill_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("cyan", "orange"))+
  theme(axis.text.x = element_text(size=10,angle=0))+
  theme_blackDisplay()
#facet_grid(dist_theta~coll_theta)
p

#ggsave("~/Dropbox/Presentations/2017/CSUF/model-results-sum.png",width=12.5,height=6.5)


