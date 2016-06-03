library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(grid)

myCenter <- function(x) {
  if (is.numeric(x)) { return(x - mean(x)) }
  if (is.factor(x)) {
    x <- as.numeric(x)
    return(x - mean(x))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m <- matrix(nrow=nrow(x), ncol=ncol(x))
    colnames(m) <- paste("c", colnames(x), sep="")
    for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) {
        y <- as.numeric(x[,i])
        m[,i] <- y - mean(y, na.rm=T)
      }
      if (is.numeric(x[,i])) {
        m[,i] <- x[,i] - mean(x[,i], na.rm=T)
      }
    }
    return(as.data.frame(m))
  }
}
bootsSummary <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE, n_boots_samps=10000) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     bootsci_high = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["97.5%"]],
                     bootsci_low = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["2.5%"]]
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  return(datac)
}

d = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

# counts and raw values

table(d$sceanrio,d$context)

aggregate(response~sceanrio*sentence_type*context*utterance,d,mean)

# reshape data

d = dcast(data=d, workerid + trial + utterance + sceanrio + context ~ sentence_type, value.var="response",mean)

# nomarlized collectivity score

d$collective = (d$coll / (d$coll + d$dist))
d$collective_ratio = (d$coll / d$dist)
d$coll_diff = (d$coll - d$dist)

# tests

centered = cbind(d, myCenter(d[,c("sceanrio","trial","utterance","context")]))

summary(d)

# collective difference score

coll_m = lmer(coll_diff~
           utterance*ccontext + utterance:csceanrio + csceanrio + ctrial +
           (1+ctrial|workerid),data=centered)
summary(coll_m)

#raw collective

raw_m = lmer(coll~
            utterance*ccontext + utterance:csceanrio + csceanrio + ctrial +
                (1+ctrial|workerid),data=centered)
summary(raw_m)

# norm collective

collective_m = lmer(collective~
                      utterance*ccontext + utterance:csceanrio + csceanrio + ctrial +
                      (1|workerid),data=centered)
summary(collective_m)






# plots

setwd("~/Documents/git/CoCoLab/collective/writing/Cubert/plots")

d_s = bootsSummary(data=d, measurevar="collective", groupvars=c("context","utterance","sceanrio"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("high","low")),y=collective,fill=sceanrio)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(title="normalized collective (95% bootsci)")+
  scale_fill_manual(values=c("blue","red"))
plot <- plot  +facet_grid(. ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt2collectivebootsci.pdf")


d_s = bootsSummary(data=d, measurevar="coll_diff", groupvars=c("context","utterance","sceanrio"))

plot <- ggplot(d_s, aes(x=factor(context,labels=c("random","regular")),y=coll_diff,color=sceanrio)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_point(position=position_dodge(width=.5))+
  geom_line(position=position_dodge(width=.5),aes(group=sceanrio))+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=factor(context,labels=c("random","regular")), width=0.1),position=position_dodge(width=0.5))+
  #geom_abline(intercept=0,slope=0)+
  ylab("collective endorsement\n")+
  ylim(-.75,.75)+
  xlab("\n variability of context") +
  labs(color="scenario")+
  scale_color_manual(values=c("red","blue"))
plot <- plot  +facet_grid(. ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
plot

ggsave("expt2colldiffbootsci.pdf")




d_se = aggregate(collective~context*utterance*sceanrio,data=d,mean)
d_se$cihigh = d_se$collective + aggregate(collective~context*utterance*sceanrio,data=d,se)$collective
d_se$cilow = d_se$collective - aggregate(collective~context*utterance*sceanrio,data=d,se)$collective
se_plot <- ggplot(d_se, aes(x=factor(context,labels=c("high","low")),y=collective,fill=sceanrio)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh, x=factor(context,labels=c("high","low")), width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement \n")+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(title="normalized collective (se)")+
  scale_fill_manual(values=c("blue","red"))+
  facet_grid(. ~ utterance) #+ theme_blackDisplay() #+ theme(legend.position="bottom", legend.margin=unit(-.7,"cm") ) 
se_plot
ggsave("expt2collectivese.pdf")





# raw sentence_type ratings plot

d_raw = read.csv("~/Dropbox/CoCoLab/CollectivePredication/Experiment/Persistence.v10.everything/experiment_persistence.v10-master/Submiterator-master/persistence.v10-trials.order.csv",header=T)

d_casted = dcast(data=d_raw, workerid + trial + utterance + sceanrio + context ~ sentence_type, value.var="response",mean)

ggplot(d_casted, aes(x=coll,y=dist,color=context))+
  geom_point()+
  geom_smooth(alpha=0.25)+
  geom_abline(intercept=1,slope=-1)


centered_raw = cbind(d_raw, myCenter(d_raw[,c("sceanrio","trial","utterance","context")]))

m = lmer(response~
           sentence_type*ccontext+sentence_type*cutterance+sentence_type*sceanrio+
           ccontext*cutterance+ccontext*sceanrio+
           cutterance*sceanrio+
           sentence_type*ccontext*cutterance+
           #cutterance*ccontext*scenario+
           (1+sentence_type+ccontext+cutterance+sceanrio|workerid)+
           (1+sentence_type+ccontext+cutterance+sceanrio|trial),
         data=centered_raw)
summary(m)


raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","utterance","sceanrio"))

raw_plot <- ggplot(raw_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))
raw_plot <- raw_plot  +facet_grid(sceanrio ~ utterance)
#+ theme_blackDisplay()
raw_plot
ggsave("expt2rawbootsci2.pdf",width=6,height=2.7)


raw_s = bootsSummary(data=d_casted, measurevar="coll", groupvars=c("context","utterance","sceanrio"))

raw_plot <- ggplot(raw_s, aes(x=context,y=coll,fill=sceanrio)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="scenario")+
  scale_fill_manual(values=c("red", "blue"))
raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
raw_plot
ggsave("expt2collbootsci.pdf")


#heavy_s = bootsSummary(data=d_casted[d_casted$utterance=="heavy",], measurevar="coll", groupvars=c("sceanrio","context"))

heavy = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/heavy2.csv",header=T)

heavy_plot <- ggplot(heavy, aes(x=data,y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\n source of data") +
  labs(fill="scenario")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~context)
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
heavy_plot
ggsave("heavymodel.pdf")


#tall_big_s = bootsSummary(data=d_casted[d_casted$utterance!="heavy",], measurevar="coll", groupvars=c("context","utterance"))

bigtall = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/big-tall.csv",header=T)

bt_plot <- ggplot(bigtall, aes(x=data,y=coll,fill=context)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\n source of data") +
  labs(fill="context\nvariability")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~predicate)
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
bt_plot
ggsave("bigtallmodel.pdf")
