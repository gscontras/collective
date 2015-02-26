library(lme4)
library(plyr)
library(lmerTest)
library(coin)
library(ggplot2)
library(reshape2)

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
                         conf.interval=.95, .drop=TRUE, n_boots_samps=10) {
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
theme_blackDisplay <- function(base_size = 12, base_family = "Helvetica") {
  require(grid)
  theme(
    line =               element_line(colour = "white", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "black", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      colour = "white", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
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
    axis.ticks.margin =  unit(0.5, "lines"),
    
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

setwd("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/")

sub = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-subject_information.tsv",sep="\t",header=T)

d = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-trials.tsv",sep="\t",header=T)

s = read.csv("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/bnc.sentences.csv",header=T)

# counts and raw values

head(d)

table(d$predicate,d$noun)

# trim to just those sentences that make sense

d = d[!is.na(d$sense)&d$sense=="Yes",]

summary(d)

# just attested sentences

a = d[d$attested=="True",]

## Attested sentence analysis (collapsing over animacy)

a$sentence = paste(a$noun,a$predicate,sep=" ")

a$faultless = s$Faultless[match(a$sentence,s$Sentence)]

a_sent_casted = dcast(data=a, sentence + faultless~ sentence_type, value.var="response",mean,na.rm=T)
a_sent_casted$CI.YMin.coll = a_sent_casted$coll - dcast(data=a, sentence+ faultless~ sentence_type, value.var="response",ci.low,na.rm=T)$coll
a_sent_casted$CI.YMin.dist = a_sent_casted$dist - dcast(data=a, sentence+ faultless~ sentence_type, value.var="response",ci.low,na.rm=T)$dist
a_sent_casted$CI.YMax.coll = a_sent_casted$coll + dcast(data=a, sentence+ faultless~ sentence_type, value.var="response",ci.high,na.rm=T)$coll
a_sent_casted$CI.YMax.dist = a_sent_casted$dist + dcast(data=a, sentence+ faultless~ sentence_type, value.var="response",ci.high,na.rm=T)$dist

a_sent_word_plot <- ggplot(a_sent_casted, aes(x=coll,y=dist,color=faultless)) +
  #  geom_point() +
  # geom_smooth() +
  #geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.5,aes(label=sentence),angle=45) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_sentence_plot.png',plot=a_sent_word_plot,width=8, height=8)

## Attested sentence collectivity ranking

a_sent_casted$dist_coll_ratio = a_sent_casted$dist/a_sent_casted$coll

a_sent_ranking_plot = ggplot(a_sent_casted,aes(x=0,y=dist_coll_ratio)) +
  geom_text(size=2,alpha=.5,aes(label=sentence))  +
  ylab("distirbutive endorsement / collective endorsement") +
  xlab("sentence") +
  scale_x_discrete(breaks=NULL) +
  ylim(.4,1.8)

ggsave(filename='attested_sentence_ranking_plot.png',plot=a_sent_ranking_plot,width=3, height=12)


## Attested Predicate analysis (collapsing over animacy)

a_pred_casted = dcast(data=a, predicate~ sentence_type, value.var="response",mean)
a_pred_casted$CI.YMin.coll = a_pred_casted$coll - dcast(data=a, predicate~ sentence_type, value.var="response",ci.low)$coll
a_pred_casted$CI.YMin.dist = a_pred_casted$dist - dcast(data=a, predicate~ sentence_type, value.var="response",ci.low)$dist
a_pred_casted$CI.YMax.coll = a_pred_casted$coll + dcast(data=a, predicate~ sentence_type, value.var="response",ci.high)$coll
a_pred_casted$CI.YMax.dist = a_pred_casted$dist + dcast(data=a, predicate~ sentence_type, value.var="response",ci.high)$dist

a_pred_word_plot <- ggplot(a_pred_casted, aes(x=coll,y=dist)) +
  #  geom_point() +
    geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_pred_word_plot.png',plot=a_pred_word_plot,width=8, height=8)

## Attested predicate collectivity ranking

a_pred_casted$coll_dist_ratio = a_pred_casted$coll/a_pred_casted$dist

a_pred_ranking_plot = ggplot(a_pred_casted,aes(x=0,y=coll_dist_ratio)) +
  geom_text(size=2,alpha=.75,aes(label=predicate),angle=45,position=position_dodge(width=.9))  +
  ylab("collective endorsement / distirbutive endorsement") +
  xlab("predicate") +
  scale_x_discrete(breaks=NULL) +
  ylim(.4,1.8)

ggsave(filename='attested_pred_ranking_plot.png',plot=a_pred_ranking_plot,width=3, height=8)

## Attested Predicate analysis (looking at animacy)

a_pred_animacy_casted = dcast(data=a, predicate+animate~ sentence_type, value.var="response",mean)
a_pred_animacy_casted$CI.YMin.coll = a_pred_animacy_casted$coll - dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.low)$coll
a_pred_animacy_casted$CI.YMin.dist = a_pred_animacy_casted$dist - dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.low)$dist
a_pred_animacy_casted$CI.YMax.coll = a_pred_animacy_casted$coll + dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.high)$coll
a_pred_animacy_casted$CI.YMax.dist = a_pred_animacy_casted$dist + dcast(data=a, predicate+animate~ sentence_type, value.var="response",ci.high)$dist

a_pred_animacy_plot <- ggplot(a_pred_animacy_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_pred_animacy_plot.png',plot=a_pred_animacy_plot,width=9, height=8)


## Attested Noun analysis (looking at animacy)

a_noun_casted = dcast(data=a, noun+animate~ sentence_type, value.var="response",mean)
a_noun_casted$CI.YMin.coll = a_noun_casted$coll - dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.low)$coll
a_noun_casted$CI.YMin.dist = a_noun_casted$dist - dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.low)$dist
a_noun_casted$CI.YMax.coll = a_noun_casted$coll + dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.high)$coll
a_noun_casted$CI.YMax.dist = a_noun_casted$dist + dcast(data=a, noun+animate~ sentence_type, value.var="response",ci.high)$dist

a_noun_word_plot <- ggplot(a_noun_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='attested_noun_word_plot.png',plot=a_noun_word_plot,width=9, height=8)









# just unattested sentences

u = d[d$attested=="False",]

## Unattested Predicate analysis (collapsing over animacy)

u_pred_casted = dcast(data=u, predicate~ sentence_type, value.var="response",mean)
u_pred_casted$CI.YMin.coll = u_pred_casted$coll - dcast(data=u, predicate~ sentence_type, value.var="response",ci.low)$coll
u_pred_casted$CI.YMin.dist = u_pred_casted$dist - dcast(data=u, predicate~ sentence_type, value.var="response",ci.low)$dist
u_pred_casted$CI.YMax.coll = u_pred_casted$coll + dcast(data=u, predicate~ sentence_type, value.var="response",ci.high)$coll
u_pred_casted$CI.YMax.dist = u_pred_casted$dist + dcast(data=u, predicate~ sentence_type, value.var="response",ci.high)$dist

u_pred_word_plot <- ggplot(u_pred_casted, aes(x=coll,y=dist)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_pred_word_plot.png',plot=u_pred_word_plot,width=8, height=8)

## Unattested Predicate analysis (looking at animacy)

u_pred_animacy_casted = dcast(data=u, predicate+animate~ sentence_type, value.var="response",mean)
u_pred_animacy_casted$CI.YMin.coll = u_pred_animacy_casted$coll - dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.low)$coll
u_pred_animacy_casted$CI.YMin.dist = u_pred_animacy_casted$dist - dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.low)$dist
u_pred_animacy_casted$CI.YMax.coll = u_pred_animacy_casted$coll + dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.high)$coll
u_pred_animacy_casted$CI.YMax.dist = u_pred_animacy_casted$dist + dcast(data=u, predicate+animate~ sentence_type, value.var="response",ci.high)$dist

u_pred_animacy_plot <- ggplot(u_pred_animacy_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_pred_animacy_plot.png',plot=u_pred_animacy_plot,width=9, height=8)

## Unattested Noun analysis (looking at animacy)

u_noun_casted = dcast(data=u, noun+animate~ sentence_type, value.var="response",mean)
u_noun_casted$CI.YMin.coll = u_noun_casted$coll - dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.low)$coll
u_noun_casted$CI.YMin.dist = u_noun_casted$dist - dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.low)$dist
u_noun_casted$CI.YMax.coll = u_noun_casted$coll + dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.high)$coll
u_noun_casted$CI.YMax.dist = u_noun_casted$dist + dcast(data=u, noun+animate~ sentence_type, value.var="response",ci.high)$dist

u_noun_word_plot <- ggplot(u_noun_casted, aes(x=coll,y=dist,color=animate)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1)

ggsave(filename='unattested_noun_word_plot.png',plot=u_noun_word_plot,width=9, height=8)

