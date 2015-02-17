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
#theme_blackDisplay <- function(base_size = 12, base_family = "Helvetica") {
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

d1 = read.table("~/Documents/git/CoCoLab/CollectivePredication/experiments/1-corpus-based/Submiterator-master/1-corpus-based-trials.tsv",sep="\t",header=T)
d1$workerid = paste(d1$workerid,"v1",sep="")
d2 = read.table("~/Documents/git/CoCoLab/CollectivePredication/experiments/1-corpus-based/Submiterator-master/1.2-corpus-based-trials.tsv",sep="\t",header=T)
d2$workerid = paste(d2$workerid,"v2",sep="")

n = read.csv("~/Documents/git/CoCoLab/CollectivePredication/experiments/1-corpus-based/nouns.csv",header=T)

d = rbind(d1,d2)

# counts and raw values

head(d)

d$individuation = n$individuation[match(d$noun,n$noun)]
d$SGPL_RATIO = n$SGPL_RATIO[match(d$noun,n$noun)]


## Predicate analysis (collapsing over animacy)

pred_casted = dcast(data=d, predicate+sense~ sentence_type, value.var="response",mean)
pred_casted$CI.YMin.coll = pred_casted$coll - dcast(data=d, predicate+sense~ sentence_type, value.var="response",ci.low)$coll
pred_casted$CI.YMin.dist = pred_casted$dist - dcast(data=d, predicate+sense~ sentence_type, value.var="response",ci.low)$dist
pred_casted$CI.YMax.coll = pred_casted$coll + dcast(data=d, predicate+sense~ sentence_type, value.var="response",ci.high)$coll
pred_casted$CI.YMax.dist = pred_casted$dist + dcast(data=d, predicate+sense~ sentence_type, value.var="response",ci.high)$dist

pred_word_plot <- ggplot(pred_casted[pred_casted$sense=="Yes",], aes(x=coll,y=dist)) +
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

ggsave(filename='pred_word_plot.png',plot=pred_word_plot,width=8, height=8)


## Animacy analysis

# reshape data for animacy plots
casted = dcast(data=d, predicate+animacy+sense~ sentence_type, value.var="response",mean)
casted$CI.YMin.coll = casted$coll - dcast(data=d, predicate+animacy+sense~ sentence_type, value.var="response",ci.low)$coll
casted$CI.YMin.dist = casted$dist - dcast(data=d, predicate+animacy+sense~ sentence_type, value.var="response",ci.low)$dist
casted$CI.YMax.coll = casted$coll + dcast(data=d, predicate+animacy+sense~ sentence_type, value.var="response",ci.high)$coll
casted$CI.YMax.dist = casted$dist + dcast(data=d, predicate+animacy+sense~ sentence_type, value.var="response",ci.high)$dist

pred_animacy_plot <- ggplot(casted[casted$sense=="Yes",], aes(x=coll,y=dist,color=animacy)) +
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

ggsave(filename='pred_animacy_plot.png',plot=pred_animacy_plot,width=9, height=8)

## Animacy difference analysis

casted$coll.dist.ratio = casted$coll/casted$dist
animacy_diff = dcast(data=casted, predicate+sense~ animacy, value.var="coll.dist.ratio",mean)
animacy_diff = na.omit(animacy_diff)
animacy_diff$animate_less_inanimate = animacy_diff$animate - animacy_diff$inanimate

diff_plot <- ggplot(animacy_diff,aes(y=animate_less_inanimate,x=sense)) +
  geom_text(alpha=.75,size=2,aes(label=predicate)) +
  ylab("animate collective/distributive -\n inanimate collective/distributive") +
  xlab("makes sense?") +
  ylim(-1.5,1.5) +
  geom_abline(intercept=0,slope=0)
  #facet_wrap(~sense)
#high values mean animate nouns are more collective than inanimate nouns

ggsave(filename="diff_plot.png",plot=diff_plot,width=3,height=8)






ggplot(d,aes(x=response, fill=sentence_type, color=sentence_type)) +
  #geom_histogram(position="dodge") +
  geom_density(alpha=.5) +
  facet_grid(. ~ sense)

agr = aggregate(response~sentence_type*animacy*sense,d,mean)
agr$CILow = aggregate(response~sentence_type*animacy*sense,data=d, FUN=ci.low)$response
agr$CIHigh = aggregate(response~sentence_type*animacy*sense,data=d,FUN=ci.high)$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
dodge=position_dodge(.9)

ggplot(agr, aes(x=animacy,y=response, fill=sentence_type)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~sense)




## Noun analysis (looking at individuation)

noun_casted = dcast(data=d, noun+sense+individuation~ sentence_type, value.var="response",mean)
noun_casted$CI.YMin.coll = noun_casted$coll - dcast(data=d, noun+sense+individuation~ sentence_type, value.var="response",ci.low)$coll
noun_casted$CI.YMin.dist = noun_casted$dist - dcast(data=d, noun+sense+individuation~ sentence_type, value.var="response",ci.low)$dist
noun_casted$CI.YMax.coll = noun_casted$coll + dcast(data=d, noun+sense+individuation~ sentence_type, value.var="response",ci.high)$coll
noun_casted$CI.YMax.dist = noun_casted$dist + dcast(data=d, noun+sense+individuation~ sentence_type, value.var="response",ci.high)$dist

noun_word_plot <- ggplot(noun_casted[noun_casted$sense=="Yes",], aes(x=coll,y=dist,color=individuation)) +
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

ggsave(filename='noun_word_plot.png',plot=noun_word_plot,width=9, height=8)

## Noun analysis (looking at SGPL_RATIO)

noun_agg = aggregate(response~noun+sense+sentence_type+SGPL_RATIO+animacy,d,mean)

noun_casted = dcast(data=noun_agg, noun+sense+SGPL_RATIO+animacy~sentence_type,value.var="response",mean)

noun_casted = dcast(data=d[!is.na(d$sense),], noun+sense+SGPL_RATIO+animacy ~ sentence_type, value.var="response",mean)
noun_casted$CI.YMin.coll = noun_casted$coll - dcast(data=d, noun+sense+SGPL_RATIO+animacy~ sentence_type, value.var="response",ci.low)$coll
noun_casted$CI.YMin.dist = noun_casted$dist - dcast(data=d, noun+sense+SGPL_RATIO+animacy~ sentence_type, value.var="response",ci.low)$dist
noun_casted$CI.YMax.coll = noun_casted$coll + dcast(data=d, noun+sense+SGPL_RATIO+animacy~ sentence_type, value.var="response",ci.high)$coll
noun_casted$CI.YMax.dist = noun_casted$dist + dcast(data=d, noun+sense+SGPL_RATIO+animacy~ sentence_type, value.var="response",ci.high)$dist

noun_sgpl_ratio_plot <- ggplot(noun_casted[!is.na(noun_casted$sense) & noun_casted$sense=="Yes" & noun_casted$noun!="roads",], aes(x=coll,y=dist,color=SGPL_RATIO)) +
  #  geom_point() +
  #  geom_smooth() +
  #geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(.3,.9) +
  xlim(.3,.9) +
  facet_wrap(~animacy)

ggsave(filename='noun_sgpl_ratio_plot.png',plot=noun_sgpl_ratio_plot,width=8, height=4)



# barplot by animacy and individuation

noun_agr = aggregate(response~sentence_type*individuation*animacy,d[d$sense=="Yes",],mean)
noun_agr$CILow = aggregate(response~sentence_type*individuation*animacy,data=d[d$sense=="Yes",], FUN=ci.low)$response
noun_agr$CIHigh = aggregate(response~sentence_type*individuation*animacy,data=d[d$sense=="Yes",],FUN=ci.high)$response
noun_agr$YMin = noun_agr$response - noun_agr$CILow
noun_agr$YMax = noun_agr$response + noun_agr$CIHigh
dodge=position_dodge(.9)

ggplot(noun_agr, aes(x=individuation,y=response, fill=sentence_type)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~animacy)



## Noun analysis by predicate (looking at gender)

noun_by_predicate_casted = dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",mean)
noun_by_predicate_casted$CI.YMin.coll = noun_by_predicate_casted$coll - dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.low)$coll
noun_by_predicate_casted$CI.YMin.dist = noun_by_predicate_casted$dist - dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.low)$dist
noun_by_predicate_casted$CI.YMax.coll = noun_by_predicate_casted$coll + dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.high)$coll
noun_by_predicate_casted$CI.YMax.dist = noun_by_predicate_casted$dist + dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.high)$dist

noun_by_predicate_casted$Gender = as.factor(ifelse(noun_by_predicate_casted$noun %in% c("women","girls"),"female",ifelse(noun_by_predicate_casted$noun %in% c("men","boys"), "male","other")))

noun_by_predicate_gender_plot <- ggplot(noun_by_predicate_casted[!is.na(noun_by_predicate_casted$sense) & noun_by_predicate_casted$sense=="Yes",], aes(x=coll,y=dist,color=Gender)) +
  #  geom_point() +
  #  geom_smooth() +
 geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1) +
  facet_wrap(~predicate,drop=TRUE)

ggsave(filename='noun_by_predicate_gender_plot.png',plot=noun_by_predicate_plot,width=11, height=8)


## Noun analysis by predicate (looking at individuation)

noun_by_predicate_casted = dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",mean)
noun_by_predicate_casted$CI.YMin.coll = noun_by_predicate_casted$coll - dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.low)$coll
noun_by_predicate_casted$CI.YMin.dist = noun_by_predicate_casted$dist - dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.low)$dist
noun_by_predicate_casted$CI.YMax.coll = noun_by_predicate_casted$coll + dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.high)$coll
noun_by_predicate_casted$CI.YMax.dist = noun_by_predicate_casted$dist + dcast(data=d, noun+sense+predicate~ sentence_type, value.var="response",ci.high)$dist

noun_by_predicate_casted$SGPL_RATIO = n$SGPL_RATIO[match(noun_by_predicate_casted$noun,n$noun)]
noun_by_predicate_casted$animacy = d$animacy[match(noun_by_predicate_casted$noun,d$noun)]


noun_by_predicate_individuation_plot <- ggplot(noun_by_predicate_casted[!is.na(noun_by_predicate_casted$sense) & noun_by_predicate_casted$sense=="Yes" & noun_by_predicate_casted$noun!="roads",], aes(x=coll,y=dist,color=SGPL_RATIO)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=noun)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1) +
  facet_grid(animacy~predicate,drop=TRUE)

ggsave(filename='noun_by_predicate_individuation_plot.png',plot=noun_by_predicate_individuation_plot,width=30, height=4)


## predicates by noun looking at individuation

predicate_by_noun_individuation_plot <- ggplot(noun_by_predicate_casted[!is.na(noun_by_predicate_casted$sense) & noun_by_predicate_casted$sense=="Yes" & noun_by_predicate_casted$noun!="roads",], aes(x=coll,y=dist)) +
  #  geom_point() +
  #  geom_smooth() +
  geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=2,alpha=.75,aes(label=predicate)) +
  ylab("distributive paraphrase endorsement") +
  xlab("collective paraphrase endorsement") +
  ylim(0,1) +
  xlim(0,1) +
  facet_wrap(animacy~noun,drop=TRUE)

ggsave(filename='predicate_by_noun_individuation_plot.png',plot=predicate_by_noun_individuation_plot,width=9, height=8)




ggplot(d, aes(x=predicate, fill=sense)) +
  geom_histogram(position="dodge") +
  #facet_wrap(~animacy) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) 
