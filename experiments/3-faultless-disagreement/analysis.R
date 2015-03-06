library(ggplot2)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
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
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

setwd("~/Documents/git/cocolab/collective/experiments/3-faultless-disagreement/Submiterator-master")

f = read.table("faultless-disagreement-trials.tsv",sep="\t",header=T)
head(f)

f = f[d$sense=="Yes",]


faultless_casted = dcast(data=d, sentence ~ faultless, value.var="response",mean)
faultless_casted$faultless = (faultless_casted$yes/faultless_casted$no)

ggplot(faultless_casted, aes(x=no,y=yes)) +
  #  geom_point() +
  # geom_smooth() +
  #geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=1,slope=-1) +
  geom_text(size=2,alpha=.5,aes(label=sentence),angle=45) +
  ylab("faultless?") +
  xlab("collective?")+
  ylim(0,1) +
  xlim(0,1)