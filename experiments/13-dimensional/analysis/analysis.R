library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

source("../analysis/helpers.R")

 setwd("~/Documents/git/cocolab/collective/experiments/13-dimensional/Submiterator-master")
# 
# a = read.csv("../analysis/adjectives.csv",header=T)
# 
# num_round_dirs = 6 # problem with round 5
# df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
#   return (read.csv(paste(
#     'round', i, '/dimensional.csv', sep='')) %>%
#     #'round', i, '/long-square-trials.csv', sep='')) %>%
#       mutate(workerid = (workerid + (i-1)*9)))}))
# 
# d = subset(df, select=c("workerid","utterance","response","context","sentence_type","language","comments"))
# unique(d$language)
# d = d[d$language!=""&d$language!="Spanish"&d$language!="Cantonese",]
# unique(d$language)
# length(unique(d$workerid)) # n=51
# 
# d$dimension = a$dimension[match(d$utterance,a$adjective)]
# d$valence = a$valence[match(d$utterance,a$adjective)]
# 
# d_raw <- d #un-transformed data
# 
# write.csv(d_raw,"../analysis/combined-data.csv")

d_raw = read.csv("../analysis/combined-data.csv",header=T)

## plots

dim_val_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","dimension","valence"))
dim_val_s$valence <- factor(dim_val_s$valence,labels=c("negative","positive"))
dv_plot <- ggplot(dim_val_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1, by = .25),labels=c("0","","","","1"))+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(valence~dimension ) + 
  theme_bw() +
  theme(legend.position="bottom")
dv_plot
#ggsave("../analysis/expt4.png",width=8.35,height=3.3)

d_raw$valence <- factor(d_raw$valence,labels=c("negative","positive"))
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
dv_violin <- ggplot(d_raw, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  geom_violin()+
  stat_summary(fun.data=data_summary,color="white",shape=18,size=.5,position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1, by = .25),labels=c("0","","","","1"))+
  #ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(valence~dimension ) + 
  theme_bw() +
  theme(legend.position="bottom")
dv_violin
#ggsave("../analysis/expt4-violin.png",width=8.35,height=4)


raw_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","utterance","dimension","valence"))
raw_plot <- ggplot(raw_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(valence ~ dimension+utterance) + 
  theme_bw()
raw_plot
#ggsave("../analysis/predicates.pdf",height=5)



collapsed_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context"))
c_plot <- ggplot(collapsed_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  #facet_wrap(~ utterance) + 
  theme_bw()
c_plot
#ggsave("../analysis/collapsed.pdf")

valence_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","valence"))
v_plot <- ggplot(valence_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_wrap(~ valence) + 
  theme_bw()
v_plot
#ggsave("../analysis/valence.pdf")

dimension_s = bootsSummary(data=d_raw, measurevar="response", groupvars=c("sentence_type","context","dimension"))
d_plot <- ggplot(dimension_s, aes(x=context,y=response,fill=factor(sentence_type,labels=c("collective","distributive")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  ylab("endorsement (out of 1)\n")+
  ylim(0,1)+
  xlab("\n variability of context") +
  labs(fill="paraphrase")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_wrap(~ dimension) + 
  theme_bw()
d_plot
#ggsave("../analysis/dimension.pdf")




 # transform data
d_tran = dcast(data=d_raw, workerid + utterance + context +dimension + valence + slide_number ~ sentence_type, value.var="response",mean)
d_tran$diff = d_tran$dist - d_tran$coll

centered = cbind(d_tran, myCenter(d_tran[,c("utterance","context","valence","dimension")]))

m = lmer(coll~ccontext*cvalence+cvalence*ccontext+(1|workerid),data=centered)
summary(m)

## individual dimension analysis

capacity = d_tran[d_tran$dimension=="capacity",]
capacity_c = cbind(capacity, myCenter(capacity[,c("context","valence")]))
capacity_m = glm(coll~ ccontext+slide_number, data=capacity_c)
summary(capacity_m) #.
capacity_m = glm(diff~ ccontext+slide_number, data=capacity_c)
summary(capacity_m) #ns

depth = d_tran[d_tran$dimension=="depth",]
depth_c = cbind(depth, myCenter(depth[,c("context","valence")]))
depth_m = glm(coll~ ccontext+slide_number, data=depth_c)
summary(depth_m) #ns
depth_m = glm(diff~ ccontext+slide_number, data=depth_c)
summary(depth_m) #ns

height = d_tran[d_tran$dimension=="height",]
height_c = cbind(height, myCenter(height[,c("context","valence")]))
height_m = glm(coll~ ccontext*cvalence+slide_number, data=height_c)
summary(height_m) #.
height_m = glm(coll~ ccontext+slide_number, data=height_c[height_c$valence=="pos",])
summary(height_m) #*
height_m = glm(diff~ ccontext*cvalence+slide_number, data=height_c)
summary(height_m) #.
height_m = glm(diff~ ccontext+slide_number, data=height_c[height_c$valence=="pos",])
summary(height_m) #*

length = d_tran[d_tran$dimension=="length",]
length_c = cbind(length, myCenter(length[,c("context","valence")]))
length_m = glm(coll~ ccontext+slide_number, data=length_c)
summary(length_m) #ns
length_m = glm(diff~ ccontext+slide_number, data=length_c)
summary(length_m) #ns

size = d_tran[d_tran$dimension=="size",]
size_c = cbind(size, myCenter(size[,c("context","valence")]))
size_m = glm(coll~ ccontext*cvalence+slide_number, data=size_c)
summary(size_m) #*
size_m = glm(coll~ ccontext+slide_number, data=size_c[size_c$valence=="pos",])
summary(size_m) #*
size_m = glm(diff~ ccontext*cvalence+slide_number, data=size_c)
summary(size_m) #.
size_m = glm(diff~ ccontext+slide_number, data=size_c[size_c$valence=="pos",])
summary(size_m) #*

weight = d_tran[d_tran$dimension=="weight",]
weight_c = cbind(weight, myCenter(weight[,c("context","valence")]))
weight_m = glm(coll~ ccontext+slide_number, data=weight_c)
summary(weight_m) #ns
weight_m = glm(diff~ ccontext+slide_number, data=weight_c)
summary(weight_m) #ns

width = d_tran[d_tran$dimension=="width",]
width_c = cbind(width, myCenter(width[,c("context","valence")]))
width_m = glm(coll~ ccontext*cvalence+slide_number, data=width_c)
summary(width_m) #*
width_m = glm(coll~ ccontext+slide_number, data=width_c[width_c$valence=="neg",])
summary(width_m) #**
width_m = glm(diff~ ccontext*cvalence+slide_number, data=width_c)
summary(width_m) #**
width_m = glm(diff~ ccontext+slide_number, data=width_c[width_c$valence=="neg",])
summary(width_m) #**



## individual predicate analysis

big = d_tran[d_tran$utterance=="big",]
big_c = cbind(big, myCenter(big[,c("context","valence")]))
big_m = glm(coll~ ccontext, data=big_c)
summary(big_m) #*
big_m = glm(diff~ ccontext, data=big_c)
summary(big_m) #ns

deep = d_tran[d_tran$utterance=="deep",]
deep_c = cbind(deep, myCenter(deep[,c("context","valence")]))
deep_m = glm(coll~ ccontext, data=deep_c)
summary(deep_m) #ns
deep_m = glm(diff~ ccontext, data=deep_c)
summary(deep_m) #ns

fat = d_tran[d_tran$utterance=="fat",]
fat_c = cbind(fat, myCenter(fat[,c("context","valence")]))
fat_m = glm(coll~ ccontext, data=fat_c)
summary(fat_m) #ns
fat_m = glm(diff~ ccontext, data=fat_c)
summary(fat_m) #ns

full = d_tran[d_tran$utterance=="full",]
full_c = cbind(full, myCenter(full[,c("context","valence")]))
full_m = glm(coll~ ccontext, data=full_c)
summary(full_m) #.
full_m = glm(diff~ ccontext, data=full_c)
summary(full_m) #ns

heavy = d_tran[d_tran$utterance=="heavy",]
heavy_c = cbind(heavy, myCenter(heavy[,c("context","valence")]))
heavy_m = glm(coll~ ccontext, data=heavy_c)
summary(heavy_m) #ns
heavy_m = glm(diff~ ccontext, data=heavy_c)
summary(heavy_m) #ns

high = d_tran[d_tran$utterance=="high",]
high_c = cbind(high, myCenter(high[,c("context","valence")]))
high_m = glm(coll~ ccontext, data=high_c)
summary(high_m) #ns
high_m = glm(diff~ ccontext, data=high_c)
summary(high_m) #ns

huge = d_tran[d_tran$utterance=="huge",]
huge_c = cbind(huge, myCenter(huge[,c("context","valence")]))
huge_m = glm(coll~ ccontext, data=huge_c)
summary(huge_m) #ns
huge_m = glm(diff~ ccontext, data=huge_c)
summary(huge_m) #ns

humongous = d_tran[d_tran$utterance=="humongous",]
humongous_c = cbind(humongous, myCenter(humongous[,c("context","valence")]))
humongous_m = glm(coll~ ccontext, data=humongous_c)
summary(humongous_m) #ns
humongous_m = glm(diff~ ccontext, data=humongous_c)
summary(humongous_m) #ns

large = d_tran[d_tran$utterance=="large",]
large_c = cbind(large, myCenter(large[,c("context","valence")]))
large_m = glm(coll~ ccontext, data=large_c)
summary(large_m) #ns
large_m = glm(diff~ ccontext, data=large_c)
summary(large_m) #ns

lengthy = d_tran[d_tran$utterance=="lengthy",]
lengthy_c = cbind(lengthy, myCenter(lengthy[,c("context","valence")]))
lengthy_m = glm(coll~ ccontext, data=lengthy_c)
summary(lengthy_m) #ns
lengthy_m = glm(diff~ ccontext, data=lengthy_c)
summary(lengthy_m) #ns

little = d_tran[d_tran$utterance=="little",]
little_c = cbind(little, myCenter(little[,c("context","valence")]))
little_m = glm(coll~ ccontext, data=little_c)
summary(little_m) #ns
little_m = glm(diff~ ccontext, data=little_c)
summary(little_m) #ns

long = d_tran[d_tran$utterance=="long",]
long_c = cbind(long, myCenter(long[,c("context","valence")]))
long_m = glm(coll~ ccontext, data=long_c)
summary(long_m) #ns
long_m = glm(diff~ ccontext, data=long_c)
summary(long_m) #ns

low = d_tran[d_tran$utterance=="low",]
low_c = cbind(low, myCenter(low[,c("context","valence")]))
low_m = glm(coll~ ccontext, data=low_c)
summary(low_m) #ns
low_m = glm(coll~ ccontext, data=low_c)
summary(low_m) #ns

mini = d_tran[d_tran$utterance=="mini",]
mini_c = cbind(mini, myCenter(mini[,c("context","valence")]))
mini_m = glm(coll~ ccontext, data=mini_c)
summary(mini_m) #ns
mini_m = glm(diff~ ccontext, data=mini_c)
summary(mini_m) #ns

narrow = d_tran[d_tran$utterance=="narrow",]
narrow_c = cbind(narrow, myCenter(narrow[,c("context","valence")]))
narrow_m = glm(coll~ ccontext, data=narrow_c)
summary(narrow_m) #*
narrow_m = glm(diff~ ccontext, data=narrow_c)
summary(narrow_m) #*

short = d_tran[d_tran$utterance=="short",]
short_c = cbind(short, myCenter(short[,c("context","valence")]))
short_m = glm(coll~ ccontext, data=short_c)
summary(short_m) #ns
short_m = glm(diff~ ccontext, data=short_c)
summary(short_m) #ns

skinny = d_tran[d_tran$utterance=="skinny",]
skinny_c = cbind(skinny, myCenter(skinny[,c("context","valence")]))
skinny_m = glm(coll~ ccontext, data=skinny_c)
summary(skinny_m) #ns
skinny_m = glm(diff~ ccontext, data=skinny_c)
summary(skinny_m) #ns

slight = d_tran[d_tran$utterance=="slight",]
slight_c = cbind(slight, myCenter(slight[,c("context","valence")]))
slight_m = glm(coll~ ccontext, data=slight_c)
summary(slight_m) #ns
slight_m = glm(diff~ ccontext, data=slight_c)
summary(slight_m) #ns

small = d_tran[d_tran$utterance=="small",]
small_c = cbind(small, myCenter(small[,c("context","valence")]))
small_m = glm(coll~ ccontext, data=small_c)
summary(small_m) #ns
small_m = glm(diff~ ccontext, data=small_c)
summary(small_m) #ns

tall = d_tran[d_tran$utterance=="tall",]
tall_c = cbind(tall, myCenter(tall[,c("context","valence")]))
tall_m = glm(coll~ ccontext, data=tall_c)
summary(tall_m) #.
tall_m = glm(diff~ ccontext, data=tall_c)
summary(tall_m) #.

thick = d_tran[d_tran$utterance=="thick",]
thick_c = cbind(thick, myCenter(thick[,c("context","valence")]))
thick_m = glm(coll~ ccontext, data=thick_c)
summary(thick_m) #ns
thick_m = glm(diff~ ccontext, data=thick_c)
summary(thick_m) #ns

thin = d_tran[d_tran$utterance=="thin",]
thin_c = cbind(thin, myCenter(thin[,c("context","valence")]))
thin_m = glm(coll~ ccontext, data=thin_c)
summary(thin_m) #ns
thin_m = glm(diff~ ccontext, data=thin_c)
summary(thin_m) #.

tiny = d_tran[d_tran$utterance=="tiny",]
tiny_c = cbind(tiny, myCenter(tiny[,c("context","valence")]))
tiny_m = glm(coll~ ccontext, data=tiny_c)
summary(tiny_m) #ns
tiny_m = glm(diff~ ccontext, data=tiny_c)
summary(tiny_m) #ns

wide = d_tran[d_tran$utterance=="wide",]
wide_c = cbind(wide, myCenter(wide[,c("context","valence")]))
wide_m = glm(coll~ ccontext, data=wide_c)
summary(wide_m) #ns
wide_m = glm(diff~ ccontext, data=wide_c)
summary(wide_m) #ns





##### analyze subject information

num_round_dirs = 6 # problem with round 5
sf = do.call(rbind, lapply(1:num_round_dirs, function(i) {
   return (read.csv(paste(
     'round', i, '/dimensional-subject_information.csv', sep='')) %>%
     #'round', i, '/long-square-trials.csv', sep='')) %>%
       mutate(workerid = (workerid + (i-1)*9)))}))
#sf = sf[sf$language!=""&sf$language!="Spanish"&sf$language!="Cantonese",]
summary(sf)
# mean age: 35; median age: 31
table(sf$education)
# -1      1       2         3         4 
#  1      9 (18%) 17 (31%)  23 (43%)  4 (8%)
table(sf$enjoyment)
table(sf$asses)
table(sf$gender)
# female: 21 (37%); male: 32 (63%)
# reward: $0.50
