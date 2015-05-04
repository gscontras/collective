## WSJ

d = read.table("/Users/Greg/Documents/git/CoCoLab/collective/corpora/wsj.tab",sep="\t",header=T,quote="")
summary(d)

ggplot(d, aes(x=Noun)) +
  geom_histogram() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

ggplot(d, aes(x=Determiner)) +
  geom_histogram() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

ggplot(d, aes(x=Predicate)) +
  geom_histogram() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

d[d$SubjectLength > 2,]


## BNC

are = read.table("/Users/Greg/Documents/git/CoCoLab/collective/corpora/results/bnc_are.tab",sep="\t",header=T,quote="")
are$tense = "are"
were = read.table("/Users/Greg/Documents/git/CoCoLab/collective/corpora/results/bnc_were.tab",sep="\t",header=T,quote="")
were$tense = "were"
d = rbind(are,were)
summary(d)

summary(d[d$SubjectLength > 2,])

sort(table(d$Noun),decreasing=TRUE)[1:22]
sort(table(d$Predicate),decreasing=TRUE)[1:24]

# just "the" determiner
t = d[d$Determiner=="the",]

big = t[t$Predicate=="big",]
sort(table(big$Noun),decreasing=TRUE)[1:22]
tall = t[t$Predicate=="tall",]
sort(table(tall$Noun),decreasing=TRUE)[1:22]
heavy = t[t$Predicate=="heavy",]
sort(table(heavy$Noun),decreasing=TRUE)[1:22]

t$noun_verb = paste(t$Noun,t$Predicate,sep=" ")

#find the most common sentences
sort(table(t$noun_verb),decreasing=TRUE)[1:50]

