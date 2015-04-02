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

d = read.table("/Users/Greg/Documents/git/CoCoLab/collective/corpora/bnc.tab",sep="\t",header=T,quote="")
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

summary(d[d$SubjectLength > 2,])

sort(table(d$Noun),decreasing=TRUE)[1:22]
sort(table(d$Predicate),decreasing=TRUE)[1:24]

# just "the" determiner
t = d[d$Determiner=="the",]

t$noun_verb = paste(t$Noun,t$Predicate,sep=" ")

#find the most common sentences
sort(table(t$noun_verb),decreasing=TRUE)[1:50]

