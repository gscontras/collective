#### heavy plot

heavy = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/heavy2.csv",header=T)

head(heavy)
heavy$context <- factor(heavy$context,labels=c("random","regular"))

heavy_plot <- ggplot(heavy, aes(x=data,y=coll,fill=scenario)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\nsource of data") +
  labs(fill="scenario")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~context)
  #raw_plot <- raw_plot  +facet_grid(. ~ utterance)
  #theme_blackDisplay()
heavy_plot
ggsave("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/heavymodel2.pdf",height=3)


## big-tall plot

bigtall = read.csv("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/big-tall.csv",header=T)

bt_plot <- ggplot(bigtall, aes(x=data,y=coll,fill=factor(context,labels=c("random","regular")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=data, width=0.1),position=position_dodge(width=0.9))+
  ylab("collective endorsement\np(collective)\n")+
  ylim(0,1)+
  xlab("\nsource of data") +
  labs(fill="context")+
  scale_fill_manual(values=c("red", "blue"))+
  facet_grid(.~predicate) #+
  #theme_blackDisplay()
#raw_plot <- raw_plot  +facet_grid(. ~ utterance)
#+ theme_blackDisplay()
bt_plot
ggsave("~/Documents/git/CoCoLab/collective/writing/Cubert/plots/bigtallmodel2.pdf",height=3)
