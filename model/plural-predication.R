

my_params = data.frame(
  noise = c(0.99, 0.75, 0.5, 0.25),
  nobjects = c(2,2,2,2),
  utterance = c("ambiguous-pos","ambiguous-pos","ambiguous-pos","ambiguous-pos")
)

model.results <- data.frame()
for (i in 1:nrow(my_params)){
  my_data = my_params[i,]
  print(my_data)
rs<-webppl(program_file = "~/Documents/git/CoCoLab/collective/model/plural-predication-arrangement-7.wppl",
       data = my_data,
       data_var = "my_data")

  rs.pk<-data.frame(rs$`partial knowledge`)
  rs.pk$noise <- my_data$noise
  rs.pk$nobjects <- my_data$nobjects
  rs.pk$knowledge <- "partial"
  
  rs.fk<-data.frame(rs$`full knowledge`)
  rs.fk$noise <- my_data$noise
  rs.fk$nobjects <- my_data$nobjects
  rs.fk$knowledge <- "full"
  
  model.results <- bind_rows(model.results, rs.pk, rs.fk)
}

r <- model.results[model.results$support==TRUE,]

ggplot(r, aes(x = noise,y = probs, fill = knowledge, group = knowledge))+
  geom_bar(stat='identity', position = position_dodge())+
  facet_wrap(~nobjects)+
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
