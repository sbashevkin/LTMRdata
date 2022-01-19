require(deltafish)
require(dplyr)
require(ggplot2)

fish <- open_fish()
fish_sum<-fish%>%
  group_by(Taxa)%>%
  summarise(Count=sum(Count), .groups="drop")%>%
  collect()

ggplot(fish_sum, aes(x=Taxa, y=Count))+
  geom_bar(stat="identity")
