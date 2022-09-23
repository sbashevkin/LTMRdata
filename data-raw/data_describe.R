require(deltafish)
require(dplyr)
require(ggplot2)
library(ggrepel)

fish <- open_fish()
fish_sum <- fish %>%
  group_by(Taxa) %>%
  summarise(Count=sum(Count, na.rm=T), .groups="drop") %>%
  arrange(Count) %>%
  collect()%>%
  filter(Count>0)%>%
  mutate(Taxa=factor(Taxa, levels = rev(unique(Taxa))))

ggplot(fish_sum, aes(x=Taxa, y=Count, fill=Count))+
  geom_bar(stat="identity", color="white")+
  geom_label_repel(data=fish_sum[seq(nrow(fish_sum), 1, by=-20),], aes(label=Taxa), direction="y", hjust=-0.5, fill="white", color="black", fontface="italic")+
  scale_y_continuous(trans = "log1p", breaks=c(0, 1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8), expand=expansion(0, 0))+
  ylab("Count (log scale)")+
  xlab("")+
  scale_fill_viridis_c(trans = "log1p")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0, hjust=1, face="italic", size=8), legend.position = "none", text=element_text(size=16))

p<-ggplot(fish_sum, aes(x=Taxa, y=Count, fill=Count))+
  geom_bar(stat="identity", color="white")+
  geom_label_repel(data=fish_sum[seq(nrow(fish_sum), 88, by=-20),], aes(label=Taxa), direction="y", hjust=-0.5, fill="white", color="black", fontface="italic", size=5)+
  scale_y_continuous(trans = "log", breaks=c(1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8), label=scales::label_number(big.mark = ","), expand=expansion(0, 0))+
  ylab("Total count (log scale)")+
  xlab("Species")+
  scale_fill_viridis_c(trans = "log1p")+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.position = "none", text=element_text(size=24), panel.grid=element_blank(), axis.ticks.x=element_blank())

ggsave(plot=p, filename="~/deltafish_plot.png", height=6, width=8, units="in", dpi=300)
