#Creating a differential log ratio plot to demonstrate log ratio difference in HE/ME Bifido groups

load(file = '../data/MouseMetaboliteManuscript.RData')

#High/Low Bifido Categories
#data: songbird_respond_final
library(ggplot2)

blab <- expression(paste("Log ", bgroup("(",italic(frac("Bifidobacteriaceae","Lachnospiraceae + Ruminococcaceae")),")"),sep = ""))
labs <- c("HE", "ME")
  ggplot(data=songbird_respond_final, aes(x=Responder2, y= Bifido_LachnoRumino, fill = Responder2)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 3) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = blab, x = "Persistence") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 9, yend = 9) +
  annotate("text", x = 1.5, y = 9.25, label ="***", size = 4) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("High", "Low")) +
  expand_limits(x = 0.5) +
    scale_x_discrete(labels = labs) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(fill=guide_legend(""))

ggsave("3D.tiff", width = 3, height =3, units = "in", dpi=1000)
