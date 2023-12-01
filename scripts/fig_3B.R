#Figure 3B: Morisita-Horn for High/Low Bifido
#data: mh_respond_glm

load(file='../data/Morisita-Horn.RData')

library(ggplot2)

# ggplot(data=mh_respond_glm, aes(x=Responder2, y= MorisitaHornDistance, fill = Responder2)) + 
#   geom_boxplot(outlier.color = "black") +
#   geom_dotplot(binaxis = 'y', stackdir = 'center',dotsize = 1) +
#   ggtitle("Morisita Horn Baseline:Day4") +
#   scale_fill_manual(values = c("seagreen3", "seagreen1"), labels=c("Responder", "Non-responder"))
labs <- c("HE", "ME")

mh_respond_glm$Responder2 <- factor(mh_respond_glm$Responder2, levels = c("1", "0"), ordered = TRUE)

ggplot(data=mh_respond_glm, aes(x=Responder2, y= MorisitaHornDistance, fill = Responder2)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 3) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = "Morisita-Horn distance", x = "Persistence") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 1, yend = 1) +
  annotate("text", x = 1.5, y = 1.01, label ="*", size = 4) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("High", "Low")) +
expand_limits(x = 0.5) +
  scale_x_discrete(labels= labs) +
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
  ) 
ggsave("mh_respond_glm.tiff", width = 3, height = 3, units = "in", dpi=1000)
