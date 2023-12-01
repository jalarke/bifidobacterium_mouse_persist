#Figure 6B: NMDS Propanediol Categorization
#Data: nmds_weighted_propanediol_plot

load(file = '../data/MouseMetaboliteManuscript.RData')
library(ggplot2)

nmds.dat <- aggregate(cbind(MDS1, MDS2) ~ group, data = nmds_weighted_propanediol_plot, FUN = mean)
nmds.dat1 <- nmds_weighted_propanediol_plot %>%
  select(group, MDS1, MDS2)
nmds.dat2 <- merge(nmds.dat1,aggregate(cbind(mean.x=MDS1,mean.y=MDS2)~group,nmds.dat1,mean),by="group")

nmds <- ggplot(nmds.dat2, aes(
  x = MDS1,
  y = MDS2,
  color = factor(group)
)) +
  geom_point(size = 3) +
  geom_point(aes(x = mean.x, y = mean.y), size = 5) +
  geom_segment(aes(
    x = mean.x,
    y = mean.y,
    xend = MDS1,
    yend = MDS2
  ), size = 1) +
  xlab("NMDS1") +
  ylab("NMDS2") +
  annotate("text", x = -0.15, y = 0.2, label ="p = 0.028") +
  scale_color_manual(values = c("red4", "red2"), labels=c("> median", "< median")) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ## legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(color=guide_legend("1,2-PD"))
nmds

ggsave("nmds_plot_propanediol.tiff", width = 3, height = 3, units = "in", dpi = 600)
