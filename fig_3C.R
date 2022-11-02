#Figure 3C (Currently): High/Low Bif Final Day NMDS to be added to supplementary
#Data: nmds_weighted_mp80_2fl_final_plot

set.seed(42)
library(tidyverse)

#Need to change from High/Low Bifido to Responder/NonResponder?
#nmds_weighted_mp80_2fl_final_plot <- mutate(nmds_weighted_mp80_2fl_final_plot, Responder2  = case_when(
#Responder2 == '1' ~ "High Bifido",
# Responder2 == '0' ~ "Low Bifido",))

#Add grouping
#nmds_weighted_mp80_2fl_final_plot$Responder2 -> nmds_weighted_mp80_2fl_final_plot$group

#Weighted: Creating data frame that contains data on Ellipses
#nmds_weighted_mp80_2fl_final_plot$group <- as.factor(nmds_weighted_mp80_2fl_final_plot$group)

#nmds_weighted_mp80_2fl_final_plot$MDS1 <- as.factor(nmds_weighted_mp80_2fl_final_plot$group)
###then, one critical step is to exactly caculate the eclipse data use the grouping factors; ef_ell is the data frame 
# df_ell_wu <- data.frame()
# for(g in levels(nmds_weighted_mp80_2fl_final_plot$group)){
#   df_ell_wu <- rbind(df_ell_wu, cbind(as.data.frame(with(nmds_weighted_mp80_2fl_final_plot[nmds_weighted_mp80_2fl_final_plot$group==g,],
#                                                          veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2))))),
#                                       group=g))
# }

nmds.dat <- aggregate(cbind(MDS1, MDS2) ~ group, data = nmds_weighted_mp80_2fl_final_plot, FUN = mean)
nmds.dat1 <- nmds_weighted_mp80_2fl_final_plot %>%
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
  annotate("text", x = 0, y = 0.18, label ="p = 0.002") +
  scale_color_manual(values = c("seagreen", "seagreen1"), labels=c("HE", "ME")) +
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
    #legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(color=guide_legend("Persistence"))
nmds

ggsave("3b.tiff", width = 3.4, height = 3, units = "in", dpi = 1000)

library(cowplot)
legend <- ggplot(nmds.dat2, aes(
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
  annotate("text", x = 0, y = 0.18, label ="p = 0.002") +
  scale_color_manual(values = c("seagreen", "seagreen1"), labels=c("High", "Low")) +
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
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(color=guide_legend(""))
legend
leg <- get_legend(legend)

ggsave("3b_legend.tiff", plot = leg, width = 1.5, height = .3, units = "in", dpi = 600)
