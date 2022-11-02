#Figure 2B (Currently): High/Low Bif Baseline NMDS to be added to supplementary
#Data: nmds_weighted_mp80_2fl_baseline_plot

set.seed(42)
library(ggplot2)
#Need to change from High/Low Bifido to Responder/NonResponder?
#nmds_weighted_mp80_2fl_baseline_plot <- mutate(nmds_weighted_mp80_2fl_baseline_plot, Responder2  = case_when(
  #Responder2 == '1' ~ "High Bifido",
  #Responder2 == '0' ~ "Low Bifido",))

#Vegan package formula
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Weighted: Creating data frame that contains data on Ellipses
nmds_weighted_mp80_2fl_baseline_plot$Responder2 -> nmds_weighted_mp80_2fl_baseline_plot$group
nmds_weighted_mp80_2fl_baseline_plot$group <- as.factor(nmds_weighted_mp80_2fl_baseline_plot$group)

###then, one critical step is to exactly calculate the eclipse data use the grouping factors; ef_ell is the data frame 
df_ell_wu <- data.frame()
for(g in levels(nmds_weighted_mp80_2fl_baseline_plot$group)){
  df_ell_wu <- rbind(df_ell_wu, cbind(as.data.frame(with(nmds_weighted_mp80_2fl_baseline_plot[nmds_weighted_mp80_2fl_baseline_plot$group==g,],
                                                         veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2))))),
                                      group=g))
}

#Weighted Unifrac NMDS
ggplot(data = nmds_weighted_mp80_2fl_baseline_plot, aes(MDS1, MDS2)) + geom_point(aes(color = group), size=4) +
  geom_path(data=df_ell_wu, aes(x=MDS1, y=MDS2,color=group), size=1, linetype=1)+ 
  xlim (-0.2,0.2) +
  ylim (-0.2,0.2) +
  labs (x="NMDS1", y="NMDS2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  #theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text (size = 20), axis.text.x  = element_text(size = 20), axis.title = element_text(size = 15)) +
  theme(legend.position=c(0.95,0.25), legend.text=element_text(size=22)) +
  #scale_color_manual(values=c("#6BB11A", "#0A3F63", "#D94579","#e28743","#1e81b0"))
  scale_fill_manual(values = c("seagreen3", "seagreen1"), labels=c("Responder", "Non-responder"))

#Create centroides
library(dplyr)
nmds.dat <- aggregate(cbind(MDS1, MDS2) ~ group, data = nmds_weighted_mp80_2fl_baseline_plot, FUN = mean)
nmds.dat1 <- nmds_weighted_mp80_2fl_baseline_plot %>%
  select(group, MDS1, MDS2)
nmds.dat2 <- merge(nmds.dat1,aggregate(cbind(mean.x=MDS1,mean.y=MDS2)~group,nmds.dat1,mean),by="group")

#NMDS with group centroids
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
  annotate("text", x = -0.1, y = -0.15, label ="p = 0.256") +
  scale_color_manual(values = c("seagreen3", "seagreen1"), labels=c("HE", "ME")) +
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


#okay, finally let's save these plots 
ggsave("nmds_mp80_2FL_baseline_centroid.tiff", width = 4, height = 3, units = "in", dpi = 1000)

