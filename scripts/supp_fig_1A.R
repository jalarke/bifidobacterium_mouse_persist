#Figure 2A (currently): Alpha diversity at baseline for high/low Bifido mice (added to supplemental)
#data: map_alpha_baseline
load("~/Research/Projects/Mouse metabolite cohorts/Britta code and figures/MouseMetaboliteManuscript.RData")
#Normally distributed?
shapiro.test(map_alpha_baseline$shan)
#W = 0.95776, p-value = 0.7515
#Normal! Proceed with t.test
t.test(map_alpha_baseline$shan[map_alpha_baseline$Responder=="1"], map_alpha_baseline$shan[map_alpha_baseline$Responder=="0"])
table(map_alpha_baseline$Responder)
#t = -1.3182, df = 8.8557, p-value = 0.2205 

library(ggplot2)
map_alpha_baseline$Responder2 <- as.character(map_alpha_baseline$Responder2)

labs <- c("HE", "ME")

ggplot(data=map_alpha_baseline, aes(x=Responder2, y= shan, fill = Responder2)) + 
  geom_boxplot(outlier.color = "black") +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = "Shannon diversity index", x = "Persistence") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 3.95, yend = 3.95) +
  annotate("text", x = 1.5, y = 3.98, label ="p = 0.221") +
  scale_fill_manual(values = c("seagreen3", "seagreen1"), labels=c("High", "Low")) +
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

ggsave("Supplemental_fig1_BaselineAlpha.tiff", width = 3, height = 3, units = "in", dpi=1000)

save(nmds_weighted_mp80_2fl_alldays_plot, nmds_weighted_mp80_2fl_baseline_plot, nmds_weighted_mp80_2fl_final_plot, nmds_weighted_propionate_plot, nmds_weighted_propanediol_plot, map_alpha_baseline_baseline, songbird_butyrate, propanediol_map, propionate_map, songbird_respond_final, file = "MouseMetaboliteManuscript.RData")
