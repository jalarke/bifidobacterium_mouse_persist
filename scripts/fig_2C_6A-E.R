library(tidyverse)

data <- read.csv("../data/metabolite_profiled_mice_081622.csv")
data$Treatment <- recode_factor(data$Treatment, pbs_water = 'CON', mp80_water = 'PRO', pbs_2fl = 'PRE', mp80_2fl = 'SYN')
data <- filter(data, !SampleID %in% c('lm3_d9', 'lm2_d9', 'lq1_d9', 'lq2_d9', 'lq3_d9', 'lo1_d9', 'lo2_d9', 'lo3_d9', 'lr1_d9', 'lr2_d9', 'lr3_d9')) #remove duplicate samples for final timepoint
data <- filter(data, TimePeriod %in% c(0, 4, 9))

data$TimePeriod <- recode_factor(data$TimePeriod, `0` = 'Baseline', `4` = 'Day 4', `9` = 'Final Day')
#Subset final day for significance testing
final_day <- data %>% filter(TimePeriod == 'Final Day')


final_day$Treatment <- factor(final_day$Treatment)
kruskal.test(final_day$Bifidobacteriaceae, final_day$Treatment) #Significance test
dunn.test::dunn.test(final_day$Bifidobacteriaceae, final_day$Treatment, method = "bh")

blab <- expression(paste(italic("Bifidobacteriaceae")))


# plot for figure 2c
bif_all_group_time <-
  ggplot(data, aes(x = TimePeriod, y = Bifidobacteriaceae, group = Treatment, color = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group=Treatment),
    size = 1
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    aes(group=Treatment),
    size = 1,
    width = 0.1
  ) +
  annotate("segment", x = 3.55, xend = 3.55, y = 0.38976, yend = 0.04522,
           colour = "black", size = 1) +
  annotate("text", x = 3.7, y = (0.38976+0.04522)/2, label = "*", size = 8) +
  annotate("segment", x = 3.15, xend = 3.15, y = 0.38976, yend = 0.012500 ,
           colour = "black", size = 1) +
  annotate("text", x = 3.35, y = (0.38976+0.012500 )/2, label = "**", size = 8) +
  labs(y = blab, x = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  theme_classic() +
  expand_limits(x =4) +
  theme(
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
bif_all_group_time
ggsave(
  file = "bif_all_group_time.tiff",
  plot = bif_all_group_time,
  width = 3.4,
  height = 3.4,
  dpi = 600)

leg <- get_legend(bif_all_group_time)

ggsave(
  file = "legend.tiff",
  plot = leg,
  width = 2.6,
  height = 0.15,
  dpi = 600,
  units = "in")


####### Bacteroides
kruskal.test(final_day$Bacteroidaceae, final_day$Treatment) #Significance test
dunn.test::dunn.test(final_day$Bacteroidaceae, final_day$Treatment, method = "bh")

baclab <- expression(paste(italic("Bacteroidaceae")))

##figure 6C
bact_line <-
  ggplot(data, aes(x = TimePeriod, y = Bacteroidaceae, group = Treatment, color = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group=Treatment),
    size = 1
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    aes(group=Treatment),
    size = 1,
    width = 0.1
  ) +
  annotate("segment", x = 3.1, xend = 3.1, y = 0.090333, yend = 0.009222,
           colour = "black", size = 1) +
  annotate("text", x = 3.3, y = (0.090333+0.009222)/2, label = "**", size = 8) +
  labs(y = baclab, x = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
bact_line
ggsave(
  file = "Bact_by_day.tiff",
  plot = bact_line,
  width = 3,
  height = 3,
  dpi = 600)


######Lachnospiraceae
kruskal.test(final_day$Lachnospiraceae, final_day$Treatment) #Significance test
dunn.test::dunn.test(final_day$Lachnospiraceae, final_day$Treatment, method = "bh")


lachlab <- expression(paste(italic("Lachnospiraceae")))

lach_line <-
  ggplot(data, aes(x = TimePeriod, y = Lachnospiraceae, group = Treatment, color = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group=Treatment),
    size = 1
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    aes(group=Treatment),
    size = 1,
    width = 0.1
  ) +
  annotate("segment", x = 3.2, xend = 3.2, y = 0.4374, yend = 0.1391515 ,
           colour = "black", size = 1) +
  annotate("text", x = 3.4, y = (0.4374+0.1391515 )/2, label = "**", size = 8) +
  annotate("segment", x = 3.6, xend = 3.6, y = 0.4374, yend = 0.17026,
           colour = "black", size = 1) +
  annotate("text", x = 3.75, y = (0.4374+0.17026)/2, label = "*", size = 8) +
  annotate("segment", x = 3.9, xend = 3.9, y = 0.3756  , yend = 0.1391515 ,
           colour = "black", size = 1) +
  annotate("text", x = 4.1, y = (0.3756+0.1391515 )/2, label = "**", size = 8) +
  labs(y = lachlab, x = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  expand_limits(x = 4.3) +
  theme(
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
lach_line
ggsave(
  file = "Lachno_by_day.tiff",
  plot = lach_line,
  width = 3,
  height = 3,
  dpi = 600)


##########Ruminococcaceae
kruskal.test(final_day$Ruminococcaceae, final_day$Treatment) #Significance test
dunn.test::dunn.test(final_day$Ruminococcaceae, final_day$Treatment, method = "bh")

rumilab <- expression(paste(italic("Ruminococcaceae")))

rumi_line <-
  ggplot(data, aes(x = TimePeriod, y = Ruminococcaceae, group = Treatment, color = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group=Treatment),
    size = 1
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    aes(group=Treatment),
    size = 1,
    width = 0.1
  ) +
  annotate("segment", x = 3.2, xend = 3.2, y = 0.09333 , yend = 0.028556  ,
           colour = "black", size = 1) +
  annotate("text", x = 3.4, y = (0.09333+0.028556  )/2, label = "**", size = 8) +
  annotate("segment", x = 3.65, xend = 3.65, y = 0.09333, yend = 0.031485 ,
           colour = "black", size = 1) +
  annotate("text", x = 3.85, y = (0.09333+0.031485 )/2, label = "**", size = 8) +
  annotate("segment", x = 4.1, xend = 4.1, y = 0.07428   , yend = 0.028556  ,
           colour = "black", size = 1) +
  annotate("text", x = 4.225, y = (0.07428 +0.028556  )/2, label = "*", size = 8) +
  annotate("segment", x = 4.35, xend = 4.35, y = 0.07428   , yend = 0.031485,
           colour = "black", size = 1) +
  annotate("text", x = 4.5, y = (0.07428 +0.031485 )/2, label = "*", size = 8) +
  labs(y = rumilab, x = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(x = 4.6) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
rumi_line
ggsave(
  file = "Rumino_by_day.tiff",
  plot = rumi_line,
  width = 3,
  height = 3,
  dpi = 600)

# create plot with legend to get_legend:
rumi_line <-
  ggplot(data, aes(x = TimePeriod, y = Ruminococcaceae, group = Treatment, color = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group=Treatment),
    size = 1
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    aes(group=Treatment),
    size = 1,
    width = 0.1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position="bottom"
  ) +
  scale_color_manual(values = c("royalblue", "firebrick", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
rumi_line

leg <- cowplot::get_legend(rumi_line)

ggsave(
  file = "figure6_legend.tiff",
  plot = leg,
  width = 2.5,
  height = 0.3,
  dpi = 600)


metab <- read.csv("../data/colonv4.csv") #Load metabolite data

metab$Treatment <- factor(metab$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"), ordered = T)
metab$Treatment <- recode_factor(metab$Treatment, control = 'CON', MP80 = 'PRO', `2FL` = 'PRE', `MP80 2FL` = 'SYN')

##########Fucose
kruskal.test(metab$Fucose, metab$Treatment) #Significance test
dunn.test::dunn.test(metab$Fucose, metab$Treatment, method = "bh")

#figure 6D
fucose <-
  ggplot(metab, aes(x = Treatment, y = Fucose, fill = Treatment)) +
  geom_boxplot(outlier.shape = 21, outlier.size =2) +
  labs(y = "Fucose (umol/g)", x = "Final day") +
  annotate("segment", x = 1, xend = 3, y = 11, yend = 11,
           colour = "black") +
  annotate("text", x = 2, y = 11.25, label = "***", size = 8) +
  annotate("segment", x = 2, xend = 3, y = 12.2, yend = 12.2,
           colour = "black") +
  annotate("text", x = 2.5, y = 12.35, label = "***", size = 8) +
  annotate("segment", x = 1, xend = 4, y = 17.5, yend = 17.5,
           colour = "black") +
  annotate("text", x = 2.5, y = 17.75, label = "***", size = 8) +
  annotate("segment", x = 2, xend = 4, y = 18.7, yend = 18.7,
           colour = "black") +
  annotate("text", x = 3, y = 18.85, label = "***", size = 8) +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.length.x =unit(0,"cm"),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    #legend.position = "none"
  ) +
  scale_fill_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN"))
fucose

ggsave(
  file = "fucose1.tiff",
  plot = fucose,
  dpi = 600,
  width = 3,
  height = 3)

#figure 6e
meta_pre_syn <- metab %>% filter(Treatment == 'PRE' | Treatment == "SYN")
meta_pre_syn <- meta_pre_syn[-3,]  #remove lj3 as it corresponds to day 6
pre_syn <- final_day %>% filter(Treatment == 'PRE' | Treatment == "SYN")

meta_pre_syn <- meta_pre_syn %>% arrange(MouseID)

pre_syn <- pre_syn %>% arrange(Subject)
pre_syn$Treatment <- NULL

micro_metab <- cbind(meta_pre_syn, pre_syn)

r_value <- cor.test(micro_metab$Bacteroidaceae, micro_metab$Propionate, method = "pearson")

round(r_value$estimate, 3)
round(r_value$p.value, 6)

r_label = paste0("italic(r) ==", round(r_value$estimate, 3))
p_label = paste0("italic(p) ==", round(r_value$p.value, 5))


baclab <- expression(paste(italic("Bacteroidaceae")))

n3 <- ggplot(micro_metab, aes(x = Bacteroidaceae, y = Propionate)) +
  geom_point(color = "black",
             size = 2,
             shape = 16) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  xlab(baclab) +
  ylab("Propionate (umol/g)") +
  annotate(
    "text",
    x = 0.05,
    y = 14,
    label = r_label,
    parse = TRUE
  ) +
  annotate(
    "text",
    x = 0.05,
    y = 12.5,
    label = p_label,
    parse = TRUE
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ## legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  )

n3

ggsave("bacteroides_propionate_pearson.tiff",
       plot = n3,
       width = 2.5,
       height = 2.5,
       dpi = 600)
