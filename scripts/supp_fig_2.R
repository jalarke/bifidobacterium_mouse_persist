#Figure S1B: Relative abundance of Lachnospiraceae and Ruminococcaceae at final time point for HE/ME groups
library(tidyverse)

# Load data
data <- read.csv("../data/metabolite_profiled_mice_081622.csv")
data$Treatment <- recode_factor(data$Treatment, pbs_water = 'CON', mp80_water = 'PRO', pbs_2fl = 'PRE', mp80_2fl = 'SYN')
data <- filter(data, !SampleID %in% c('lm3_d9', 'lm2_d9', 'lq1_d9', 'lq2_d9', 'lq3_d9', 'lo1_d9', 'lo2_d9', 'lo3_d9', 'lr1_d9', 'lr2_d9', 'lr3_d9')) #remove duplicate samples for final timepoint
data <- filter(data, TimePeriod %in% c(0, 4, 9))

data$TimePeriod <- recode_factor(data$TimePeriod, `0` = 'Baseline', `4` = 'Day 4', `9` = 'Final Day')
#Subset final day for significance testing
final_day <- data %>% filter(TimePeriod == 'Final Day') %>% filter(Treatment == "SYN")


final_day$Treatment <- factor(final_day$Treatment)
final_day$Responder <- as.factor(recode_factor(final_day$Responder, `1` = 'HE', `0` = 'ME'))

## HE/ME bif mice and proprotions of Lachno and Rumino
rumilab <- expression(paste(italic("Ruminococcaceae")))
lachlab <- expression(paste(italic("Lachnospiraceae")))
labs <- c("HE", "ME")

bif_status_lachno <-
  ggplot(final_day, aes(x = Responder, y = Lachnospiraceae, fill = Responder)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = lachlab, x = "Persistence") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # annotate("segment", x = 2, xend = 3, y = 0.11, yend = 0.11,
  #          colour = "black", size = 1) +
  # annotate("text", x = 2.5, y = 0.1125, label = "***", size = 8) +
  scale_fill_manual(values = c("seagreen3", "seagreen1"), labels=c("High", "Low")) +
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
  ) 
bif_status_lachno

ggsave(
  file = "lachno_final_day.tiff",
  plot = bif_status_lachno,
  width = 3,
  height = 3.5,
  dpi = 600)

bif_status_rumino <-
  ggplot(final_day, aes(x = Responder, y = Ruminococcaceae, fill = Responder)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = rumilab, x = "Persistence") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # annotate("segment", x = 2, xend = 3, y = 0.11, yend = 0.11,
  #          colour = "black", size = 1) +
  # annotate("text", x = 2.5, y = 0.1125, label = "***", size = 8) +
  scale_fill_manual(values = c("seagreen3", "seagreen1"), labels=c("High", "Low")) +
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
  ) 
bif_status_rumino

ggsave(
  file = "rumino_final_day.tiff",
  plot = bif_status_rumino,
  width = 3,
  height = 3.5,
  dpi = 600)
