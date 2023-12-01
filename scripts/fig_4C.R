# Compare total and ratios of organic acids across treatment groups

library(tidyverse)
library(forcats)
library(RColorBrewer)
library(cowplot)

#Load data
data <- read_csv("../data/total_ratios.csv")
data$Treatment <- factor(data$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"))

data1 <- pivot_longer(data, c(acetate_ratio, propionate_ratio, butyrate_ratio, lactate_ratio, formate_ratio, pyruvate_ratio, succinate_ratio), names_to = "acid", values_to = "proportion")

data1$Treatment <- factor(data1$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"))
data1$acid <- as.factor(data1$acid)
data1$acid <- factor(data1$acid, levels = rev(c("acetate_ratio", "propionate_ratio", "butyrate_ratio", "lactate_ratio", "formate_ratio", "pyruvate_ratio", "succinate_ratio")))

mycol <- RColorBrewer::brewer.pal(8, "Dark2")

p1 <-
  ggplot(data = data1, aes(x = Treatment, y = proportion, fill = acid)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c(
    "cyan4",
    "orangered4",
    "goldenrod3",
    "deeppink3",
    "olivedrab",
    "#D95F02",
    "steelblue4"
  ), labels = rev(c(
    "acetate",
    "propionate",
    "butyrate",
    "lactate",
    "formate",
    "pyruvate",
    "succinate"
  ))) +
  labs(title = "", x = "", y = "Organic acid composition") +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::percent,
    breaks = c(0.2,0.4,0.6,0.8,1),
    limits = c(0, 1)
  ) +
  scale_x_discrete(expand = c(0, 0), labels = c("CON", "PRO", "PRE", "SYN")) +
  theme_classic() +
  theme(title = element_text(size = 8),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8, face = "italic"),
    axis.title.y = element_text(size = 8),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
p1
ggsave(
  file = "SCFA_ratio.tiff",
  plot = p1,
  dpi = 600,
  width = 2.8,
  height = 4,
  units = "in")
