library(tidyverse)
##Set file paths
getwd()
wd <- list()
wd$data <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/data/"
wd$output <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/output/"

data <- read.csv(file.path(wd$data,"brain_liver_pg.csv"))

data$Sample.type <- factor(data$Sample.type, levels = c("Liver", "Brain"), ordered = TRUE)


brain <-
  ggplot(data, aes(x = Sample.type, y = Propylene.glycol, group = Treatment, color = Treatment)) +
  geom_point(size = 4, position = position_dodge2(width=0.7)) +
  geom_point(size = 4, pch = 21,  color = "black", position = position_dodge2(width=0.7)) +
  #scale_y_continuous(limits = c(0, 0.8)) +
  #annotate("segment", x = 1, xend = 2, y = 0.04522, yend = 0.31289,
  #          colour = "black", size = 1) +
  # annotate("text", x = 3.15, y = (0.04522+0.31289)/2, label = "*", size = 8) +
 scale_color_manual(values = c("royalblue", "seagreen"), labels=c("CON", "SYN")) +
#scale_fill_manual(values = c("blue", "green"), labels=c("Control", "B.p. MP80 + 2'FL")) +
  labs(y = "1,2-propanediol (umol/g)", x = "") +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank()
  )
brain

ggsave(
  file = "brain_liver_pg.tiff",
  plot = brain,
  width = 4,
  height = 3,
  dpi = 600)
