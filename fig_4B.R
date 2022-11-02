
##Load libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(plyr)
library(FSA)


##Set file paths
getwd()
wd <- list()
wd$data <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/data/"
wd$output <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/output/"


##Load data sets
colon <- read.csv(file.path(wd$data, "colon_bifstatus.csv"))

colon$Treatment <- factor(colon$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"), ordered = TRUE)

mp802fl <- colon[colon$Treatment == "MP80 2FL",]
twofl <- colon[colon$Treatment == "2FL",]
mp80 <-colon[colon$Treatment == "MP80",]
control <- colon[colon$Treatment == "control",]


## Create table of p-values to annotate plots with symbols for significance
pValues <- matrix(NA, nrow = 1, ncol = 38)
colnames(pValues) <- colnames(colon)[7:44]
##Dunn's posthoc test for kruskal wallis
for (i in 7:ncol(colon)) {  #loop
  model = t.test(mp802fl[,i] ~ bifstatus, mp802fl) #wilcox.test 
  pValues[,i-6] = unlist(model$p.value) #putting p values into empty matrix
  
}

##Correct for multiple comparisons
pValueadj <- as.data.frame(p.adjust(pValues, method = "fdr"))
colnames(pValueadj) <- "MP80+2FL_bifstatus"
rownames(pValueadj) <- colnames(pValues)
pValueadj


write.csv(pValueadj, file = "output/ANOVA/FDR_corrected_t_test_20210401.csv")

labs <- c("HE", "ME")

boxplot1 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Propylene.glycol, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "1,2-propanediol", y = "umol/g", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Propylene.glycol)+0.2, yend = max(mp802fl$Propylene.glycol)+0.2,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Propylene.glycol)+0.25, label = "**", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("HE", "ME"))
boxplot1

leg <- get_legend(boxplot1)

boxplot1 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Propylene.glycol, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "1,2-propanediol", y = "(umol/g)", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Propylene.glycol)+0.2, yend = max(mp802fl$Propylene.glycol)+0.2,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Propylene.glycol)+0.25, label = "**", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot1

boxplot2 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Formate, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "Formate", y = "", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Formate)+0.25, yend = max(mp802fl$Formate)+0.25,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Formate)+0.5, label = "‡", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot2

boxplot3 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Lactate, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "Lactate", y = "", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Lactate)+2.5, yend = max(mp802fl$Lactate)+2.5,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Lactate)+3.5, label = "**", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot3

boxplot4 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Acetate, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "Acetate", y = "(umol/g)", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Acetate)+4.5, yend = max(mp802fl$Acetate)+4.5,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Acetate)+5, label = "*", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot4

boxplot5 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Propionate, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "Propionate", y = "", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Propionate)+2.75, yend = max(mp802fl$Propionate)+2.75,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Propionate)+3, label = "*", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot5

boxplot6 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Butyrate, fill = bifstatus)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  labs(title = "Butyrate", y = "", x = "") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Butyrate)+3.5, yend = max(mp802fl$Butyrate)+3.5,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Butyrate)+4, label = "*", size = 3) +
  scale_x_discrete(labels = labs) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(),
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Non-responder", "Responder"))
boxplot6


grid3 <- cowplot::plot_grid(boxplot1, boxplot2, boxplot3, boxplot4, boxplot5, boxplot6, nrow = 2)

ggsave(
  file = "bifstatus_major_products_all.tiff",
  plot = grid3,
  dpi = 1000,
  width = 3.8,
  height = 4,
  units = "in"
)

ggsave(
  file = "bifstatus_legend.tiff",
  plot = leg,
  dpi = 600,
  width = 1.5,
  height = 1.5,
  units = "in"
)
