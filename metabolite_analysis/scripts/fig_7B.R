
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
serum <- read.csv(file.path(wd$data, "allserum_bifstatus.csv"))

##Remove metabolites to which quantitation is below LOQ
serum$X2..fucosyl.lactose <- NULL
serum$AMP <- NULL
serum$N.N.Dimethylglycine <- NULL
serum$O.Acetylcarnitine <- NULL
serum$Trimethylamine <- NULL

# serum <-
#   rename(
#     serum,
#     c(
#       "X2.Oxoglutarate" = "2-oxoglutarate",
#       "X4.Hydroxyphenylacetate" = "4-hydroxyphenylacetate",
#       "X5.Aminopentanoate" = "5-aminopentanoate",
#       "Propylene.glycol" = "1,2-propanediol",
#       "myo.Inositol" = "myo-Inositol"
#     )
#   )

serum$bifstatus <- factor(serum$bifstatus, levels = c(">50%", "<50%"), ordered = TRUE)

mp802fl <- serum[serum$Treatment == "MP80 2FL",]



## Create table of p-values to annotate plots with symbols for significance
pValues <- matrix(NA, nrow = 1, ncol = 44)
colnames(pValues) <- colnames(serum)[7:50]
##Dunn's posthoc test for kruskal wallis
for (i in 7:ncol(serum)) {  #loop
  model = t.test(mp802fl[,i] ~ bifstatus, mp802fl) #wilcox.test 
  pValues[,i-6] = unlist(model$p.value) #putting p values into empty matrix
  
}

##Correct for multiple comparisons
pValueadj <- as.data.frame(p.adjust(pValues, "fdr"))
colnames(pValueadj) <- "MP80+2FL_bifstatus"
rownames(pValueadj) <- colnames(pValues)
pValueadj


write.csv(pValueadj, file = "output/ANOVA/Serum_FDR_corrected_t_test_20210108.csv")

labs <- c("HE", "ME")

boxplot1 <-
  ggplot(mp802fl, aes(x = bifstatus, y = Propylene.glycol, fill = bifstatus)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 3) +
  labs(title = "", y = "1,2-propanediol (uM)", x = "Persistence") +
  annotate("segment", x = 1, xend = 2, y = max(mp802fl$Propylene.glycol)+10, yend = max(mp802fl$Propylene.glycol)+10,
           colour = "black") +
  annotate("text", x = 1.5, y = max(mp802fl$Propylene.glycol)+15, label = "*", size = 3) +
  theme_classic() +
  scale_x_discrete(labels = labs) +
  theme(
    axis.ticks.length.x =unit(0,"cm"),
    title= element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  ) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("Highly enriched", "Moderately enriched")) +
  guides(fill=guide_legend("Persistence"))
boxplot1

ggsave(
  file = "12pd_serum.tiff",
  plot = boxplot1,
  width = 2.5,
  height = 3
)

