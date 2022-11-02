
##Load libraries
library(vegan)
library(goeveg)
library(ggpubr)
library(ggrepel)
library(tidyverse)
library(viridis)

##Set file paths
getwd()
wd <- list()
wd$data <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/data/"
wd$output <- "C:/Users/jalarke/Documents/Research/Projects/Mouse metabolite cohorts/output/"


##Load data sets

serum <- read.csv(file.path(wd$data, "allserum.csv"))
serum$X2..fucosyl.lactose <- NULL
serum$N.N.Dimethylglycine <- NULL
serum$Trimethylamine <- NULL
serum$Acetoacetate <- NULL
#serum$AMP <- NULL
#serum$Ethanol <- NULL

##transform data
colnames(serum)
for(j in 5:ncol(serum)){
  for(i in 1:nrow(serum)){
    if(serum[i,j] > 0){
      serum[i,j] <- log(serum[i,j] + sqrt(serum[i,j]^2 + 1))
    } else {
      serum[i,j] <- 0
    }
  }
}

serum <-
  rename(
    serum,
    c(
      "3-hydroxybutyrate" = "X3.Hydroxybutyrate",
      "3-hydroxyisobutyrate" = "X3.Hydroxyisobutyrate",
      "1,2-propanediol" = "Propylene.glycol",
      "myo-inositol" = "myo.Inositol" 
    )
  )

##NMDS plots used as ordination method for comparing serum metabolites between Treatment groups


##Create new groups for each timepoint
serum$Cohort <- as.factor(serum$Cohort)
serum$Treatment <- factor(serum$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"), ordered = TRUE)

##NMDS
colnames(serum)
nmds.serum <- metaMDS(serum[5:ncol(serum)],autotransform=F, noshare=F, k = 2, distance = "euclidian")

##Check stress, values below 0.2 are recommended
stressplot(nmds.serum)
plot(nmds.serum)
scree<- metaMDS(serum[5:ncol(serum)], distance= "euclidean") ## Stress = 0.

##Rename grouos
mouse_id <- serum$MouseID
Treatment <- serum$Treatment
row.names(nmds.serum[["points"]]) <- Treatment

metab <- as.data.frame(nmds.serum["species"])

metab$metabolite <- row.names(metab) 

metab$metabolite[16] <- "Formate"
metab$metabolite[17] <- "Fucose"
metab$metabolite[35] <- "1,2-propanediol"


##Visualize with ggplot

##Calculate centroids
cent <- aggregate(cbind(MDS1, MDS2) ~ Treatment, data = nmds.serum$points, FUN = mean)

cent.serum <- cbind.data.frame(Treatment,mouse_id, nmds.serum$points)

g1 <- merge(cent.serum,aggregate(cbind(mean.x=MDS1,mean.y=MDS2)~Treatment,cent.serum,mean),by="Treatment")

##NMDS plot  
nmds <- ggplot(g1, aes(
  x = MDS1,
  y = MDS2,
  color = factor(Treatment)
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
  geom_point(data = metab, x = metab[,1], y = metab[,2], color = "black") +
  geom_text_repel(
    data = metab,
    aes(x = metab[,1], y = metab[,2], label = metab[,3]),
    #force = 5,
    min.segment.length = Inf,
    max.overlaps = 10,
    cex = 3,
    color = "black",
  ) +
 scale_color_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN")) + 
#annotate("text", x = g1$MDS1, y = g1$MDS2, label = g1$mouse_id) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
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

nmds
ggsave(
  filename = "serum_NMDS.tiff",
  plot = nmds,
  device = "tiff",
  path = "output/",
  width = 6.8,
  height = 4.5,
  units = "in",
  dpi = 600
)


#test whether the dispersons are the same (H0: dispersion A = dispersion B)

nmdsdisp <- vegdist(serum[5:ncol(serum)], method = "euclidean")

disper.all<-betadisper(nmdsdisp, Treatment)

TukeyHSD(disper.all) ##Dispersions are significantly different, cannot permanova
plot(disper.all)
