##NMDS plots used as ordination method for comparing metabolites between Treatment groups

##Load libraries
library(vegan)
library(goeveg)
library(ggpubr)
library(ggrepel)
library(tidyverse)

##Load data
colon <- read.csv("../data/colonv4.csv")
colon$X2..fucosyl.lactose <- NULL # remove 2FL as CON and PRO groups did not receive for experimental setup

##transform data
colnames(colon)
for(j in 4:ncol(colon)){
  for(i in 1:nrow(colon)){
    if(colon[i,j] > 0){
      colon[i,j] <- log(colon[i,j] + sqrt(colon[i,j]^2 + 1))
    } else {
      colon[i,j] <- 0
    }
  }
}

colon <-
  rename(
    colon,
    c(
      "X4.Hydroxyphenylacetate" = "4-hydroxyphenylacetate",
      "X2.Oxoglutarate" = "2-oxoglutarate",
      "X5.Aminopentanoate" = "5-aminopentanoate",
      "Propylene.glycol" = "1,2-propanediol", 
      "myo.Inositol" = "myo-inositol"
    )
  )

##Create new groups for each timepoint
colon$Treatment <- factor(colon$Treatment, levels = c("control", "MP80", "2FL", "MP80 2FL"), ordered = TRUE)

##NMDS
colnames(colon)
nmds.colon <- metaMDS(colon[4:ncol(colon)],autotransform=F, noshare=F, distance = "euclidian")

##Check stress, values below 0.2 are recommended
stressplot(nmds.colon)
plot(nmds.colon)
scree<- metaMDS(colon[4:ncol(colon)], distance= "euclidean") ## Stress = 0.

##Rename grouos
Treatment <- colon$Treatment
row.names(nmds.colon[["points"]]) <- Treatment

metab <- as.data.frame(nmds.colon["species"])

metab$metabolite <- rownames(metab)

##Visualize with ggplot

##Calculate centroids
cent <- aggregate(cbind(MDS1, MDS2) ~ Treatment, data = nmds.colon$points, FUN = mean)

cent.colon <- cbind.data.frame(Treatment, nmds.colon$points)

g1 <- merge(cent.colon,aggregate(cbind(mean.x=MDS1,mean.y=MDS2)~Treatment,cent.colon,mean),by="Treatment")


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
    aes(x = metab[,1], y = metab[,2], label = row.names(metab)),
    force = 6,
    max.overlaps = 16,
    min.segment.length =1,
    cex = 3,
    color = "black",
  ) +
 scale_colour_manual(values = c("royalblue", "purple", "firebrick", "seagreen"), labels=c("CON", "PRO", "PRE", "SYN")) +
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
  filename = "colon_NMDS.tiff",
  plot = nmds,
  path = "output/",
  width = 6.8,
  height = 4.5,
  units = "in",
  dpi = 600
)

#test whether the dispersons are the same (H0: dispersion A = dispersion B)

nmdsdisp <- vegdist(colon[4:ncol(colon)], method = "euclidean")

disper.all<-betadisper(nmdsdisp, Treatment)

TukeyHSD(disper.all) ##Dispersions not the same, Ad

plot(disper.all)

##reject null hypothesis; the groups have different dispersions; will NOT meet assumptions of permanova
