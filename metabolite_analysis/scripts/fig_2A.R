#Figure 2A 
#Stratify: BpMP80+2FL treatment to assess throughout time, metabolite profiled mice only!
#Color Code by Time Period
##NEVER FORGET YOU NEED TO MODIFY PERMANOVA IF LONGITUDINAL DATA!! (strata!)



library(dplyr)
library(GUniFrac)
setwd("/Volumes/GoogleDrive/Shared drives/Mills Lab/Britta/MetaboliteAnalysis/ANOVA_NMDS")

#for GUniFrac command, need rarefied asv table
#rownames = sampleid 
#Need a data frame of ASV counts
asv_map <- filter(map, Subject %in% c("lj1","lj2","lj3","lm1","lm2","lm3","ma1","ma2","ma3","ma4","ma5","ma6"))

#View(asv_map)
taxa_unifrac <- subset(taxa_rare, rownames(taxa_rare) %in% asv_map$SampleID)
summary(rowSums(taxa_unifrac))
#Rarified to 3000

#Remove ASVs with 0
taxa_unifrac <- taxa_unifrac[,colSums(taxa_unifrac) > 0]
summary(colSums(taxa_unifrac))
sort(colSums(taxa_unifrac))

Unifracs <- GUniFrac(taxa_unifrac, tree=tree)
#lets create a weighted unifrac (wu)
wu <- Unifracs$unifracs[,,"d_1"]

#need to create a scree plot to determine the number of dimensions necessary based on stress
nmds.scree <- function(x){
  plot(rep(1,10),replicate(10, metaMDS(x, autotransform=F,k=1)$stress), xlim=c(1, 10), ylim=c(0,0.5), xlab="# of Dimensions", ylab="Stress", main="NMDS Scree Plot")
  for (i in 1:10) {
    points(rep(i+1,10), replicate(10, metaMDS(x, autotransform=F, k=i+1)$stress))
  }
}
set.seed(1)
#Don't want too many dimensions! Look for elbow
#Can sometimes have two convergences that are super close together
#No exponential decay of results --> can't trust results
#Flat line --> one dimension plot
nmds.scree(wu) #2
#Create a wu NMDS from Unifrac Distances
nmds_unifrac.wu <- metaMDS(wu, k=2, try = 1000, trymax = 100)
nmds_unifrac.wu
#wu: k=2, stress = 0.07145842  
#This package (metaMDS) multiplies stress by 100, and shows NMDS coordinate 1 as most important

#Metadata file for wu
nmds_unifrac.wu_md <- data.frame(MDS1=nmds_unifrac.wu$points[,1], MDS2=nmds_unifrac.wu$points[,2])
nmds_unifrac.wu_md$SampleID <- rownames(nmds_unifrac.wu_md)
nmds_unifrac.wu_plot <- merge(nmds_unifrac.wu_md, asv_map, by = "SampleID")
#Need to ensure metadata and matrix are in same order
#match the order in dist_mp80 and nmds_mp80_plot
nmds_unifrac.wu_plot <- nmds_unifrac.wu_plot[order(match(nmds_unifrac.wu_plot$SampleID, rownames(wu))),]
identical(as.character(nmds_unifrac.wu_plot$SampleID), as.character(rownames(wu)))
identical(rownames(wu), colnames(wu))

#Assess dispersion for Time Period
class(nmds_unifrac.wu_plot$TimePeriod)
disp_unifrac <- anova(betadisper(as.dist(wu),nmds_unifrac.wu_plot$TimePeriod))
disp_unifrac
#Dispersion: TimePeriod = 0.003944 *

#################################################################################################################
#betadisper
#Fewer samples, larger dispersion = test is too liberal. If result NS, we don’t really care about the dispersion because if it's n.s. when the test is too liberal, it's really n.s.
#More samples at that time point, greater dispersion = test is too conservative. Results are significant, again we don't care so much as we can still trust the result.
mod <- betadisper(as.dist(wu),nmds_unifrac.wu_plot$TimePeriod)
anova(mod)
TukeyHSD(mod)
#Day 9 to 0 is significant
boxplot(mod)
table(nmds_unifrac.wu_plot$TimePeriod, nmds_unifrac.wu_plot$Cohort)
#TimePeriod 9 is the highest number of samples and the highest dispersion!
#Therefore, test is too conservative and we can move forward with PERMANOVA. 

############################################################

#PERMANOVA
permanova <- adonis2(as.dist(wu) ~ TimePeriod, strata = as.factor(nmds_unifrac.wu_plot$Subject), data = nmds_unifrac.wu_plot)
permanova
#Time Period: 0.001 *** (DISPERSION: TOO CONSERVATIVE)
#Posthoc testing
library(RVAideMemoire)
pairwise.perm.manova(nmds_unifrac.wu_plot[,2:3],nmds_unifrac.wu_plot$TimePeriod,nperm=500)
#TimePeriod: = All significant, except Day 9 when compared to Day 4 or 6 (during 2FL supplementation)

############################################################################

