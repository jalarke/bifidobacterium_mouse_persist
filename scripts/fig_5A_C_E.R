#Creating a differential log ratio plot to demonstrate log ratio difference
#Butyrate, Propionate, Propanediol, Lactate,and High/Low Bifido

#Propionate
#data: propionate_map
# Fig 5c
library(dplyr)
library(ggplot2)
class(propionate_map$Bact_Bifido)
labs <- c("High", "Low")
lab1 <- expression(paste("Log ", bgroup("(",italic(frac("Bacteroidaceae","Bifidobacteriaceae")),")"),sep = ""))

ggplot(data=propionate_map, aes(x=Propionate_category, y= Bact_Bifido, fill = Propionate_category)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 2) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = lab1, x = "") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 4, yend = 4) +
  annotate("text", x = 1.5, y = 4.25, label ="*", size =3) +
  scale_fill_manual(values = c("red4", "red2"), labels=c("> median", "< median")) +
  expand_limits(x = 0.5) +
  scale_x_discrete(labels= labs) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(fill=guide_legend("Propionate"))

ggsave("songbird_propionate.tiff", width = 2.5, height = 2.5, units = "in", dpi=600)

#Butyrate
#Data: songbird_butyrate. NOTE: data has two NAs due to zero values (log(0) = NA)
# Fig 5A
lab2 <- expression(paste("Log ", bgroup("(",italic(frac("Lachnospiraceae + Ruminococcaceae","Bifidobacteriaceae")),")"),sep = ""))

ggplot(data=songbird_butyrate, aes(x=Butyrate_category2, y= LachnoRumino_Bifido, fill = Butyrate_category2)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 2) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = lab2, x = "") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 6.5, yend = 6.5) +
  annotate("text", x = 1.5, y = 6.75, label ="**", size = 3) +
  scale_fill_manual(values = c("red4", "red2"), labels=c("> median", "< median")) +
  expand_limits(x = 0.5) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(fill=guide_legend("Butyrate"))
ggsave("songbird_butyrate.tiff", width = 2.5, height = 2.5, units = "in", dpi=600)

blab <- expression(paste("Log ", bgroup("(",italic(frac("Bifidobacteriaceae","Lachnospiraceae + Ruminococcaceae")),")"),sep = ""))

#Propanediol
#data: songbird_propanediol
#Fig 5E
ggplot(data=propanediol_map, aes(x=Propanediol_category2, y= Bifido_LachnoRumino, fill = Propanediol_category2)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 2) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = blab, x = "") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 8.5, yend = 8.5) +
  annotate("text", x = 1.5, y = 8.75, label ="*", size = 3) +
  scale_fill_manual(values = c("red4", "red2"), labels=c("> median", "< median")) +
  expand_limits(x = 0.5) +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  guides(fill=guide_legend("1,2-PD"))

ggsave("songbird_propanediol.tiff", width = 2.5, height = 2.5, units = "in", dpi=600)

#High/Low Bifido Categories
#data: songbird_respond_final
#Fig 3D
View(songbird_respond_final)
library(ggplot2)

blab <- expression(paste("Log ", bgroup("(",italic(frac("Bifidobacteriaceae","Lachnospiraceae + Ruminococcaceae")),")"),sep = ""))
labs <- c("HE", "ME")
  ggplot(data=songbird_respond_final, aes(x=Responder2, y= Bifido_LachnoRumino, fill = Responder2)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 3) +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.005) +
  labs(y = blab, x = "Persistence") +
  #scale_color_manual(values=c("#457A84", "#DCAE1D", "#0A3F63", "#6B6C6C"))
  annotate("segment", x = 1, xend = 2, y = 9, yend = 9) +
  annotate("text", x = 1.5, y = 9.25, label ="***", size = 4) +
  scale_fill_manual(values = c("seagreen", "seagreen1"), labels=c("High", "Low")) +
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
  ) +
  guides(fill=guide_legend(""))

ggsave("3D.tiff", width = 3, height =3, units = "in", dpi=1000)
