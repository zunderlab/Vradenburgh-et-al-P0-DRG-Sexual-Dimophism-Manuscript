#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)
library(ggstance)
library(ggpubr)
library(forcats)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_Markers"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
PANEL.FILENAME <- "/panel.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
VIOLIN.HEIGHT.FACTOR <- 5

## Read needed files
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

## Prep dataframe for plotting
plotting.vars <- get.plotting.annotate(panel)
plotting.df <- as.data.frame(cbind(concat.transformed[,plotting.vars],clusters.in))
colnames(plotting.df)[ncol(plotting.df)] <- "cluster"

#Make list of violin plots by cluster
plist = sapply(plotting.vars, function(marker.plotting) {
  if (marker.plotting == plotting.vars[1]){
    ggplot(plotting.df, aes(x = plotting.df[,marker.plotting], y = fct_rev(factor(cluster)), fill = factor(cluster))) + geom_violinh(trim=FALSE,scale = "width") + xlab(marker.plotting) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none",axis.text.x = element_blank(),axis.title.y = element_blank(),axis.title.x = element_text(size = 8))
  }
  else {
    ggplot(plotting.df, aes(x = plotting.df[,marker.plotting], y = fct_rev(factor(cluster)), fill = factor(cluster))) + geom_violinh(trim=FALSE,scale = "width") + xlab(marker.plotting) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none",axis.text = element_blank(),axis.title.y = element_blank(),axis.title.x = element_text(size = 8))
  }
}, simplify=FALSE)
#save Violin plots
ggsave("ViolinPlots.png",annotate_figure(ggarrange(plotlist = plist,ncol=length(plist)),left = text_grob("Cluster",rot=90)),height=max(clusters.in)/VIOLIN.HEIGHT.FACTOR,width=length(plotting.vars))
