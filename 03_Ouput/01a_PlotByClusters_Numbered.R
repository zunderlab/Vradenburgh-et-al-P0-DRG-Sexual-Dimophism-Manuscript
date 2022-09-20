#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

print("Start PlotByCluster.R")

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)

print("libraries loaded")

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
LAYOUT.FILENAME <- "/UMAP_layout.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
CLUSTER.PLOT.FILENAME <- "Clusters_Numbered.png"
POINT.SIZE <- 0.1
ANNOTATE.CLUSTERS <- TRUE #(TRUE if you want to print cluster numbers on output plot)

print("input parameters loaded, reading needed files")

## Read needed files ==============================================================================
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

print("needed files read, prepping data to plot")

## Prep dataframe for plotting ====================================================================
plotting.df <- as.data.frame(cbind(layout.in,clusters.in))
colnames(plotting.df) <- c("umap_x","umap_y","cluster")

print("data ready to plot, plotting")

## Save plots colored by each marker ==============================================================
#set output folder
setwd(OUTPUT.FOLDER)
#loop through variables to plot
if (ANNOTATE.CLUSTERS) {
  ggsave(CLUSTER.PLOT.FILENAME,plot = ggplot(plotting.df,aes(x=umap_x,y=umap_y, 
                                                             color=factor(cluster))) + 
           geom_point(size = POINT.SIZE) + theme(panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), 
                                                 panel.background = element_blank(), 
                                                 axis.line = element_line(colour = "black")) +
           annotate("text", 
                    x = plotting.df[match(unique(plotting.df[,3]),plotting.df[,3]),1], 
                    y = plotting.df[match(unique(plotting.df[,3]),plotting.df[,3]),2], 
                    size = 5,
                    label = as.character(unique(plotting.df[,3]))) +
           guides(colour = guide_legend(override.aes = list(shape=15, size=8))),
         #^^should work for changing size/shape of legend elements... might have to tweak size per preference
         height = 7,width = 7)
} else {
  ggsave(CLUSTER.PLOT.FILENAME,plot = ggplot(plotting.df,aes(x=umap_x,y=umap_y, 
                                                             color=factor(cluster))) + 
           geom_point(size = POINT.SIZE) + theme(panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), 
                                                 panel.background = element_blank(), 
                                                 axis.line = element_line(colour = "black")) +
           guides(colour = guide_legend(override.aes = list(shape=15, size=8))),
         #^^should work for changing size/shape of legend elements... might have to tweak size per preference
         height = 7,width = 7)
}

print("data plotted and file outputted")
print("End PlotByCluster.R")
