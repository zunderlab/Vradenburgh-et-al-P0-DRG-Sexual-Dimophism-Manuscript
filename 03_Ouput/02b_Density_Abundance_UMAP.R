#Shayla Vradenburgh, University of Virginia
#31 May 2022
#Overlay of density over UMAP layout

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

##Load Libraries
library(ZunderPipelineFunctions)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(car)

##Input Parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
PLOT_FILENAME <- "MvsF_Density_UMAP_75.png"
PLOT_CSV_FILENAME <- "MvsF_Density_Dataframe_75.csv"
BINS_NUM <- 75
print("input parameters loaded, reading needed files")

##Read Needed Files 
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME) 
print("needed files read, prepping data to plot")

##Prep Dataframe for Plotting 
concat.df <- as.data.frame(concat.transformed$File)
colnames(concat.df) <- "file"
metadata.transform <- recode(concat.df$file, "c('1','3','5','7')='Female'; c('2','4','6','8')='Male'")
plotting.df <- as.data.frame(cbind(layout.in,clusters.in,metadata.transform))
colnames(plotting.df) <- c("umap_x","umap_y","cluster","sex")
print("data ready to plot, plotting")

##Save Plots
#set output folder
setwd(OUTPUT.FOLDER)
#Plot and save density overlayed UMAPs
plot <- ggplot(plotting.df) + geom_bin2d(aes(x=umap_x, y=umap_y, fill=..ndensity..), bins=BINS_NUM) + 
  scale_fill_viridis_c(trans="log2") +
  theme(panel.background = element_blank())
ggsave(PLOT_FILENAME, plot, height = 7, width = 7)

##Extract and assign dataframe used to make 2d histogram plot as a new variable
hist_conversion <- ggplot_build(plot)
plotted_data <- as.data.frame(hist_conversion$data)
orig_plotted_df <- plotted_data %>%
  select(x, y, ndensity)
#Save orig_plotted_df as csv
write.csv(orig_plotted_df, PLOT_CSV_FILENAME)

