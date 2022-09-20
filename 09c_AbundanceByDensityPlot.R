###Shayla Vradenburgh, University of Virginia
###11 September 2022
###Create scatter plot of abundance by density. 
###This script can only be run after the Abundance UMAP script and Density UMAP script

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

##Load Libraries
library(ZunderPipelineFunctions)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(car)
library(scales)

##Input Parameters--------------------------------------------------------------
#Future Goal: Add if else statement for fixed range
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
ABUNDANCE.DF.FILENAME <- "MvsF_Ratios_Dataframe_75_10.csv"
DENSITY.DF.FILENAME <- "MvsF_Density_Dataframe_75.csv"
PLOT.FILENAME <- "Abundance_by_Density_Scatterplot.png"

## Read Needed Files 
abundance.in <- read.csv(ABUNDANCE.DF.FILENAME)
abundance.in <- subset(abundance.in, select = -1)
density.in <- read.csv(DENSITY.DF.FILENAME)
density.df <- subset(density.in, select = -1)

## Prep Dataframe
abundance.df <- subset(abundance.in, select = c("x", "y", "normalized_ratio"))
#Order x-coordinates so they are in the correct order for both abudnance.df and density.df
abundance.order <- abundance.df[order(abundance.df$x, abundance.df$y),]
density.order <- density.df[order(density.df$x, density.df$y),]
#Create new dataframe combining the correctly ordered ratios and density values
plotting.df <- as.data.frame(cbind(abundance.order$normalized_ratio, density.order$ndensity))
colnames(plotting.df) <- c("abundance_ratio", "normalized_density")

#Plot and save abundance by density scatterplot
plot <- ggplot(plotting.df, aes(x=normalized_density, y=abundance_ratio)) + geom_count() +
  theme_bw()
ggsave(PLOT.FILENAME, plot, height = 7, width = 7)
