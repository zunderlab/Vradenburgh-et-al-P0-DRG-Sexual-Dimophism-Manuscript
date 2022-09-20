##Shayla A Vradenburgh
##5 July 2022
##Script to subtract 1 from cluster scripts so when 1 is added for the GetURDMetadata it works

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
CLUSTERS.IN <- "/Clusters_Root.csv"
CLUSTERS.OUT <- "01_Clusters_Root.csv"
print("input parameters loaded, reading needed files")

## Read Needed Files 
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.IN) 
print("needed files read, prepping data to plot")

##Subtract 1 from each number in the dataframe
clusters <- as.data.frame(clusters.in - 2)
colnames(clusters) <- "clusters"

##Save file
write.csv(clusters, CLUSTERS.OUT, col.names = FALSE, row.names = FALSE)






