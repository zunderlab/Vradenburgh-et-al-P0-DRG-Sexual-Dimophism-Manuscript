rm(list = ls())
.libPaths( c( .libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.6") )

library(ZunderPipelineFunctions)
library(data.table)
library(dplyr)
library(cluster)
library(ggfortify)

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CLUSTERS.FILENAME <- "/clusters_P0_11_F.csv"
CELLS.PER.CLUSTER.OUT <- "CellsPerClusters_P0_11_F.csv"
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

print(table(clusters.in))

clusters.out <- table(clusters.in)
clusters.out <- as.matrix(clusters.out)

print(clusters.out)

fwrite(clusters.out, file = CELLS.PER.CLUSTER.OUT)

