#Corey Williams, University of Virginia
#29 Jul, 2020
#Take a subset of clusters for subclustering

print("Start ExtractClusters.R")

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(data.table)

## Input parameters ===============================================================================
CLUSTERS_KEEP <- c() #comma separated cluster ID's

INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
EXTRACT.OUT.FILENAME <- "Concat_Transformed_Extracted_.csv"
LAYOUT.OUT.FILENAME <- "Layout_Extracted_.csv"
CLUSTERS.OUT.FILENAME <- "Clusters_Extracted_.csv"

print("Finished reading input parameters, reading files")

## Read needed files ==============================================================================
concat.transformed <- fread(paste0(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME), stringsAsFactors = F)
colnames(concat.transformed) <- gsub("-", "_", colnames(concat.transformed))
colnames(concat.transformed) <- gsub(".", "_", colnames(concat.transformed), fixed = TRUE)
concat.transformed <- as.data.frame(concat.transformed)
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

print("Finished reading needed files, extracting cells based on cluster id")

## Subset cells based on cluster id ===============================================================
cells_keep <- which(clusters.in %in% CLUSTERS_KEEP)
expression_out <- concat.transformed[cells_keep,]
layout_out <- layout.in[cells_keep,]
clusters_out <- clusters.in[cells_keep]

print("Finished downsampling, writing output file")

## Write output file ==============================================================================
fwrite(expression_out,file = EXTRACT.OUT.FILENAME,row.names = FALSE)
fwrite(layout_out,file = LAYOUT.OUT.FILENAME)
fwrite(list(clusters_out),file = CLUSTERS.OUT.FILENAME,col.names = FALSE,sep = ",")

print("Completed ExtractClusters.R")
