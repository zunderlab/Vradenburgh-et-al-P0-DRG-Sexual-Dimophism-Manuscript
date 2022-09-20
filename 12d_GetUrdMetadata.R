#Corey Williams, University of Virginia
#16 Feb, 2020
#Make metadata csv for URD
#Sets up metadata to have columns for file, genotype, and stage of each cell

print("Start GetUrdMetadata.R")

rm(list = ls())
.libPaths( c( .libPaths(), "/project/zunderlab/R/3.6") )

library(ZunderPipelineFunctions)
library(data.table)
library(dplyr)

print("libraries loaded")

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
STAGE.TAG <- "stage" #column name in metadata for developmental stage
METADATA.FILENAME <- "/metadata_edited_URD.csv" #Must include "stage" column for URD
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed_URD.csv" #.csv file for pipeline clustering
URD.META.FILENAME <- "metadata_URD.csv" #.txt file For URD - unused if no subsampling

print("got input parameters, loading input files")

## Read input files ===============================================================================
md <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)

print("finished reading input files, getting metadata")

## Get standard metadata ==========================================================================
#initialize array
urd.md <- matrix(0,nrow = nrow(concat.transformed),ncol = 3)
#get metadata for each cell
urd.md <- md[concat.transformed[,"Stage"],c("file_name",STAGE.TAG)]

## Get Root identities ============================================================================
if ("Root_clusters" %in% colnames(md)){
  
  #Get file ID(s) (as corresponds to metadata and concat transformed) that have roots
  root_files <- which(md$Root_clusters != "")
  
  #Get root cluster filenames if name independently. 
  #Default is name of fcs file (sans .fcs) + "_clusters.csv", so cells.fcs is cells_clusters.csv
  root_filenames <- vector(mode = "character",length = length(root_files))
  for (this_file in seq_along(root_files)) {
    #Blank filenames will be NA if all left blank, will be blank string if only one left blank
    if (md$Root_cluster_filename[root_files[this_file]] == "" | 
        is.na(md$Root_cluster_filename[root_files[this_file]]) == TRUE) {
      #Generate cluster filename
      this_filename <- md$file_name[root_files[this_file]]
      root_filenames[this_file] <- paste0(substr(this_filename,start=1,
                                                 stop=nchar(this_filename)-4),
                                          "_clusters.csv")
    } else {
      #Give inputted filename
      root_filenames[this_file] <- as.character(md$Root_cluster_filename[root_files[this_file]])
    }
  }
  
  #Read in clusters from root files
  root_clusters_in <- lapply(root_filenames, function(this_file){
    read.clusters(INPUT.FOLDER,paste0("/",this_file))})
  
  #Get root cluster ID's from metadata
  root_clusters <- lapply(root_files, function(this_file){
    md$Root_clusters[this_file] %>%
      as.character %>%
      strsplit(",") %>% 
      unlist() %>% 
      as.numeric
  })
  
  #Identify cells in each fcs file corresponding to the root cluster ID's
  root_cells <- lapply(seq_along(root_clusters_in),function(this_file){
    root_clusters_in[[this_file]] %in% root_clusters[[this_file]]
  })
  
  #Make full vector of logicals to include all cells
  root_cells_out <- vector(mode = "logical",length = nrow(urd.md))
  for (this_file in seq_along(root_files)) {
    root_cells_out[which(concat.transformed$Stage==root_files[this_file])]<-root_cells[[this_file]]
  }
  
  #append root info to metadata
  urd.md <- data.frame(urd.md,root_cells_out)
  colnames(urd.md)[ncol(urd.md)] <- "roots"
}

## Get tip identities =============================================================================
if ("Tip_clusters" %in% colnames(md)){
  
  #Get file ID(s) (as corresponds to metadata and concat transformed) that have tips
  tip_files <- which(md$Tip_clusters != "")
  
  #Get tip cluster filenames if name independently. 
  #Default is name of fcs file (sans .fcs) + "_clusters.csv", so cells.fcs is cells_clusters.csv
  tip_filenames <- vector(mode = "character",length = length(tip_files))
  for (this_file in seq_along(tip_files)) {
    #Blank filenames will be NA if all left blank, will be blank string if only one left blank
    if (md$Tip_cluster_filename[tip_files[this_file]] == "" | 
        is.na(md$Tip_cluster_filename[tip_files[this_file]]) == TRUE) {
      #Generate cluster filename
      this_filename <- md$file_name[tip_files[this_file]]
      tip_filenames[this_file] <- paste0(substr(this_filename,start=1,
                                                stop=nchar(this_filename)-4),
                                         "_clusters.csv")
    } else {
      #Give inputted filename
      tip_filenames[this_file] <- as.character(md$Tip_cluster_filename[tip_files[this_file]])
    }
  }
  
  #Read in clusters from tip files
  tip_clusters_in <- lapply(tip_filenames, function(this_file){
    read.clusters(INPUT.FOLDER,paste0("/",this_file))})
  
  #Get tip cluster ID's from metadata
  tip_clusters <- lapply(tip_files, function(this_file){
    md$Tip_clusters[this_file] %>%
      as.character %>%
      strsplit(",") %>% 
      unlist() %>% 
      as.numeric
  })
  
  #Identify cells in each fcs file corresponding to the tip cluster ID's
  cluster_count <- 0
  tip_cells <- list()
  for (this_file in seq_along(tip_clusters_in)){
    tip_cell_clusters <- vector(mode = "integer",length = length(tip_clusters_in[[this_file]]))
    
    #find which cells correspond to which cluster
    for (this_cluster in seq_along(tip_clusters[[this_file]])){
      tip_cell_clusters[which(tip_clusters_in[[this_file]] == tip_clusters[[this_file]][this_cluster])] <- this_cluster + cluster_count
    }
    tip_cells[[this_file]] <- tip_cell_clusters
    
    #keep track of clusters recorded so that numbering is coordinated across multiple files
    cluster_count <- cluster_count + this_cluster
  }
  
  #Make full vector of logicals to include all cells
  tip_cells_out <- vector(mode = "integer",length = nrow(urd.md))
  for (this_file in seq_along(tip_files)) {
    tip_cells_out[which(concat.transformed$Stage == tip_files[this_file])] <- tip_cells[[this_file]]
  }
  
  #append tip info to metadata
  urd.md <- data.frame(urd.md,tip_cells_out)
  colnames(urd.md)[ncol(urd.md)] <- "tips"
}

## Write urd metadata file ========================================================================
fwrite(urd.md,URD.META.FILENAME)