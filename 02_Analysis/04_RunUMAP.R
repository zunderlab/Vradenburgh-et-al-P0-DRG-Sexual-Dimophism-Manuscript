#Corey Williams, University of Virginia
#02 Apr, 2019
#Run UMAP dimensionality reduction

print("Start RunUMAP.R")

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(umap)
library(ZunderPipelineFunctions)
library(data.table)

print("libraries loaded")

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
PANEL.FILENAME <- "/panel.csv"
UMAP.LAYOUT.FILENAME <- "UMAP_layout.csv" 
UMAP.INDEXES.FILENAME <- "UMAP_knn_indexes.csv"
UMAP.DISTANCES.FILENAME <- "UMAP_knn_distances.csv"
UMAP_N_NEIGHBORS <- 15
UMAP_MIN_DIST <- 0.000001 #UMAP default is 0.1

#Advanced settings
UMAP_N_COMPONENTS <- 2
UMAP_METRIC <- "euclidean"
UMAP_N_EPOCHS <- 1000
UMAP_INPUT <- "data"
UMAP_INIT <- "spectral"
UMAP_SET_OP_MIX_RATIO <- 1
UMAP_LOCAL_CONNECTIVITY <- 1
UMAP_BANDWIDTH <- 1
UMAP_ALPHA <- 1
UMAP_GAMMA <- 1
UMAP_NEGATIVE_SAMPLE_RATE <- 5
UMAP_A <- NA
UMAP_B <- NA
UMAP_SPREAD <- 1
UMAP_RANDOM_STATE <- 1
UMAP_TRANSFORM_STATE <- NA
UMAP_KNN_REPEATS <- 1
UMAP_VERBOSE <- FALSE
UMAP_LEARN_ARGS <- NA

print("got input parameters, loading data files")

##Read Concat_Transformed =========================================================================
concat.exprs <- fread(paste0(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME), stringsAsFactors = F)
  colnames(concat.exprs) <- gsub("-", "_", colnames(concat.exprs))
  colnames(concat.exprs) <- gsub(".", "_", colnames(concat.exprs), fixed = TRUE)
concat.exprs <- as.data.frame(concat.exprs)

##Pull out clustering variables ===================================================================
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)
clustering.vars <- get.clustering.annotate(panel)
umap.in <- as.matrix(concat.exprs[,clustering.vars])

print("files loaded, running UMAP")

##Run UMAP ========================================================================================
#Set up UMAP settings
umap.settings <- umap.defaults
umap.settings$n_neighbors <- UMAP_N_NEIGHBORS
umap.settings$n_components <- UMAP_N_COMPONENTS
umap.settings$metric <- UMAP_METRIC
umap.settings$n_epochs <- UMAP_N_EPOCHS
umap.settings$input <- UMAP_INPUT
umap.settings$init <- UMAP_INIT
umap.settings$min_dist <- UMAP_MIN_DIST
umap.settings$set_op_mix_ratio <- UMAP_SET_OP_MIX_RATIO
umap.settings$local_connectivity <- UMAP_LOCAL_CONNECTIVITY
umap.settings$bandwidth <- UMAP_BANDWIDTH
umap.settings$alpha <- UMAP_ALPHA
umap.settings$gamma <- UMAP_GAMMA
umap.settings$negative_sample_rate <- UMAP_NEGATIVE_SAMPLE_RATE
umap.settings$a <- UMAP_A
umap.settings$b <- UMAP_B
umap.settings$spread <- UMAP_SPREAD
umap.settings$random_state <- UMAP_RANDOM_STATE
umap.settings$transform_state <- UMAP_TRANSFORM_STATE
umap.settings$knn_repeats <- UMAP_KNN_REPEATS
umap.settings$verbose <- UMAP_VERBOSE
umap.settings$umap_learn_args <- UMAP_LEARN_ARGS
#Run UMAP
umap.out <- umap(umap.in,config = umap.settings)

print("ran UMAP, outputting files")

## Save UMAP layout and knn graph =================================================================
fwrite(umap.out$knn$indexes,file = UMAP.INDEXES.FILENAME,row.names = FALSE,col.names = FALSE,
       sep = ",")
fwrite(umap.out$knn$distances,file = UMAP.DISTANCES.FILENAME,row.names = FALSE,col.names = FALSE,
       sep = ",")
colnames(umap.out$layout) <- c("umap_x","umap_y")
fwrite(umap.out$layout,file = UMAP.LAYOUT.FILENAME,row.names = FALSE,col.names = TRUE, sep = ",")

print("UMAP files outputted")
print("Finish RunUMAP.R")
