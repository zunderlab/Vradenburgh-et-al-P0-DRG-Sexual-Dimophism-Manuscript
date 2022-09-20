#Corey Williams, University of Virginia
#17 May, 2019
#Subsample and make csv file containing all points asinh transformed

print("Start subsampleTransform.R")

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(flowCore)
library(ZunderPipelineFunctions)
library(data.table)

print("libraries loaded")

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
METADATA.FILENAME <- "/metadata.csv"
PANEL.FILENAME <- "/panel.csv"
SUBSAMPLE.ID.FILENAME <- "Subsample_Ids.csv" #unused if no subsampling
CONCATTRANSFORMED.FILENAME <- "Concat_Transformed.csv"
ASINH.FACTOR <- NULL #set to NULL if set in panel
SUBSAMPLES <- NULL #change this value if you want subsampling. Number of cells per file

print("got input parameters, loading fcs files")

## Read metadata ==================================================================================
md <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)

## Read fcs files =================================================================================
fcs.in <- read.flowSet(md$file_name, path=INPUT.FOLDER, transformation = FALSE,
                       truncate_max_range = FALSE)

print("loaded fcs files")

## Read panel =====================================================================================
panel <- read.panel(INPUT.FOLDER, PANEL.FILENAME)

## Pull out markers for transformation ============================================================
#Transform all markers marked for clustering or plotting
transform.markers <- get.transform.markers(panel)
transform.markers.annotate <- get.transform.annotate(panel)

## Get asinh factors
if (is.null(ASINH.FACTOR) == TRUE) {
  asinh.factor <- panel$asinh.factor[panel$Metal %in% transform.markers]
} else {
  asinh.factor <- ASINH.FACTOR
}

##Subsample & make concatenated exprs arrays ======================================================
if (is.numeric(SUBSAMPLES) == TRUE){
  #Get subsample id's for each file
  subsample.ids <- matrix(0,nrow = length(fcs.in),ncol = SUBSAMPLES)
  subsample.ids <- fsApply(fcs.in,function(x) sample(1:nrow(x),SUBSAMPLES))
  #add file to left of subsample id's for clarity
  rownames(subsample.ids) <- md$file_name
  #Get concatenated exprs array
  concat.exprs.list <- lapply(seq_along(fcs.in),function(x) 
    cbind(exprs(fcs.in[[x]])[subsample.ids[x,],transform.markers],rep(x,SUBSAMPLES)))
  concat.exprs <- do.call(rbind,concat.exprs.list)
  #give column names
  colnames(concat.exprs) <- c(transform.markers.annotate,"File")
  #perform asinh transform
  concat.exprs[,transform.markers.annotate] <- asinh(t(sapply(
    seq_along(1:nrow(concat.exprs)), 
    function(x){concat.exprs[x,transform.markers.annotate]/asinh.factor})))
  #save concatenated exprs array
  fwrite(subsample.ids,file = SUBSAMPLE.ID.FILENAME,col.names = FALSE,sep = ",")
  fwrite(concat.exprs,file = CONCATTRANSFORMED.FILENAME,row.names = FALSE)
  
  print("saved subsample ID file & concatenated and transformed file")
} else {
  #Get concatenated exprs array
  concat.exprs.list <- lapply(seq_along(fcs.in),function(x) 
    cbind(exprs(fcs.in[[x]])[,transform.markers],rep(x,nrow(fcs.in[[x]]))))
  concat.exprs <- do.call(rbind,concat.exprs.list)
  #give column names
  colnames(concat.exprs) <- c(transform.markers.annotate,"File")
  #perform asinh transform
  concat.exprs[,transform.markers.annotate] <- asinh(t(sapply(
    seq_along(1:nrow(concat.exprs)), 
    function(x){concat.exprs[x,transform.markers.annotate]/asinh.factor})))
  #save concatenated exprs array
  fwrite(concat.exprs,file = CONCATTRANSFORMED.FILENAME,row.names = FALSE)
  
  print("saved concatenated and transformed file (no subsampling)")
}
