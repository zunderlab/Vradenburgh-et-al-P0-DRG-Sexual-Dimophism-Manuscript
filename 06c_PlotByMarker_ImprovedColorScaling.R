#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)
library(data.table)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_Markers"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
PANEL.FILENAME <- "/panel.csv"
POINT.SIZE <- 0.1
PLOT.HEIGHT <- 7
PLOT.WIDTH <- 7
#vary values in line 44 scale_color_viridis_c(values=) to alter color expressions

## Read needed files
concat.transformed <- fread(paste0(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME), stringsAsFactors = F)
colnames(concat.transformed) <- gsub("-", "_", colnames(concat.transformed))
colnames(concat.transformed) <- gsub(".", "_", colnames(concat.transformed), fixed = TRUE)
concat.transformed <- as.data.frame(concat.transformed)
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)

## Prep dataframe for plotting
plotting.vars <- get.plotting.annotate(panel)
plotting.df <- as.data.frame(cbind(concat.transformed[,plotting.vars],layout.in))

## Save plots colored by each marker
#make output folder
time.now <- Sys.time()
output.dir <- paste0(OUTPUT.FOLDER,"/",substr(time.now,start=1,stop=10),"_",
                     substr(time.now,start=12,stop=13),".",substr(time.now,start=15,stop=16),".",
                     substr(time.now,start=18,stop=19),OUTPUT.FOLDER.NAME)
dir.create(output.dir)
setwd(output.dir)
#loop through variables to plot
for (var.to.plot in plotting.vars){
  ggsave(paste0(var.to.plot,".png"),plot = ggplot(plotting.df,aes_string(x="umap_x",y="umap_y",
                                                                         color=var.to.plot)) + 
           geom_point(size = POINT.SIZE) + scale_color_viridis_c(values = c(0,0.1,0.3,1)) + 
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")),
         height = PLOT.HEIGHT, width = PLOT.WIDTH)
}