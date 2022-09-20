##Modified from scipt bu Amy Van Deusen
##Shayla Vradenburgh, UVA
##18 July 2022
##Extract out pseudotime, segment, and sex from URD_Object and save as a csv


rm(list = ls())
.libPaths( c( .libPaths(), "~/R/3.6.3_URD_Debug") )

library(ZunderPipelineFunctions)
library(URD)
library(umap)
library(dplyr)
library(car)

print("Libraries loaded")


##Output Parameters
OUTPUT_BASENAME <- "URD_Tree_ColoredBy_"
OUTPUT.DEVICE <- "png"

##Input Parameters
INPUT.FOLDER <- getwd()
URD_IN <- "/URD.RData"
load(paste0(INPUT.FOLDER,URD_IN))

##Add a new column to the concat transformed where file is recoded to the appropriate sex
concat.file <- as.data.frame(concat.transformed$File)
colnames(concat.file) <- "File"
metadata.transform <- as.data.frame(recode(concat.file$File, "c('1','3','5','7')='Female'; c('2','4','6','8')='Male'"))
colnames(metadata.transform) <- "Sex"
concat.transformed_sex <- cbind(concat.transformed, metadata.transform)

##Extract sex from each cell in the concat transformed
sex <- as.character(concat.transformed_sex$Sex)

##Append information about sex for each cell to the URD_Object.Tree
URD_meta_sex <- as.list(append(URD_Object.tree@meta, as.data.frame(sex)))
URD_Object.tree@meta <- as.data.frame(URD_meta_sex)

##Output dendrogram colored by sex
p.sex <- plotTree(URD_Object.tree, label.type="meta", "sex", title="Sex")
cells.included <- rownames(p.sex$layers[[1]]$data)
cells.inc.IDs <- substring(cells.included, 2)
cell.IDs <- as.vector(as.numeric(cells.inc.IDs))
sex.all <- URD_Object.tree@meta$sex
sex.included <- sex.all[cell.IDs]
p.sex.final <- p.sex
p.sex.final$layers[[1]]$data$expression <- sex.included
sex.colors <- c("darkorange1", "blue3")
p.sex.final <- p.sex.final + scale_color_manual(values = sex.colors)
p.sex.filename <- paste0(OUTPUT_BASENAME, "Sex", ".", OUTPUT.DEVICE)
ggsave(p.sex.filename, plot = p.sex.final, device = OUTPUT.DEVICE)


##Output dendrogram colored orange for female
p.sex <- plotTree(URD_Object.tree, label.type="meta", "sex", title="Sex")
cells.included <- rownames(p.sex$layers[[1]]$data)
cells.inc.IDs <- substring(cells.included, 2)
cell.IDs <- as.vector(as.numeric(cells.inc.IDs))
sex.all <- URD_Object.tree@meta$sex
sex.included <- sex.all[cell.IDs]
p.sex.final <- p.sex
p.sex.final$layers[[1]]$data$expression <- sex.included
female_subset <- filter(p.sex.final$layers[[1]]$data, expression=="Female")
p.sex.final$layers[[1]]$data <- female_subset
sex.colors <- "darkorange1"
p.sex.final <- p.sex.final + scale_color_manual(values = sex.colors)
p.sex.filename <- paste0(OUTPUT_BASENAME, "Sex_Female", ".", OUTPUT.DEVICE)
ggsave(p.sex.filename, plot = p.sex.final, device = OUTPUT.DEVICE)

##Output dendrogram colored blue for male
p.sex <- plotTree(URD_Object.tree, label.type="meta", "sex", title="Sex")
cells.included <- rownames(p.sex$layers[[1]]$data)
cells.inc.IDs <- substring(cells.included, 2)
cell.IDs <- as.vector(as.numeric(cells.inc.IDs))
sex.all <- URD_Object.tree@meta$sex
sex.included <- sex.all[cell.IDs]
p.sex.final <- p.sex
p.sex.final$layers[[1]]$data$expression <- sex.included
male_subset <- filter(p.sex.final$layers[[1]]$data, expression=="Male")
p.sex.final$layers[[1]]$data <- male_subset
sex.colors <- "blue3"
p.sex.final <- p.sex.final + scale_color_manual(values = sex.colors)
p.sex.filename <- paste0(OUTPUT_BASENAME, "Sex_Male", ".", OUTPUT.DEVICE)
ggsave(p.sex.filename, plot = p.sex.final, device = OUTPUT.DEVICE)

