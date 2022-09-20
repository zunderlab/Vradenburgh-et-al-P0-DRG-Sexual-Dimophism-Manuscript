##Shayla A Vradenburgh
##5 July 2022
##Script to add column to concat transformed with which sex each file is and what stage the cells belong to

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

##Load Libraries
library(ZunderPipelineFunctions)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(car)

##This script would be run after extracting cells for the root, tip, or other groups
##Input Parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed_Root.csv"
CONCAT_FILENAME <- "01_Concat_Transformed_Root.csv"
STAGE <- 1 #Chnage this number to be 1 for root, 2 for other, and 3 for tips
print("input parameters loaded, reading needed files")

## Read Needed Files 
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME) 
print("needed files read, prepping data to plot")

## Make dataframe with additional column for sex
concat.df <- as.data.frame(concat.transformed)
concat.file <- as.data.frame(concat.transformed$File)
colnames(concat.file) <- "file"
metadata.transform <- as.data.frame(recode(concat.file$file, "c('1','3','5','7')='Female'; c('2','4','6','8')='Male'"))
colnames(metadata.transform) <- "Sex"

##Make dataframe with additional column for stage
count <- nrow(concat.file)
stage <- as.data.frame(rep((STAGE), times=count))
colnames(stage) <- "Stage"

##Combine dataframes
combined_concat <- as.data.frame(cbind(concat.df,metadata.transform,stage))
print("data ready to save, saving")

##Save Concat Transform to input folder
write.csv(combined_concat, CONCAT_FILENAME, row.names = FALSE)
