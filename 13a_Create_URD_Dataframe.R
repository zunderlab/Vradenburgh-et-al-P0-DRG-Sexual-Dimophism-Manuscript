##Shayla Vradenburgh, UVA
##22 August 2022
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
FULL_URD_DATA_FILENAME <- "URD_Data.csv"
PT_0_DATA_FILENAME <- "URD_Data_Percent_PT_0.csv"
URD_DATA_PT_0_FILENAME <- "URD_Data_PT_0.csv"

##Input Parameters
INPUT.FOLDER <- getwd()
URD_IN <- "/URD.RData"
load(paste0(INPUT.FOLDER,URD_IN))

##Add a new column to the concat transformed where file is recoded to the appropriate sex
concat.file <- as.data.frame(concat.transformed$File)
colnames(concat.file) <- "File"
metadata.transform <- as.data.frame(recode(concat.file$File, "c('1','3','5','7')='Female'; c('2','4','6','8')='Male'"))
colnames(metadata.transform) <- "Sex"
concat.file.copy <- as.data.frame(concat.transformed$File)
colnames(concat.file) <- "File"
concat.transformed_sex <- cbind(concat.transformed, metadata.transform)

##Extract sex and file from each cell in the concat transformed
sex <- as.character(concat.transformed_sex$Sex)
file <- as.numeric(concat.transformed_sex$File)

##Append information about sex and file for each cell to the URD_Object.Tree
URD_meta_sex <- as.list(append(URD_Object.tree@meta, as.data.frame(sex)))
URD_Object.tree@meta <- as.data.frame(URD_meta_sex)
URD_meta_file <- as.list(append(URD_Object.tree@meta, as.data.frame(file)))
URD_Object.tree@meta <- as.data.frame(URD_meta_file)

##Extract out psuedotime, segment, and sex and concatenate into one csv
pseudotime <- as.data.frame(URD_Object.tree@pseudotime$pseudotime)
segment <- as.data.frame(URD_Object.tree@group.ids$segment)
sex <- as.data.frame(URD_Object.tree@meta$sex)
file <- as.data.frame(URD_Object.tree@meta$file)
URD_df_merge <- cbind(pseudotime, segment, sex, file)
colnames(URD_df_merge) <- c("pseudotime", "segment", "sex", "file")
URD_df <- na.omit(URD_df_merge)

##Save URD_df as a csv
write.csv(URD_df, FULL_URD_DATA_FILENAME)

##Filter URD_df to make df only containing rows where PT=0
PT0_URD_df <- filter(URD_df, pseudotime == 0)

##Save PT0_URD_df as a csv
write.csv(PT0_URD_df, URD_DATA_PT_0_FILENAME)

##For loop to determine the percent of male and female cells where PT=0 for each file
file_num <- unique(URD_df$file)
#Empty dataframe to store outputs
percent_PT0 = data.frame()
for(i in 1:length(file_num)){
  file_number <- i
  sub_full_URD <- filter(URD_df, file == file_number)
  sub_data <- filter(PT0_URD_df, file == file_number)
  count_full_URD <- nrow(sub_full_URD)
  count_sub_data <- nrow(sub_data)
  percent <- count_sub_data/count_full_URD*100
  sex <- unique(sub_full_URD$sex)
  data <- cbind(file_number, sex, percent)
  percent_PT0 <- rbind(percent_PT0, data)
}

##Save percent_PT0 as a csv
write.csv(percent_PT0, PT_0_DATA_FILENAME)




