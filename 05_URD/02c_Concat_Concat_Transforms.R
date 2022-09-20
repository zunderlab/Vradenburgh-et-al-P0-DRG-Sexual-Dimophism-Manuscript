
#Austin Keeler & Ashley Hirt, University of Virginia
#August 5th, 2020
#Concat multiple .csv files from downsampling and prep for URD

rm(list = ls())

library(dplyr)
library(data.table)

print("Start concatenation of concat_transform files script")

##Put all concat_transformed csv files from Concat_Update_URD.R into a separate folder and combine into one csv
#Inputs
INPUT_FOLDER <- getwd()
CONCATTRANSFORMED_FILENAME <- "Concat_Transformed_URD.csv"

#Input each concat_transform in order by age
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

print("inputs finished")

#Combine all concat_transforms
CT <- do.call("rbind", myfiles)
print("combine all files")

fwrite(CT,file = CONCATTRANSFORMED_FILENAME,row.names = FALSE)

print("new concat_transformed.csv created, script done")
