##Shayla Vradenburgh, UVA
##18 July 2022
##Cycle through each segment and save pseudotime information for each sex and normalize

rm(list = ls())
.libPaths( c( .libPaths(), "~/R/3.6.3_URD_Debug") )

library(ZunderPipelineFunctions)
library(URD)
library(umap)
library(dplyr)

print("Libraries loaded")

##Input Parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
URD_DF <- "URD_Data.csv"
RATIOS_FILENAME <- "Normalized_Means_Medians_Ratios.csv"

##Read in URD_df file
read_URD_data <- read.csv(URD_DF)
URD_df <- subset(read_URD_data, select = -X)
URD_df<-as.data.frame(URD_df)

##Extract segment info from URD_df csv
segments <- unique(URD_df$segment)


##Function to calculate median pseudotime value for each segment
median_function <- function(df){
  data <- df %>%
    group_by(sex) %>%
    summarise(Median=median(pseudotime))
  return(data)
}

##Function to calculate mean pseudotime value for each segment
mean_function <- function(df){
  data <- df %>%
    group_by(sex) %>%
    summarise(Mean=mean(pseudotime))
  return(data)
}

##Function to normalize values on 0-1 scale
normalize <- function(x){(x-min(x))/(max(x)-min(x))}


##Create empty list to store data
df_by_segment = {}

##Loop through data to extract out data by segment
for (i in 1:length(segments)){
  seg_num <- i 
  subset_data <- as.data.frame(filter(URD_df, segment == seg_num))
  df_by_segment <- append(df_by_segment, list(subset_data))
}

##Create empty dataframe to store output
normalized_pt = {}

##For loop to extract out PT=0 and then normalize data from 0-1
for(j in 1:length(segments)){
  seg_num <- j
  data <- as.data.frame(df_by_segment[seg_num])
  subset_data <- filter(data, pseudotime != 0)
  normed <- normalize(subset_data$pseudotime)
  normed_data <- cbind(subset_data, normed)
  normalized_pt <- append(normalized_pt, list(normed_data))
}

##Create empty dataframe to store data
normed_means_median = data.frame()

##Loop through different segments to calculate medians
means_medians <- for(i in 1:length(segments)){
  segment_num <- i
  sub_data <- as.data.frame(normalized_pt[segment_num])
  median_data <- median_function(sub_data)
  mean_data <- mean_function(sub_data)
  vals_df <- cbind(median_data, mean_data$Mean, segment_num)
  colnames(vals_df)<-c("sex", "median", "mean", "segment")
  normed_means_median = rbind(normed_means_median, vals_df)
}

##Create empty dataset to store female/male ratios
pt_ratios = data.frame()

##Make normed means and medians into a ratio of female/male for each segment
ratios <- for (j in 1:length(segments)){
  segment_num <- j
  sub_data <- filter(normed_means_median, segment == segment_num)
  female_data <- filter(sub_data, sex == "Female")
  male_data <- filter(sub_data, sex == "Male")
  median_female_pt <- female_data$median
  median_male_pt <- male_data$median
  mean_female_pt <- female_data$mean
  mean_male_pt <- male_data$mean
  median_ratio <- median_female_pt/median_male_pt
  mean_ratio <- mean_female_pt/mean_male_pt
  merged_df <- cbind(segment_num, median_ratio, mean_ratio)
  pt_ratios = rbind(pt_ratios, merged_df)
}


##Save normalized ratios as a csv
write.csv(pt_ratios, RATIOS_FILENAME)





