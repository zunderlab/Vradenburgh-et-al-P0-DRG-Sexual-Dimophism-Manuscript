##Shayla Vradenburgh, UVA
##2 August 2022
##Count the number of male vs female cells above and below the range for each segment

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
PERCENTAGES_FILENAME <- "Psuedotime_Percentages_Above_Below_Center.csv"
CENTER_FILENAME <- "Center_By_Segment.csv"


##Read in URD_df file
read_URD_data <- read.csv(URD_DF)
URD_df <- subset(read_URD_data, select = -X)
URD_df<-as.data.frame(URD_df)

##Extract segment info from URD_df csv
segments_extract <- unique(URD_df$segment)
segments <- na.omit(segments_extract)

##Function to calculate median pseudotime value for each segment
range_function <- function(df){
  data <- df %>%
    summarise(Range=(max(pseudotime)-min(pseudotime))/2) %>%
  return(data)
}

##Create empty list to store data
df_by_segment = {}

##Loop through data to extract out data by segment and extract out files where PT=0
for (i in 1:length(segments)){
  seg_num <- i 
  subset_data <- as.data.frame(filter(URD_df, segment == seg_num))
  cleaned_df <- filter(subset_data, pseudotime !=0)
  df_by_segment <- append(df_by_segment, list(cleaned_df))
}

##Create empty dataframe to store data
center = data.frame()

##Loop through different segments to calculate center
range_centers <- for(i in 1:length(segments)){
  segment_num <- i
  sub_data <- as.data.frame(df_by_segment[segment_num])
  range_data <- range_function(sub_data)
  min_val <- min(sub_data$pseudotime)
  center_data <- range_data+min_val
  vals_df <- cbind(center_data, segment_num)
  colnames(vals_df)<-c("center", "segment")
  center = rbind(center, vals_df)
}

##Create empty list to store data
f_df = data.frame()
m_df = data.frame()

##Loop through data to extract out data by segment and extract out males versus females
for (i in 1:length(segments)){
  seg_num <- i 
  sub_data <- as.data.frame(df_by_segment[seg_num])
  f_data <- filter(sub_data, sex == "Female")
  m_data <- filter(sub_data, sex == "Male")
  f_df <- rbind(f_df, f_data)
  m_df <- rbind(m_df, m_data)
}

##Calculate the total number of male and female cells
f_total <- sum(nrow(f_df))
m_total <- sum(nrow(m_df))

##Create empty dataset to store outputs
f_cells_by_segment = data.frame()
m_cells_by_segment = data.frame()

##Loop through each segment, to determine total number of M vs F cells per segment
for(i in 1:length(segments)){
  segment_num <- i
  sub_f <- filter(f_df, segment == segment_num)
  f_total <- sum(nrow(sub_f))
  f_data <- cbind(segment_num, f_total)
  colnames(f_data) <- c("segment","count")
  sub_m <- filter(m_df, segment == segment_num)
  m_total <- sum(nrow(sub_m))
  m_data <- cbind(segment_num, m_total)
  colnames(m_data) <- c("segment","count")
  f_cells_by_segment <- rbind(f_cells_by_segment, f_data)
  m_cells_by_segment <- rbind(m_cells_by_segment, m_data)
}

##Create empty dataframes to store outputs
percentages = data.frame()


##Loop through each segment to output percentage of male/female cells above or equal to/below range and save each dataframe
for(i in 1:length(segments)){
  segment_num <- i
  sub_data <- as.data.frame(df_by_segment[segment_num])
  centers <- center[i,]$center
  f_cell_num <- f_cells_by_segment[i,]$count
  m_cell_num <- m_cells_by_segment[i,]$count
  filtered_df_above <- filter(sub_data, pseudotime >= centers)
  filtered_df_below <- filter(sub_data, pseudotime < centers)
  f_above <- filter(filtered_df_above, sex == "Female")
  m_above<- filter(filtered_df_above, sex == "Male")
  f_below <- filter(filtered_df_below, sex == "Female")
  m_below <- filter(filtered_df_below, sex == "Male")
  write.csv(f_above, paste0(i, "segment_f_above_center.csv"), row.names = FALSE)
  write.csv(m_above, paste0(i, "segment_m_above_center.csv"), row.names = FALSE)
  write.csv(f_below, paste0(i, "segment_f_below_center.csv"), row.names = FALSE)
  write.csv(m_below, paste0(i, "segment_m_below_center.csv"), row.names = FALSE)
  #female_above_by_segment = append(female_above_by_segment, list(f_above))
  #male_above_by_segement = append(male_above_by_segment, list(m_above))
  #female_below_by_segment = append(female_below_by_segment, list(f_below))
  #male_below_by_segement = append(male_below_by_segment, list(m_below))
  #female_same_by_segment = append(female_same_by_segment, list(f_same))
  #male_same_by_segement = append(male_same_by_segment, list(m_same))
  f_above_percent <- nrow(f_above)/f_cell_num
  m_above_percent <- nrow(m_above)/m_cell_num
  f_below_percent <- nrow(f_below)/f_cell_num
  m_below_percent <- nrow(m_below)/m_cell_num
  data <- cbind(segment_num, f_above_percent, m_above_percent, f_below_percent, m_below_percent)
  percentages <- rbind(percentages, data)
}


##Save percentages as csv
write.csv(percentages, PERCENTAGES_FILENAME)
write.csv(center, CENTER_FILENAME)





