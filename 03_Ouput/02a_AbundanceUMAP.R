###Shayla Vradenburgh, University of Virginia
###21 August 2022
###Plot ratio heatmap over UMAP layout.
###This script has been updated so downsampling is not necessary.

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

##Load Libraries
library(ZunderPipelineFunctions)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(car)
library(scales)


##Input Parameters--------------------------------------------------------------
#Future Goal: Add if else statement for fixed range
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
PLOT_FILENAME <- "MvsF_Ratios_of_Percent_75_10.png"
PLOT_CSV_FILENAME <- "MvsF_Ratios_Dataframe_75_10.csv"
SCALE_BAR_FILENAME <- "MvsF_Ratios_of_Percent_Scale_Bar_75_10.png"
SCALE_BAR_MAX_FILENAME <- "Scale_Bar_Min_Max_75_10.csv"
BIN_NUM <- 75
PROPORTION_ABOVE_MAX <- 0.1
HEX_CODE_STEP_VALUE <- 0.05
print("input parameters loaded, reading needed files")

## Read Needed Files 
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
print("needed files read, prepping data to plot")

## Prep Dataframe for 2D Histogram
concat.df <- as.data.frame(concat.transformed$File)
colnames(concat.df) <- "file"
metadata.transform <- recode(concat.df$file, "c('1','3','5','7')='Female'; c('2','4','6','8')='Male'")
histogram.df <- as.data.frame(cbind(layout.in,clusters.in,metadata.transform))
colnames(histogram.df) <- c("umap_x","umap_y","cluster","sex")
print("data ready to plot, plotting")

## Create plot for 2D histogram colored by sex and count
hist_plot <- ggplot(histogram.df) + geom_bin2d(aes(x=umap_x, y=umap_y, alpha=..count.., fill=sex), bins=BIN_NUM) + 
  scale_fill_manual(values=c("darkorange2","dodgerblue")) +
  scale_alpha_continuous(trans="log2") +
  theme(panel.background = element_blank())

##Extract and assign dataframe used to make 2d histogram plot as a new variable
hist_conversion <- ggplot_build(hist_plot)
plotted_data <- as.data.frame(hist_conversion$data)
orig_plotted_df <- plotted_data %>%
  select(x, y, count, group)

##Calculate total number of cells for females and males
f_counts <- filter(orig_plotted_df, group == 1)
m_counts <- filter(orig_plotted_df, group == 2)
sum_f_counts <- sum(f_counts$count)
sum_m_counts <- sum(m_counts$count)

##Divide count values by total sum values for females and males
f_percents <- as.data.frame(f_counts$count/sum_f_counts)
m_percents <- as.data.frame(m_counts$count/sum_m_counts)

##Replace count with percent and combine make and female dataframes
female_data <- cbind(subset(f_counts, select = -count), f_percents)
colnames(female_data) <- c("x", "y", "group", "percent")
male_data <- cbind(subset(m_counts, select = -count), m_percents)
colnames(male_data) <- c("x", "y", "group", "percent")
mf_combined_df <- rbind(female_data, male_data)
percent_df <- as.data.frame(mf_combined_df$percent*100)
colnames(percent_df) <- "percent"
merged_df <- cbind(subset(mf_combined_df, select = -percent), percent_df)

#Determine what unique x-y coordinates exist
x_y_coords <- unique(merged_df[c("x", "y")])
num_coords <- nrow(x_y_coords)

##Function to filter data by x and y coordinates
coord_data <- function(orig_df,coord_df){
  extracted_df <- filter(orig_df, orig_df$x == coord_df$x & orig_df$y == coord_df$y)
}

##Create empty dataframe to store outputs
#Empty dataframe to store ratios for x-y coordinates with both male and female values
plotting.df.ratios <- data.frame(matrix(ncol=3, nrow=num_coords))
colnames(plotting.df.ratios) <- c("x", "y", "ratio")
#Empty dataframe to store data for x-y coordinates with only male or only female values
all_m_f_df = data.frame()

#For loop to recalculate percentages so each square adds to 100% and add ratios into a new df for plotting
for(i in 1:nrow(x_y_coords)){
  row <- x_y_coords[i,]
  new_df <- coord_data(merged_df, row)
  if(nrow(new_df) == 2) {
    female_data <- filter(new_df, group == "1")
    male_data <- filter(new_df, group == "2")
    total_percent <- female_data$percent + male_data$percent
    female_prop <- female_data$percent/total_percent*100
    male_prop <- 100-female_prop
    ratio <- female_prop/male_prop
    plotting.df.ratios[i,] <- c(row$x, row$y, ratio)
  } else if(new_df$group == "1" && new_df$group != "2") {
    all_m_f_df <- rbind(all_m_f_df, new_df)
  } else if(new_df$group == "2" && new_df$group != "1") {
    all_m_f_df <- rbind(all_m_f_df, new_df)
  }
}

##Remove all NAs from plotting.df and log2 all ratios
plotting.ratio <- na.omit(plotting.df.ratios)
plotting.log <- as.data.frame(log2(plotting.ratio$ratio))
colnames(plotting.log) <- "ratio_log"

##Function to calculate the absolute max
absmax <- function(x){
  x[which.max( abs(x) )]
}

##Determine absolute max of log values and then create a value for all male/female values of max+max_percentile
max_log <- abs(absmax(plotting.log$ratio_log))
neg_max_log <- max_log*-1
max_val <- max_log*(1+PROPORTION_ABOVE_MAX)
neg_max_val <- max_val*-1

##Determine x-y coordinates related to absolute max ratio and determine proportion that created that ratio
abs_max_x_y <- filter(plotting.ratio, ratio == absmax(plotting.ratio$ratio))
abs_x_coord <- abs_max_x_y$x[1]
abs_y_coord <- abs_max_x_y$y[1]
filtered_x <- filter(merged_df, x == abs_x_coord)
filtered_df <- filter(filtered_x, y == abs_y_coord)
female_data_filtered <- filter(filtered_df, group == "1")
male_data_filtered <- filter(filtered_df, group == "2")
total_percent <- female_data_filtered$percent + male_data_filtered$percent
female_prop <- female_data_filtered$percent/total_percent*100
male_prop <- 100-female_prop
scale_bar_data <- cbind(female_prop, male_prop, abs_max_x_y$ratio)
colnames(scale_bar_data) <- c("female_prop", "male_prop", "ratio")
write.csv(scale_bar_data, SCALE_BAR_MAX_FILENAME)

##Create empty dataframe
all_m_f_ratios = data.frame()

##For loop to set ratios to max_val and neg_max_val
for(i in 1:nrow(all_m_f_df)){
  row_df <- all_m_f_df[i,]
  if(row_df$group == "1" && row_df$group != "2") {
    ratio <- max_val
  } else if(row_df$group == "2" && row_df$group != "1") {
    ratio <- neg_max_val
  }
  x <- as.data.frame(row_df$x)
  colnames(x) <- "x"
  y <- as.data.frame(row_df$y)
  colnames(y) <- "y"
  data <- cbind(x,y,ratio)
  all_m_f_ratios <- rbind(all_m_f_ratios, data)
}

##Replace ratio in plotting.ratio with log2 ratio and then combine with all_m_f_ratios
plotting.ratio_log <- cbind(subset(plotting.ratio, select = -ratio), plotting.log)
colnames(plotting.ratio_log) <- c("x", "y","ratio")
plotting.df <- rbind(plotting.ratio_log, all_m_f_ratios)

##Divide all values by the absolute max to get values that range from -1 to 1
max_ratio <- max_val
normalized_ratio_df <- as.data.frame(plotting.df$ratio/abs(max_ratio))
colnames(normalized_ratio_df) <- "normalized_ratio"
plotting.df <- cbind(plotting.df, normalized_ratio_df)
colnames(plotting.df) <- c("x", "y", "ratio", "normalized_ratio")

##Create a list of hex code cutoff values
max_hex <- 1
min_hex <- (max_hex+HEX_CODE_STEP_VALUE)*-1
hex_cutoffs <- seq(min_hex, max_hex, by=HEX_CODE_STEP_VALUE)
pixel_values <- na.omit(as.data.frame(cut(hex_cutoffs, breaks = hex_cutoffs)))
colnames(pixel_values) <- "pixels"

##Add a pixels column to dataframe where values that fall between cutoff values are assigned a pixel number
pixels_df <- plotting.df %>% 
  mutate(pixels = cut(normalized_ratio, breaks = hex_cutoffs))
pix_length <- nrow(pixel_values)
num_seq <- as.data.frame(1:pix_length)
colnames(num_seq) <- "pixel_number"
pixel_info <- cbind(pixel_values, num_seq)

##Match elements to replace pixels with pixel number
pixel_numbers <- pixels_df %>% 
  mutate(across(everything(), ~ pixel_info$pixel_number[match(., pixel_info$pixels)]))
#Edit out the unnecessary columns
pixel_numbers_df <- as.data.frame(pixel_numbers$pixels)
colnames(pixel_numbers_df) <- "pixel_numbers"
#Combine into originial dataframe
plotting.df <- cbind(pixels_df, pixel_numbers_df)

##Assign a hex code value for every pixel number
min_pixel <- min(pixel_info$pixel_number)
mid_pixel <- median(pixel_info$pixel_number)
max_pixel <- max(pixel_info$pixel_number)
hex_fn <- scales::gradient_n_pal(colours = c("blue3", "grey90", "darkorange1"), values = c(min_pixel, mid_pixel, max_pixel))
hex_codes <- hex_fn(plotting.df$pixel_numbers)
plotting.df <- cbind(plotting.df, hex_codes)

##Make new UMAP with ratio overlay
#set output folder
setwd(OUTPUT.FOLDER)
#Plot and save ratio UMAP overlay colored by sex
plot <- ggplot(plotting.df,aes(x=x, y=y, fill=hex_codes)) + geom_tile(aes()) + 
  scale_fill_identity() +
  theme(panel.background = element_blank())
ggsave(PLOT_FILENAME, plot, height = 7, width = 7)

##Save plotting.df as a csv file
write.csv(plotting.df, PLOT_CSV_FILENAME)

##Make 1 dimensional heatmap to use as scale bar
#Create dataframe to use for plotting
pixel_numbers_sb <- pixel_info$pixel_number
hex_sb <- hex_fn(pixel_numbers_sb)
pixel_hex_df <- cbind(as.data.frame(pixel_numbers_sb), as.data.frame(hex_sb))
colnames(pixel_hex_df) <- c("pixel_numbers_scale_bar", "hex_codes_scale_bar")
faux_x_axis_val <- as.data.frame(rep(5, times=nrow(pixel_hex_df)))
colnames(faux_x_axis_val) <- "x"
scale_bar_df <- cbind(pixel_hex_df, faux_x_axis_val)
#Add extra white bars = to the proportion_above_max *10. 
#Then add one more blue and orange bar to the top and bottom of the scale bar
white_bar_n <- PROPORTION_ABOVE_MAX*10
max_blue <- filter(scale_bar_df, pixel_numbers_scale_bar == min_pixel)
white_hex_code <- "#FFFFFF"
#Make a small dataframe with info for white, max_blue, and max_orange bar info
white_bar_number <- seq(1, white_bar_n, by=1)
white_pixel_num_low <- white_bar_number + 1
faux_x <- max_blue$x
#For loop to make a dataframe for each white bar
white_low_df = data.frame()
for(i in 1:length(white_pixel_num_low)){
  pixel_number <- white_pixel_num_low[i]
  data <- cbind(as.data.frame(pixel_number),as.data.frame(white_hex_code), as.data.frame(faux_x))
  colnames(data) <- c("pixel_numbers_scale_bar", "hex_codes_scale_bar", "x")
  white_low_df <- rbind(white_low_df, data)
}
#Add blue_max above all the white values in the white_low_df
low_scale_bar_add <- rbind(max_blue, white_low_df)

#Repeat above for max orange
max_orange <- filter(scale_bar_df, pixel_numbers_scale_bar == max_pixel)
white_pixel_num_high <- white_bar_number + (max_pixel+nrow(low_scale_bar_add))
#For loop to make a dataframe for each white bar
white_high_df = data.frame()
for(i in 1:length(white_pixel_num_high)){
  pixel_number <- white_pixel_num_high[i]
  data <- cbind(as.data.frame(pixel_number),as.data.frame(white_hex_code), as.data.frame(faux_x))
  colnames(data) <- c("pixel_numbers_scale_bar", "hex_codes_scale_bar", "x")
  white_high_df <- rbind(white_high_df, data)
}
#Add orange_max below all the white values in the white_low_df
max_orange <- mutate(max_orange, pixel_numbers_scale_bar = (max(white_pixel_num_high)+1))
high_scale_bar_add <- rbind(white_high_df, max_orange)

#Add the nrow of low add df to the scale_bar_df
length_add <- nrow(low_scale_bar_add)
updated_pixel_num <- scale_bar_df$pixel_numbers_scale_bar + length_add
##Combine all relevant dataframes
scale_bar_df <- cbind(updated_pixel_num, scale_bar_df[-1])
colnames(scale_bar_df) <- c("pixel_numbers_scale_bar", "hex_codes_scale_bar", "x")
scale_bar_df <- rbind(low_scale_bar_add, scale_bar_df, high_scale_bar_add)

#Plot and save what will become scale bar
scale_bar <- ggplot(scale_bar_df, aes(x=x, y=pixel_numbers_scale_bar, fill=hex_codes_scale_bar)) +
  geom_tile(aes()) + scale_fill_identity() + theme(panel.background = element_blank())
ggsave(SCALE_BAR_FILENAME, scale_bar, height = 3, width = 1)
  
  
  
