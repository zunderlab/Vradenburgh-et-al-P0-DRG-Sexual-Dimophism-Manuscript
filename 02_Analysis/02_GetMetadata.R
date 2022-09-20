#smgoggin
#05.14.19

library(flowCore)
library(data.table)

##Set filepaths ===================================================================================
#path to fcs files
#(modify here if you want to specify another folder -- "/home/...")
FCS.FILEPATH <- getwd()
#path to where you want to save csv files 
#(modify here if you want to specify another folder -- "/home/...")
WRITE_PATH <- FCS.FILEPATH
METADATA.FILENAME <- "metadata.csv"
PANEL.FILENAME <- "panel.csv" 

##read in fcs files =============================================================================== 
file_list <- list.files(FCS.FILEPATH, full.names = TRUE, pattern = "*.fcs")
fs <- read.flowSet(file_list, transformation=FALSE)

##read in metadata ================================================================================ 
#file names
md.fileNames <- as.data.frame(cbind(sampleNames(fs), 
                                    c(rep(0,length(sampleNames(fs)))),
                                    c(rep(0,length(sampleNames(fs)))),
                                    c(rep(0,length(sampleNames(fs)))),
                                    c(rep(0,length(sampleNames(fs)))),
                                    c(rep(0,length(sampleNames(fs)))),
                                    c(rep(0,length(sampleNames(fs))))))
colnames(md.fileNames) <- c("file_name", "sample_type", "tissue", "age", "genotype", "region", "Misc")
#Antibody names (from "desc" parameter of flowFrame)
#NOTE: THIS ASSUMES YOU HAVE THE SAME PANEL FOR ALL FCS FILES!!!!!!!
md.params.metal <- colnames(fs)
md.params.ab <- lapply(pData(parameters(fs[[1]]))$desc, 
                       function(x) unlist(strsplit(x, split='_', fixed=TRUE))[2])
md.params.clust <- c(rep(0,length(md.params.metal))) #fill with zeroes to start
md.params.plot <- c(rep(0,length(md.params.metal))) #fill with zeroes to start
sins <- rep(5, length(md.params.metal))
md.params <- as.data.frame(cbind(unlist(md.params.metal),unlist(md.params.ab), sins, md.params.clust,
                                 md.params.plot))
colnames(md.params) <- c("Metal", "Antigen", "asinh.factor", "Clustering", "Plotting")

##write metadata files ============================================================================
fwrite(md.fileNames,file = paste(WRITE_PATH,METADATA.FILENAME,sep = '/'),row.names = FALSE)
fwrite(md.params,file = paste(WRITE_PATH,PANEL.FILENAME,sep = '/'),row.names = FALSE)
