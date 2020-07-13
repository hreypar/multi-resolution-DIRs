#!/usr/bin/env Rscript
#
# hreyes July 2020
# get-DIRs-summary.R
#######################################################################
# Read in a list of significant pairs (DIRs)
# and summarize them in order to build descriptive plots.
########################################################################
#
#################### import libraries and set options ##################
options(scipen = 10)
#
args = commandArgs(trailingOnly=TRUE)
#
########################## functions ###################################
# it's dangerous to go alone! take this.
#
##################### Infer resolution from data #######################
infer_resolution <- function(interactions) {
  unique(interactions[, "end1"] - interactions[, "start1"]) + 1
}
#
####################### format chromosome names ########################
format_chromosomes <- function(chrs) {
  chrs <- replace(x = chrs, chrs == "chr23", "chrX")
  chrs <- factor(chrs, levels = c(paste0("chr", seq(1,22,1)), "chrX"))
  chrs <- droplevels(chrs)
  return(chrs)
}
#
####################### map logFC values to factor #####################
logfc_to_category <- function(logfc) {
  sapply(logfc, function(l) {
    if(l > 0) "+logFC"
    else if (l < 0) "-logFC"
  })
}
#
############## obtain summary of significant interactions. #############
generate_DIRs_summary <- function(comparison) {
  
  # obtain comparison data from the list
  comparison.data <- significantpairs.list[[comparison]]
  
  # get resolution
  resolution = infer_resolution(comparison.data)
  
  # edit comparison name to use in DIRs column
  c = gsub("MCF10-", "", gsub("\\.", " vs ", gsub("sig.", "", comparison)))
  # remap logfc to factor and create new DIRs factor column
  comparison.data$DIRs <- paste(c, logfc_to_category(comparison.data[, "logFC"]))
  
  # format chromosomes and create new chr factor column
  comparison.data$chr <- format_chromosomes(comparison.data[, "chr1"])
  
  # get the count of each category and transform into dataframe
  dirs.summary <- data.frame(table(comparison.data$DIRs, comparison.data$chr),
                             resolution)
  colnames(dirs.summary) <- c("DIRs", "Chromosome", "Count", "Resolution")
  
  # clean up
  rm(comparison.data, resolution, c)
  
  return(dirs.summary)
}
#
############################ read in data #################################
significantpairs.list <- readRDS(args[1])
#
######################## obtan summary of DIRs ############################
DIRs.summary <- do.call(rbind, lapply(names(significantpairs.list), generate_DIRs_summary))
#
############################# save ouput ##################################
saveRDS(DIRs.summary, file = )
