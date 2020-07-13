#!/usr/bin/env Rscript
#
# hreyes July 2020
# plot-DIRs-multiresolution-heatmap.R
#######################################################################
# Read in a Rds files with summary DIRs and create a heatmap.
########################################################################
#
#################### import libraries and set options ##################
suppressMessages(library(optparse))
suppressMessages(library(ggplot2))
#
options(scipen = 10)
#
######################## create opts ###################################
option_list = list(
  make_option(opt_str = c("-i", "--input"), 
              type = "character",
              help = "All of the files that will be used to produce a heatmap, separated by a blank space"),
  make_option(opt_str = c("-o", "--output"), 
              type = "character", 
              help = "output filepath for the heatmap")
)
#
opt <- parse_args(OptionParser(option_list=option_list))
#
### check the hicexp parameter is not empty
if (is.null(opt$input)){
  print_help(OptionParser(option_list=option_list))
  stop("The input file is mandatory.n", call.=FALSE)
}
#
########################################################################
########################## functions ###################################
########################################################################
# it's dangerous to go alone! take this.
#
#################### resolution of the data ############################
format_hicres <- function(hicres) { 
  if(hicres >= 1000000) {
    hicunit = paste0(hicres/1000000, "Mb")
  } else {
    hicunit = paste0(hicres/1000, "kb")
  }
}  
#
########################################################################
######################### read in data #################################
multiresolution.DIRs.summary <- do.call(rbind, lapply(opt$input, readRDS))
#
########################## change levels ###############################
#
# AGAIN, THIS MODULE IS HIGHLY SPECIFIC TO THESE MCF10 CANCER PROGRESSION
# MODEL DATA. NOTHING SHOULD BE HARDCODED LIKE THIS
multiresolution.DIRs.summary$DIRs <- factor(multiresolution.DIRs.summary$DIRs, 
       levels = c("MCF-10AT1 vs MCF-10A -logFC", "MCF-10AT1 vs MCF-10A +logFC",
                  "MCF-10CA1A vs MCF-10AT1 -logFC", "MCF-10CA1A vs MCF-10AT1 +logFC",
                  "MCF-10CA1A vs MCF-10A -logFC", "MCF-10CA1A vs MCF-10A +logFC"))
#
######################### plot heatmap #################################
####### prepare variables for plot

####### plot1
  # generate file
  png(filename = opt$output, height = 9, width = 15, units = "in", res = 300)
  
  plot1 <- 
    
    ggplot(distance.fc, aes(D, logFC)) + 
    geom_point(shape=20, alpha=0.6) + theme_minimal() +
    geom_rug(col="steelblue",alpha=0.1, size=1.25) +
    ggtitle(t.main) + xlab("Distance (bins)") + ylab("log fold-change")
  
  print(plot1)
  dev.off()
  
####### plot2
  # generate file
  png(filename = paste0(outdir, "/", sigpairs, "-distance-vs-logfc-", r, ".png"),
      height = 9, width = 15, units = "in", res = 300)
  
  plot2 <- ggplot(distance.fc, aes(D, logFC)) + 
    geom_point(shape=20, alpha=0.6) +  facet_wrap(~chr) + #theme_minimal() +
    geom_rug(col="steelblue",alpha=0.1, size=1.25) +
    ggtitle(t.main) + xlab("Distance (bins)") + ylab("log fold-change")
  
  print(plot2)
  dev.off()


  
  
  