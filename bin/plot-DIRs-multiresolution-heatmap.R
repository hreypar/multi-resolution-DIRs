#!/usr/bin/env Rscript
#
# hreyes July 2020
# plot-DIRs-multiresolution-heatmap.R
#######################################################################
# Read in a Rds files with summary DIRs and create a heatmap.
########################################################################
#
#################### import libraries and set options ##################
suppressMessages(library(ggplot2))
library(patchwork)
# I just have to say that I LOVE the people who made patchwork
#
options(scipen = 10)
#
args = commandArgs(trailingOnly=TRUE)
#
########################################################################
########################## functions ###################################
########################################################################
# it's dangerous to go alone! take this.
#
#################### resolution of the data ############################
# format_hicres <- function(hicres) { 
#   if(hicres >= 1000000) {
#     hicunit = paste0(hicres/1000000, "Mb")
#   } else {
#     hicunit = paste0(hicres/1000, "kb")
#   }
# }  
#
########################################################################
########################## parse args ##################################
args = unlist(strsplit(args, " "))
#
output = args[grep(".png", args)]
input = args[grep(".summary.significantpairs.Rds", args)]
#
######################### read in data #################################
multiresolution.DIRs.summary <- do.call(rbind, lapply(input, readRDS))
#
######################### format dataframe #############################
#
# AGAIN, THIS MODULE IS HIGHLY SPECIFIC TO THESE MCF10 CANCER PROGRESSION
# MODEL DATA. NOTHING SHOULD BE HARDCODED LIKE THIS
#
# set DIRs levels so they show up properly in the plot
multiresolution.DIRs.summary$DIRs <- factor(multiresolution.DIRs.summary$DIRs, 
       levels = c("MCF10AT1 vs MCF10A -logFC", "MCF10CA1a vs MCF10AT1 -logFC",
                  "MCF10CA1a vs MCF10A -logFC", "MCF10AT1 vs MCF10A +logFC",
                  "MCF10CA1a vs MCF10AT1 +logFC", "MCF10CA1a vs MCF10A +logFC"))

# remove the chr from chromosome so it doesn't clutter in the plot
multiresolution.DIRs.summary$Chromosome <- gsub("chr", "", multiresolution.DIRs.summary$Chromosome)
multiresolution.DIRs.summary$Chromosome <- factor(multiresolution.DIRs.summary$Chromosome, 
                                                  levels = c(seq(1,22), "X"))
#

# create factor to determine if it is a loss or a gain
multiresolution.DIRs.summary$Interaction <- unlist(lapply(multiresolution.DIRs.summary$DIRs, function(s) {
  if(grepl(pattern = "\\+", s)) {
    return("Gain")
  } else if(grepl(pattern = "\\-", s)) {
    return("Loss")
  }
}))
#
######################### plot heatmap #################################
####### prepare variables for plot

####### plot1
png(filename = output, height = 12, width = 19, units = "in", res = 300)

pgain <-   ggplot(multiresolution.DIRs.summary[multiresolution.DIRs.summary$Interaction == "Gain", ],
                  aes(x = Chromosome, y = as.factor(Resolution), fill = Count)) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "Number of\ngained DIR",
                      low = "#bfc0c0",
                      high = "#466995") +
  ggtitle("Differentially Interacting Regions by Resolution\n") +
  ylab("Resolution (bp)")

ploss <-   ggplot(multiresolution.DIRs.summary[multiresolution.DIRs.summary$Interaction == "Loss", ],
                  aes(x = Chromosome, y = as.factor(Resolution), fill = Count)) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "Number of\nlost DIRs",
                      low = "#bfc0c0",
                      high = "#53131e") +
  ylab("Resolution (bp)")

pgain / ploss

dev.off()

####### plot1 log10
png(filename = gsub(".png", "-log2.png", output), 
    height = 12, width = 19, units = "in", res = 300)

pgain.log2 <-   ggplot(multiresolution.DIRs.summary[multiresolution.DIRs.summary$Interaction == "Gain", ],
                  aes(x = Chromosome, y = as.factor(Resolution), fill = log2(Count))) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "Number of\ngained DIRs\n(log2)",
                      low = "#bfc0c0",
                      high = "#466995") +
  ggtitle("Differentially Interacting Regions by Resolution\n") +
  ylab("Resolution (bp)")

ploss.log2 <-   ggplot(multiresolution.DIRs.summary[multiresolution.DIRs.summary$Interaction == "Loss", ],
                  aes(x = Chromosome, y = as.factor(Resolution), fill = log2(Count))) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "Number of\nlost DIRs\n(log2)",
                      low = "#bfc0c0",
                      high = "#53131e") +
  ylab("Resolution (bp)")

pgain.log2 / ploss.log2

dev.off()
  
  