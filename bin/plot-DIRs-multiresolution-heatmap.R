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
       levels = c("MCF-10AT1 vs MCF-10A -logFC", "MCF-10CA1A vs MCF-10AT1 -logFC",
                  "MCF-10CA1A vs MCF-10A -logFC", "MCF-10AT1 vs MCF-10A +logFC",
                  "MCF-10CA1A vs MCF-10AT1 +logFC", "MCF-10CA1A vs MCF-10A +logFC"))

# remove the chr from chromosome so it doesn't clutter in the plot
multiresolution.DIRs.summary$Chromosome <- gsub("chr", "", multiresolution.DIRs.summary$Chromosome)
multiresolution.DIRs.summary$Chromosome <- factor(multiresolution.DIRs.summary$Chromosome, 
                                                  levels = c(seq(1,22), "X"))
#
######################### plot heatmap #################################
####### prepare variables for plot

####### plot1
png(filename = output, height = 12, width = 19, units = "in", res = 300)

plot1 <- ggplot(multiresolution.DIRs.summary, 
       aes(x = Chromosome, y = as.factor(Resolution), fill = Count)) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "DIR Count",
                      low = "#BDC2BF",
                      high = "#023364") +
  ggtitle("Differentially Interacting Regions by Resolution\n") +
  ylab("Resolution (bp)")

print(plot1)
dev.off()

####### plot1 log10
png(filename = gsub(".png", "-log10.png", output), 
    height = 12, width = 19, units = "in", res = 300)

plot1.log10 <- ggplot(multiresolution.DIRs.summary, 
                aes(x = Chromosome, y = as.factor(Resolution), fill = log10(Count))) +
  geom_tile() + facet_wrap(~DIRs) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.title.y = element_text(size = 12 ),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  scale_fill_gradient(name = "DIR Count\n(log10)",
                      low = "#BDC2BF",
                      high = "#023364") +
  ggtitle("Differentially Interacting Regions by Resolution\n") +
  ylab("Resolution (bp)")

print(plot1.log10)

dev.off()
  
  