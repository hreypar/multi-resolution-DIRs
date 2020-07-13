# DESCRIPTION:
# mk module to filter hicexp objects and
# obtain significant region pairs
#
# USAGE:
# Single target execution: `mk <TARGET>` where TARGET is
# any line printed by the script `bin/mk-targets`
#
# Multiple target execution in tandem `bin/mk-targets | xargs mk`
#
# AUTHOR: HRG
#
< config.mk
#
# Run R script to filter significant pairs.
#
results/%.summary.significantpairs.Rds:	data/%.significantpairs.Rds
	mkdir -p `dirname $target`
	bin/get-DIRs-summary.R \
		--input $prereq \
		--output $target

# Plot significant pairs stats.
#
plot_multiresolution_heatmap:V:	results/
	find -L $prereq \
		-type f \
		-name "*.summary.significantpairs.Rds" \
		-exec bin/dummy {} +;

