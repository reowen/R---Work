
setwd('C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\visualizations')
source('coverage_plot_cleaning.R')

plot <- district[(district$country_name %in% ENVISION & district$fiscal_year == 2014), ]
plot[(plot$prg_cvg == 0 | is.nan(plot$prg_cvg)), 'prg_cvg'] <- NA

library(ggplot2)
library(gridExtra)

