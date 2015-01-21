
setwd('C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\visualizations')
source('coverage_plot_cleaning.R')

library(ggplot2)
library(gridExtra)


#########################################
### line graphs with treatment trends ###
#########################################

line <- country[(country$country_name %in% ENVISION & country$disease == 'LF'), ]
line[(line$usaid_targeted == 0 | is.nan(line$usaid_targeted)), 'usaid_targeted'] <- NA
line[(line$usaid_treated == 0 | is.nan(line$usaid_treated)), 'usaid_treated'] <- NA
line[(line$persons_at_risk == 0 | is.nan(line$persons_at_risk)), 'persons_at_risk'] <- NA

lgraph <- ggplot(line, aes(x=fiscal_year, y=usaid_treated, 
                           group=country_name, color=country_name)) + geom_line() + geom_point()