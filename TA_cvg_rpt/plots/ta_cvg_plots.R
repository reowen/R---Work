COUNTRY <- 'Mali'
FY <- c(2013, 2014)
DISEASE <- 'STH'

#####
library(ggplot2)

source('C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\TA_cvg_rpt\\TA_cvg_rpt_for_developers_v2.R')

cvg <- cvg_final
rm(countries, country, i, latest_year_ENVISION, latest_year_other, reg_cols, cvg_final, raw_cvg)

setwd("C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\TA_cvg_rpt\\plots")


unique = paste(cvg$country_name, cvg$region_name, cvg$district_name, cvg$disease)
cvg['avg_hist_cvg'] <- ave(cvg[,'prg_cvg'], 
                           unique, 
                           FUN = function(x) mean(x, na.rm=TRUE))

cvg['times_treated'] <- ave(cvg[, 'treated'], 
                            unique, 
                            FUN = function(x) sum(x > 0, na.rm=TRUE))

cvg[cvg$treated == 0, 'treated'] <- NA
cvg[cvg$targeted == 0, 'targeted'] <- NA

rm(unique)


cplots <- cvg[(cvg$country_name == COUNTRY & cvg$fiscal_year %in% FY), ]

c_hist_all <- ggplot(cplots, aes(x=prg_cvg)) + 
  geom_histogram(binwidth=0.1, fill="white", color="black") + 
  scale_x_continuous(breaks = round(seq(0, (max(cplots$prg_cvg, na.rm=TRUE) + 0.1), by=0.1), 1)) + 
  facet_grid(disease ~ .) + 
  labs(title = paste('Program Coverage:', COUNTRY, 'FY', FY),
       x = 'Program Coverage', 
       y = '# districts')

scatter <- ggplot(cplots, aes(x=treated, y=prg_cvg)) + 
  geom_point(shape=1) + 
  geom_smooth() #+ 
#   facet_grid(disease ~ .)

density <- ggplot(cplots, aes(x=prg_cvg)) + 
  geom_density() + 
  scale_x_continuous(breaks = round(seq(0, (max(cplots$prg_cvg, na.rm=TRUE) + 0.1), by=0.1), 1)) + 
  scale_y_continuous(breaks=NULL) +
  facet_wrap(disease ~ fiscal_year, ncol=2) + 
  labs(title = paste('Program Coverage Distribution:', COUNTRY, ', FY', FY),
       x = 'Program Coverage', 
       y = '')