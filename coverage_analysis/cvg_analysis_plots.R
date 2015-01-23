AUTHOR <- 'Russell Owen'
COUNTRY <- 'Mali'
FY <- 2014
DISEASE <- 'LF'

#####
library(ggplot2)

setwd("C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\coverage_analysis")
source('cvg_analysis.R')

unique = paste(district$country_name, district$region_name, district$district_name, district$disease)
district['avg_historical_cvg'] <- ave(district[,'prg_cvg'], 
                                      unique, 
                                      FUN = function(x) mean(x, na.rm=TRUE))
rm(unique)


# Make dataset, histogram showing distribution

cdata <- district[(district$country_name == COUNTRY & district$fiscal_year == FY & district$disease == DISEASE), ]

cvg_hist <-  ggplot(cdata, aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1), limits = c(0,1.5)) + 
  labs(title = paste(DISEASE, 'Program Coverage:', COUNTRY, 'FY', FY),
       x = 'Program Coverage', 
       y = '# districts')

# Categorize districts

under_60 <- cdata[cdata$prg_cvg < 0.6, ]
under_60 <- under_60[!(is.na(under_60$country_name)), 
                     c('region_name', 'district_name', 'prg_cvg', 'times_treated', 
                       'avg_historical_cvg', 'min_prg_cvg', 'max_prg_cvg')]
under_60 <- under_60[with(under_60, order(prg_cvg)), ]


sixty_to_eighty <- cdata[cdata$prg_cvg >= 0.6 & cdata$prg_cvg < 0.8, ]
sixty_to_eighty <- sixty_to_eighty[!(is.na(sixty_to_eighty$country_name)), 
                                   c('region_name', 'district_name', 'prg_cvg', 'times_treated', 
                                     'avg_historical_cvg', 'min_prg_cvg', 'max_prg_cvg')]
sixty_to_eighty <- sixty_to_eighty[with(sixty_to_eighty, order(prg_cvg)), ]

eighty_to_100 <- cdata[cdata$prg_cvg >= 0.8 & cdata$prg_cvg <= 1, ]
eighty_to_100 <- eighty_to_100[!(is.na(eighty_to_100$country_name)), 
                               c('region_name', 'district_name', 'prg_cvg', 'times_treated', 
                                 'avg_historical_cvg', 'min_prg_cvg', 'max_prg_cvg')]
eighty_to_100 <- eighty_to_100[with(eighty_to_100, order(prg_cvg)), ]


hundred_plus <- cdata[cdata$prg_cvg > 1, ]
hundred_plus <- hundred_plus[!(is.na(hundred_plus$country_name)), c('region_name', 'district_name', 'prg_cvg', 'times_treated', 'avg_historical_cvg', 'min_prg_cvg', 'max_prg_cvg')] 
hundred_plus <- hundred_plus[with(hundred_plus, order(prg_cvg)), ]


# library(gridExtra)
# pdf('output\\table.pdf', height=11, width=8.5)
# # grid.arrange(hist)
# grid.table(under_60, gp=gpar(fontsize=8))
# grid.table(sixty_to_eighty, gp=gpar(fontsize=8))
# dev.off()


# # # For use in Knitr
# # 
# library(xtable)
# under_60.table <- xtable(under_60)
# sixty_to_eighty.table <- xtable(under_60)
# eighty_to_100.table <- xtable(eighty_to_100)
# hundred_plus.table <- xtable(hundred_plus)
# 
# # print(under_60.table, include.rownames=FALSE)


