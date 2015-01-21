
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#columns used in the script
colnames = c("country_name", "region_name", "district_name", "fiscal_year", "reporting_period", 
             "disease", "persons_at_risk", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", 
             "persons_targeted_usaid_funding_r2", "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", 
             "persons_treated_usaid_funding_r2", "sac_at_risk", "sac_targeted_with_usaid_support", 
             "sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2", "sac_treated_usaid_funding", 
             "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw = master_file[, colnames]
#free up some memory (this takes 45 mb)
# rm(master_file)

for(i in 7:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 7:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
rm(colnames, i)


#create number targeted and treated (usaid support)

usaid_targeted = c("persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2")
usaid_treated = c("persons_treated_usaid_funding", "persons_treated_usaid_funding_r1","persons_treated_usaid_funding_r2")

raw['usaid_targeted'] = apply(raw[, usaid_targeted], 1, max)
raw['usaid_treated'] = apply(raw[, usaid_treated], 1, max)
rm(usaid_targeted, usaid_treated)

#create SAC treated variable

sac_targeted = c("sac_targeted_with_usaid_support", "sac_targeted_with_usaid_support_r1", 
                 "sac_targeted_with_usaid_support_r2")

sac_treated = c("sac_treated_usaid_funding", "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw['sac_targeted'] = apply(raw[, sac_targeted], 1, max)
raw['sac_treated'] = apply(raw[, sac_treated], 1, max)
rm(sac_targeted, sac_treated)


#######################################################
###### Code program and epidemiological coverage ######
#######################################################

keep_cols = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
              "persons_at_risk", 'sac_at_risk', 'usaid_targeted', 'usaid_treated', 'sac_targeted', 'sac_treated')

raw_cvg = raw[raw$reporting_period == '2nd SAR (October-September)', keep_cols]
rm(raw, keep_cols)

#Code program coverage variable
sac = c('STH', "Schisto")

raw_cvg['prg_cvg'] = with(raw_cvg, ifelse((disease %in% sac), 
                                          (sac_treated / sac_targeted), 
                                          (usaid_treated / usaid_targeted)))
raw_cvg[(raw_cvg$prg_cvg == 0 | is.nan(raw_cvg$prg_cvg) | is.infinite(raw_cvg$prg_cvg)), 'prg_cvg'] <- NA

raw_cvg['epi_cvg'] = with(raw_cvg, ifelse((disease %in% sac), 
                                          (sac_treated / sac_at_risk), 
                                          (usaid_treated / persons_at_risk)))
raw_cvg[(raw_cvg$epi_cvg == 0 | is.nan(raw_cvg$epi_cvg) | is.infinite(raw_cvg$epi_cvg)), 'epi_cvg'] <- NA
rm(sac)

################################################
######## code the country-level dataset ########
################################################

library(plyr)
country <- ddply(raw_cvg, c('country_name', 'disease', 'fiscal_year'), summarize, 
                 min_cvg = min(prg_cvg, na.rm=TRUE), 
                 max_cvg = max(prg_cvg, na.rm=TRUE), 
                 median_cvg = median(prg_cvg, na.rm=TRUE), 
                 mean_cvg = mean(prg_cvg, na.rm=TRUE))

vars = c('min_cvg', 'max_cvg', 'median_cvg', 'mean_cvg')
for(i in 1:length(vars)){
  country[(is.nan(country[, vars[i]]) | is.infinite(country[, vars[i]])), vars[i]] <- NA
}
rm(vars, i)

################################################
######## code the district-level dataset #######
################################################

unique = paste(raw_cvg$country_name, raw_cvg$region_name, raw_cvg$district_name, raw_cvg$disease)

raw_cvg['times_treated'] = ave(raw_cvg[,'usaid_treated'], 
                               unique, 
                               FUN = function(x) sum(x > 0))

raw_cvg['min_prg_cvg'] = ave(raw_cvg[,'prg_cvg'], 
                             unique,
                             FUN = function(x) min(x, na.rm=TRUE))

raw_cvg['max_prg_cvg'] = ave(raw_cvg[,'prg_cvg'], 
                             unique,
                             FUN = function(x) max(x, na.rm=TRUE))

raw_cvg['count_below_epi_threshold'] = ave(raw_cvg[,'epi_cvg'], 
                                           unique, 
                                           FUN = function(x) sum(x < 0.65, na.rm=TRUE))

raw_cvg['count_above_epi_threshold'] = ave(raw_cvg[,'epi_cvg'], 
                                           unique, 
                                           FUN = function(x) sum(x >= 0.65, na.rm=TRUE))

unique = paste(raw_cvg$country_name, raw_cvg$region_name, raw_cvg$district_name, raw_cvg$fiscal_year)
raw_cvg['average_cvg'] = ave(raw_cvg[,'prg_cvg'], 
                             unique,
                             FUN = function(x) mean(x, na.rm=TRUE))
rm(unique)

vars = c('min_prg_cvg', 'max_prg_cvg', 'times_treated', 'count_below_epi_threshold', 
         'count_above_epi_threshold', 'average_cvg')
for(i in 1:length(vars)){
  raw_cvg[(is.nan(raw_cvg[, vars[i]]) | is.infinite(raw_cvg[, vars[i]])), vars[i]] <- NA
}
rm(vars, i)

#####################################
### create district-level dataset ###
#####################################




