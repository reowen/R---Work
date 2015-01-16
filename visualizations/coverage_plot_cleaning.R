
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#columns used in the script
colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "funding_src","funding_src_r1", "funding_src_r2", 
             "disease_distribution", "most_recent_prevalence_results", "total_pop", 
             "persons_at_risk", "persons_targeted_usaid_funding", "persons_targeted_all_funding", 
             "persons_targeted_usaid_funding_r1", "persons_targeted_all_funding_r1", "persons_targeted_usaid_funding_r2",
             "persons_targeted_all_funding_r2", "persons_treated_usaid_funding", "persons_treated_all_funding",
             "persons_treated_usaid_funding_r1", "persons_treated_all_funding_r1", "persons_treated_usaid_funding_r2", 
             "persons_treated_all_funding_r2", "sac_at_risk", "sac_targeted_with_usaid_support_r1", 
             "sac_targeted_with_usaid_support_r2", "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2", 
             "sac_targeted_with_usaid_support", "sac_treated_all_funding", "sac_treated_usaid_funding", 
             "sac_treated_all_funding_r1", "sac_treated_all_funding_r2", 
             "total_pop", "people_living_mda_acheived")

raw = master_file[, colnames]
#free up some memory (this takes 45 mb)
rm(master_file)

for(i in 13:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 13:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
rm(colnames)

#Find most recent fiscal year (ENVISION/other separate due to lack of non-ENVISION workplanning)
ENVISION = c("Benin", "Cameroon", "Democratic Republic of Congo", "Ethiopia", "Guinea", "Haiti", "Indonesia", 
             "Mali", "Mozambique", "Nepal", "Nigeria", "Senegal", "Sierra Leone", "Tanzania", "Uganda")
latest_year_ENVISION = max(raw[(raw$country_name %in% ENVISION), 'fiscal_year'])
latest_year_other = max(raw[!(raw$country_name %in% ENVISION), 'fiscal_year'])


countries = levels(raw$country_name)
raw['most_recent_pop'] = 0
for(country in countries){
  #find latest year by country
  latest_year = max(raw[(raw$country_name == country), 'fiscal_year']) 
  #update latest_year variable with pop from country's latest year
  raw[(raw$country_name == country & raw$fiscal_year == latest_year), 'most_recent_pop'] = raw[(raw$country_name == country & raw$fiscal_year == latest_year), 'total_pop']
}
rm(latest_year)

unique = paste(raw$country_name, raw$region_name, raw$district_name)
raw['most_recent_pop'] = ave(raw[,'most_recent_pop'], 
                             unique,
                             FUN = max)
rm(unique)


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
## Condense the dataset to something more manageable ##
#######################################################

keep_cols = c("country_name", "region_name", "district_name", "fiscal_year", "disease", "most_recent_pop",
              "persons_at_risk", 'sac_at_risk', 'usaid_targeted', 'usaid_treated','sac_targeted', 'sac_treated',
              "disease_distribution", "people_living_mda_acheived", "most_recent_prevalence_results")

# In this particular case, I used only SAR2 due to the context.  However, the script for the
# database would need to pull from most recent submission period for each year.
raw_cvg = raw[raw$reporting_period == '2nd SAR (October-September)', keep_cols]
rm(raw, keep_cols)

#Code program coverage variable
raw_cvg['prg_cvg'] = round((raw_cvg[, 'usaid_treated'] / raw_cvg[, 'usaid_targeted']), digits=4)
raw_cvg['sac_prg_cvg'] = round((raw_cvg[, 'sac_treated'] / raw_cvg[, 'sac_targeted']), digits=4)

#Code epi coverage variable
raw_cvg['epi_cvg'] = round((raw_cvg[,'usaid_treated'] / raw_cvg[,'persons_at_risk']), digits=4)
raw_cvg['sac_epi_cvg'] = round((raw_cvg[,'sac_treated'] / raw_cvg[,'sac_at_risk']), digits=4)

################################################
### Pull out only relevant indicators to keep ##
################################################

reg_cols = c('country_name', 'region_name', 'district_name', 'disease', "most_recent_pop", 'fiscal_year',
             "disease_distribution", 'most_recent_prevalence_results', 'persons_at_risk', 'sac_at_risk', 
             'usaid_targeted', 'usaid_treated', 'sac_targeted', 'sac_treated', 'prg_cvg', 'sac_prg_cvg', 
             'epi_cvg', 'sac_epi_cvg', 'people_living_mda_acheived')

cvg_final = raw_cvg[, reg_cols]


###################################################################
############### Pull out TAS and TIS indicators ###################
###################################################################

#me_master is a .csv version of the me.tab offline file
me_master = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\me_master.csv")

colnames = c('country_name', 'region_name', 'district_name', 'applicable_disease',
             'fiscal_year', 'reporting_period',  'assessment_type', 'assessment_completed', 
             'action_oriented_conclusion')

dsa = me_master[me_master$reporting_period == '2nd SAR (October-September)', colnames]
names(dsa)[4] = 'disease'
dsa = dsa[dsa$disease != 'NULL', ]

#code DSA variable
DSA_values = c('LF TAS: Post-MDA Surveillance I', 'LF TAS: Stop MDA', 
               'LF Transmission Assessment Survey (TAS)', 'LF TAS: Post-MDA Surveillance II',
               'Trachoma impact survey', 'Trachoma health impact surveys', 
               'STH evaluation', 'Schisto evaluation', 'Oncho epidemiological assessment')

dsa['DSA'] = dsa$assessment_type
dsa[(!(dsa$assessment_type %in% DSA_values) | dsa$assessment_completed != 'yes'), 'DSA'] = NA

#Create DSA_results variable
dsa['DSA_results'] = dsa$action_oriented_conclusion
dsa[(!(dsa$assessment_type %in% DSA_values) | dsa$assessment_completed != 'yes'), 'DSA_results'] = NA


# #These are valid values for TAS type
# TAS_values = c('LF TAS: Post-MDA Surveillance I', 'LF TAS: Stop MDA', 
#                'LF Transmission Assessment Survey (TAS)', 'LF TAS: Post-MDA Surveillance II')
# 
# #TAS column shows the TAS type, only if the assessment has been completed
# dsa['TAS'] = dsa$assessment_type
# dsa[(!(dsa$assessment_type %in% TAS_values) | dsa$assessment_completed != 'yes'), 'TAS'] = NA
# 
# #Valid values for a TIS... the 'health impact surveys' one i think is a data entry error
# TIS_values = c('Trachoma impact survey', 'Trachoma health impact surveys')
# 
# dsa['TIS'] = dsa$assessment_type
# dsa[(!(dsa$assessment_type %in% TIS_values) | dsa$assessment_completed != 'yes'), 'TIS'] = NA
# 
# 
# #Create DSA_results variable
# dsa['DSA_results'] = dsa$action_oriented_conclusion
# dsa[(!(dsa$assessment_type %in% TAS_values | dsa$assessment_type %in% TIS_values) | dsa$assessment_completed != 'yes'), 'DSA_results'] = NA


#pull out only relevant indicators to keep
keep = c('country_name', 'region_name', 'district_name', 'disease',
         'fiscal_year', 'DSA', 'DSA_results')

dsa_final = dsa[, keep]

####################################################################################
# Merge DSA and MDA data -- RESHAPE, CONDENSE THE DATASET: fiscal years in columns #
####################################################################################

# merge dsa_final and cvg_final
district = merge(cvg_final, dsa_final, 
                 by=c('country_name', 'region_name', 'district_name', 'disease', 'fiscal_year'), 
                 all = TRUE)

district = unique(district)

library(plyr)

region = ddply(district, c('country_name', 'region_name', 'disease', 'fiscal_year'), summarize, 
               persons_at_risk = sum(persons_at_risk),
               usaid_targeted = sum(usaid_targeted), 
               usaid_treated = sum(usaid_treated))

district = district[district$district_name != 'NULL', ]

country = ddply(district, c('country_name', 'disease', 'fiscal_year'), summarize, 
                persons_at_risk = sum(persons_at_risk),
                usaid_targeted = sum(usaid_targeted), 
                usaid_treated = sum(usaid_treated))

rm(cvg_final, dsa, dsa_final, me_master, raw_cvg, DSA_values, colnames, countries, country, i, keep, 
   latest_year_ENVISION, latest_year_other, reg_cols)




