
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#columns used in the script
colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "funding_src","funding_src_r1", "funding_src_r2", 
             "disease_distribution", 
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
rm(master_file)

for(i in 12:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 12:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
rm(colnames)


#Create number targeted and treated (all support)

#To calculated the treated and targeted columns:
#Take the max of the columns specified in "all_targeted" and "all_treated" vectors
all_targeted = c("persons_targeted_all_funding", "persons_targeted_all_funding_r1", "persons_targeted_all_funding_r2", 
                 "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2")
all_treated = c("persons_treated_all_funding", "persons_treated_all_funding_r1", "persons_treated_all_funding_r2", 
                "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", "persons_treated_usaid_funding_r2")

#note these are the max values across persons targeted, persons targeted r1 and r2
raw['targeted'] = apply(raw[, all_targeted], 1, max)
raw['treated'] = apply(raw[, all_treated], 1, max)
rm(all_targeted, all_treated)


#####################################################
##Condense the dataset to something more manageable##
#####################################################

keep_cols = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
              "persons_at_risk", 'targeted', 'treated', "disease_distribution", "people_living_mda_acheived")

# In this particular case, I used only SAR2 due to the context.  However, the script for the
# database would need to pull from most recent submission period for each year.
raw_cvg = raw[raw$reporting_period == '2nd SAR (October-September)', keep_cols]
rm(raw, keep_cols)

#Code program coverage variable
raw_cvg['prg_cvg'] = round((raw_cvg[, 'treated'] / raw_cvg[, 'targeted']), digits=4)

#Code epi coverage variable
raw_cvg['epi_cvg'] = round((raw_cvg[,'treated'] / raw_cvg[,'persons_at_risk']), digits=4)

##############################################
###Pull out only relevant indicators to keep##
##############################################

reg_cols = c('country_name', 'region_name', 'district_name', 'disease', 'fiscal_year',
             "disease_distribution", 'persons_at_risk', 'targeted', 'treated', 
             'prg_cvg', 'epi_cvg', 'people_living_mda_acheived')

cvg_final = raw_cvg[, reg_cols]


###################################################################
## Pull out TAS and TIS indicators ##
###################################################################

#me_master is a .csv version of the me.tab offline file
me_master = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\me_master.csv")

colnames = c('country_name', 'region_name', 'district_name', 'applicable_disease',
             'fiscal_year', 'reporting_period',	'assessment_type', 'assessment_completed', 
             'action_oriented_conclusion')

dsa = me_master[me_master$reporting_period == '2nd SAR (October-September)', colnames]
names(dsa)[4] = 'disease'
dsa = dsa[dsa$disease != 'NULL', ]


#These are valid values for TAS type
TAS_values = c('LF TAS: Post-MDA Surveillance I', 'LF TAS: Stop MDA', 
               'LF Transmission Assessment Survey (TAS)', 'LF TAS: Post-MDA Surveillance II')

#TAS column shows the TAS type, only if the assessment has been completed
dsa['TAS'] = dsa$assessment_type
dsa[(!(dsa$assessment_type %in% TAS_values) | dsa$assessment_completed != 'yes'), 'TAS'] = NA

#Valid values for a TIS... the 'health impact surveys' one i think is a data entry error
TIS_values = c('Trachoma impact survey', 'Trachoma health impact surveys')

dsa['TIS'] = dsa$assessment_type
dsa[(!(dsa$assessment_type %in% TIS_values) | dsa$assessment_completed != 'yes'), 'TIS'] = NA


#Create DSA_results variable
dsa['DSA_results'] = dsa$action_oriented_conclusion
dsa[(!(dsa$assessment_type %in% TAS_values | dsa$assessment_type %in% TIS_values) | dsa$assessment_completed != 'yes'), 'DSA_results'] = NA


#pull out only relevant indicators to keep
keep = c('country_name', 'region_name', 'district_name', 'disease',
         'fiscal_year', 'TAS', 'TIS', 'DSA_results')

dsa_final = dsa[, keep]

###################################################################
#Merge DSA and MDA data -- RESHAPE, CONDENSE THE DATASET: fiscal years in columns#
###################################################################

# merge dsa_final and cvg_final
final = merge(dsa_final, cvg_final, 
              by=c('country_name', 'region_name', 'district_name', 'disease', 'fiscal_year'), 
              all = TRUE)

final = final[with(final, order(fiscal_year)), ]

final = reshape(final, 
                timevar = 'fiscal_year', 
                idvar = c('country_name', 'region_name', 'district_name', 'disease'), 
                direction = 'wide')

final = final[with(final, order(disease, country_name, region_name, district_name)), ]

write.csv(final, "C:\\Users\\reowen\\Documents\\Datasets\\mda_cvg_report.csv")
