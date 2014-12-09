

master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#Create the raw file

colnames = c("country_name", "region_name", "district_name", "fiscal_year", 
             "reporting_period", "disease", "funding_src", 
             "persons_treated_usaid_funding", "persons_treated_all_funding")

raw = master_file[master_file$disease == "Schisto", colnames]

for(i in length(colnames)){
  names(raw)[i] = colnames[i]
}

for(i in 8:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 8:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}

#####################
##Create Indicators##
#####################
keep = c("country_name", "fiscal_year", "funding_src", 
         "persons_treated_usaid_funding", "persons_treated_all_funding")
treated <- raw[(raw$reporting_period == "2nd SAR (October-September)"), keep]

treated['USAID_treated'] = ave(treated[,"persons_treated_usaid_funding"], 
                               treated[,"country_name"], 
                               treated[,"fiscal_year"], 
                               FUN = sum)

treated['RTI_treated'] = 0

treated[(treated$funding_src == 1 | treated$funding_src == 99), 'RTI_treated'] = ave(treated[(treated$funding_src == 1 | treated$funding_src == 99),"persons_treated_usrti = c(1,99)

sch['rti_evals_conducted'] = 0
sch[(sch$funding_src %in% rti), 'rti_evals_conducted'] = ave(sch[(sch$funding_src %in% rti), 'sch_eval'],
                                                             sch[(sch$funding_src %in% rti), 'country_name'],
                                                             sch[(sch$funding_src %in% rti), 'fiscal_year'], 
                                                             FUN = sum)

sch['rti_sen_site'] = 0
sch[(sch$funding_src %in% rti), 'rti_sen_site'] = ave(sch[(sch$funding_src %in% rti), 'sentinel_site'],
                                                           sch[(sch$funding_src %in% rti), 'country_name'],
                                                           sch[(sch$funding_src %in% rti), 'fiscal_year'], 
                                                           FUN = sum)aid_funding"], 
                                                                                     treated[(treated$funding_src == 1 | treated$funding_src == 99),"country_name"], 
                                                                                     treated[(treated$funding_src == 1 | treated$funding_src == 99),"fiscal_year"], 
                                                                                     FUN = sum)

treated[, 'RTI_treated'] = ave(treated[,"RTI_treated"], 
                               treated[,"country_name"], 
                               treated[,"fiscal_year"], 
                               FUN = max)

treated[(treated$country_name == 'Burkina Faso' & treated$fiscal_year == 2011) | (treated$country_name == 'Niger' & treated$fiscal_year == 2011), 
        'RTI_treated'] = 0

keep = c('country_name', 'fiscal_year', 'USAID_treated', 'RTI_treated')

treated_collapsed = treated[, keep]
treated_collapsed = unique(treated_collapsed)

write.csv(treated_collapsed, "C:\\Users\\reowen\\Documents\\Datasets\\SCH_treatments.csv")

rm(master_file, raw, treated, colnames, i, keep)

####################
##Schisto DSA data##
####################

me_master = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\me_master.csv")

colnames = c("country_name", "region_name", "district_name", "fiscal_year", 
             "assessment_type", "funding_src", "assessment_completed")

sch = me_master[((me_master$assessment_type == 'Schisto sentinel site' | me_master$assessment_type == 'Schisto evaluation') & me_master$reporting_period == '2nd SAR (October-September)'),
                colnames]

sch['sentinel_site'] = ifelse((sch$assessment_type == 'Schisto sentinel site' & sch$assessment_completed == 'yes'), 1, 0)
sch['sch_eval'] = ifelse((sch$assessment_type == 'Schisto evaluation' & sch$assessment_completed == 'yes'), 1, 0)

usaid = c(1, 2, 3, 4, 5, 99)

sch['USAID_evals_conducted'] = 0
sch[(sch$funding_src %in% usaid), 'USAID_evals_conducted'] = ave(sch[(sch$funding_src %in% usaid), 'sch_eval'],
                                                                 sch[(sch$funding_src %in% usaid), 'country_name'],
                                                                 sch[(sch$funding_src %in% usaid), 'fiscal_year'], 
                                                                 FUN = sum)

sch['USAID_sen_site'] = 0
sch[(sch$funding_src %in% usaid), 'USAID_sen_site'] = ave(sch[(sch$funding_src %in% usaid), 'sentinel_site'],
                                                                 sch[(sch$funding_src %in% usaid), 'country_name'],
                                                                 sch[(sch$funding_src %in% usaid), 'fiscal_year'], 
                                                                 FUN = sum)

rti = c(1,99)

sch['rti_evals_conducted'] = 0
sch[(sch$funding_src %in% rti), 'rti_evals_conducted'] = ave(sch[(sch$funding_src %in% rti), 'sch_eval'],
                                                             sch[(sch$funding_src %in% rti), 'country_name'],
                                                             sch[(sch$funding_src %in% rti), 'fiscal_year'], 
                                                             FUN = sum)

sch['rti_sen_site'] = 0
sch[(sch$funding_src %in% rti), 'rti_sen_site'] = ave(sch[(sch$funding_src %in% rti), 'sentinel_site'],
                                                           sch[(sch$funding_src %in% rti), 'country_name'],
                                                           sch[(sch$funding_src %in% rti), 'fiscal_year'], 
                                                           FUN = sum)



keep = c('country_name', 'fiscal_year', 'USAID_evals_conducted', 'USAID_sen_site', 
         'rti_evals_conducted', 'rti_sen_site')

sch_out = sch[, keep]
sch_out = unique(sch_out)

write.csv(sch_out, "C:\\Users\\reowen\\Documents\\Datasets\\SCH_evals.csv")

rm(me_master, sch, colnames, keep, usaid)


########################
##Schisto mapping data##
########################
# 
# map = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\mapping.csv")
# 
# colnames = c("country_name", "region_name", "district_name", "fiscal_year", 
#              "assessment_type", "funding_src", "assessment_completed")



final = merge(sch_out, treated_collapsed, by=c('country_name', 'fiscal_year'), all=TRUE)
write.csv(final, "C:\\Users\\reowen\\Documents\\Datasets\\SCH_data.csv")

# merge sch_out, treated_collapsed (by country name and FY)
