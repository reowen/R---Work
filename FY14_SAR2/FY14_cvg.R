
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#columns used in the script
colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "persons_targeted_usaid_funding",
             "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2",
             "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", "persons_treated_usaid_funding_r2", 
             "sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2", 
             "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw = master_file[(master_file$fiscal_year == 2014 & master_file$reporting_period == "2nd SAR (October-September)"), colnames]
#free up some memory (this takes 45 mb)
# rm(master_file)

for(i in 8:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 8:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
# rm(colnames)

#code treated indicators for all except STH 
all_targeted = c("persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2")
all_treated = c("persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", "persons_treated_usaid_funding_r2")

raw['num_targeted'] = apply(raw[, all_targeted], 1, max)
raw['num_treated'] = apply(raw[, all_treated], 1, max)
rm(all_targeted, all_treated)

raw['treated'] = ifelse(raw$num_treated > 0, 1, 0)

raw['prg_cvg'] = round(raw[, 'num_treated'] / raw[, 'num_targeted'], digits=4)
raw[is.nan(raw$prg_cvg), 'prg_cvg'] = 0
raw['cvg_pass'] = ifelse(raw$prg_cvg >= 0.8, 1, 0)

#SAC indicator for STH
sac_targeted = c("sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2")
sac_treated = c("sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw['sac_targeted'] = apply(raw[, sac_targeted], 1, max)
raw['sac_treated'] = apply(raw[, sac_treated], 1, max)

raw['if_sac_treated'] = ifelse(raw$sac_treated > 0, 1, 0)
raw['sac_cvg'] = round(raw[, 'sac_treated'] / raw[, 'sac_targeted'], digits=4)
raw[is.nan(raw$sac_cvg), 'sac_cvg'] = 0
raw['sac_cvg_pass'] = ifelse(raw$sac_cvg >= 0.8, 1, 0)




keep = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
         "treated", "cvg_pass", "if_sac_treated", "sac_cvg_pass")
final = raw[, keep]

write.csv(final, "C:\\Users\\reowen\\Documents\\Datasets\\FY14_cvg\\FY14_cvg_district.csv")

library(plyr)
c_level = ddply(final, c("country_name", "disease"), summarise, 
                num_treated = sum(treated), 
                pass_cvg = sum(cvg_pass), 
                sac_treated = sum(if_sac_treated), 
                sac_pass_cvg = sum(sac_cvg_pass))
write.csv(c_level, "C:\\Users\\reowen\\Documents\\Datasets\\FY14_cvg\\FY14_cvg_country.csv")

