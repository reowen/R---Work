setwd("Coding/R_Scripts/cvg_comments")

#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#columns used in the script
colnames = c("country_name", "region_name", "district_name", "fiscal_year", "reporting_period", 
             "disease", "mda_comments", "persons_at_risk", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", 
             "persons_targeted_usaid_funding_r2", "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", 
             "persons_treated_usaid_funding_r2", "sac_at_risk", "sac_targeted_with_usaid_support", 
             "sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2", "sac_treated_usaid_funding", 
             "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw = master_file[master_file$fiscal_year > 2011 & master_file$reporting_period == "2nd SAR (October-September)", colnames]
#free up some memory (this takes 45 mb)
rm(master_file)

for(i in 8:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 8:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
rm(colnames, i)

raw['prg_cvg'] <- raw[,"persons_treated_usaid_funding"] / raw[,"persons_targeted_usaid_funding"]
raw['prg_cvg_r1'] <- raw[,"persons_treated_usaid_funding_r1"] / raw[,"persons_targeted_usaid_funding_r1"]
raw['prg_cvg_r2'] <- raw[,"persons_treated_usaid_funding_r2"] / raw[,"persons_targeted_usaid_funding_r2"]

raw['under_80'] <- ifelse(raw$prg_cvg < 0.8 & raw$prg_cvg > 0, 1, 0)
raw['under_80_r1'] <- ifelse(raw$prg_cvg_r1 < 0.8 & raw$prg_cvg_r1 > 0, 1, 0)
raw['under_80_r2'] <- ifelse(raw$prg_cvg_r2 < 0.8 & raw$prg_cvg_r2 > 0, 1, 0)

raw['over_100'] <- ifelse(raw$prg_cvg > 1, 1, 0)
raw['over_100_r1'] <- ifelse(raw$prg_cvg_r1 > 1, 1, 0)
raw['over_100_r2'] <- ifelse(raw$prg_cvg_r2 > 1, 1, 0)

keep_cols = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
              "prg_cvg", "under_80", "over_100", "prg_cvg_r1", "under_80_r1", "over_100_r1", 
              "prg_cvg_r2", "under_80_r2", "over_100_r2", "mda_comments")

cvg_data <- raw[, keep_cols]
write.csv(cvg_data, "cvg_comments.csv")


status = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\disease_status.csv")

yrs <- c("FY13 (October 2012 - September 2013)", "FY14 (October 2013 - September 2014)", "FY12 (October 2011 - September 2012)")
status_out <- status[(status$workbook_yr %in% yrs & status$reporting_period_id == 2), ]

write.csv(status_out, "status.csv")

