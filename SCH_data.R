

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

treated[(treated$funding_src == 1 | treated$funding_src == 99), 'RTI_treated'] = ave(treated[(treated$funding_src == 1 | treated$funding_src == 99),"persons_treated_usaid_funding"], 
                                                                                     treated[(treated$funding_src == 1 | treated$funding_src == 99),"country_name"], 
                                                                                     treated[(treated$funding_src == 1 | treated$funding_src == 99),"fiscal_year"], 
                                                                                     FUN = sum)

treated[, 'RTI_treated'] = ave(treated[,"RTI_treated"], 
                               treated[,"country_name"], 
                               treated[,"fiscal_year"], 
                               FUN = max)

treated[(treated$country_name == 'Burkina Faso' & treated$fiscal_year == 2011) | (treated$country_name == 'Niger' & treated$fiscal_year == 2011), 
        'RTI_treated'] = 0


write.csv(treated, "C:\\Users\\reowen\\Documents\\Datasets\\SCH_treatments.csv")

rm(master_file, raw, treated, colnames, i, keep)

####################
##Schisto DSA data##
####################

me_master = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\me_master.csv")

colnames = c("country_name", "region_name", "district_name", "fiscal_year", 
             "assessment_type", "funding_src", "assessment_completed")

