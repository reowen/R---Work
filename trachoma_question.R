master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


colnames = c("country_name", "region_name", "district_name", 
             "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease",
             "disease_distribution",
             "persons_treated_usaid_funding", 
             "persons_treated_all_funding")

raw = master_file[master_file$disease == 'Trachoma', colnames]

# for(i in 1:length(colnames)){
#   names(raw)[i] = colnames[i]
# }

for(i in 9:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 9:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}

#code treated=1 if treated
raw['treated'] = 0
raw[(raw$persons_treated_all_funding > 0), 'treated'] = 1

raw['ever_treated'] = ave(raw[,'treated'],
                          raw[,'district_global_task_force_id'], 
                          FUN = max)


keep = c("country_name", "region_name", "district_name", 
         "disease_distribution", 'ever_treated')

tra_2015 = raw[(raw$fiscal_year == 2015), keep]
tra_2015['counts'] = 0
tra_2015[tra_2015$disease_distribution == 3 & tra_2015$ever_treated == 0, 
         'counts'] = 1

write.csv(tra_2015, "C:\\Users\\reowen\\Documents\\Datasets\\tra_2015.csv")

#2014

tra_2014 = raw[(raw$fiscal_year == 2014 & raw$reporting_period == '2nd SAR (October-September)'),
               keep]
tra_2014['counts'] = 0
tra_2014[tra_2014$disease_distribution == 3 & tra_2014$ever_treated == 0, 
         'counts'] = 1

write.csv(tra_2014, "C:\\Users\\reowen\\Documents\\Datasets\\tra_2014.csv")