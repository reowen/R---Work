
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#########################
#Create the raw file: LF#
#########################

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "disease_distribution", "objective_of_next_planned_tas", 
             "month_of_next_planned_tas", "year_of_next_planned_tas", "year_mda_started", 
             "mdas_completed", "consecutive_mdas_completed", "year_determined_achieved_criteria_mda", "persons_treated_usaid_funding")

envision = c("Benin", "Cameroon", "Guinea", "Haiti", "Indonesia", "Nepal", "Senegal", "Uganda", "Mali")


lf = master_file[, colnames]

for(i in 1:length(colnames)){
  names(lf)[i] = colnames[i]
}

lf[, colnames[16]] = as.numeric(as.character(lf[, colnames[16]]))

lf[is.na(lf[,colnames[16]]) == TRUE, colnames[16]] = 0

lf = lf[lf$disease == "LF", ]


lf["treated_holder"] = 0
lf[(lf$persons_treated_usaid_funding > 0), "treated_holder"] = 1

lf["ever_treated"] = ave(lf$treated_holder, lf$district_global_task_force_id, FUN = max)
lf["ENVISION"] = with(lf, ifelse((lf$country_name %in% envision) == TRUE, 1, 0))

lf = lf[((lf$fiscal_year == 2015 & lf$ENVISION == 1) | (lf$fiscal_year == 2014 & lf$ENVISION == 0)), ]

lf = lf[,-(5:7)]
lf = lf[,-(13:14)]

write.csv(lf, "C:\\Users\\reowen\\Documents\\ENVISION\\Various Tasks\\Crazy cleaning task\\lf.csv")

##########################
#Create the raw file: TRA#
##########################

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "disease_distribution", 
             "achieved_criteria_stop_district_level_mda", "month_of_planned_trachoma_impact_survey", 
             "year_of_planned_trachoma_impact_survey", "year_mda_started", "mdas_completed", 
             "consecutive_mdas_completed", "year_determined_achieved_criteria_mda", "persons_treated_usaid_funding")


trachoma = data.frame(master_file[,colnames[1]])
for(i in 2:length(colnames)){
  trachoma[colnames[i]] = master_file[,colnames[i]]
}

for(i in 1:length(colnames)){
  names(trachoma)[i] = colnames[i]
}

trachoma$persons_treated_usaid_funding = as.numeric(as.character(trachoma$persons_treated_usaid_funding))

#trachoma$persons[, colnames[16]] = as.numeric(as.character(trachoma[, colnames[16]]))

trachoma[is.na(trachoma$persons_treated_usaid_funding) == TRUE, "persons_treated_usaid_funding"] = 0

trachoma = trachoma[trachoma$disease == "Trachoma", ]


trachoma["treated_holder"] = 0
trachoma[(trachoma$persons_treated_usaid_funding > 0), "treated_holder"] = 1

trachoma["ever_treated"] = ave(trachoma$treated_holder, trachoma$district_global_task_force_id, FUN = max)

trachoma["ENVISION"] = with(trachoma, ifelse((trachoma$country_name %in% envision) == TRUE, 1, 0))

trachoma = trachoma[((trachoma$fiscal_year == 2015 & trachoma$ENVISION == 1) | (trachoma$fiscal_year == 2014 & trachoma$ENVISION == 0)), ]

trachoma = trachoma[,-(5:7)]
trachoma = trachoma[,-(13:14)]

write.csv(trachoma, "C:\\Users\\reowen\\Documents\\ENVISION\\Various Tasks\\Crazy cleaning task\\trachoma.csv")


