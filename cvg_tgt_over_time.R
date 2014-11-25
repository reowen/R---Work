master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#Create the raw file

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "funding_src",
             "funding_src_r1", "funding_src_r2", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1",
             "persons_targeted_usaid_funding_r2", "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", 
             "persons_treated_usaid_funding_r2", "sac_at_risk", "sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2", 
             "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2", "sac_targeted_with_usaid_support", 
             "sac_treated_usaid_funding")

cvg_targets = master_file[, colnames]

for(i in 8:length(colnames)){
  cvg_targets[, colnames[i]] = as.numeric(as.character(cvg_targets[, colnames[i]]))
}

#so that max() function will work
for(i in 8:length(colnames)){
  cvg_targets[is.na(cvg_targets[,colnames[i]]) == TRUE, colnames[i]] = 0
}

cvg_targets["ENVISION_support"] <- with(cvg_targets, ifelse(funding_src == 1 | funding_src == 99, 1, 
                                                            ifelse(funding_src_r1 == 1 | funding_src_r1 == 99, 1, 
                                                                   ifelse(funding_src_r2 == 1 | funding_src_r2 == 99, 1, 0))))

cvg_targets["number_targeted"] = 0
cvg_targets["number_treated"] = 0

cvg_targets[(cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011), "number_targeted"] = apply(cvg_targets[(cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011),19:20], 1, max)
cvg_targets[(cvg_targets$disease == "Schisto" & cvg_targets$fiscal_year > 2011), "number_targeted"] = cvg_targets[(cvg_targets$disease == "Schisto" & cvg_targets$fiscal_year > 2011),"sac_targeted_with_usaid_support"]
cvg_targets[(cvg_targets$disease != "STH" & cvg_targets$disease != "Schisto" & cvg_targets$fiscal_year > 2011), "number_targeted"] = apply(cvg_targets[(cvg_targets$disease != "STH" & cvg_targets$disease != "Schisto" & cvg_targets$fiscal_year > 2011),12:14], 1, max)

cvg_targets[(cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011), "number_treated"] = apply(cvg_targets[(cvg_targets$disease == "STH"),21:22], 1, max)
cvg_targets[(cvg_targets$disease == "Schisto" & cvg_targets$fiscal_year > 2011), "number_treated"] = cvg_targets[(cvg_targets$disease == "Schisto"),"sac_treated_usaid_funding"]
cvg_targets[(cvg_targets$disease != "STH" & cvg_targets$disease != "Schisto" & cvg_targets$fiscal_year > 2011), "number_treated"] = apply(cvg_targets[(cvg_targets$disease != "STH" & cvg_targets$disease != "Schisto" & cvg_targets$fiscal_year > 2011),15:17], 1, max)

#for the NTDCP numbers
cvg_targets[(cvg_targets$fiscal_year < 2012), "number_targeted"] = apply(cvg_targets[(cvg_targets$fiscal_year < 2012),12:14], 1, max)

cvg_targets[(cvg_targets$fiscal_year < 2012), "number_treated"] = apply(cvg_targets[(cvg_targets$fiscal_year < 2012),15:17], 1, max)


#flag if it was treated with ENVISION support
cvg_targets["treated"] = 0
cvg_targets[((cvg_targets$number_treated > 0) & (cvg_targets$ENVISION_support == 1)), "treated"] = 1


#If not adjacent, the code below would have worked
#cvg_targets["number_treated"] = apply(data.frame(cvg_targets[,15], cvg_targets[,16], cvg_targets[,17]), 1, max)

cvg_targets = cvg_targets[,-(9:17)]
cvg_targets = cvg_targets[,-(10:15)]
#cvg_targets = cvg_targets[(cvg_targets$number_treated > 0 & cvg_targets$ENVISION_support == 1), ]

#calculate program and epi coverage
#cvg_targets$number_treated = as.numeric(cvg_targets$number_treated)
#cvg_targets$number_targeted = as.numeric(cvg_targets$number_targeted)

#class(cvg_targets$persons_at_risk)
#cvg_targets$persons_at_risk = as.numeric(as.character(cvg_targets$persons_at_risk))
#cvg_targets$sac_at_risk = as.numeric(as.character(cvg_targets$sac_at_risk))

#program coverage and epi cvg
cvg_targets["epi_cvg"] = 0
cvg_targets["prg_cvg"] = round((cvg_targets[,"number_treated"] / cvg_targets[,"number_targeted"]), digits=4)
cvg_targets[(cvg_targets$disease != "Schisto" & cvg_targets$disease != "STH" & cvg_targets$fiscal_year > 2011), "epi_cvg"] = round((cvg_targets[(cvg_targets$disease != "Schisto" & cvg_targets$disease != "STH" & cvg_targets$fiscal_year > 2011),"number_treated"] / cvg_targets[(cvg_targets$disease != "Schisto" & cvg_targets$disease != "STH" & cvg_targets$fiscal_year > 2011),"persons_at_risk"]), digits=4)
cvg_targets[(cvg_targets$disease == "Schisto" | cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011), "epi_cvg"] = round((cvg_targets[(cvg_targets$disease == "Schisto" | cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011),"number_treated"] / cvg_targets[(cvg_targets$disease == "Schisto" | cvg_targets$disease == "STH" & cvg_targets$fiscal_year > 2011),"sac_at_risk"]), digits=4)

#NTDCP data
cvg_targets[(cvg_targets$fiscal_year < 2012), "epi_cvg"] = round((cvg_targets[(cvg_targets$fiscal_year < 2012),"number_treated"] / cvg_targets[(cvg_targets$fiscal_year < 2012),"persons_at_risk"]), digits=4)

cvg_targets[is.na(cvg_targets$prg_cvg) == TRUE, "prg_cvg"] = 0
cvg_targets[is.na(cvg_targets$epi_cvg) == TRUE, "epi_cvg"] = 0


#then, create a flag if it's above or below the threshold

cvg_targets["prg_target_not_met"] = 1
cvg_targets["epi_target_not_met"] = 1

diseases = levels(cvg_targets$disease)

for(i in 1:2){
  cvg_targets[((cvg_targets$disease == diseases[i]) & (cvg_targets$prg_cvg >= 0.8)), "prg_target_not_met"] = 0
  cvg_targets[((cvg_targets$disease == diseases[i]) & (cvg_targets$epi_cvg >= 0.65)), "epi_target_not_met"] = 0
}

for(i in 3:4){
  cvg_targets[((cvg_targets$disease == diseases[i]) & (cvg_targets$prg_cvg >= 0.8)), "prg_target_not_met"] = 0
  cvg_targets[((cvg_targets$disease == diseases[i]) & (cvg_targets$epi_cvg >= 0.75)), "epi_target_not_met"] = 0
}

cvg_targets[((cvg_targets$disease == "Trachoma") & (cvg_targets$prg_cvg >= 0.8)), "prg_target_not_met"] = 0
cvg_targets[((cvg_targets$disease == "Trachoma") & (cvg_targets$epi_cvg >= 0.8)), "epi_target_not_met"] = 0

cvg_targets[cvg_targets$treated == 0, "prg_target_not_met"] = 0
cvg_targets[cvg_targets$treated == 0, "epi_target_not_met"] = 0


cvg_all_FY = cvg_targets
cvg_all_FY = cvg_all_FY[(cvg_all_FY$reporting_period != "Work Planning" & cvg_all_FY$reporting_period != "Mid-year data submission"),]

cvg_all_FY = cvg_all_FY[c("country_name", "region_name", "district_name",	"district_global_task_force_id", 
                          "fiscal_year",	"reporting_period",	"disease",	"ENVISION_support",	"treated",
                          "persons_at_risk",	"sac_at_risk",	"number_targeted",	"number_treated",
                          "prg_cvg",	"epi_cvg",	"prg_target_not_met",	"epi_target_not_met")]

  


write.csv(cvg_all_FY, "C:\\Users\\reowen\\Documents\\ENVISION\\Workplanning\\FY15\\Project\\Coverage Targets All FY\\master_list.csv")


