

master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#Create the raw file

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "funding_src",
             "funding_src_r1", "funding_src_r2", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1",
             "persons_targeted_usaid_funding_r2", "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", 
             "persons_treated_usaid_funding_r2")

cvg_targets = data.frame(master_file[,colnames[1]])
for(i in 2:length(colnames)){
  cvg_targets[colnames[i]] = master_file[,colnames[i]]
}

for(i in length(colnames)){
  names(cvg_targets)[i] = colnames[i]
}

for(i in 8:length(colnames)){
  cvg_targets[cvg_targets[,colnames[i]] == "NULL", colnames[i]] = 0
}

cvg_targets = cvg_targets[(cvg_targets$fiscal_year == 2013 & cvg_targets$reporting_period == "2nd SAR (October-September)"),]

cvg_targets["ENVISION_support"] <- with(cvg_targets, ifelse(funding_src == 1, 1, 
                                                            ifelse(funding_src_r1 == 1, 1, 
                                                                   ifelse(funding_src_r2 == 1, 1, 0))))

cvg_targets["number_targeted"] = apply(cvg_targets[,12:14], 1, max)
cvg_targets["number_treated"] = apply(cvg_targets[,15:17], 1, max)

#If not adjacent, the code below would have worked
#cvg_targets["number_treated"] = apply(data.frame(cvg_targets[,15], cvg_targets[,16], cvg_targets[,17]), 1, max)

cvg_targets = cvg_targets[,-(9:17)]
#cvg_targets = cvg_targets[(cvg_targets$number_treated > 0 & cvg_targets$ENVISION_support == 1), ]

#calculate program and epi coverage
cvg_targets$number_treated = as.numeric(cvg_targets$number_treated)
cvg_targets$number_targeted = as.numeric(cvg_targets$number_targeted)

#class(cvg_targets$persons_at_risk)
cvg_targets$persons_at_risk = as.numeric(as.character(cvg_targets$persons_at_risk))

#program coverage
cvg_targets["prg_cvg"] = round((cvg_targets[,"number_treated"] / cvg_targets[,"number_targeted"]), digits=4)
cvg_targets["epi_cvg"] = round((cvg_targets[,"number_treated"] / cvg_targets[,"persons_at_risk"]), digits=4)

cvg_targets[is.na(cvg_targets$prg_cvg) == TRUE, "prg_cvg"] = 0
cvg_targets[is.na(cvg_targets$epi_cvg) == TRUE, "epi_cvg"] = 0


#then, create a flag if it's above or below the threshold

cvg_targets["treated"] = 0
cvg_targets[((cvg_targets$number_treated > 0) & (cvg_targets$ENVISION_support == 1)), "treated"] = 1

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


write.csv(cvg_targets, "C:\\Users\\reowen\\Documents\\ENVISION\\Workplanning\\FY15\\Project\\Coverage Targets Table\\master_list.csv")
