master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#####################
#Create the raw file#
#####################

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "disease_distribution", "persons_at_risk", 
             "funding_src", "persons_targeted_all_funding", "persons_targeted_usaid_funding", 
             "persons_treated_all_funding", "persons_treated_usaid_funding")

map_ref = data.frame(master_file[,colnames[1]])
for(i in 2:length(colnames)){
  map_ref[colnames[i]] = master_file[,colnames[i]]
}

for(i in 1:length(colnames)){
  names(map_ref)[i] = colnames[i]
}

for(i in 9:length(colnames)){
  map_ref[, colnames[i]] = as.numeric(as.character(map_ref[, colnames[i]]))
}

for(i in 9:length(colnames)){
  map_ref[is.na(map_ref[,colnames[i]]) == TRUE, colnames[i]] = 0
}

ENV_countries = c("Uganda", "Mali", "Haiti", "Cameroon", "Guinea", "Senegal", "Tanzania", "Nigeria", "Nepal", "Mozambique", 
                  "Benin", "Indonesia")

# map_ref = map_ref[(map_ref$reporting_period == "2nd SAR (October-September)" 
#                    & map_ref$fiscal_year == 2013 
#                    & map_ref$disease == "LF"),]
# 
# map_ref = subset(map_ref, country_name %in% ENV_countries)

map_ref = map_ref[(map_ref$reporting_period == "2nd SAR (October-September)" 
                   & map_ref$fiscal_year == 2013 
                   & map_ref$disease == "LF" 
                   & (map_ref$country_name %in% ENV_countries)),]

write.csv(map_ref, "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_ref.csv")

#####################
#Create Epi Cvg File#
#####################

epicols = c("country_name", "region_name", "district_name", "district_global_task_force_id",
            "persons_at_risk", "persons_treated_all_funding", "persons_treated_usaid_funding")

epi_cvg = data.frame(map_ref[,epicols[1]])

for(i in 2:length(epicols)){
  epi_cvg[epicols[i]] = map_ref[,epicols[i]]
}

for(i in 1:length(epicols)){
  names(epi_cvg)[i] = epicols[i]
}


#######################
#Code the Epi Coverage#
#######################


epi_cvg["epi_cvg_usaid"] = NA
epi_cvg[, "epi_cvg_usaid"] = round((epi_cvg[,"persons_treated_usaid_funding"] / epi_cvg[,"persons_at_risk"]), digits=4)

ecu = epi_cvg$epi_cvg_usaid
epi_cvg[is.na(ecu),"epi_cvg_usaid"] = 999


# epi_cvg["epi_cvg_all"] = NA
# epi_cvg[, "epi_cvg_all"] = round((epi_cvg[,"persons_treated_all_funding"] / epi_cvg[,"persons_at_risk"]), digits=4)
# 
# eca = epi_cvg$epi_cvg_all
# epi_cvg[is.na(eca),"epi_cvg_all"] = 999



#######################################
## LF Epi Coverage codes description  #
## Legend                             #
## Code 1	Good coverage (>=65%)       #
## Code 2	Fair coverage (55-64%)      #
## Code 3	Poor coverage (<55%)        #
#######################################



ecu = epi_cvg$epi_cvg_usaid
epi_cvg["LF_epi_cvg_usaid"] = NA

epi_cvg[(ecu >= 0.65 & ecu < 999), "LF_epi_cvg_usaid"] = 1
epi_cvg[(ecu >= 0.55 & ecu < 0.65), "LF_epi_cvg_usaid"] = 2
epi_cvg[(ecu < 0.55), "LF_epi_cvg_usaid"] = 3
epi_cvg[(ecu == 0), "LF_epi_cvg_usaid"] = 4
epi_cvg[(ecu == 999), "LF_epi_cvg_usaid"] = 5
epi_cvg[(ecu == 1), "LF_epi_cvg_usaid"] = 999


# eca = epi_cvg$epi_cvg_all
# epi_cvg["LF_epi_cvg_all"] = NA
# 
# epi_cvg[(eca >= 0.65 & eca < 999), "LF_epi_cvg_all"] = 1
# epi_cvg[(eca >= 0.55 & eca < 0.65), "LF_epi_cvg_all"] = 2
# epi_cvg[(eca < 0.55), "LF_epi_cvg_all"] = 3
# epi_cvg[(eca == 0), "LF_epi_cvg_all"] = 4
# epi_cvg[(eca == 999), "LF_epi_cvg_all"] = 5



############################
#Make the dataset to export#
############################

lf_epi_cvg = epi_cvg[,-(5:8)]

write.csv(epi_cvg, "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\epi_draft.csv")
write.csv(lf_epi_cvg, "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\lf_epi_final.csv")



