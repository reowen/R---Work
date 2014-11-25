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

map_ref = map_ref[(map_ref$reporting_period == "Work Planning" & map_ref$fiscal_year == 2015),]
write.csv(map_ref, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_ref.csv")

###############################
#Create the mapping gaps table#
###############################

mgcols = c("country_name", "region_name", "district_name", "district_global_task_force_id",
           "disease", "disease_distribution")

mapping_gap = data.frame(map_ref[(map_ref$disease == "LF" | map_ref$disease == "Trachoma"),colnames[1]])

for(i in 2:length(mgcols)){
  mapping_gap[mgcols[i]] = map_ref[(map_ref$disease == "LF" | map_ref$disease == "Trachoma"),mgcols[i]]
}

for(i in 1:length(mgcols)){
  names(mapping_gap)[i] = mgcols[i]
}


###################################
#Code the LF mapping gaps variable#
###################################

# Code 1  -- Take districts with disease distribution code '1' 'Pending', for LF 
#            '1, 2, 3, 4, 5, Pending' for Trachoma
# Code 2	-- Take districts with disease distribution code 0, NS, 100 for LF and trachoma
# Code 3	-- Take districts with disease distribution code 'M'

mglf = mapping_gap[mapping_gap$disease == "LF",]
mglf = mglf[,-(5)]

dist = mglf$disease_distribution
disease = mglf$disease

mglf["LF_mapping_gaps"] = dist

mglf[(dist == 1 | dist == "Pending"), "LF_mapping_gaps"] = 1

mglf[(dist == 0 | dist == "NS" | dist == 100), "LF_mapping_gaps"] = 2

mglf[(dist == "M"), "LF_mapping_gaps"] = 3

mglf[(dist == "NULL"), "LF_mapping_gaps"] = 4

names(mglf)[5] = "LF_dd"

#########################################
#Code the Trachoma mapping gaps variable#
#########################################

mgtra = mapping_gap[mapping_gap$disease == "Trachoma",]
mgtra = mgtra[,-(5)]

dist = mgtra$disease_distribution
disease = mgtra$disease

mgtra["Trachoma_mapping_gaps"] = dist

mgtra[(dist == 1 | dist == "Pending" | dist == 3 | dist == 4 | dist == 5), "Trachoma_mapping_gaps"] = 1

mgtra[(dist == 0 | dist == 2 | dist == "NS" | dist == 100), "Trachoma_mapping_gaps"] = 2

mgtra[(dist == "M"), "Trachoma_mapping_gaps"] = 3

mgtra[(dist == "NULL"), "Trachoma_mapping_gaps"] = 4

names(mgtra)[5] = "Tra_dd"


# write.csv(mgtra, 
#           "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\trachoma_draft.csv")


###############
#Merge the two#
###############

map_gap = merge(mglf, mgtra, by="district_global_task_force_id", all = TRUE)
map_gap[(is.na(map_gap$Trachoma_mapping_gaps)), "Trachoma_mapping_gaps"] = 4
map_gap_final = map_gap[,-(7:9)]

write.csv(map_gap, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_gap_draft.csv")

write.csv(map_gap_final, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_gap_final.csv")
















# ###################################
# #Code the LF mapping gaps variable#
# ###################################
# 
# # Code 1  -- Take districts with disease distribution code '1' 'Pending', for LF 
# #            '1, 2, 3, 4, 5, Pending' for Trachoma
# # Code 2	-- Take districts with disease distribution code 0, NS, 100 for LF and trachoma
# # Code 3	-- Take districts with disease distribution code 'M'
# 
# dist = mapping_gap$disease_distribution
# disease = mapping_gap$disease
# 
# mapping_gap["LF_mapping_gaps"] = dist
# #mapping_gap["LF_mapping_gaps"] = mapping_gap["disease_distribution"]
# mapping_gap[((dist == 1 | dist == "Pending") & disease == "LF"), "LF_mapping_gaps"] = 1
# 
# mapping_gap[(dist == 0 | dist == "NS" | dist == 100), "LF_mapping_gaps"] = 2
# 
# mapping_gap[(dist == "M"), "LF_mapping_gaps"] = 3
# 
# mapping_gap[(disease == "Trachoma"), "LF_mapping_gaps"] = NA
# 
# #########################################
# #Code the Trachoma mapping gaps variable#
# #########################################
# mapping_gap["Trachoma_mapping_gaps"] = dist
# 
# mapping_gap[(dist == 1 | dist == "Pending" | dist == 2 | dist == 3 | dist == 4 | dist == 5), "Trachoma_mapping_gaps"] = 1
# 
# mapping_gap[(dist == 0 | dist == "NS" | dist == 100), "Trachoma_mapping_gaps"] = 2
# 
# mapping_gap[(dist == "M"), "Trachoma_mapping_gaps"] = 3
# 
# mapping_gap[(disease == "LF"), "Trachoma_mapping_gaps"] = NA
# 
# # write.csv(mapping_gap, 
# #           "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_gap.csv")
