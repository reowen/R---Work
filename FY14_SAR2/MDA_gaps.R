master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#####################
#Create the raw file#
#####################

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "disease_distribution", "funding_src",
             "people_living_mda_acheived", "year_of_next_planned_tas")

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

#note this sets NULL funding source codes to 0
for(i in 9:length(colnames)){
  map_ref[is.na(map_ref[,colnames[i]]) == TRUE, colnames[i]] = 0
}

map_ref = map_ref[(map_ref$reporting_period == "Work Planning" & map_ref$fiscal_year == 2015),]
write.csv(map_ref, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\map_ref.csv")

##For LF:
# Category 1: funding source 1
# Category 2: Funding source 2, 3, 4, 5, 6
# Category 3: Disease distribution NS
# Category 4: Disease distribution 0
# Category 5: Disease distribution 100
# Category 6: Disease distribution code 1, no funding source (or funding source 0)
# Category 7: Disease distribution M
# Category 8: Disease distribution "Pending"
# Category 9: No data available

lf_ref = map_ref[(map_ref$disease == "LF"),]

fs = lf_ref$funding_src
dd = lf_ref$disease_distribution
tas = lf_ref$year_of_next_planned_tas

lf_ref['category'] = 9

lf_ref[(dd == 1 & fs == 0), 'category'] = 6
lf_ref[(dd == 'NS'), 'category'] = 3
lf_ref[(dd == 0), 'category'] = 4
lf_ref[(dd == 'M'), 'category'] = 7
lf_ref[(dd == 'Pending' | tas == 2015 | tas == 2016), 'category'] = 8
# categories 1, 2, 5 must dominate other categories where
# a district meets multiple criteria
lf_ref[(fs == 1), 'category'] = 1
lf_ref[(fs >= 2 & fs <= 6), 'category'] = 2
lf_ref[(dd == 100), 'category'] = 5

# lf_weird = lf_ref[((dd == 0 | dd == 100 | dd == 'NS' | dd == 'M' | dd == 'Pending') & fs != 0),]
# write.csv(lf_weird, 
#           "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\lf_weird.csv")

lf_ref_merge = lf_ref[, -(5:7)]
names(lf_ref_merge)[5] = 'dd_LF'
names(lf_ref_merge)[6] = 'fund_src_LF'
names(lf_ref_merge)[8] = 'yr_TAS'
names(lf_ref_merge)[9] = 'category_LF'
lf_ref_merge = lf_ref_merge[,-(7)]

##For Trachoma
# Category 1: Funding source 1
# Category 2: Funding source 2, 3, 4, 5, 6
# Category 3: Disease distribution NS
# Category 4: Disease distribution 0, 2, 3 (only include if from Trachoma baseline survey)
# Category 5: Disease distribution 100, 1, 2, 3 -- and have population included in "people_living_mda_acheived"
#               -Include district if it has been targeted for treatment
#               -Exclude if code 3 is from Trachoma impact survey
# Category 6: Disease distribution 1, 4, 5 -- no funding source (or funding source 0)
# Category 7: Disease distribution M
# Category 8: Disease distribution 'Pending'
# Category 9: No data available

tra_ref = map_ref[(map_ref$disease == "Trachoma"),]

fs = tra_ref$funding_src
dd = tra_ref$disease_distribution
plma = tra_ref$people_living_mda_acheived

tra_ref['category'] = 9

tra_ref[((dd == 1 | dd == 4 | dd == 5) & fs == 0), 'category'] = 6
tra_ref[(dd == 'NS'), 'category'] = 3
tra_ref[(dd == 0 | dd == 2 | dd == 3), 'category'] = 4
tra_ref[(dd == 'M'), 'category'] = 7
tra_ref[(dd == 'Pending'), 'category'] = 8
# Categories 1 and 2 must dominate other categories where a district
# meets multiple criteria
tra_ref[(fs == 1), 'category'] = 1
tra_ref[(fs >= 2 & fs <= 6), 'category'] = 2
tra_ref[((dd == 100 | dd == 1 | dd == 2 | dd == 3) & (plma > 0)), 'category'] = 5

# tra_weird = tra_ref[((dd == 0 | dd == 2 | dd == 3 | dd == 'NS' | dd == 'M' | dd == 'Pending') & fs !=0),]
# write.csv(tra_weird, 
#           "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\tra_weird.csv")

tra_ref_merge = tra_ref[,-(5:7)]
names(tra_ref_merge)[5] = 'dd_Tra'
names(tra_ref_merge)[6] = 'fund_src_Tra'
names(tra_ref_merge)[7] = 'persons_living_stop_mda'
tra_ref_merge = tra_ref_merge[,-8]
names(tra_ref_merge)[8] = 'category_Tra'


#################################################
###MERGE THE DATASETS, DROP EXTRANEOUS COLUMNS###
#################################################

mda_gap = merge(lf_ref_merge, tra_ref_merge, by="district_global_task_force_id", all = TRUE)
mda_gap_final = mda_gap[,-(9:11)]

write.csv(mda_gap_final, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\mda_gap_final.csv")
