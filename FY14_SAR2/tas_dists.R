master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

#####################
#Create the raw file#
#####################

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "year_of_next_planned_tas", 
             "year_of_planned_trachoma_impact_survey")

map_ref = master_file[, colnames]

tas = map_ref$year_of_next_planned_tas
map_ref = map_ref[(tas == 2015 | tas == 2016),]
map_ref = map_ref[(map_ref$reporting_period == "Work Planning" & map_ref$fiscal_year == 2015 & map_ref$disease == "LF"),]

map_ref = map_ref[,-(4:7)]
map_ref = map_ref[,-5]
write.csv(map_ref, 
          "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\tas_districts.csv")


# colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
#              "fiscal_year", "reporting_period", "disease", "year_of_next_planned_tas", 
#              "year_of_planned_trachoma_impact_survey")
# 
# map_ref = master_file[, colnames]
# 
# tis = map_ref$year_of_planned_trachoma_impact_survey
# map_ref = map_ref[(tis == 2015 | tis == 2016),]
# map_ref = map_ref[(map_ref$reporting_period == "Work Planning" & map_ref$fiscal_year == 2015 & map_ref$disease == "LF"),]
# 
# map_ref = map_ref[,-(4:7)]
# map_ref = map_ref[,-5]
# write.csv(map_ref, 
#           "C:\\Users\\reowen\\Documents\\ENVISION\\SAR\\FY14 SAR 2\\Project\\Maps\\R scripts\\tas_districts.csv")
