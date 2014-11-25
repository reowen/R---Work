
#create MDA_support section

master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")

master_file_WP <- master_file[master_file$fiscal_year == 2015, ]
master_file_WP <- master_file_WP[master_file_WP$reporting_period == "Work Planning", ]
master_file_WP <- master_file_WP[master_file_WP$district_name != "NULL", ]

master_file_WP <- data.frame(master_file_WP$country_name, master_file_WP$region_name, 
                             master_file_WP$district_name, master_file_WP$district_global_task_force_id, 
                             master_file_WP$funding_src, master_file_WP$funding_src_r1, 
                             master_file_WP$funding_src_r2)

master_file_WP["mda_support_holder"] <- with(master_file_WP, ifelse(master_file_WP.funding_src == 1, 1, 
                                                             ifelse(master_file_WP.funding_src_r2 == 1, 1,
                                                             ifelse(master_file_WP.funding_src_r1 ==1, 1,0))))

master_file_WP["mda_support"] = ave(master_file_WP$mda_support_holder, 
                                    master_file_WP$master_file_WP.district_global_task_force_id, FUN = max)

mda_support <- data.frame(master_file_WP$master_file_WP.country_name, 
                          master_file_WP$master_file_WP.region_name, 
                          master_file_WP$master_file_WP.district_name, 
                          master_file_WP$master_file_WP.district_global_task_force_id, 
                          master_file_WP$mda_support)

mda_support <- unique(mda_support)

column_names = c("country", "region", "district", "taskforce_id", "ENVISION_MDA")
for(i in 1:length(column_names)){
  names(mda_support)[i] = column_names[i]
}

remove(master_file, master_file_WP, column_names)

#create mapping variable

mapping = read.delim("C:\\Users\\reowen\\Documents\\Offline Files\\mapping.txt")

mapping_WP <- mapping[mapping$fiscal_year == 2015, ]
mapping_WP <- mapping_WP[mapping_WP$reporting_period == "Work Planning", ]
mapping_WP <- mapping_WP[mapping_WP$district_name != "NULL", ]

mapping_WP <- data.frame(mapping_WP$country_name, mapping_WP$region_name, mapping_WP$district_name, 
                         mapping_WP$district_global_task_force_id, mapping_WP$funding_src)

mapping_WP["ENVISION_mapping"] <- with(mapping_WP, ifelse(mapping_WP.funding_src == 1, 1, 0))

mapping_WP <- data.frame(mapping_WP$mapping_WP.country_name, 
                          mapping_WP$mapping_WP.region_name, 
                          mapping_WP$mapping_WP.district_name, 
                          mapping_WP$mapping_WP.district_global_task_force_id, 
                          mapping_WP$ENVISION_mapping)

mapping <- unique(mapping_WP)

column_names = c("country", "region", "district", "taskforce_id", "ENVISION_mapping")
for(i in 1:length(column_names)){
  names(mapping)[i] = column_names[i]
}

remove(mapping_WP, column_names)

#set up the M&E tab


me = read.delim("C:\\Users\\reowen\\Documents\\Offline Files\\me.txt")

me_WP <- me[me$fiscal_year == 2015, ]
me_WP <- me_WP[me_WP$reporting_period == "Work Planning", ]
me_WP <- me_WP[me_WP$funding_src == 1, ]
me_WP <- me_WP[me_WP$district_name != "NULL", ]

DSAs = c("LF baseline sentinel site", "LF midterm sentinel site", "LF midterm spot check", "LF Pre-TAS sentinel site", 
         "LF Pre-TAS spot-check site", "LF TAS: Stop MDA", "LF TAS: Post-MDA Surveillance I", "LF TAS: Post-MDA Surveillance II", 
         "Oncho epidemiological assessment", "Schisto sentinel site", "Schisto evaluation", "STH sentinel site", 
         "STH evaluation", "Trachoma impact survey", "Post-MDA surveillance")

me_WP["ENVISION_dsa"] = 0
me_WP[((me_WP$assessment_type %in% DSAs)==TRUE), "ENVISION_dsa"] = 1

me_WP = me_WP[me_WP$ENVISION_dsa == 1, ]

me_WP = data.frame(me_WP$country_name, 
                   me_WP$region_name, 
                   me_WP$district_name, 
                   me_WP$district_global_task_force_id, 
                   me_WP$ENVISION_dsa)

dsa_support = unique(me_WP)

column_names = c("country", "region", "district", "taskforce_id", "ENVISION_dsa")

for(i in 1:length(column_names)){
  names(dsa_support)[i] = column_names[i]
}

remove(me, me_WP, DSAs, column_names)

#Next, Merge the data

FY15_CountryMap_data = merge(mapping, mda_support, by="taskforce_id", all = TRUE)
FY15_CountryMap_data = merge(FY15_CountryMap_data, dsa_support, by="taskforce_id", all=TRUE)

FY15_CountryMap_data = data.frame(FY15_CountryMap_data$country.y, 
                                  FY15_CountryMap_data$region.y, 
                                  FY15_CountryMap_data$district.y, 
                                  FY15_CountryMap_data$taskforce_id, 
                                  FY15_CountryMap_data$ENVISION_mapping, 
                                  FY15_CountryMap_data$ENVISION_MDA, 
                                  FY15_CountryMap_data$ENVISION_dsa)

column_names = c("country", "region", "district", "taskforce_id", "ENVISION_mapping", "ENVISION_MDA", 
                 "ENVISION_dsa")

for(i in 1:length(column_names)){
  names(FY15_CountryMap_data)[i] = column_names[i]
}

FY15_CountryMap_data = FY15_CountryMap_data[ order(FY15_CountryMap_data[,1], FY15_CountryMap_data[,2], FY15_CountryMap_data[,3]), ]

#AFTER MERGING, REPLACE THE NULL VALUES WITH ZEROES BEFORE WRITING TO A CSV FILE
FY15_CountryMap_data[(is.na(FY15_CountryMap_data$ENVISION_mapping) == TRUE), "ENVISION_mapping"] = 0
FY15_CountryMap_data[(is.na(FY15_CountryMap_data$ENVISION_MDA) == TRUE), "ENVISION_MDA"] = 0
FY15_CountryMap_data[(is.na(FY15_CountryMap_data$ENVISION_dsa) == TRUE), "ENVISION_dsa"] = 0

write.csv(FY15_CountryMap_data, "C:\\Users\\reowen\\Documents\\ENVISION\\Workplanning\\Mapping Task\\FY15_WorkPlan_Country_Maps--RAW.csv")

remove(dsa_support, mapping, mda_support, column_names, i)