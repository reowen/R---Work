

master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#Create the raw file

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "funding_src",
             "funding_src_r1", "funding_src_r2", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1",
             "persons_targeted_usaid_funding_r2", "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", 
             "persons_treated_usaid_funding_r2")

raw = master_file[, colnames]

for(i in length(colnames)){
  names(raw)[i] = colnames[i]
}

for(i in 8:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 8:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
# rm(colnames)


cvg_targets = raw[(raw$fiscal_year == 2013 & raw$reporting_period == "2nd SAR (October-September)"),]



cvg_targets["ENVISION_support"] <- with(cvg_targets, ifelse(funding_src == 1, 1, 
                                                            ifelse(funding_src_r1 == 1, 1, 
                                                                   ifelse(funding_src_r2 == 1, 1, 0))))

cvg_targets["number_targeted"] = apply(cvg_targets[,12:14], 1, max)
cvg_targets["number_treated"] = apply(cvg_targets[,15:17], 1, max)
cvg_targets['treated'] = with(cvg_targets, ifelse(number_treated > 0, 1, 0))

#remove superfluous columns
cvg_targets = cvg_targets[,-(9:17)]
cvg_targets = cvg_targets[,-(5:6)]
cvg_targets = cvg_targets[,-(6)]

#calculate program coverage

#program coverage
cvg_targets['prg_cvg'] = 0
cvg_targets[(cvg_targets$number_targeted > 0), 'prg_cvg'] = round((cvg_targets[(cvg_targets$number_targeted > 0),"number_treated"] / cvg_targets[(cvg_targets$number_targeted > 0),"number_targeted"]), digits=4)

cvg_targets['over_80'] = 0
cvg_targets[(cvg_targets$prg_cvg >= 0.8), 'over_80'] = 1

write.csv(cvg_targets, "C:\\Users\\reowen\\Documents\\Datasets\\FY13_prg_cvg_district.csv")

#Condense to country-level
keep_cols = c("country_name", "region_name", "district_name", "disease", "treated", "over_80")

country_cvg = cvg_targets[, keep_cols]

groups = c(country_)

country_cvg['total_treated'] = ave(country_cvg[,'treated'], country_cvg[,'country_name'], 
                                   country_cvg[,'disease'], FUN = sum)

country_cvg['total_over_80'] = ave(country_cvg[,'over_80'], country_cvg[,'country_name'], 
                                   country_cvg[,'disease'], FUN = sum)

keep_cols = c("country_name", "disease", 'total_treated', 'total_over_80')
country_cvg = country_cvg[, keep_cols]
country_cvg['percent_over_80'] = with(country_cvg, ifelse(total_treated > 0, (total_over_80 / total_treated), NA))

#drop duplicates
country_cvg = unique(country_cvg)

table = country_cvg[, c('country_name', 'disease', 'percent_over_80')]

table = reshape(table, 
                timevar = 'disease', 
                idvar = 'country_name', 
                direction = 'wide')


write.csv(country_cvg, "C:\\Users\\reowen\\Documents\\Datasets\\FY13_prg_cvg_country.csv")

