
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#columns used in the script
colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "funding_src","funding_src_r1", "funding_src_r2", 
             "persons_at_risk", "persons_targeted_usaid_funding", "persons_targeted_all_funding", 
             "persons_targeted_usaid_funding_r1", "persons_targeted_all_funding_r1", "persons_targeted_usaid_funding_r2",
             "persons_targeted_all_funding_r2", "persons_treated_usaid_funding", "persons_treated_all_funding",
             "persons_treated_usaid_funding_r1", "persons_treated_all_funding_r1", "persons_treated_usaid_funding_r2", 
             "persons_treated_all_funding_r2", "sac_at_risk", "sac_targeted_with_usaid_support_r1", 
             "sac_targeted_with_usaid_support_r2", "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2", 
             "sac_targeted_with_usaid_support", "sac_treated_all_funding", "sac_treated_usaid_funding", 
             "sac_treated_all_funding_r1", "sac_treated_all_funding_r2")

raw = master_file[, colnames]
rm(master_file)

for(i in 11:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 11:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
rm(colnames)


#Create number targeted and treated (all support)

all_targeted = c("persons_targeted_all_funding", "persons_targeted_all_funding_r1", "persons_targeted_all_funding_r2")
all_treated = c("persons_treated_all_funding", "persons_treated_all_funding_r1", "persons_treated_all_funding_r2")

raw['targeted'] = apply(raw[, all_targeted], 1, max)
raw['treated'] = apply(raw[, all_treated], 1, max)
rm(all_targeted, all_treated)

#create number targeted and treated (usaid support)

usaid_targeted = c("persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2")
usaid_treated = c("persons_treated_usaid_funding", "persons_treated_usaid_funding_r1","persons_treated_usaid_funding_r2")

raw['number_targeted_usaid'] = apply(raw[, usaid_targeted], 1, max)
raw['number_treated_usaid'] = apply(raw[, usaid_treated], 1, max)
rm(usaid_targeted, usaid_treated)

#create SAC treated variable

sac_targeted = c("sac_targeted_with_usaid_support", "sac_targeted_with_usaid_support_r1", 
                 "sac_targeted_with_usaid_support_r2")

sac_treated = c("sac_treated_all_funding", "sac_treated_usaid_funding", 
                "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2", 
                "sac_treated_all_funding_r1", "sac_treated_all_funding_r2")

raw['sac_targeted'] = apply(raw[, sac_targeted], 1, max)
raw['sac_treated'] = apply(raw[, sac_treated], 1, max)
rm(sac_targeted, sac_treated)

#####################################################
##Condense the dataset to something more manageable##
#####################################################

keep_cols = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
              "persons_at_risk", "sac_at_risk", 'targeted', 'treated', 
              'number_targeted_usaid', 'number_treated_usaid', 'sac_targeted', 'sac_treated')

#Use only SAR2, and the above specified columns
raw_cvg = raw[raw$reporting_period == '2nd SAR (October-September)', keep_cols]
rm(raw, keep_cols)

#Code program coverage variables
raw_cvg['prg_cvg'] = round((raw_cvg[, 'treated'] / raw_cvg[, 'targeted']), digits=4)
raw_cvg['prg_cvg_usaid'] = round((raw_cvg[, 'number_treated_usaid'] / raw_cvg[, 'number_targeted_usaid']), digits=4)

#Code epi coverage variables
raw_cvg['epi_cvg'] = round((raw_cvg[,'treated'] / raw_cvg[,'persons_at_risk']), digits=4)
raw_cvg['epi_cvg_usaid'] = round((raw_cvg[,'number_treated_usaid'] / raw_cvg[,'persons_at_risk']), digits=4)

raw_cvg['sac_epi_cvg'] = NA
raw_cvg[(raw_cvg$disease == 'Schisto' | raw_cvg$disease == 'STH'), 'sac_epi_cvg'] = round((raw_cvg[(raw_cvg$disease == 'Schisto' | raw_cvg$disease == 'STH'), 'sac_treated'] / raw_cvg[(raw_cvg$disease == 'Schisto' | raw_cvg$disease == 'STH'), 'sac_at_risk']), digits=4)


# #Write to CSV
# write.csv(raw_cvg, "C:\\Users\\reowen\\Documents\\Datasets\\raw_cvg_trends.csv")

###################################################################
#RESHAPE, CONDENSE THE DATASET by disease: fiscal years in columns#
###################################################################

reg_cols = c('country_name', 'region_name', 'district_name', 'fiscal_year',
             'persons_at_risk', 'targeted', 'treated', 'prg_cvg', 'epi_cvg')

lf = raw_cvg[raw_cvg[,'disease'] == 'LF', reg_cols]
lf = lf[with(lf, order(fiscal_year)), ]
lf = reshape(lf,
             timevar = 'fiscal_year',
             idvar = c("country_name", "region_name", "district_name"), 
             direction = 'wide')
lf = lf[with(lf, order(country_name, region_name, district_name)), ]

oncho = raw_cvg[raw_cvg[,'disease'] == 'Oncho', reg_cols]
oncho = oncho[with(oncho, order(fiscal_year)), ]
oncho = reshape(oncho, 
                timevar = 'fiscal_year', 
                idvar = c("country_name", "region_name", "district_name"), 
                direction = 'wide')
oncho = oncho[with(oncho, order(country_name, region_name, district_name)), ]

trachoma = raw_cvg[raw_cvg[,'disease'] == 'Trachoma', reg_cols]
trachoma = trachoma[with(trachoma, order(fiscal_year)), ]
trachoma = reshape(trachoma, 
                   timevar = 'fiscal_year', 
                   idvar = c("country_name", "region_name", "district_name"), 
                   direction = 'wide')
trachoma = trachoma[with(trachoma, order(country_name, region_name, district_name)), ]

rm(reg_cols)
sac_cols = c('country_name', 'region_name', 'district_name', 'fiscal_year', 
             'persons_at_risk', 'sac_at_risk', 'targeted', 'treated', 
             'prg_cvg', 'epi_cvg', 'sac_epi_cvg')

sch = raw_cvg[raw_cvg[,'disease'] == 'Schisto', sac_cols]
sch = sch[with(sch, order(fiscal_year)), ]
sch = reshape(sch, 
              timevar = 'fiscal_year', 
              idvar = c("country_name", "region_name", "district_name"), 
              direction = 'wide')
sch = sch[with(sch, order(country_name, region_name, district_name)), ]

sth = raw_cvg[raw_cvg[,'disease'] == 'STH', sac_cols]
sth = sth[with(sth, order(fiscal_year)), ]
sth = reshape(sth, 
              timevar = 'fiscal_year', 
              idvar = c("country_name", "region_name", "district_name"), 
              direction = 'wide')
sth = sth[with(sth, order(country_name, region_name, district_name)), ]

rm(sac_cols)

#save excel workbook, multi-tabs
library(xlsx)
cvg_trends = createWorkbook()
lf_tab = createSheet(wb=cvg_trends, sheetName="LF")
oncho_tab = createSheet(wb=cvg_trends, sheetName="Oncho")
sch_tab = createSheet(wb=cvg_trends, sheetName="Schisto")
sth_tab = createSheet(wb=cvg_trends, sheetName="STH")
trachoma_tab = createSheet(wb=cvg_trends, sheetName="Trachoma")
addDataFrame(x=lf, sheet=lf_tab)
addDataFrame(x=oncho, sheet=oncho_tab)
addDataFrame(x=sch, sheet=sch_tab)
addDataFrame(x=sth, sheet=sth_tab)
addDataFrame(x=trachoma, sheet=trachoma_tab)
saveWorkbook(cvg_trends, "C:\\Users\\reowen\\Documents\\Datasets\\cvg_trends.xlsx")