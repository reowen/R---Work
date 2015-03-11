library(RMySQL)

# set working directory to the directory where this script is saved
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
rm(script.dir)

con <- dbConnect(MySQL(), 
                 user="envision", password="envisionRead!C4eMfw", 
                 dbname="ntd", host="productionread.c6u52zchwjde.us-east-1.rds.amazonaws.com")

query <- 
  "SELECT country, region, district, district_id, disease, workbook_year, funding_source,
MAX(disease_distribution) AS disease_distribution,
MAX(prg_cvg_all) AS prg_cvg_all, 
MAX(prg_cvg_usaid) AS prg_cvg_usaid, 
MAX(ppl_targeted_all) AS ppl_targeted_all, 
MAX(ppl_targeted_usaid) AS ppl_targeted_usaid, 
MAX(ppl_treated_all) AS ppl_treated_all, 
MAX(ppl_treated_usaid) AS ppl_treated_usaid

FROM
(SELECT
country_desc as 'country', region_desc as 'region', district_desc as 'district', district_id, disease, 
workbook_year, funding_source,
CASE WHEN indicator = 'disease_distribution' THEN value_str END AS disease_distribution,
CASE WHEN indicator = 'program_coverage_all' THEN value_num END AS prg_cvg_all, 
CASE WHEN indicator = 'program_coverage_usaid' THEN value_num END AS prg_cvg_usaid, 
CASE WHEN indicator = 'ppl_targeted_all_num' THEN value_num END AS ppl_targeted_all, 
CASE WHEN indicator = 'ppl_targeted_usaid_num' THEN value_num END AS ppl_targeted_usaid, 
CASE WHEN indicator = 'ppl_treated_all_num' THEN value_num END AS ppl_treated_all, 
CASE WHEN indicator = 'ppl_treated_usaid_num' THEN value_num END AS ppl_treated_usaid

FROM reporting_values
WHERE most_recent_submission_f = 1 
AND country_desc IN ('Ghana', 'Togo', 'Sierra Leone', 'Niger', 'Burkina Faso', 'Mali', 'Benin', 'Uganda', 'Haiti', 'Nepal')
AND indicator IN ('disease_distribution', 'program_coverage_usaid', 'program_coverage_all', 'ppl_targeted_all_num', 
'ppl_targeted_usaid_num', 'ppl_treated_all_num', 'ppl_treated_usaid_num')
AND disease IN ('lf', 'trachoma'))x
GROUP BY country, region, district, disease, workbook_year;"

rs <- dbSendQuery(con, query)
rm(query)

data <- dbFetch(rs, n = -1)
check <- dbHasCompleted(rs)

dbClearResult(rs)
dbDisconnect(con)
rm(con, check, rs)

# # strip zeros from coverage indicators
# cvg <- c("prg_cvg_all", "prg_cvg_usaid")
# for(c in cvg){ data[data[,c] == 0 & !is.na(data[,c]), c] <- NA }
# rm(c, cvg)


###################################
# Program Coverage, FY13 and FY14 #
###################################

order_df <- function(df){
  return(df[with(df, order(country, region, district)), ])
}

cvg_cols <- c("country", "region", "district", "district_id", "funding_source", 
              "prg_cvg_usaid", "prg_cvg_all", "ppl_treated_all", "ppl_targeted_all", 
              "ppl_targeted_usaid", "ppl_treated_usaid")

final_cols <- c("country", "region", "district", "district_id", "cvg_cat_usaid", "cvg_cat_other")

for(year in c(2013, 2014)){
  for(disease in c('lf', 'trachoma')){
    df <- data[data$workbook_year == year & data$disease == disease, cvg_cols]
    invisible(order_df(df))
    
    # cvg scores
    df['cvg_cat_usaid'] <- with(df, ifelse(prg_cvg_usaid > 0 & prg_cvg_usaid < 0.7, 1, 
                                           ifelse(prg_cvg_usaid >= 0.7 & prg_cvg_usaid < 0.8, 2, 
                                                  ifelse(prg_cvg_usaid >= 0.8 & prg_cvg_usaid <= 1, 3, 
                                                         ifelse(prg_cvg_usaid > 1, 4, 0)))))
    
    df['cvg_cat_other'] <- with(df, ifelse((funding_source == 6 & (prg_cvg_all > 0 & prg_cvg_all < 0.7)), 1, 
                                           ifelse((funding_source == 6 & (prg_cvg_all >= 0.7 & prg_cvg_all < 0.8)), 2, 
                                                  ifelse((funding_source == 6 & (prg_cvg_all >= 0.8 & prg_cvg_all <= 1)), 3, 
                                                         ifelse((funding_source == 6 & (prg_cvg_all > 1)), 4, 0)))))
    
    # not supported
    df[df$funding_source == 0, 'cvg_cat_usaid'] <- 5
    df[df$funding_source == 0, 'cvg_cat_other'] <- 5
    
    
    # not reported
    df[(df$funding_source > 0 & df$funding_source < 6 & (df$ppl_treated_all == 0 | is.na(df$ppl_treated_all)) & df$ppl_targeted_all > 0), 'cvg_cat_usaid'] <- 6
    df[(df$funding_source == 6 & (df$ppl_treated_all == 0 | is.na(df$ppl_treated_all)) & df$ppl_targeted_all > 0), 'cvg_cat_other'] <- 6
    
    df <- df[, final_cols]
    
    write.csv(df, paste('data/', 'cvg', year, '_', disease,  '.csv', sep=""))
  }
}
rm(year, disease, df, cvg_cols, order_df, final_cols)

