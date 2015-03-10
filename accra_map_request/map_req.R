library(RMySQL)

# set working directory to the directory where this script is saved
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
rm(script.dir)

con <- dbConnect(MySQL(), 
                 user="envision", password="envisionRead!C4eMfw", 
                 dbname="ntd", host="productionread.c6u52zchwjde.us-east-1.rds.amazonaws.com")

query <- 
  "SELECT country, region, district, district_id, disease, workbook_year, 
MAX(disease_distribution) AS disease_distribution,
MAX(prg_cvg) AS prg_cvg

FROM
(SELECT
country_desc as 'country', region_desc as 'region', district_desc as 'district', district_id, disease, workbook_year, 
CASE WHEN indicator = 'disease_distribution' THEN value_str END AS disease_distribution,
CASE WHEN indicator = 'program_coverage_all' THEN value_num END AS prg_cvg

FROM reporting_values
WHERE most_recent_submission_f = 1 
AND country_desc IN ('Ghana', 'Togo', 'Sierra Leone', 'Niger', 'Burkina Faso', 'Mali', 'Benin', 'Uganda', 'Haiti', 'Nepal')
AND indicator IN ('disease_distribution', 'program_coverage_all') 
AND disease IN ('lf', 'trachoma'))x
GROUP BY country, region, district, disease, workbook_year;"

rs <- dbSendQuery(con, query)
rm(query)

data <- dbFetch(rs, n = -1)
check <- dbHasCompleted(rs)

dbClearResult(rs)
dbDisconnect(con)
rm(con, check, rs)

# strip zeros from coverage indicators
cvg <- c("prg_cvg")
for(c in cvg){ data[data[,c] == 0 & !is.na(data[,c]), c] <- NA }
rm(c, cvg)


###################################
# Program Coverage, FY13 and FY14 #
###################################

cvg_cols <- c("country", "region", "district", "district_id", "prg_cvg")

cvgFY13 <- data[data$workbook_year == 2013, cvg_cols]
cvgFY13 <- cvgFY13[with(cvgFY13, order(country, region, district)), ]

cvgFY14 <- data[data$workbook_year == 2014, cvg_cols]
cvgFY14 <- cvgFY14[with(cvgFY14, order(country, region, district)), ]

write.csv(cvgFY13, 'data/cvgFY13.csv')
write.csv(cvgFY14, 'data/cvgFY14.csv')
rm(cvgFY13, cvgFY14, cvg_cols)

