-- select * from reporting_values;

-- global query
SELECT country, region, district, district_id, disease, workbook_year, funding_source,
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
GROUP BY country, region, district, disease, workbook_year;



