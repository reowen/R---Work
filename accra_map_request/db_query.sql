-- select * from reporting_values;

-- global query
SELECT country, region, district, district_id, disease, workbook_year, 
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
GROUP BY country, region, district, disease, workbook_year;