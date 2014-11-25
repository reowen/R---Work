

master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#program and epi coverage tab

colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "funding_src",
             "funding_src_r1", "funding_src_r2", "persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1",
             "persons_targeted_usaid_funding_r2")

prg_epi_cvg = master_file[, colnames]

for(i in 1:length(colnames)){
  names(prg_epi_cvg)[i] = colnames[i]
}

for(i in 8:length(colnames)){
  prg_epi_cvg[,colnames[i]] = as.numeric(as.character(prg_epi_cvg[,colnames[i]]))
}

for(i in 1:length(colnames)){
  prg_epi_cvg[(is.na(prg_epi_cvg[,colnames[i]]) == TRUE),colnames[i]] = 0
}



prg_epi_cvg = prg_epi_cvg[prg_epi_cvg$fiscal_year == 2015, ]
prg_epi_cvg = prg_epi_cvg[prg_epi_cvg$reporting_period == "Work Planning", ]

prg_epi_cvg["usaid_supported"] = 0
prg_epi_cvg[((prg_epi_cvg$funding_src != 6 & prg_epi_cvg$funding_src != 0) | (prg_epi_cvg$funding_src_r1 != 6 & prg_epi_cvg$funding_src_r1 != 0) | (prg_epi_cvg$funding_src_r2 != 6 & prg_epi_cvg$funding_src_r2 != 0)),
            "usaid_supported"] = 1

prg_epi_cvg["persons_targeted"] = apply(prg_epi_cvg[,12:14], 1, max)


##################################################
#num persons at risk in USAID-supported districts#
##################################################

persons_at_risk = data.frame(x=c("LF", "Oncho", "Schisto", "STH", "Trachoma"))

countries = c("Mali", "Cameroon", "Senegal", "Guinea")
diseases = c("LF", "Oncho", "Schisto", "STH", "Trachoma")

for(i in 1:length(countries)){
  persons_at_risk[countries[i]] = 0
  for(d in 1:length(diseases)){
    persons_at_risk[persons_at_risk$x == diseases[d], countries[i]] = sum(prg_epi_cvg[(prg_epi_cvg$country_name == countries[i] & prg_epi_cvg$disease == diseases[d] & prg_epi_cvg$usaid_supported == 1), "persons_at_risk"])
  }
}

###########################################
#num persons targeted in treated districts#
###########################################

persons_targeted = data.frame(x=c("LF", "Oncho", "Schisto", "STH", "Trachoma"))

for(i in 1:length(countries)){
  persons_targeted[countries[i]] = 0
  for(d in 1:length(diseases)){
    persons_targeted[persons_targeted$x == diseases[d], countries[i]] = sum(prg_epi_cvg[(prg_epi_cvg$country_name == countries[i] & prg_epi_cvg$disease == diseases[d] & prg_epi_cvg$usaid_supported == 1), "persons_targeted"])
  }
}



############
####DSAs####
############

me = read.delim("C:\\Users\\reowen\\Documents\\Offline Files\\me.txt")

ME2 = me[me$fiscal_year == 2015,]
ME2 = ME2[ME2$reporting_period == "Work Planning",]

ME2$funding_src = as.numeric(as.character(ME2$funding_src))
ME2[is.na(ME2$funding_src) == TRUE, "funding_src"] = 0

ME2["usaid_support"] = 1
ME2[ME2$funding_src == 0 | ME2$funding_src == 6, ] = 0

DSA_tab = data.frame(x=c("LF baseline sentinel site", "LF baseline spot check", "LF midterm sentinel site", 
                         "LF midterm spot check", "LF Pre-TAS sentinel site", "LF Pre-TAS spot-check site", 
                         "LF TAS: Stop MDA", "LF TAS: Post-MDA Surveillance I", "LF TAS: Post-MDA Surveillance II", 
                         "Oncho epidemiological assessment", "Schisto sentinel site", "Schisto evaluation", 
                         "STH sentinel site", "STH evaluation", "Trachoma impact survey", "Post-MDA coverage survey", 
                         "Post-MDA surveillance", "KAP survey", "Data Quality Assessment (DQA)", "Other"))

dsas = c("LF baseline sentinel site", "LF baseline spot check", "LF midterm sentinel site", 
                "LF midterm spot check", "LF Pre-TAS sentinel site", "LF Pre-TAS spot-check site", 
                "LF TAS: Stop MDA", "LF TAS: Post-MDA Surveillance I", "LF TAS: Post-MDA Surveillance II", 
                "Oncho epidemiological assessment", "Schisto sentinel site", "Schisto evaluation", 
                "STH sentinel site", "STH evaluation", "Trachoma impact survey", "Post-MDA coverage survey", 
                "Post-MDA surveillance", "KAP survey", "Data Quality Assessment (DQA)", "Other")

for(i in 1:length(countries)){
  DSA_tab[countries[i]] = 0
  for(n in 1:length(dsas)){
    DSA_tab[DSA_tab$x == dsas[n], countries[i]] = sum((ME2$country_name == countries[i] & ME2$assessment_type == dsas[n] & ME2$usaid_support == 1))
  }
}

remove(prg_epi_cvg, ME2, master_file, me, dsas, i, n, d, colnames, countries, diseases)
