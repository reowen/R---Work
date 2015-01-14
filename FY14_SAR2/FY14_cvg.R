
#master_file is a .csv version of the mda_demography_decreased-ntd-burden.tab offline file
master_file = read.csv("C:\\Users\\reowen\\Documents\\Offline Files\\master_file.csv")


#columns used in the script
colnames = c("country_name", "region_name", "district_name", "district_global_task_force_id",
             "fiscal_year", "reporting_period", "disease", "persons_at_risk", "persons_targeted_usaid_funding",
             "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2",
             "persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", "persons_treated_usaid_funding_r2", 
             "sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2", 
             "sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw = master_file[(master_file$fiscal_year == 2014 & master_file$reporting_period == "2nd SAR (October-September)"), colnames]
#free up some memory (this takes 45 mb)
# rm(master_file)

for(i in 8:length(colnames)){
  raw[, colnames[i]] = as.numeric(as.character(raw[, colnames[i]]))
}

for(i in 8:length(colnames)){
  raw[is.na(raw[,colnames[i]]) == TRUE, colnames[i]] = 0
}
# rm(colnames)

#code treated indicators for all except STH 
all_targeted = c("persons_targeted_usaid_funding", "persons_targeted_usaid_funding_r1", "persons_targeted_usaid_funding_r2")
all_treated = c("persons_treated_usaid_funding", "persons_treated_usaid_funding_r1", "persons_treated_usaid_funding_r2")

raw['num_targeted'] = apply(raw[, all_targeted], 1, max)
raw['num_treated'] = apply(raw[, all_treated], 1, max)
rm(all_targeted, all_treated)

raw['treated'] = ifelse(raw$num_treated > 0, 1, 0)

raw['prg_cvg'] = round(raw[, 'num_treated'] / raw[, 'num_targeted'], digits=4)
raw[is.nan(raw$prg_cvg), 'prg_cvg'] = 0
raw['cvg_pass'] = ifelse(raw$prg_cvg >= 0.8, 1, 0)

#SAC indicator for STH
sac_targeted = c("sac_targeted_with_usaid_support_r1", "sac_targeted_with_usaid_support_r2")
sac_treated = c("sac_treated_usaid_funding_r1", "sac_treated_usaid_funding_r2")

raw['sac_targeted'] = apply(raw[, sac_targeted], 1, max)
raw['sac_treated'] = apply(raw[, sac_treated], 1, max)

raw['if_sac_treated'] = ifelse(raw$sac_treated > 0, 1, 0)
raw['sac_cvg'] = round(raw[, 'sac_treated'] / raw[, 'sac_targeted'], digits=4)
raw[is.nan(raw$sac_cvg), 'sac_cvg'] = 0
raw['sac_cvg_pass'] = ifelse(raw$sac_cvg >= 0.8, 1, 0)


keep = c("country_name", "region_name", "district_name", "fiscal_year", "disease", 
         "treated", "cvg_pass", "if_sac_treated", "sac_cvg_pass")
final = raw[, keep]

write.csv(final, "C:\\Users\\reowen\\Documents\\Datasets\\FY14_cvg\\FY14_cvg_district.csv")

library(plyr)
c_level = ddply(final, c("country_name", "disease"), summarise, 
                num_treated = sum(treated), 
                pass_cvg = sum(cvg_pass), 
                sac_treated = sum(if_sac_treated), 
                sac_pass_cvg = sum(sac_cvg_pass))
write.csv(c_level, "C:\\Users\\reowen\\Documents\\Datasets\\FY14_cvg\\FY14_cvg_country.csv")


#playing with plots

library(ggplot2)


# testkeep = c("country_name", "region_name", "district_name", "fiscal_year", "disease", "prg_cvg")
# test = raw[(raw$country_name == 'Cameroon'), testkeep]
# test[(test$prg_cvg == 0), 'prg_cvg'] = NA
# 
# 
# 
# #density curve for LF
# density <-  ggplot(test[(test$disease == 'STH'),], aes(x=prg_cvg)) + 
#   geom_density() + 
#   geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1)
# 
# hist <- ggplot(test[(test$disease == 'LF'),], aes(x=prg_cvg)) + 
#   geom_histogram(binwidth=.1, colour="black", fill="white") + 
#   geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1)
# 
# 
# 
# #ggsave(filename="cmr_sth_fy14.png", plot=density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")


ENVISION = c("Benin", "Cameroon", "Democratic Republic of Congo", "Ethiopia", "Guinea", "Haiti", "Indonesia", 
             "Mali", "Mozambique", "Nepal", "Nigeria", "Senegal", "Sierra Leone", "Tanzania", "Uganda")

plotkeep = c("country_name", "region_name", "district_name", "fiscal_year", "disease", "prg_cvg")
plot = raw[(raw$country_name %in% ENVISION), plotkeep]
plot[(plot$prg_cvg == 0 | is.nan(plot$prg_cvg)), 'prg_cvg'] = NA

#lf density graph
lf_density <-  ggplot(plot[(plot$disease == 'LF'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1)) +
  ggtitle("LF prg_cvg distribution")

# ggsave(filename="env_lf_fy14.png", plot=lf_density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")

#oncho density graph
oncho_density <-  ggplot(plot[(plot$disease == 'Oncho'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1)) +
  ggtitle("Oncho prg_cvg distribution")

# ggsave(filename="env_oncho_fy14.png", plot=oncho_density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")

#sth density graph
sth_density <-  ggplot(plot[(plot$disease == 'STH'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.2)) +
  ggtitle("STH prg_cvg distribution")

# ggsave(filename="env_sth_fy14.png", plot=sth_density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")

#schisto density graph
sch_density <-  ggplot(plot[(plot$disease == 'Schisto'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.1)) +
  ggtitle("Schisto prg_cvg distribution")

# ggsave(filename="env_sch_fy14.png", plot=sch_density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")

#trachoma density graph
tra_density <-  ggplot(plot[(plot$disease == 'Trachoma'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.1)) +
  ggtitle("Trachoma prg_cvg distribution")

# ggsave(filename="env_tra_fy14.png", plot=tra_density, path="C:\\Users\\reowen\\Documents\\Datasets\\plots")


p = list(lf_density, oncho_density, sch_density, sth_density, tra_density)

library(gridExtra)
pdf('C:\\Users\\reowen\\Documents\\Datasets\\plots\\envision_fy14.pdf', onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[i])
}
dev.off()