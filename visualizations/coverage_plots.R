
setwd('C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\visualizations')
source('coverage_plot_cleaning.R')

library(ggplot2)
library(gridExtra)

#####################################
### portfolio-level density plots ###
#####################################

plot <- district[(district$country_name %in% ENVISION), ]
plot[(plot$prg_cvg == 0 | is.nan(plot$prg_cvg)), 'prg_cvg'] <- NA

#lf density graph
lf_density <-  ggplot(plot[(plot$disease == 'LF'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1), limits = c(0,1.5)) +
  ggtitle("LF prg_cvg distribution")

#oncho density graph
oncho_density <-  ggplot(plot[(plot$disease == 'Oncho'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1)) +
  ggtitle("Oncho prg_cvg distribution")

#sth density graph
sth_density <-  ggplot(plot[(plot$disease == 'STH'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.2), limits = c(0,2)) +
  ggtitle("STH prg_cvg distribution")

#schisto density graph
sch_density <-  ggplot(plot[(plot$disease == 'Schisto'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 2, by=0.2), limits = c(0,2)) +
  ggtitle("Schisto prg_cvg distribution")

#trachoma density graph
tra_density <-  ggplot(plot[(plot$disease == 'Trachoma'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.1)) +
  ggtitle("Trachoma prg_cvg distribution")

# Save plots to pdf
p = list(lf_density, oncho_density, sch_density, sth_density, tra_density)
pdf('outputs\\envision_fy14.pdf', onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[i])
}
dev.off()


# plot FY14
plot14 <- final[final$fiscal_year == 2014,]

