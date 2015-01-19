
setwd('C:\\Users\\reowen\\Documents\\Coding\\R_Scripts\\visualizations')
source('coverage_plot_cleaning.R')

plot <- district[(district$country_name %in% ENVISION & district$fiscal_year == 2014), ]
plot[(plot$prg_cvg == 0 | is.nan(plot$prg_cvg)), 'prg_cvg'] <- NA

library(ggplot2)
library(gridExtra)

#####################################
### portfolio-level density plots ###
#####################################

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
  scale_x_continuous(breaks = seq(0, 2, by=0.2), limits = c(0, 1.6)) +
  ggtitle("Schisto prg_cvg distribution")

#trachoma density graph
tra_density <-  ggplot(plot[(plot$disease == 'Trachoma'),], aes(x=prg_cvg)) + 
  geom_density() + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.1)) +
  ggtitle("Trachoma prg_cvg distribution")

# Save plots to pdf
p = list(lf_density, oncho_density, sch_density, sth_density, tra_density)
pdf('outputs\\envision_density.pdf', onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[i])
}
dev.off()
rm(p, lf_density, sch_density, oncho_density, sth_density, tra_density)

##################################
### portfolio-level histograms ###
##################################

#lf density graph
lf_hist <-  ggplot(plot[(plot$disease == 'LF'),], aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1), limits = c(0,1.5)) +
  ggtitle("LF prg_cvg distribution")

#oncho density graph
oncho_hist <-  ggplot(plot[(plot$disease == 'Oncho'),], aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 1.5, by=0.1)) +
  ggtitle("Oncho prg_cvg distribution")

#sth density graph
sth_hist <-  ggplot(plot[(plot$disease == 'STH'),], aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.2), limits = c(0,2)) +
  ggtitle("STH prg_cvg distribution")

#schisto density graph
sch_hist <-  ggplot(plot[(plot$disease == 'Schisto'),], aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 2, by=0.2), limits = c(0, 1.6)) +
  ggtitle("Schisto prg_cvg distribution")

#trachoma density graph
tra_hist <-  ggplot(plot[(plot$disease == 'Trachoma'),], aes(x=prg_cvg)) + 
  geom_histogram(binwidth=.1, colour="black", fill="white") + 
  geom_vline(aes(xintercept=median(prg_cvg, na.rm=T)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 3, by=0.1)) +
  ggtitle("Trachoma prg_cvg distribution")

# Save plots to pdf
p = list(lf_hist, sch_hist, oncho_hist, sth_hist, tra_hist)
pdf('outputs\\envision_histograms.pdf', onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[i])
}
dev.off()
rm(p, i, lf_hist, sch_hist, oncho_hist, sth_hist, tra_hist)


#########################
### fun with boxplots ###
#########################

p <- list()

# boxplot comparing all ENVISION countries' coverage distributions
bp <- ggplot(plot[(plot$disease == 'LF'),], aes(x=country_name, y=prg_cvg)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 2, by=0.1), limits = c(0, 1.5)) + 
  geom_hline(aes(yintercept=0.8), color="red", linetype="dashed", size=1) + 
  coord_flip() + 
  labs(title = 'LF Program Coverage Distributions, FY14 ENVISION', 
       x = '', 
       y = 'Program Coverage')

p[[length(p) + 1]] <- bp

mali <- plot[plot$country_name == 'Mali', ]
# boxplot comparing coverage across diseases, Mali
bp <- ggplot(mali, aes(x=disease, y=prg_cvg)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 2, by=0.1), limits = c(0, 1.1)) + 
  geom_hline(aes(yintercept=0.8), color="red", linetype="dashed", size=1) + 
  coord_flip() + 
  labs(title = 'Program Coverage Distributions, FY14 Mali', 
       x = '', 
       y = 'Program Coverage')

p[[length(p) + 1]] <- bp

pdf('outputs\\boxplots.pdf', onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[i])
}
dev.off()

rm(bp, p, i)
