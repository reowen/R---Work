library(foreign)
mydata = read.spss("C:\\Users\\reowen\\Documents\\Coding\\SPSS\\Cancer.sav", to.data.frame = TRUE)


write.dta(mydata, "C:\\Users\\reowen\\Documents\\Coding\\SPSS\\mydata.dta")