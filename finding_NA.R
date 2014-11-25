

mydf <- data.frame(A = c(1, 2, NA, 4), B = c(1, NA, 3, 4), 
                   C = c(1, NA, 3, 4), D = c(NA, 2, 3, 4), 
                   E = c(NA, 2, 3, 4))
mydf


mydf["has_NA"] = 0
x = apply(mydf, 1, is.na)

for(i in 1:nrow(mydf)){
  if((TRUE %in% x[,i])==TRUE){
    mydf[i, "has_NA"] = 1
  }
}

mydf