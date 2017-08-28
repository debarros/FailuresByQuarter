#functions.R

library(openxlsx)
library(data.table)
library(dBtools)


lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


lm.coeffp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  p = summary(modelobject)$coefficients[,"Pr(>|t|)"]
  return(p)
}

gpa <- function(x){
  mean(x, na.rm = T)/25
}


Vgpa <- function(id_list, grade.frame){
  x = vector(mode = "numeric", length = length(id_list))
  for(i in 1:length(x)){
    x[i] = gpa(grade.frame$Percent[grade.frame$Student.Number == id_list[i]])
  }
  return(x)
}