# install.packages("docstring")

# install.packages("renv")

library(docstring)
deletezerovector <- function(df, dim){
  
  #' Square a number
  #'
  #' Calculates the square of the input
  #' 
  #' @param df the input to be analyzed
  
  if (sum(apply(df, dim, sum) == 0) != 0) {
    
    if (class(df) != "matrix" | class(df) != "data.frame") {
      stop("The input is not matrix or data.frame.")
    }
    
    if (dim != 1 | dim != 2) {
      stop("The dim should be 1 or 2")
    }
    
    if (dim == 1){
      df <- df[-c(which(apply(df, dim, sum) == 0)), ]
    } else {
      df <- df[ , -c(which(apply(df, dim, sum) == 0))]
    }
  }
  
  return(df)
}

?deletezerovector
