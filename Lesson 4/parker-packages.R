install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("C:/Users/MARTA/Desktop/Geoscripting/GitHub/Lesson 4/")
create("cats")

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}