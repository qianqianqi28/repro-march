install.packages("lintr")
library("lintr")
lint("R/madeupblock.R")

install.packages("styler")
library("styler")
style_file("R/madeupblock.R")
lint("R/madeupblock.R")

library(renv)
init()
