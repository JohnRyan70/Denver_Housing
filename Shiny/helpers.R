
setwd("C:/Users/John/Denver_Housing_Project")
library(maps)
library(mapproj)
source("Shiny_Maps/helpers.R")
counties <- readRDS("Shiny_Maps/data/ACSData10.RData")
percent_map(counties$white, "darkgreen", "% white")