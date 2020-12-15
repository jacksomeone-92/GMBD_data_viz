library(dplyr)
library(ggplot2)
library(readr)

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#read data
