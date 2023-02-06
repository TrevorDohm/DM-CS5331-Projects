# Imports
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Read Data
COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census.csv")
COVID_19_cases_TX <- read_csv("Datasets/COVID-19_cases_TX.csv")
Global_Mobility_Report <- read_csv("Datasets/Global_Mobility_Report.csv")

# View Data
View(COVID_19_cases_plus_census)
View(COVID_19_cases_TX)
View(Global_Mobility_Report)

# write.csv(df, file = "df.csv")
# df2 <- read.csv("df.csv")
# unlink("df.csv") # remove the file
