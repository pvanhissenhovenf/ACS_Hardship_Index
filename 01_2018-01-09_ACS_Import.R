########################
# Title : ACS_Import
# Purpose : Import census data and select indicators
# Author: Patricia van Hissenhoven 
########################

########################
# Install packages
########################
install.packages("sp")
install.packages("tigris")
install.packages("tidycensus")

library(tidycensus)
library(tidyverse)
library(ggplot2)
library(magrittr)

########################
# Import data 
########################
census_api_key("6b2a3bf0f9ec6f097062213125bc40cad0351578") 

v15 <- load_variables(2016, "acs5", cache = TRUE)
View(v15)

# acs_cook <- get_acs( geography = "tract"
#                      , table = "S1501"
#                      , year = "2016"
#                      , state = "IL"
#                      , county = "Cook County"
#                      , geometry = TRUE
#                      )
