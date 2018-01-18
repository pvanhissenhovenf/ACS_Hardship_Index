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
install.packages("acs")

library(tidycensus)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(acs)
library(stringr)
library(XML)
library(plyr)
########################
# Import key
########################
census_api_key("6b2a3bf0f9ec6f097062213125bc40cad0351578") 


########################
# Search variables using tidycensus package
########################
v15 <- load_variables(2016, "acs5", cache = TRUE)
View(v15)



########################
# Search variables using acs package
########################
api.key.install("6b2a3bf0f9ec6f097062213125bc40cad0351578")

my.tract <- geo.make(state = "IL"
                     , county = "Cook"
                     , check = T
                     )

total.pop <- acs.fetch( endyear = 2016
                        , span = 5
                        , geography = my.tract
                        , table.name = "B01001"
                        , dataset = "acs"
                        , col.names = "auto"
                        , case.sensitive = FALSE
                      )





