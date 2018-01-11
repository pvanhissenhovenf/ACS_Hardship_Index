########################
# Title : Hardship_Chicago
# Purpose : Re-calculate hardship index for 2008-2012 data
# Author: Patricia van Hissenhoven 
# Date : Jan. 10th, 2018 
########################

########################
# Import packages 
########################
library(ggplot2)
library(magrittr)
library(MASS)
library(car)
library(mosaic)
library(knitr)
library(tidyverse)
library(ggformula)
library(gridExtra)
library(broom)
library(GGally)

########################
# Import and explore data 
########################
indicators.chicago <- read.csv("https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD")
dim(indicators.chicago) # 78 rows, 9 columns
colnames(indicators.chicago) # Community area number, Community area name, Percent of housing crowded, 
                             # Percent households below poverty, Percent aged 16 unemployed, Percent aged 25 without High School Diploma, 
                             # Percent aged under 18 or over 64, Per capita income, Hardship Index


########################
# Re-calculate Hardship index 
########################
# The exlanation below is the written formula for the hardship score
#   The same formulation as the original study was used to calculate the Intercity Hardship Index (see Appendix
#   1 in Nathan and Adams 1989):
#   X = ((Y-Ymin)/(Ymax — Ymin))*100
#   where: 
#   X = standardized value of component variable (for example, unemployment rate) for each city to be computed.
#   Y = unstandardized value of component variable for each city.
#   Ymin = the minimum value for Y across all cities.
#   Ymax = the maximum value for Y across all cities.
#   The (Ymax — Ymin ) part of the formula was reversed to (Ymin — Ymax ) for the calculation of Income
#   Level so that the resulting ratio would be interpreted consistently with the other ratios — a higher value
#   indicating higher hardship.  The formula standardizes each of the component variables so that they are
#   all given equal weight in the composite Intercity Hardship Index. The Index represents the average of
#   the standardized ratios of all six component variables. The Intercity Hardship Index ranges from 0 to
#   100 with a higher number indicating greater hardship

#################################
# TRIAL 1 
#################################

# In this trial I tried to recreate the formula above and weigh each indicator equally
# For each indicator I found the max and min values and standardized each observation with the following formula
# Z = ((Observation-min.indicator)/(max.indicator-min.indicator))
# Then I averaged the values

# The first step seems to be to identify the min and max for each indicator (the means were used in the second trial)

  # Crowded housing
  cro.hou.max <- max(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                     , na.rm = TRUE)
  cro.hou.min <- min(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                     , na.rm = TRUE)
  cro.hou.mean <- mean(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                     , na.rm = TRUE)
  cro.hou.s <- sd(indicators.chicago$PERCENT.OF.HOUSING.CROWDED
                     , na.rm = TRUE)
  # Poverty 
  bel.pov.max <- max(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                      , na.rm = TRUE)
  bel.pov.min <- min(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                     , na.rm = TRUE)
  bel.pov.mean <- mean(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                     , na.rm = TRUE)
  bel.pov.s <- sd(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                       , na.rm = TRUE)
  # Unemployment
  une.max <- max(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                     , na.rm = TRUE)
  une.min <- min(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                     , na.rm = TRUE)
  une.mean <- mean(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                 , na.rm = TRUE)
  une.s <- sd(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                   , na.rm = TRUE)
  # Education Attainment
  no.hsd.max <- max(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                 , na.rm = TRUE)
  no.hsd.min <- min(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                 , na.rm = TRUE)
  no.hsd.mean <- mean(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                    , na.rm = TRUE)
  no.hsd.s <- sd(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                      , na.rm = TRUE)
  # Dependency 
  dep.max <- max(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                 , na.rm = TRUE)
  dep.min <- min(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                 , na.rm = TRUE)
  dep.mean <- mean(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                 , na.rm = TRUE)
  dep.s <- sd(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                   , na.rm = TRUE)
  # Per capita income
  inc.max <- max(indicators.chicago$PER.CAPITA.INCOME
                 , na.rm = TRUE)
  inc.min <- min(indicators.chicago$PER.CAPITA.INCOME
                 , na.rm = TRUE)
  inc.mean <- mean(indicators.chicago$PER.CAPITA.INCOME
                 , na.rm = TRUE)
  inc.s <- sd(indicators.chicago$PER.CAPITA.INCOME
                   , na.rm = TRUE)
  
# The next step is to standardize the values according to the formula
  # Crowded housing 
   indicators.chicago$std.cro.hou <- ((indicators.chicago$PERCENT.OF.HOUSING.CROWDED - cro.hou.min) / (cro.hou.max-cro.hou.min))*100
  # Poverty 
   indicators.chicago$std.pov <- ((indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.min) / (bel.pov.max-bel.pov.min))*100
  # Unemployment
   indicators.chicago$std.une <- ((indicators.chicago$PERCENT.AGED.16..UNEMPLOYED - une.min) / (une.max-une.min))*100
  # Education Attainment
   indicators.chicago$std.no.hsd <- ((indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.min) / (no.hsd.max-no.hsd.min))*100
  # Dependency
   indicators.chicago$std.dep <- ((indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.min) / (dep.max-dep.min))*100
  # Per capita income, inverting the denominator, as suggested in the cited part
   indicators.chicago$std.inc <- ((indicators.chicago$PER.CAPITA.INCOME - inc.min) / (inc.min-inc.max))*100
   

   
# Re-calculate the hardship index
  indicators.chicago$recalc.trial1 <- (1/6)*(indicators.chicago$std.cro.hou + indicators.chicago$std.une + indicators.chicago$std.dep + indicators.chicago$std.inc + indicators.chicago$std.no.hsd + indicators.chicago$std.pov)

# Compare the values of the recalculation to the actual index. The difference should be zero.  
  dif<- indicators.chicago$HARDSHIP.INDEX - indicators.chicago$recalc.trial1
  
# Since the differences were not zero, I tried to standardize it with another mechanism
# This time, I subtracted the mean from each observation and divided by the standard deviation 
# (Observation- indicator.mean)/standard.deviation.indicator

#############################
# Trial 2 
#############################
  
# Standardize the values (second trial)
  # Crowded housing 
  std2.cro.hou <- ((indicators.chicago$PERCENT.OF.HOUSING.CROWDED - cro.hou.mean) / (cro.hou.s))
  # Poverty 
  std2.pov <- ((indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.mean) / (bel.pov.s))
  # Unemployment
  std2.une <- ((indicators.chicago$PERCENT.AGED.16..UNEMPLOYED - une.mean) / (une.s))
  # Education Attainment
  std2.no.hsd <- ((indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.mean) / (no.hsd.s))
  # Dependency
  std2.dep <- ((indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.mean) / (dep.s))
  # Per capita income, inverting the denominator
  std2.inc <- ((indicators.chicago$PER.CAPITA.INCOME - inc.mean) / (inc.s))
  
# Re-calculate the hardship index
  recalc.trial2 <- (1/6)*(std2.cro.hou + std2.une + std2.dep + std2.inc + std2.no.hsd + std2.pov)
# Compare with the real hardship index
  dif2 <- indicators.chicago$HARDSHIP.INDEX - recalc.trial2 
 
################################
# Trial 3
################################  
# Again, the difference is steep. I tried a third time using the Chicago-based values  
# The next step is to standardize the values (third trial, using Chicago values)
  # Crowded housing 
  std3.cro.hou <- ((indicators.chicago$PERCENT.OF.HOUSING.CROWDED - 4.7) / (4.7))*100
  # Poverty 
  std3.pov <- ((indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY - 19.7) / (19.7))*100
  # Unemployment
  std3.une <- ((indicators.chicago$PERCENT.AGED.16..UNEMPLOYED - 12.9) / (12.9))*100
  # Education Attainment
  std3.no.hsd <- ((indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - 19.5) / (19.5))*100
  # Dependency
  std3.dep <- ((indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64 - 33.5) / (33.5))*100
  # Per capita income, inverting the denominator
  std3.inc <- ((indicators.chicago$PER.CAPITA.INCOME - 28202) / (28202))*100
  
  
  recalc.trial3 <- (1/6)*(std3.cro.hou + std3.une + std3.dep + std3.inc + std3.no.hsd + std3.pov)
  dif3 <- indicators.chicago$HARDSHIP.INDEX-recalc.trial3 
  # These also seem to be dramatically different from the hardship index
  
#################################
# Trial 4
#################################
# Import data again (Start off with clean data)  
  trial4 <- read.csv("https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD")

# Exclude Chicago observation  
  trial4  <- trial4[1:77,]

# I am going to do each step of the formula independently  
# I already identified the max and min for each indicator
  
  # I add values to the column for each indicator that shows the value (Observation-indicator.min) 
  trial4$crowded.housing.diff <- trial4$PERCENT.OF.HOUSING.CROWDED-cro.hou.min
  trial4$education.attainment.diff <- trial4$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.min
  trial4$poverty.diff <- trial4$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.min
  trial4$dependency.diff <- trial4$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.min
  trial4$unemployment.diff <- trial4$PERCENT.AGED.16..UNEMPLOYED - une.min
  trial4$income.diff <- inc.max - trial4$PER.CAPITA.INCOME
  
  # I add values to the column for each indicator that shows the difference divided by the range
  trial4$crowded.housing.rat <- (trial4$crowded.housing.diff/(cro.hou.max-cro.hou.min))*100
  trial4$education.attainment.rat <- (trial4$crowded.housing.diff/(no.hsd.max-no.hsd.min))*100
  trial4$poverty.rat <- (trial4$poverty.diff/ (bel.pov.max - bel.pov.min))*100
  trial4$dependency.rat <- (trial4$dependency.diff / (dep.max-dep.min))*100
  trial4$unemployment.rat <- (trial4$unemployment.diff/ (une.max- une.min))*100
  trial4$income.rat <- trial4$income.diff/(inc.min-inc.max)*100
  
  
  # Hardship index
  trial4$hardship.index.2 <- (1/6)*(trial4$crowded.housing.rat + trial4$education.attainment.rat + trial4$poverty.rat + trial4$dependency.rat + trial4$unemployment.rat+ trial4$income.rat)
  trial4$difference <- trial4$HARDSHIP.INDEX-trial4$hardship.index.2
  
  ###########################
  # TRIAL 5
  ###########################
 
  # Part a
  # In this trial I try to derive a formula based on the standardized values
  # and the actual score. I want to see if there is any formula to it. 
  
  # The documentation says that each of the values are equally weighed,
  # so I am going to see if there is a coefficient that is applied to the sum 
  # of the values. 
  
  trial4$coefficients <-  trial4$HARDSHIP.INDEX/(trial4$crowded.housing.rat + trial4$education.attainment.rat + trial4$poverty.rat + trial4$dependency.rat + trial4$unemployment.rat+ trial4$income.rat)
  
  # There seems to be something interesting: the hardship index and the coefficients seem 
  # to correlate. This means that the coefficient by which we multiply the sum of the indicators
  # increases as the hardship increases. 
  
  # Part b 
  # Straightup try a linear correalation of the standardized values
  
  model1 <- lm(HARDSHIP.INDEX ~ crowded.housing.rat + education.attainment.rat + poverty.rat + dependency.rat + unemployment.rat + income.rat , data = trial4)
  myvars <- c( "HARDSHIP.INDEX" , "hardship.index.2" , "difference")
 
  trial5 <- trial4[myvars]
  trial5$yhat <- fitted(model1)
  trial5$diff.yhat <- trial5$HARDSHIP.INDEX-trial5$yhat
  
  # The difference seems to be large, however it is the best model and all of the coefficients are significant. 

  
  