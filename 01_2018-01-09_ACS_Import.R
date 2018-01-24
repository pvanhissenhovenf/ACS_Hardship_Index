########################
# Title : ACS_Import
# Purpose : Updating Census Data - Selected socioeconomic indicators in Chicago, 2008 - 2012
#             Using ACS 2012-2016 5-Year Estimate data
# Author: Patricia van Hissenhoven 
########################

## load necessary packages
library( acs )
library( dplyr )
library( sp )
library( rgdal )

## load necessary data

# transform URL character vector into spatial dataframe
comarea606 <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                       , layer = "OGRGeoJSON"
                       , stringsAsFactors = FALSE
                       , verbose = FALSE # to hide progress message after object is created
)



#2010 Census Tracts - Source: https://www.cityofchicago.org/city/en/depts/doit/dataset/boundaries_-_censustracts.html
chicago.census.tracts <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON"
                                  , layer = "OGRGeoJSON"
                                  , stringsAsFactors = FALSE
)

# What's inside the data slot?
str( object = slot( object = chicago.census.tracts, name = "data" ) )

# 'data.frame':	801 obs. of  9 variables:
# $ statefp10 : chr  "17" "17" "17" "17" ...
# $ name10    : chr  "8424" "8403" "8411" "8412" ...
# $ commarea_n: chr  "44" "59" "34" "31" ...
# $ namelsad10: chr  "Census Tract 8424" "Census Tract 8403" "Census Tract 8411" "Census Tract 8412" ...
# $ commarea  : chr  "44" "59" "34" "31" ...
# $ geoid10   : chr  "17031842400" "17031840300" "17031841100" "17031841200" ...
# $ notes     : chr  "" "" "" "" ...
# $ tractce10 : chr  "842400" "840300" "841100" "841200" ...
# $ countyfp10: chr  "031" "031" "031" "031" ...

# Excellent. They've given us a crosswalk between fips codes (statefp10+name10+tractce10)
# and commarea!

# NOTE: There a few judgements being made regarding which census tracts belong in which community areas
View( x = slot( object = chicago.census.tracts, name = "data")[ which(
  slot( object = chicago.census.tracts, name = "data")$note != ""
) , ])

# store transparent gray color
custom.color <- rgb( r = 204, g = 204, b = 204, alpha = 100, maxColorValue = 255 )

# overlay the two geographies 
# on top of each other to gain 
# a better sense of the judgements
# being made
par( mar = c(2, 0, 2, 0 ) )
plot( x = chicago.census.tracts
      , col = "#4d4d4d"
      , border = "#4d4d4d"
      , main = "Overlaying Geographies in Chicago"
      )
plot( x = comarea606
      , col = custom.color
      , border = custom.color
      , add = TRUE
      )
legend( x = "left"
        , legend = c( "Chicago Census Tracts", "Chicago Current Community Areas" )
        , pch = 15
        , pt.cex = 2
        , col = c( "#4d4d4d", "#CCCCCC" )
        , bty = "n"
        , title = "Legend"
        )
mtext( side = 1
       , adj = 0.99
       , cex = 0.75
       , line = 1
       , text = "Source: Chicago Data Portal"
       )

# Because the goal is to update this data set,
# it is an assumption we are making that the
# City used the crosswalk contain in the census tract spatial polygons data frame
# to determine which census tracts belong in which community areas

# create the census tract to community area number crosswalk
census.tract.community.area.crosswalk.df <-
  slot( object = chicago.census.tracts, name = "data" )[ c( "commarea_n", "tractce10" ) ]

# create the community area number to community area name crosswalk
community.area.number.name.crosswalk.df <- 
  slot( object = comarea606, name = "data" )[ c( "community", "area_numbe" ) ]

# create a master census tract to community area crosswalk
master.census.tract.community.area.crosswalk.df <-
  left_join( x = census.tract.community.area.crosswalk.df
             , y = community.area.number.name.crosswalk.df
             , by = c( "commarea_n" = "area_numbe" ) 
             )

# my personal acs api key
my.key <- "6b2a3bf0f9ec6f097062213125bc40cad0351578"

# Identify census tracts within Cook County
cook.county.census.tracts <- geo.make(state = "IL"
                                      , county = "Cook"
                                      , tract = "*"
)

# Download ACS 2011-2015 5-Year Estimate Data
# https://census.gov/programs-surveys/acs/technical-documentation/table-shells.html
# Methodology
# 1. Avoiding the use of the `table.number` parameter
# 2. keep the 'geography' and 'estimate' data frames within each acs object
# 3. keep only the census tracts from the acs data frame
#    that appear in the master.census.tract.community.area.crosswalk.df
#   

# population by age group by gender
population.df <- acs.fetch( endyear = 2016
                                   , span = 5
                                   , geography = cook.county.census.tracts
                                   , variable = c( paste( "B01001"
                                                       , 1:9
                                                       , sep = "_00"
                                                       )
                                                   , paste( "B01001"
                                                            , 10:49
                                                            , sep = "_0"
                                                            )
                                                   )
                                   , key = my.key 
                            )
population.df <- cbind.data.frame(
  slot( object = population.df, name = "geography" )
  , slot( object = population.df, name = "estimate" )
)

# this one takes about ~1 minute
# educational attainment
education.df <- acs.fetch( endyear = 2016
                            , span = 5
                            , geography = cook.county.census.tracts
                            , variable = c( paste( "B15002"
                                                   , 1:9
                                                   , sep = "_00"
                                                   )
                                            , paste( "B15002"
                                                     , 10:35
                                                     , sep = "_0"
                                                     )
                                            )
                            , key = my.key 
)
education.df <- cbind.data.frame(
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
)

# poverty status
poverty.df <- acs.fetch( endyear = 2016
                           , span = 5
                           , geography = cook.county.census.tracts
                           , variable = c(
                             paste( "B17017"
                                    , 1:9
                                    , sep = "_00"
                                    )
                             , paste( "B17017"
                                      , 10:59
                                      , sep = "_0"
                                      )
                             )
                           , key = my.key 
                           )
poverty.df <- cbind.data.frame(
  slot( object = poverty.df, name = "geography" )
  , slot( object = poverty.df, name = "estimate" )
)

# per capita income
# in 2016-inflation adjusted dollars
pci.df <- acs.fetch( endyear = 2016
                         , span = 5
                         , geography = cook.county.census.tracts
                         , variable = "B19301_001"
                         , key = my.key 
)
pci.df <- cbind.data.frame(
  slot( object = pci.df, name = "geography" )
  , slot( object = pci.df, name = "estimate" )
)

# employment status for folks aged 16+ by sex by age
employment.df <- acs.fetch( endyear = 2016
                            , span = 5
                            , geography = cook.county.census.tracts
                            , variable = c(
                              paste( "B23001"
                                     , 1:9
                                     , sep = "_00"
                              )
                              , paste( "B23001"
                                       , 10:99
                                       , sep = "_0"
                              )
                              , paste( "B23001"
                                       , 100:173
                                       , sep = "_"
                              )
                            )
                            , key = my.key
                            )
employment.df <- cbind.data.frame(
  slot( object = employment.df, name = "geography" )
  , slot( object = employment.df, name = "estimate" )
)

# tenure by occupants per room
occupants.per.room.df <- acs.fetch( endyear = 2016
                                    , span = 5
                                    , geography = cook.county.census.tracts
                                    , variable = c(
                                      paste( "B25014"
                                             , 1:9
                                             , sep = "_00"
                                      )
                                      , paste( "B25014"
                                               , 10:13
                                               , sep = "_0"
                                      )
                                    )
                                    , key = my.key
)
occupants.df <- cbind.data.frame(
  slot( object = occupants.per.room.df , name = "geography" )
  , slot( object = occupants.per.room.df , name = "estimate" )
)

## filter data to match the socioeconomic descriptors

# Name:           Percent Aged Under 18 or Over 64	
# Description:    Percent of the population under 18 or over 64 years of age 
#                 (i.e., dependency)




# Name:           Percent Aged 25+ Without High School Diploma
# Description:    Percent of persons aged 25 years or older without a high school diploma

# Name:           Percent Households Below Poverty
# Description:    Percent of households living below the federal poverty level

# Name:           Per Capita Income
# Description:    A community area’s per capita income was estimated by dividing 
#                 the aggregate income of the census tracts within the community area 
#                 (from table B19313) by the [aggregate] number of residents.


# Name:           Percent Aged 16+ Unemployed
# Description:    Percent of persons aged16 years or older in the labor force that are unemployed


# Name:           Percent of Housing Crowded
# Description:    Percent of occupied housing units with more than one person per room 
#                 (i.e., crowded housing)


# Name:           Hardship Index
# Description:    The hardship index is a score that incorporates each of the six selected socioeconomic indicators according to the method described in An Update on Urban Hardship 
#                 (see footnote 2 on page 53 of http://www.rockinst.org/pdf/cities_and_neighborhoods/2004-08-an_update_on_urban_hardship.pdf). 
#                 Scores on the index can range from 1 to 100, with a higher index number representing a greater level of hardship. 
#                 The scores are standardized according to the data for the 77 community areas, 
#                 and therefore cannot be compared to scores generated for other jurisdictions.

## aggregation - not complete #
# Please use dplyr for aggregation
# and see here for more info: http://www.datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function

## Create a data frame that contains 78 rows and 9 columns
## 77 rows for each community area, plus 1 for the entire City
## 2 columns for the community area number + name
## 6 columns for each socioeconomic indicator
## 1 column for the Hardship Index (excluding a value for the entire City )
## See here for an example of the schema: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data

## Export the results into the /Data folder as both an RDS and CSV

 



###############
# I will now filter data to describe socio-economic descriptors
###############
################
# Population for Dependency
################
 
  # We need to figure out what population is younger than 18 or older than 64.
    pop.vars <- c(  "NAME"
                    , "state"
                    , "county"
                    , "tract"
                    ,  "B01001_003" # Males under 5
                    , "B01001_004" # Males 5-9
                    , "B01001_005" # Males 10-14
                    , "B01001_006" # Males 15-17
                    , "B01001_027" # Females under 5
                    , "B01001_028" # Females 5-9 
                    , "B01001_029" # Females 10-14
                    , "B01001_030" # Females 15-17
                    , "B01001_020" # Males 65-66
                    , "B01001_021" # Males 67-69
                    , "B01001_022" # Males 70-74
                    , "B01001_023" # Males 75-79
                    , "B01001_024" # Males 80-84
                    , "B01001_025" # Males 85 and older 
                    , "B01001_044" # Females 65-66
                    , "B01001_045" # Females 67-69
                    , "B01001_046" # Females 70-74
                    , "B01001_047" # Females 75-79
                    , "B01001_048" # Females 80-84
                    , "B01001_049" # Females 85 and older 
              # We also need to add values for the entire population to obtain percentages
                    , "B01001_001" # Total population              
              
               )
  # I then remove all of the tables that are not relevant
    population.df <- population.df[,pop.vars]
    
  # Calculate the percentage of people in dependency
  # This means that we add the populations under 18 and over 64 and divide by the total of the population
    population.df$population.score <-  (rowSums(population.df[,5:24], na.rm = TRUE)/population.df$B01001_001)*100
  
  # Select all of the values for the census tracts that correspond to the city of Chicago
    population.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                             , y = population.df
                                             , by = c("tractce10" = "tract")
                                            )
    
################
# Educational attainment
################    
    # We need to select all of the people who do not have a high school diploma
    edu.vars <- c("NAME"
                  , "state"
                  , "county"
                  , "tract"
                  , "B15002_003" # Males no schooling completed
                  , "B15002_004" # Males nursery to 4th grade
                  , "B15002_005" # Males 5th and 6th grade
                  , "B15002_006" # Males 7th and 8th grade
                  , "B15002_007" # Males 9th grade
                  , "B15002_008" # Males 10th grade
                  , "B15002_009" # Males 11th grade
                  , "B15002_010" # Males 12th grade (no diploma)
                  , "B15002_020" # Females no schooling completed
                  , "B15002_021" # Females nursery to 4th grade
                  , "B15002_022" # Females 5th and 6th grade
                  , "B15002_023" # Females 7th and 8th grade
                  , "B15002_024" # Females 9th grade
                  , "B15002_025" # Females 10th grade
                  , "B15002_026" # Females 11th grade
                  , "B15002_027" # Females 12th grade (no diploma)
                  # We also need to add values for the entire population to obtain percentages
                  , "B15002_001" # Total population 
                  )
## NOTE: there seem to be another group of tables that have 
# the same data summarized in less tables: 15002_001-017
# C15002_03 Males less than 9th grade
# C15002_04 Males 9th to 12th grade, no diploma
# C15002_11 Females less than 9th grade 
# C15002_12 Females 9th to 12th grade, no diploma  
    # I then remove all of the tables that are not relevant
    education.df <- education.df[,edu.vars]
    
    # Calculate the percentage of people over 25 without a highschool diploma
    # This means that we add the row values and divide by the total of the population
    education.df$education.score <-  (rowSums(education.df[,5:20], na.rm = TRUE)/education.df$B15002_001)*100
    
    # Select all of the values for the census tracts that correspond to the city of Chicago
    education.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                             , y = education.df
                                             , by = c("tractce10" = "tract")
                                            )
    
################
# Poverty Status
################
    # We only need the households below the poverty level and the total. This appears in tables B17017_002 and B17017_001 respectively
    pov.vars <- c("NAME"
                  , "state"
                  , "county"
                  , "tract"
                  , "B17017_002" # Households with income in the past 12 months below poverty level
                  # We also need to add values for the entire population to obtain percentages
                  , "B17017_001" # Total households
                 )
    # I then remove all of the tables that are not relevant
    poverty.df <- poverty.df[,pov.vars]
    
    # Calculate the percentage of households with income below the poverty level
    # This means that we divide the number of households below the poverty level by the total number of households
    poverty.df$poverty.score <- (poverty.df$B17017_002/poverty.df$B17017_001)*100
    
    # Select all of the values for the census tracts that correspond to the city of Chicago
    poverty.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                            , y = poverty.df
                                            , by = c("tractce10" = "tract")
                                           )
    
################
# Per capita Income
################    
    # Per capita income df. does not need to be modified because it includes all of the data
    # necessary to calculate the hardship index. 
    # I will just change the name of the column to make it easier to bind later in the final df. 
    colnames(pci.df)[5] <- "income"
  
    # Select all of the values for the census tracts that correspond to the city of Chicago
    pci.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                    , y = pci.df
                                    , by = c("tractce10" = "tract")
                                    )
################
# Employment
################    
    # We need to select all of the people who are unemployed (this excludes people in the Armed Forces according to https://www.bls.gov/cps/cps_htgm.htm)
    employ.vars <- c(  "NAME"
                       , "state"
                       , "county"
                       , "tract"
                       , "B23001_008" # Males 16-19 Unemployed
                       , "B23001_015" # Males 20-21 Unemployed
                       , "B23001_022" # Males 22-24 Unemployed
                       , "B23001_029" # Males 25-29 Unemployed
                       , "B23001_036" # Males 30-34 Unemployed
                       , "B23001_043" # Males 35-44 Unemployed
                       , "B23001_050" # Males 45-54 Unemployed
                       , "B23001_057" # Males 55-59 Unemployed 
                       , "B23001_064" # Males 60-61 Unemployed
                       , "B23001_071" # Males 62-64 Unemployed
                       , "B23001_076" # Males 65-69 Unemployed
                       , "B23001_081" # Males 70-74 Unemployed
                       , "B23001_086" # Males over 75 Unemployed
                       , "B23001_094" # Females 16-19 Unemployed
                       , "B23001_101" # Females 20-21 Unemployed
                       , "B23001_108" # Females 22-24 Unemployed
                       , "B23001_115" # Females 25-29 Unemployed
                       , "B23001_122" # Females 30-34 Unemployed
                       , "B23001_129" # Females 35-44 Unemployed
                       , "B23001_136" # Females 45-54 Unemployed
                       , "B23001_143" # Females 55-59 Unemployed 
                       , "B23001_150" # Females 60-61 Unemployed
                       , "B23001_157" # Females 62-64 Unemployed
                       , "B23001_162" # Females 65-69 Unemployed
                       , "B23001_167" # Females 70-74 Unemployed
                       , "B23001_172" # Females over 75 Unemployed
# According to the link above, our unemployment percentage is the number of unemployed
# divided by the people in the labor force. This means that we need to identify the labor force. 
                       , "B23001_004" # Males 16 to 19 in labor force
                       , "B23001_011" # Males 20 to 21 in labor force
                       , "B23001_018" # Males 22 to 24 in labor force
                       , "B23001_025" # Males 25 to 29 in labor force
                       , "B23001_032" # Males 30 to 34 in labor force
                       , "B23001_039" # Males 35 to 44 in labor force
                       , "B23001_046" # Males 45 to 54 in labor force
                       , "B23001_053" # Males 55 to 59 in labor force
                       , "B23001_060" # Males 60 to 61 in labor force
                       , "B23001_067" # Males 62 to 64 in labor force
                       , "B23001_074" # Males 65 to 69 in labor force
                       , "B23001_079" # Males 70 to 75 in labor force
                       , "B23001_084" # Males over 75 in labor force
                       , "B23001_090" # Females 16 to 19 in labor force
                       , "B23001_097" # Females 20 to 21 in labor force
                       , "B23001_104" # Females 22 to 24 in labor force
                       , "B23001_111" # Females 25 to 29 in labor force
                       , "B23001_118" # Females 30 to 34 in labor force
                       , "B23001_125" # Females 35 to 44 in labor force
                       , "B23001_132" # Females 45 to 54 in labor force
                       , "B23001_139" # Females 55 to 59 in labor force
                       , "B23001_146" # Females 60 to 61 in labor force
                       , "B23001_153" # Females 62 to 64 in labor force
                       , "B23001_160" # Females 65 to 69 in labor force
                       , "B23001_165" # Females 70 to 75 in labor force
                       , "B23001_170" # Females over 75 in labor force
                    )
# NOTE: There is another group of data that could potentially serve to calculate unemployment
# C23001_001-093. The age ranges are wider, making this list easier to manage. 
    
    # I then remove all of the tables that are not relevant
    employment.df <- employment.df[,employ.vars]
    
    # Calculate the unemployment rate
    # We have to add the total unemployment and divide it by the size of the labor force
    unemployment <- rowSums(employment.df[,5:30], na.rm = TRUE)
    labor.force <- rowSums(employment.df[,31:56])
    employment.df$unemployment.score <- (unemployment/labor.force)*100
    
    # Select all of the values for the census tracts that correspond to the city of Chicago
    employment.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                           , y = employment.df
                                           , by = c("tractce10" = "tract")
                                            )
    
################
# Crowded Housing
################  
    # We need the percentage of housing units with more than one occupant per room
    crowded.vars <- c(    "NAME"
                        , "state"
                        , "county"
                        , "tract"
                        , "B25014_005" # Owner occupied 1.01 to 1.50 occupants per room
                        , "B25014_006" # Owner occupied 1.51 to 2.00 occupants per room
                        , "B25014_007" # Owner occupied 2.01 or more occupants per room
                        , "B25014_011" # Renter occupied 1.01 to 1.50 occupants per room
                        , "B25014_012" # Renter occupied 1.51 to 2.00 occupants per room
                        , "B25014_013" # Renter occupied 2.01 or more occupants per room
                        # We also need to add values for the entire population to obtain percentages
                        , "B25014_001" # Total households
                        )
    # I then remove all of the tables that are not relevant
    occupants.df <- occupants.df[,crowded.vars]
    
    # Calculate the percentage of households with income below the poverty level
    # This means that we divide the number of households below the poverty level by the total number of households
    occupants.df$crowded.housing.score <- (rowSums(occupants.df[,5:10])/occupants.df$B25014_001)*100
    
    
    # Select all of the values for the census tracts that correspond to the city of Chicago
    crowded.housing.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                                , y = occupants.df
                                                , by = c("tractce10" = "tract")
                                                )
    
    
####################    
# CREATE THE NEW TABLE
####################
    # Create a dataframe with all of the percentages from the individual indicators 
     
    hardship.index  <-  cbind.data.frame( population.chicago.only.df$commarea_n 
                                      , population.chicago.only.df$tractce10
                                      , population.chicago.only.df$community
                                      , population.chicago.only.df$NAME
                                      , population.chicago.only.df$state
                                      , population.chicago.only.df$county
                                      , population.chicago.only.df$population.score
                                      , poverty.chicago.only.df$poverty.score
                                      , pci.chicago.only.df$income
                                      , employment.chicago.only.df$unemployment.score
                                      , crowded.housing.chicago.only.df$crowded.housing.score
                                      , education.chicago.only.df$education.score
                                    )
    
    # Rename the columns (make it comprehensible)  
    colnames(hardship.index) <- c("Commarea_n" , "Tract" , "Comm_Area" , "Tract_Name" , "State"
                                  , "County" , "Dependency" , "Poverty" , "Income" , "Employment"
                                  , "Crowded_Housing" , "Education")
   
     # Calculate the hardship index for the census tracts
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    