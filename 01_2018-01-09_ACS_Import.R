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
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
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
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
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
                                       , 10:173
                                       , sep = "_0"
                              )
                            )
                            , key = my.key
                            )
employment.df <- cbind.data.frame(
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
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
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
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
#### Population for Dependency
  # First I want to understand what each table contains
    names(attributes(population.df)) #  [1] "endyear"        "span"           "acs.units"      "currency.year" 
                                     #  [5] "modified"       "geography"      "acs.colnames"   "estimate"      
                                     #  [9] "standard.error" "class" 
    attr( population.df , "acs.colnames") # Tables numbered B01001_01 - B01001_049)
                                          # Includes data on 49 tables in all census tracts. Dividing population by age group and sex. 
  # We need to figure out what population is younger than 18 or older than 64.
    pop.vars <- c(  "B01001_003" # Males under 5
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
  # try to clean the data from population.df
  
    
  # Convert the data to a data.frame 
    dependency.df <- data.frame( population.df@geography$state
                                , population.df@geography$county
                                , population.df@geography$tract
                                , population.df@estimate[, pop.vars]
                                , stringsAsFactors = FALSE
                                )
    dependency.df$population.df.geography.tract <- as.numeric(dependency.df$population.df.geography.tract)
                                
  # Calculate the percentage of people in dependency
  # This means that we add the populations under 18 and over 64 and divide by the total of the population
    
    dependency.df$percent <-  rowsum(dependency.df[,4:23])
    
    
    

