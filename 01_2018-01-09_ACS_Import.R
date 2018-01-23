########################
# Title : ACS_Import
# Purpose : Import census data and select indicators
# Author: Patricia van Hissenhoven 
########################

# import necessary packages
library( acs )
library( sp )
library( rgdal )

# my personal acs api key
my.key <- "6b2a3bf0f9ec6f097062213125bc40cad0351578"

# Identify census tracts within Cook County
cook.county.census.tracts <- geo.make(state = "IL"
                                      , county = "Cook"
                                      , tract = "*"
)

# Download ACS 2011-2015 5-Year Estimate Data
# https://census.gov/programs-surveys/acs/technical-documentation/table-shells.html
# Avoiding the use of the `table.number` parameter

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

# per capita income
# in 2016-inflation adjusted dollars
pci.df <- acs.fetch( endyear = 2016
                         , span = 5
                         , geography = cook.county.census.tracts
                         , variable = "B19301_001"
                         , key = my.key 
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

# filter data to match the socioeconomic descriptors

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

# aggregation - not complete #

# store Chicago current community area
# GeoJSON URL as a character vector
geojson_comarea_url <- "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"

# transform URL character vector into spatial dataframe
comarea606 <- readOGR( dsn = geojson_comarea_url
                       , layer = "OGRGeoJSON"
                       , stringsAsFactors = FALSE
                       , verbose = FALSE # to hide progress message after object is created
)



#2010 Census Tracts - Source: https://www.cityofchicago.org/city/en/depts/doit/dataset/boundaries_-_censustracts.html
chicago.census.tracts <- readOGR( "https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON"
                                  , "OGRGeoJSON"
                                  , stringsAsFactors = FALSE
)
=======
total.pop <- acs.fetch( endyear = 2016
                        , span = 5
                        , geography = my.tract
                        , table.name = "B01001"
                        , dataset = "acs"
                        , col.names = "auto"
                        , case.sensitive = FALSE
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

# overlay to spdf on top of each other
plot( x = chicago.census.tracts, col = "#4d4d4d", border = "#4d4d4d")
plot( x = comarea606, col = "#CCCCCC", border = "#CCCCCC", add = TRUE)
