# 
# Author: Grace Hwang
# Purpose: Draft map
#

#############################
## Load necessary packages ##
#############################
library( leaflet )
library( htmltools )
library( shiny )
library( sp )
library( rgdal )

######################
## Build the server ##
######################
server <- function( input, output ) {
  
  # render leaflet output
  output$mymap <- leaflet::renderLeaflet({
    census_map 
  }) # end of renderleaflet
  
  # Dynamic Legend
  observeEvent( input$mymap_groups,{
    
    mymap <- leafletProxy( "mymap" ) %>% clearControls()
    
    if( input$mymap_groups == "Percent of Crowded Housing" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "0.3% - 1.8%"
                                 , "1.9% - 3.2%"
                                 , "3.3% - 4.5%"
                                 , "4.6% - 7.4%"
                                 , "7.5% - 15.8%" )
                   , opacity = 1
                   , group = "Percent of Crowded Housing" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Households Below Poverty" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "3.3% - 12.3%"
                                 , "12.9% - 16.9%"
                                 , "17.1% - 21.7%"
                                 , "23.4% - 29.6%"
                                 , "29.8% - 56.5%" )
                   , opacity = 1
                   , group = "Percent of Households Below Poverty" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged 16+ Unemployed" ) {
      mymap <- mymap %>%
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "4.7%- 8.7%"
                                 , "8.8% - 11.7%"
                                 , "12.1% - 16.5%"
                                 , "16.7% - 21.1%"
                                 , "21.2% - 35.9%" )
                   , opacity = 1
                   , group = "Percent of Aged 16+ Unemployed" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged 25+ Without High School Diploma" ) {
      mymap <- mymap %>%
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "2.5% - 10.9%"
                                 , "11.0% - 15.9%"
                                 , "16.2% - 20.8%"
                                 , "21.0% - 28.5%"
                                 , "31.2% - 54.8%" )
                   , opacity = 1
                   , group = "Percent of Aged 25+ Without High School Diploma" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged Under 18 or Over 64" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "13.5% - 30.7%"
                                 , "31.0% - 36.4%"
                                 , "36.8% - 39.0%"
                                 , "39.2% - 41.0%"
                                 , "41.1% - 51.5%" )
                   , opacity = 1
                   , group = "Percent of Aged Under 18 or Over 64" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Per Capita Income" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "$8,201 - $14,685"
                                 , "$15,089 - $17,949"
                                 , "$18,672 - $23,791"
                                 , "$23,939 - $33,385"
                                 , "$34,381 - $88,669" )
                   , opacity = 1
                   , group = "Per Capita Income" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Hardship Index" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "1 - 20"
                                 , "21 - 39"
                                 , "41 - 58"
                                 , "60 - 78"
                                 , "79 - 98" )
                   , opacity = 1
                   , group = "Hardship Index" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
    } # end of else if statements
    
  }) # end of observe event
  
} # closing out server