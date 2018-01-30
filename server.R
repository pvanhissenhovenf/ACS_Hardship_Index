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
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 1.8%", "1.9 - 3.2%", "3.3 - 4.5%", "4.6 - 7.4%", "7.5 - 15.8%" )
                                    , opacity = 1
                                    , group = "Percent of Crowded Housing"
      ) }
    else if( input$mymap_groups == "Percent of Households Below Poverty" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 12.3%", "12.4 - 16.9%", "17.0 - 21.7%", "21.8 - 29.6%", "29.7 - 56.5%" )
                                    , opacity = 1
                                    , group = "Percent of Households Below Poverty"
      ) }
    else if( input$mymap_groups == "Percent of Aged 16+ Unemployed" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 8.7%", "8.8 - 11.7%", "11.8 - 16.5%", "16.6 - 21.1%", "21.2 - 35.9%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 16+ Unemployed"
      ) }
    else if( input$mymap_groups == "Percent of Aged 25+ Without High School Diploma" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 10.9%", "11.0 - 15.9%", "16.0 - 20.8%", "20.9 - 28.5%", "28.6 - 54.8%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 25+ Without High School Diploma"
      ) }
    else if( input$mymap_groups == "Percent of Aged Under 18 or Over 64" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 30.7%", "30.8 - 36.4%", "36.5 - 39.0%", "39.1 - 41.0%", "41.1 - 51.5%" )
                                    , opacity = 1
                                    , group = "Percent of Aged Under 18 or Over 64"
      ) }
    else if( input$mymap_groups == "Per Capita Income" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "$0 - 14,685", "$14,686 - 17,949", "$17,950 - 23,791", "$23,792 - 33,385", "$33,386 - 88,669" )
                                    , opacity = 1
                                    , group = "Per Capita Income"
      ) }
    else if( input$mymap_groups == "Hardship Index" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0 - 20", "21 - 39", "40 - 58", "59 - 78", "79 - 98" )
                                    , opacity = 1
                                    , group = "Hardship Index"
      ) } # end of else if
  }
  ) # end of observe event
  
} # closing out server