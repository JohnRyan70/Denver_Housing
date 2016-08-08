setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')

# Load the data
load("ACSData9_14.RData")

# Get Denver Coordinates 
DenverLon <- -104.9903
DenverLat <-  39.73924

# Mapping data using Leaflet
library(leaflet)
library(rgdal)
tract <- readOGR("cb_2014_08_tract_500k.shp",
                 layer = "cb_2014_08_tract_500k", verbose = FALSE)

# Fix the id in ACSData9
ACSData9_14$id <- paste("0", ACSData9_14$id, sep ="")
str(ACSData9_14$id)

spdf <- merge(tract, ACSData9_14, by.x = 'GEOID', by.y = 'id')


# Create Shiny App
library(shiny)
ui <- fluidPage(
  titlePanel("Denver Housing Leaflet Map"),  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Explore Denver with census-tract information 
               from the 2014 US Census' American Community Survey."),      
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Median Housing Values", 
                              "Percent change, median housing ('10 to '14)",
                              "Total Population",
                              "Median Age of Population",
                              "Percentage of People Under 18",
                              "Percentage of People 65 and Older",
                              "Household Income: Percent Under $35K",
                              "Household Income: Percent $35 to 100K",
                              "Household Income: Percent Above $100K",
                              "Perent of Houses with Multiple Units",
                              "Percent of Households as Married Couple Family",
                              "Percent of Population as Never Married",
                              "Percent of Adults with at Least High School Degree",
                              "Percent of Adults with at Least Bachelor's Degree",
                              "Median Year House Built",
                              "Percent of Housing Units Vacant",
                              "Percent of Houses as Owned (not Rented)",
                              "State Ranking for School District, Past Year",
                              "State Ranking for School District, Two Years Ago"
                              
                              
                  ),
                  selected = "Median Housing Values")  
      
      ),
    mainPanel(leafletOutput("mymap"))
  )
  )

server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    
    data <- switch(input$var, 
                   "Median Housing Values" = spdf$MedHouseVal,
                   "Percent change, median housing ('10 to '14)" = spdf$HV_4YrChg,
                   "Total Population" = spdf$TotalPop.x,
                   "Median Age of Population" = spdf$MedAge,
                   "Percentage of People Under 18" = spdf$Perc_Under18,
                   "Percentage of People 65 and Older" = spdf$Perc_65Plus,
                   "Household Income: Percent Under $35K" = spdf$HHInc_PercUnder35K,
                   "Household Income: Percent $35 to $100K"= spdf$HHInc_Own_Perc35Kto100K,
                   "Household Income: Percent Above $100K"= spdf$HHInc_Own_PercOver100K,
                   "Perent of Houses with Multiple Units" = spdf$HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure,
                   "Percent of Households as Married Couple Family"= spdf$HHs_Perc_MarriedCplFam,
                   "Percent of Population as Never Married"= spdf$Marital_Perc_NeverMarried,
                   "Percent of Adults with at Least High School Degree"= spdf$PercHSGradorMore,
                   "Percent of Adults with at Least Bachelor's Degree"= spdf$PercBAorMore,
                   "Median Year House Built"= spdf$MedianHouseYr,
                   "Percent of Housing Units Vacant"= spdf$Housing_Perc_Vacant,
                   "Percent of Houses as Owned (not Rented)"= spdf$Perc_Owned,
                   "State Ranking for School District, Past Year"= spdf$SchlDistRnkLYr,
                   "State Ranking for School District, Two Years Ago"= spdf$SchlDistRnk2YrAgo
    )
    
    fill_map <<- data
    
    popup <- paste0("<strong>", fill_map, "</strong>")
    
    leaflet(spdf)  %>%
      setView(lat = DenverLat, lng = DenverLon, zoom = 11) %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        popup = popup,
        color = ~colorNumeric(palette = "RdYlGn",
                              domain = fill_map)(fill_map)
      )
  })
}

shinyApp(ui, server)

