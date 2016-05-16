setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')

# Load the data
load("ACSData9.RData")

# Get Denver Coordinates 
DenverLon <- -104.9903
DenverLat <-  39.73924

# Mapping data using Leaflet
library(leaflet)
library(rgdal)
tract <- readOGR("cb_2014_08_tract_500k.shp",
                 layer = "cb_2014_08_tract_500k", verbose = FALSE)

# Fix the id in ACSData9
ACSData9$id <- paste("0", ACSData9$id, sep ="")
str(ACSData9$id)

spdf <- merge(tract, ACSData9, by.x = 'GEOID', by.y = 'id')


# Create Shiny App
library(shiny)
ui <- fluidPage(
  titlePanel("Denver Housing Leaflet Map"),  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Explore Denver with census-tract information 
               from the 2013 US Census."),      
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Median Housing Values", 
                              "Total Population",
                              "Median Age of Population",
                              "Percentage of People Under 18",
                              "Percentage of People 65 and Older",
                              "Household Income: Percent Under $25K",
                              "Household Income: Percent $25 to 75K",
                              "Household Income: Percent Above $75K",
                              "Perent of Houses with Multiple Units",
                              "Percent of Households as Married Couple Family",
                              "Percent of Population as Never Married",
                              "Percent of Adults with at Least High School Degree",
                              "Percent of Adults with at Least Bachelor's Degree",
                              "Median Year House Built",
                              "Percent of Housing Units Vacant",
                              "Percent of Houses as Owned (not Rented)",
                              "State Ranking for School District",
                              "State Ranking for School District, Previous Year"
                              
                              
                  ),
                  selected = "Median Housing Values")  
      
      ),
    mainPanel(leafletOutput("mymap"))
  )
  )

server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    
    data <- switch(input$var, 
                   "Median Housing Values" = spdf$Median_House_Value,
                   "Total Population" = spdf$TotalPop,
                   "Median Age of Population" = spdf$MedianAge,
                   "Percentage of People Under 18" = spdf$Perc_Under18,
                   "Percentage of People 65 and Older" = spdf$Perc_65Plus,
                   "Household Income: Percent Under $25K" = spdf$HHInc12Mos_Owner_Perc25KandBelow,
                   "Household Income: Percent $25 to 75K"= spdf$HHInc12Mos_Owner_Perc25Kto75K,
                   "Household Income: Percent Above $75K"= spdf$HHInc12Mos_Owner_Perc75KandAbove,
                   "Perent of Houses with Multiple Units" = spdf$HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure,
                   "Percent of Households as Married Couple Family"= spdf$HHs_Perc_MarriedCoupleFamilyOnly,
                   "Percent of Population as Never Married"= spdf$SCs_Marital_Perc_NeverMarried,
                   "Percent of Adults with at Least High School Degree"= spdf$SCs_PercAtLeastHSGrad,
                   "Percent of Adults with at Least Bachelor's Degree"= spdf$SCs_PercAtLeastBADegree,
                   "Median Year House Built"= spdf$House_MedianYearBuilt,
                   "Percent of Housing Units Vacant"= spdf$HousingUnits_Perc_Vacant,
                   "Percent of Houses as Owned (not Rented)"= spdf$TvRO_Perc_Owner,
                   "State Ranking for School District"= spdf$Same_School_Year_Ranking,
                   "State Ranking for School District, Previous Year"= spdf$Last_School_Year_Ranking
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
