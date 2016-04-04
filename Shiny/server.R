# server.R
setwd('C:/Users/John/Denver_Housing_Project/Shiny_Maps/Data')

library(shiny)
library(maps)
library(mapproj)
load("ACSData10.RData")

               
shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      data <- switch(input$var, 
                     "Median Housing Values" = ACSData10$HouseValue_Median
    )
      
      color <- switch(input$var, 
                      "Median Housing Values" = "darkgreen",
                      "Percent Housing Owners Below $25K Income" = "black",
                      "Percent Housing Owners Above $75K Income" = "darkorange",
                      "Percent Households with Married-Couple Families" = "darkviolet",
                      "Median Individual Income, Past 12 Months" = "green",
                      "Percent Population With College Degree or More" = "blue",
                      "Percent Population Below 150 Percent Poverty Level" = "red",
                      "Median Year House Built" = "yellow",
                      "Percent Housing Units Vacant" = "pink",
                      "Percent Housing Units Rented" = "orange",
                      "Average Hours Worked" = "darkred",
                      "School District State Rank, Same Yr" = "darkblue",
                      "School District State Rank, Previous Yr" = "violet")
    
    
      legend <- switch(input$var, 
                       "Median Housing Values" = "Median House Value",
                       "Percent Housing Owners Below $25K Income" = "% Owners Below $25K Inc",
                       "Percent Housing Owners Above $75K Income" = "% Owners Above $75K Inc",
                       "Percent Households with Married-Couple Families" = "% HH as Married-Couple Family",
                       "Median Individual Income, Past 12 Months" = "Median Individual Inc",
                       "Percent Population With College Degree or More" = "% w/College Degree+",
                       "Percent Population Below 150 Percent Poverty Level" = "% Pop Under 150% Poverty",
                       "Median Year House Built" = "Median Yr House Built",
                       "Percent Housing Units Vacant" = "% Housing Vacancy",
                       "Percent Housing Units Rented" = "% Houses Rented",
                       "Average Hours Worked" = "Average Hrs Worked",
                       "School District State Rank, Same Yr" = "Schl Dist State Rank, Same Yr",
                       "School District State Rank, Previous Yr" = "Schl Dist State Rank, Previous Yr")
      
      
      
      percent_map(var = data, 
                  color = color, 
                  legend.title = legend, 
                  max = input$range[2], 
                  min = input$range[1])
    })
  }
)