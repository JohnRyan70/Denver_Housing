# ui.R

shinyUI(fluidPage(
  titlePanel("Denver Five-County Neighborhoods Demographics, 2013"),  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore Denver with census-tract information from the 2013 US Census."),      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Median Housing Values", 
                              "Percent Housing Owners Below $25K Income",
                              "Percent Housing Owners Above $75K Income",
                              "Percent Households with Married-Couple Families",
                              "Median Individual Income, Past 12 Months",
                              "Percent Population With College Degree or More",
                              "Percent Population Below 150 Percent Poverty Level",
                              "Median Year House Built",
                              "Percent Housing Units Vacant",
                              "Percent Housing Units Rented",
                              "Average Hours Worked",
                              "School District State Rank, Same Yr",
                              "School District State Rank, Previous Yr"
                  )
                  
                  selected = "Median Housing Values"),      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 1000000, value = c(0, 1000000))
      ),
    
    mainPanel(plotOutput("map"))
  )
))