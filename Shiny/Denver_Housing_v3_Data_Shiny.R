#####
#
# Create Shiny App to map housing values and important factors


setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')
load('ACSData10.RData', verbose=FALSE)

library(shiny)
library(leaflet)
library(rgdal)
library(ggplot2)

###################################################################
# Creating leaflet and Shiny app
#
#

library(shiny)
library(leaflet)
library(rgdal)
library(ggplot2)
library(tmap)


###############
#
#
#  Start with basic map


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-105, lat=39.7, popup="Center of map")
m

m2 <- leaflet() %>% setView(lng = -105, lat = 39.6, zoom = 8)
m2 %>% addTiles()

leaflet(data = ACSData10) %>% addTiles() %>%
  addMarkers(~long, ~lat)


m %>% fitBounds(-106, 40.3, -104, 39.1)


#####
#
# Create a static map

qtm(ACSData10, "HouseValue_Median")

qtm(ACSData10, fill="HouseValue_Median", text="iso_a3", text.size="AREA", root=5, fill.title="Median House Value")



tm_shape(ACSData10) +
  tm_fill("HouseValue_Median", title="Median House Value, 2013", palette = "PRGn") +
  tm_borders(alpha=.5) +
  tm_text("Census Tract", size=0.8)


#####
#
# Create leaflet


housePalette <- colorFunction(palette = "colors I want", domain = ACSData10$HouseValue_Median)

tractpopup <- paste0("Census Tract: ", ACSData10$id, "Median Value: ", ACSDAta10$HouseValue_Median)



leaflet(data = ACSData10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE,
              smoothFactor=0.2,
              fillOpacity = .8,
              popup = tractpopup,
              color= ~housePalette(ACSData10$HouseValue_Median)
  )

educationPalette <-colorNumeric(palette = "Purples", domain = ACSData10$SCs_TotalPop_25YrsPlus_EducationLevel_Perc_AtLeastBADegree)

housepopup <-paste0("Census Tract: ", ACSData10$id),
"Median House Value: ", ACSData10$HouseValue_Median,
"Pct w college ed: ", ACSData10$SCs_TotalPop_25YrsPlus_EducationLevel_Perc_AtLeastBADegree)

leaflet(Denverhousing) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=housepopup
              color= ~housePalette(ACSData10$HouseValue_Median)
  ) %>%
  
  
  
  Denverhousingmap <- leaflet(Denverhousing) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=popup, 
              color= ~housePalette(ACSData10$HouseValue_Median)
              group="Median House Value"
  ) %>% 
  addLegend(position="bottomleft", colors=c("#984ea3"), labels=c("Median House Value"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~educationPalette(scmap@data$`Donald J TrumpPct`),
              group="Education"
  ) %>%
  
  
  addLayersControl(
    baseGroups=c("Winners", "Trump", "Rubio", "Cruz", "College degs"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 
