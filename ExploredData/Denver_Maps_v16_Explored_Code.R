
#################################
#

setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')

load("ACSData9_14.RData")

# Noticing the nine missing values
table(is.na(ACSData9_14$MedHouseVal))

# Removing the missing values
test <- subset(ACSData9_14, is.na(MedHouseVal))
ACSData9_14nona <- subset(ACSData9_14, !is.na(MedHouseVal)) 

#Now no NA's in median housing value
table(is.na(ACSData9_14nona$MedHouseVal)) 
save(ACSData9_14nona, file = "ACSData9_14_nona.RData")


library(maptools)
library(rgdal)
library(sp)
library(ggplot2)
library(ggmap)
library(digest)
library(Hmisc)
gpclibPermit()
library(psych)
library(ggthemes)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(colorspace)

ACSData9_14nona.mhv <-ACSData9_14nona[c("Tract",
                                        "JR_Name",
                                        "County",
                                        "MedHouseVal",
                                        "MedHouseVal_10",
                                        "HV_4YrChg")]

save(ACSData9_14nona.mhv, file = "ACSData9_14_nona.mhv.RData")

class(ACSData9_14nona.mhv)
class(ACSData9_14nona.mhv$MedHouseVal)

summary(ACSData9_14nona$MedHouseVal)
summary(ACSData9_14nona$MedHouseVal_10)
summary(ACSData9_14nona$HV_4YrChg)


library(plotly)
library(ggplot2)

qplot(x = MedHouseVal, data = ACSData9_14, binwidth = 10000,
      xlab = 'Median housing value',
      ylab = 'Number of neighborhoods',
      color = MedHouseVal)


ACSData9_14nona$MedHouseVal2 <-ACSData9_14nona$MedHouseVal/1000

t <- ggplot(ACSData9_14nona, aes(x=County, y=MedHouseVal2)) + stat_summary(fun.y="median", geom="bar", fill="darkgreen", color="lightgreen")
t + ylab("Middle value") + xlab("County") +
  labs(title = "Median Housing by County") +
  geom_hline(yintercept=seq(0, 380, by=10), alpha=0.10)

aggdata <-aggregate(ACSData9_14nona$MedHouseVal, by=list(ACSData9_14nona$County), 
                    FUN=median, na.rm=TRUE)
aggdata
summary(ACSData9_14nona$MedHouseVal, na.rm=TRUE)


ACSData9_14nona.mhv %>%
  arrange(-MedHouseVal) %>%
  subset(MedHouseVal > 600000, select = c(Tract, JR_Name, County, MedHouseVal, MedHouseVal_10, HV_4YrChg))

ACSData9_14nona.mhv %>%
  arrange(MedHouseVal) %>%
  subset(MedHouseVal < 108800, select = c(Tract, JR_Name, County, MedHouseVal, MedHouseVal_10, HV_4YrChg))


qplot(x = HV_4YrChg, data = ACSData9_14, binwidth = 5,
      xlab = 'Four year change, median housing value',
      ylab = 'Number of neighborhoods')
describe(ACSData9_14$HV_4YrChg)
summary(ACSData9_14$HV_4YrChg)

ACSData9_14nona.mhv$MedHouseVal_10_2 <-ACSData9_14nona$MedHouseVal_10/1000

n <-ACSData9_14nona.mhv %>%
  filter(MedHouseVal_10 < 1000000 & HV_4YrChg < 100) %>%
  ggplot(aes(x = MedHouseVal_10_2, y = HV_4YrChg)) +
  geom_point(alpha = 0.7) + stat_smooth(method = "lm")

n + ylab("Four-year change") + xlab("Median housing value") +
  labs(title = "Four-year change, median housing (2010-2014)") +
  geom_hline(yintercept=seq(0, 20, by=10), alpha=0.10)


ACSData9_14nona.mhv %>%
  arrange(-HV_4YrChg) %>%
  subset(HV_4YrChg >= 20.0, select = c(Tract, JR_Name, County, MedHouseVal, MedHouseVal_10, HV_4YrChg))


ACSData9_14nona.mhv %>%
  arrange(HV_4YrChg) %>%
  subset(HV_4YrChg < -22.0, select = c(Tract, JR_Name, County, MedHouseVal, MedHouseVal_10, HV_4YrChg))

ACSData9_14nona.mhv %>%
  arrange(-MedHouseVal_10) %>%
  subset(MedHouseVal_10 >= 444000 & MedHouseVal_10 < 1000000, select = c(Tract, JR_Name, County, MedHouseVal, MedHouseVal_10, HV_4YrChg))

o <-ACSData9_14nona.mhv %>%
  filter(MedHouseVal_10 >= 444000 & MedHouseVal_10 < 1000000) %>%
  ggplot(aes(x = MedHouseVal_10_2, y = HV_4YrChg)) +
  geom_point(alpha = 0.7) + stat_smooth(method = "lm")

o + ylab("Four-year change") + xlab("Median housing value, 2010") +
  labs(title = "Four-year change, median housing (2010-2014)") +
  geom_hline(yintercept=seq(0, 20, by=10), alpha=0.10)


ACSData9_14nona2 <-ACSData9_14nona[c("MedHouseVal",
                                     "Perc_18Plus",
                                     "Perc_65Plus",
                                     "MedAge",
                                     "Perc_Under18",
                                     "HHInc_Perc35to50K",
                                     "HHInc_Perc35to50K_Owner",
                                     "HHInc_Perc50to75K",
                                     "HHInc_Perc50to75K_Owner",
                                     "HHInc_Perc75to100K",
                                     "HHInc_Perc75to100K_Owner",
                                     "HHInc_PercUnder35K",
                                     "HHInc_Own_PercUnder35K",
                                     "HHInc_Perc35Kto100K",
                                     "HHInc_Own_Perc35Kto100K",
                                     "HHInc_PercOver100K",
                                     "HHInc_Own_PercOver100K",
                                     "HHs_Perc_MarriedCplFam",
                                     "MedianAge",
                                     "PercNotHSGrad",
                                     "PercGradorProfDegree",
                                     "IndInc_Perc25to35K",
                                     "IndInc_Perc35to50K",
                                     "IndInc_Perc50to65K",
                                     "IndInc_Perc75KPlus",
                                     "IndInc_Median",
                                     "PercBAorMore",
                                     "IndInc_PercUpto25K",
                                     "IndInc_Perc25Kto75K",
                                     "IndInc_Perc75KPlus",
                                     "PercUnder150PercPov",
                                     "Perc_Rented",
                                     "MeanHrsWkd",
                                     "SchlDistRnkLYr")]
save(ACSData9_14nona2, file = "ACSData9_14_nona2.RData")


library(corrgram)
corrgram(ACSData9_14nona2, order=NULL, 
         lower.panel=panel.shade,
         upper.panel=NULL,
         text.panel=panel.txt,
         main="Correlogram of data")


p1 <- ggplot(aes(x = HHInc_Own_PercUnder35K, 
                 y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Owners with Income below $35K", y="Median House Value")
p1 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Own_PercUnder35K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_PercUnder35K, use = "pairwise.complete.obs", method = c("pearson"))

p2 <-ggplot(aes(x = HHInc_Own_Perc35Kto100K, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Owners with Income from $35K to $100K", y="Median House Value")
p2 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc35Kto100K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Own_Perc35Kto100K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc35to50K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc35to50K_Owner, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc50to75K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc50to75K_Owner, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc75to100K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Perc75to100K_Owner, use = "pairwise.complete.obs", method = c("pearson"))

p3 <-ggplot(aes(x = HHInc_Own_PercOver100K, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Owners with Income at Least $100K", y="Median House Value")
p3 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_PercOver100K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHInc_Own_PercOver100K, use = "pairwise.complete.obs", method = c("pearson"))


p33 <-ggplot(aes(x = IndInc_Median, 
                 y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Population with Individual Income from $25K to $75K", y="Median House Value")
p33 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Median, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc25to35K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc35to50K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc50to65K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc65to75K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc75KPlus, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_PercUpto25K, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$IndInc_Perc25Kto75K, use = "pairwise.complete.obs", method = c("pearson"))


p4 <-ggplot(aes(x = HHs_Perc_MarriedCplFam, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Households as Married-Couple Families", y="Median House Value")
p4 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$HHs_Perc_MarriedCplFam, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHs_AvgFamilySize, use = "pairwise.complete.obs", method = c("pearson"))

p6 <-ggplot(aes(x = PercBAorMore, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of People with at Least Bachelor's Degree", y="Median House Value")
p6 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$PercBAorMore, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercNotHSGrad, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercHSGradOnly, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercSomeCollegetoAA, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercHSGradorMore, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercBADegreeOnly, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercGradorProfDegree, use = "pairwise.complete.obs", method = c("pearson"))


p7 <-ggplot(aes(x = PercUnder150PercPov, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of People Living Below 150 Percent of Poverty Level", y="Median House Value")
p7 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$PercUnder150PercPov, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$PercUnderPov, use = "pairwise.complete.obs", method = c("pearson"))

p8 <-ggplot(aes(x = MedianHouseYr, 
                y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Median Year House Built", y="Median House Value")
p8 + stat_smooth(method = "lm", alpha = .2, size = 0.5)
cor(ACSData9_14$MedHouseVal, ACSData9_14$MedianHouseYr, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure, use = "pairwise.complete.obs", method = c("pearson"))


p10 <-ggplot(aes(x = Perc_Rented, 
                 y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Percent of Units Rented [not Owned]", y="Median House Value")
p10 + stat_smooth(method = "lm", alpha = .2, size = 0.5)
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_Rented, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Housing_Perc_Vacant, use = "pairwise.complete.obs", method = c("pearson"))

p11 <-ggplot(aes(x = MeanHrsWkd, 
                 y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Person's Average Usual Hours Worked", y="Median House Value")
p11 + stat_smooth(method = "lm", alpha = .2, size = 0.5)
cor(ACSData9_14$MedHouseVal, ACSData9_14$MeanHrsWkd, use = "pairwise.complete.obs", method = c("pearson"))
p12 <-ggplot(aes(x = SchlDistRnkLYr, y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) + 
  labs(x="School District Ranking, Prior Year", y="Median House Value")
p13 <- p12 + stat_smooth(method = "lm", alpha = .2, size = 0.5)
p13 + geom_jitter(width=.08)
cor(ACSData9_14$MedHouseVal, ACSData9_14$SchlDistRnkLYr, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$SchlDistRnk2YrAgo, use = "pairwise.complete.obs", method = c("pearson"))
p14 <-ggplot(aes(x = MedAge, 
                 y = MedHouseVal), data = ACSData9_14) +
  geom_point(alpha = .4) +
  labs(x="Neighborhood Median Age", y="Median House Value")
p14 + stat_smooth(method = "lm", alpha = .2, size = 0.5)

cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_18Plus, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_65Plus, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$MedAge, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_Under18, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_20to29, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_30to39, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_40to49, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_50to59, use = "pairwise.complete.obs", method = c("pearson"))
cor(ACSData9_14$MedHouseVal, ACSData9_14$Perc_60to69, use = "pairwise.complete.obs", method = c("pearson"))

#  Start mapping
#  Define map source type and color


DenverMetro <- c(-106, 40.3, -104, 39.1)
DenverMetroMap <- get_map(location=DenverMetro, source = "google", 
                          maptype = "roadmap", zoom = 11, crop=FALSE)


# Add polygons from tract file
# https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')
getwd()
tract <- readOGR(dsn=".", layer = "cb_2014_08_tract_500k")
tract@data$GEOID<-as.character(tract@data$GEOID)


# convert polygons to data.frame
Denver_tract<-fortify(tract, region = "GEOID") 
str(Denver_tract)
Denver_tract$id <-substring(Denver_tract$id, 2) # id had extra 0 to left

str(Denver_tract$id)

library(reshape) 
Denver_tract <-rename(Denver_tract, c('id'='id2'))



##########################################
#
# Join polygons to housing value data
#

Denver_tract$id <-as.character(Denver_tract$id)
ACSData9_14nona$id <-as.character(ACSData9_14nona$id)


ACSData10_14 <-left_join(Denver_tract, ACSData9_14nona, by=c('id')) 

save(ACSData10_14, file = "ACSData10_14.RData")


###########################
# 
# Plot median housing value
# 

describe(ACSData9_14$MedHouseVal, na.rm = TRUE)
quantile(ACSData9_14$MedHouseVal, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

# Plot median housing values

ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = MedHouseVal),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .3968125, .330950, .2810125, .243100, .215150, .181900, .149325, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Median House Value by Census Tract, 2014')



#########################
#
# Plot Low-income households
#

describe(ACSData9_14$HHInc_Own_PercUnder35K, na.rm = TRUE)
quantile(ACSData9_14$HHInc_Own_PercUnder35K, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)


ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = HHInc_Own_PercUnder35K), data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .184, .14, .106, .0895, .071, .05, .031, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Owners with Household Income Below $35K')

##########################################
# 
# Plot middle-income values
#

describe(ACSData9_14$HHInc_Own_Perc35Kto100K, na.rm = TRUE)
quantile(ACSData9_14$HHInc_Own_Perc35Kto100K, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)


ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = HHInc_Own_Perc35Kto100K), data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .548, .4805, .41675, .355, .313, .255, .189375, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Owners with Household Income from $35K to $100K')



##########################################
# 
# Plot upper-income values
#

describe(ACSData9_14$HHInc_Own_PercOver100K, na.rm = TRUE)
quantile(ACSData9_14$HHInc_Own_PercOver100K, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = HHInc_Own_PercOver100K),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .748, .68, .607875, .545, .47025, .372, .289375, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Owners with Household Income Above $100K')



##########################################
# 
# Plot married-couple values
#

describe(ACSData9_14$HHs_Perc_MarriedCplFam, na.rm = TRUE)
quantile(ACSData9_14$HHs_Perc_MarriedCplFam, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)


ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = HHs_Perc_MarriedCplFam),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(100, .7175, .6265, .5455, .474, .42025, .355, .278, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Married Couple Families, 2014')


##########################################
# 
# Plot percent with college degree values
#

describe(ACSData9_14$PercBAorMore, na.rm = TRUE)
quantile(ACSData9_14$PercBAorMore, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = PercBAorMore),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .635125, .552, .475, .394, .313625, .23175, .135, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent of People with A College Degree or More')


##########################################
# 
# Plot percent of people below 150 percent poverty level values
#

describe(ACSData9_14$PercUnder150PercPov, na.rm = TRUE)
quantile(ACSData9_14$PercUnder150PercPov, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = PercUnder150PercPov),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .398, .3035, .217, .157, .11225, .082, .052, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Living Below 150 Perent Poverty Level')



####################################
# 
# Plot percent vacant data
#

describe(ACSData9_14$Housing_Perc_Vacant, na.rm = TRUE)
quantile(ACSData9_14$Housing_Perc_Vacant, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

# Plot percent vacant values


ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = Housing_Perc_Vacant),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .09425, .073, .06, .048, .036, .0255, .012, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Housing Units Vacant by Census Tract, 2014')

####################################
# 
# Plot renter data


describe(ACSData9_14$Perc_Rented, na.rm = TRUE)
quantile(ACSData9_14$Perc_Rented, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

# Plot percent renter built values


ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = Perc_Rented),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .65697053, .51911798, .41133582, .31193581, .23761539, .15011655, .07952541, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Percent Housing Units Rented by Census Tract, 2014')


####################################
#
# Plot school district rank data

describe(ACSData9_14$SchlDistRnkLYr, na.rm = TRUE)
quantile(ACSData9_14$SchlDistRnkLYr, c(.875, .75, .625, .5, .375, .25, .125), na.rm = TRUE)

ggmap(DenverMetroMap) + 
  geom_polygon(aes(x = long, y = lat, group=id, fill = SchlDistRnkLYr),
               data = ACSData10_14, color="black", alpha = .4, size = .2) +
  scale_fill_gradientn(colours = c("red4", "red2", "red", "lightcoral", "lightblue3", "cyan4", "blue", "midnightblue"),
                       values = c(1, .763, .682, .59, .59, .558, .426, .294, 0)) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('School District State Rank by Census Tract, 2013')

