
# Mapping the Denver Housing Market
### John Francis Ryan (jfryan70@gmail.com)

# Part One: Acquiring and Merging Data


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

setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')

## Median Housing Value

# The ACS data include the median self-reported house value, our variable of interest.
HouseValue_Median_14 <-read.csv('ACS_14_5YR_B25077_DHP.csv', stringsAsFactors = FALSE)
HouseValue_Median_14 <-select(HouseValue_Median_14, GEO.id2,
                              County, HD01_VD01) %>%
  rename(id=GEO.id2, MedHouseVal=HD01_VD01)
HouseValue_Median_14 <- mutate(HouseValue_Median_14,
                               id=as.character(id),
                               County=as.character(County),
                               MedHouseVal = as.numeric(MedHouseVal))


HouseValue_Median_10 <-read.csv('ACS_10_5YR_B25077_DHP.csv', stringsAsFactors = FALSE)
HouseValue_Median_10 <-select(HouseValue_Median_10, GEO.id2,
                              HD01_VD01) %>%
  rename(id=GEO.id2, MedHouseVal_10=HD01_VD01)
HouseValue_Median_10 <- mutate(HouseValue_Median_10,
                               id=as.character(id),
                               MedHouseVal_10 = as.numeric(MedHouseVal_10))

# Town or neighborhood description

Census2010_Neighborhoods <-read.csv('ACS_2010_Census_Tracts_Neighborhoods_2.csv', stringsAsFactors=FALSE)
Census2010_Neighborhoods <-select(Census2010_Neighborhoods, GEO.id2, GEO.display.label,
                                  Zillow_Name, JR_Name) %>%
  rename(id=GEO.id2, Tract=GEO.display.label)
Census2010_Neighborhoods <- mutate(Census2010_Neighborhoods,
                                   id=as.character(id),
                                   Tract=as.character(Tract),
                                   Zillow_Name=as.character(Zillow_Name),
                                   JR_Name = as.character(JR_Name))

HouseValue_Median_x <-left_join(Census2010_Neighborhoods, HouseValue_Median_14, by=c("id")) 
HouseValue_Median_14 <-left_join(HouseValue_Median_x, HouseValue_Median_10, by=c("id")) 

HouseValue_Median_14$HV_4YrChg <- round((((HouseValue_Median_14$MedHouseVal/HouseValue_Median_14$MedHouseVal_10) - 1)*100), digits = 1)


### Neighborhood Ages

# Age and sex variables
Age_Sex_14 <-read.csv('ACS_14_5YR_S0101_DHP.csv',  stringsAsFactors = FALSE)
Age_Sex_14 <-select(Age_Sex_14, 
                    GEO.id2, 
                    HC01_EST_VC01, HC01_EST_VC07, HC01_EST_VC08,
                    HC01_EST_VC09, HC01_EST_VC10, HC01_EST_VC11,
                    HC01_EST_VC12, HC01_EST_VC13, HC01_EST_VC14,
                    HC01_EST_VC15, HC01_EST_VC16, HC01_EST_VC28, 
                    HC01_EST_VC31, HC01_EST_VC35, HC02_EST_VC35, 
                    HC03_EST_VC35) %>%
  rename(id=GEO.id2, 
         TotalPop=HC01_EST_VC01,
         Perc_20to24=HC01_EST_VC07,
         Perc_25to29=HC01_EST_VC08,
         Perc_30to34=HC01_EST_VC09,
         Perc_35to39=HC01_EST_VC10,
         Perc_40to44=HC01_EST_VC11,
         Perc_45to49=HC01_EST_VC12,
         Perc_50to54=HC01_EST_VC13,
         Perc_55to59=HC01_EST_VC14,
         Perc_60to64=HC01_EST_VC15,
         Perc_65to69=HC01_EST_VC16,
         Perc_18Plus=HC01_EST_VC28,
         Perc_65Plus=HC01_EST_VC31,
         MedAge=HC01_EST_VC35,
         MedAge_M=HC02_EST_VC35,
         MedAge_F=HC03_EST_VC35)

Age_Sex_14 <- mutate(Age_Sex_14,
                     id=as.character(id),
                     TotalPop=as.numeric(TotalPop),
                     Perc_20to24=as.numeric(Perc_20to24),
                     Perc_25to29=as.numeric(Perc_25to29),
                     Perc_30to34=as.numeric(Perc_30to34),
                     Perc_35to39=as.numeric(Perc_35to39),
                     Perc_40to44=as.numeric(Perc_40to44),
                     Perc_45to49=as.numeric(Perc_45to49),
                     Perc_50to54=as.numeric(Perc_50to54),
                     Perc_55to59=as.numeric(Perc_55to59),
                     Perc_60to64=as.numeric(Perc_60to64),
                     Perc_65to69=as.numeric(Perc_65to69),
                     Perc_18Plus=as.numeric(Perc_18Plus),
                     Perc_65Plus=as.numeric(Perc_65Plus),
                     MedAge=as.numeric(MedAge),
                     MedAge_M=as.numeric(MedAge_M),
                     MedAge_F=as.numeric(MedAge_F))

Age_Sex_14$Perc_Under18 <- 100 - (Age_Sex_14$Perc_18Plus)
Age_Sex_14$Perc_20to29 <- (Age_Sex_14$Perc_20to24 + Age_Sex_14$Perc_25to29)
Age_Sex_14$Perc_30to39 <- (Age_Sex_14$Perc_30to34 + Age_Sex_14$Perc_35to39)
Age_Sex_14$Perc_40to49 <- (Age_Sex_14$Perc_40to44 + Age_Sex_14$Perc_45to49)
Age_Sex_14$Perc_50to59 <- (Age_Sex_14$Perc_50to54 + Age_Sex_14$Perc_55to59)
Age_Sex_14$Perc_60to69 <- (Age_Sex_14$Perc_60to64 + Age_Sex_14$Perc_65to69)


### Income

# Financials - S2503
FinChars_14 <-read.csv('ACS_14_5YR_S2503_DHP.csv',  stringsAsFactors = FALSE)
FinChars_14 <-select(FinChars_14, 
                     GEO.id2, 
                     HC01_EST_VC01, HC02_EST_VC01, HC01_EST_VC03,
                     HC02_EST_VC03, HC01_EST_VC04, HC02_EST_VC04,
                     HC01_EST_VC05, HC02_EST_VC05, HC01_EST_VC06,
                     HC02_EST_VC06, HC01_EST_VC07, HC02_EST_VC07,
                     HC01_EST_VC08, HC02_EST_VC08, HC01_EST_VC09,
                     HC02_EST_VC09, HC01_EST_VC10, HC02_EST_VC10,
                     HC01_EST_VC11, HC02_EST_VC11, HC01_EST_VC12,
                     HC02_EST_VC12, HC01_EST_VC13, HC02_EST_VC13) %>%
  rename(id=GEO.id2, 
         Houses_Total=HC01_EST_VC01,
         Houses_Owner=HC02_EST_VC01,
         HHInc_PercUnder5K=HC01_EST_VC03,
         HHInc_PercUnder5K_Owner=HC02_EST_VC03,
         HHInc_Perc5to9K=HC01_EST_VC04,
         HHInc_Perc5to9K_Owner=HC02_EST_VC04,
         HHInc_Perc10to14K=HC01_EST_VC05,
         HHInc_Perc10to14K_Owner=HC02_EST_VC05,
         HHInc_Perc15to20K=HC01_EST_VC06,
         HHInc_Perc15to20K_Owner=HC02_EST_VC06,
         HHInc_Perc20to25K=HC01_EST_VC07,
         HHInc_Perc20to25K_Owner=HC02_EST_VC07,
         HHInc_Perc25to35K=HC01_EST_VC08,
         HHInc_Perc25to35K_Owner=HC02_EST_VC08,
         HHInc_Perc35to50K=HC01_EST_VC09,
         HHInc_Perc35to50K_Owner=HC02_EST_VC09,
         HHInc_Perc50to75K=HC01_EST_VC10,
         HHInc_Perc50to75K_Owner=HC02_EST_VC10,
         HHInc_Perc75to100K=HC01_EST_VC11,
         HHInc_Perc75to100K_Owner=HC02_EST_VC11,
         HHInc_Perc100to150K=HC01_EST_VC12,
         HHInc_Perc100to150K_Owner=HC02_EST_VC12,
         HHInc_Perc150KPlus=HC01_EST_VC13,
         HHInc_Perc150KPlus_Owner=HC02_EST_VC13)


FinChars_14 <- mutate(FinChars_14, 
                      id=as.character(id),
                      Houses_Total=as.numeric(Houses_Total),
                      Houses_Owner=as.numeric(Houses_Owner),
                      HHInc_PercUnder5K=as.numeric(HHInc_PercUnder5K),
                      HHInc_PercUnder5K_Owner=as.numeric(HHInc_PercUnder5K_Owner),
                      HHInc_Perc5to9K=as.numeric(HHInc_Perc5to9K),
                      HHInc_Perc5to9K_Owner=as.numeric(HHInc_Perc5to9K_Owner),
                      HHInc_Perc10to14K=as.numeric(HHInc_Perc10to14K),
                      HHInc_Perc10to14K_Owner=as.numeric(HHInc_Perc10to14K_Owner),
                      HHInc_Perc15to20K=as.numeric(HHInc_Perc15to20K),
                      HHInc_Perc15to20K_Owner=as.numeric(HHInc_Perc15to20K_Owner),
                      HHInc_Perc20to25K=as.numeric(HHInc_Perc20to25K),
                      HHInc_Perc20to25K_Owner=as.numeric(HHInc_Perc20to25K_Owner),
                      HHInc_Perc25to35K=as.numeric(HHInc_Perc25to35K),
                      HHInc_Perc25to35K_Owner=as.numeric(HHInc_Perc25to35K_Owner),
                      HHInc_Perc35to50K=as.numeric(HHInc_Perc35to50K),
                      HHInc_Perc35to50K_Owner=as.numeric(HHInc_Perc35to50K_Owner),
                      HHInc_Perc50to75K=as.numeric(HHInc_Perc50to75K),
                      HHInc_Perc50to75K_Owner=as.numeric(HHInc_Perc50to75K_Owner),
                      HHInc_Perc75to100K=as.numeric(HHInc_Perc75to100K),
                      HHInc_Perc75to100K_Owner=as.numeric(HHInc_Perc75to100K_Owner),
                      HHInc_Perc100to150K=as.numeric(HHInc_Perc100to150K),
                      HHInc_Perc100to150K_Owner=as.numeric(HHInc_Perc100to150K_Owner),
                      HHInc_Perc150KPlus=as.numeric(HHInc_Perc150KPlus),
                      HHInc_Perc150KPlus_Owner=as.numeric(HHInc_Perc150KPlus_Owner))



FinChars_14$HHInc_PercUnder35K <- (FinChars_14$HHInc_PercUnder5K + 
                                     FinChars_14$HHInc_Perc5to9K + 
                                     FinChars_14$HHInc_Perc10to14K + 
                                     FinChars_14$HHInc_Perc15to20K + 
                                     FinChars_14$HHInc_Perc20to25K +
                                     FinChars_14$HHInc_Perc25to35K)

FinChars_14$HHInc_Own_PercUnder35K <- (FinChars_14$HHInc_PercUnder5K_Owner + 
                                         FinChars_14$HHInc_Perc5to9K_Owner + 
                                         FinChars_14$HHInc_Perc10to14K_Owner + 
                                         FinChars_14$HHInc_Perc15to20K_Owner + 
                                         FinChars_14$HHInc_Perc20to25K_Owner +
                                         FinChars_14$HHInc_Perc25to35K_Owner)


FinChars_14$HHInc_Perc35Kto100K<-(FinChars_14$HHInc_Perc35to50K + 
                                    FinChars_14$HHInc_Perc50to75K +
                                    FinChars_14$HHInc_Perc75to100K)

FinChars_14$HHInc_Own_Perc35Kto100K<-(FinChars_14$HHInc_Perc35to50K_Owner + 
                                        FinChars_14$HHInc_Perc50to75K_Owner +
                                        FinChars_14$HHInc_Perc75to100K_Owner)

FinChars_14$HHInc_PercOver100K <-(FinChars_14$HHInc_Perc100to150K + 
                                    FinChars_14$HHInc_Perc150KPlus)

FinChars_14$HHInc_Own_PercOver100K <-(FinChars_14$HHInc_Perc100to150K_Owner + 
                                        FinChars_14$HHInc_Perc150KPlus_Owner)

### Education and Marital Status

# Percent married couple family households
HHnFams_14 <-read.csv('ACS_14_5YR_S1101_DHP.csv', stringsAsFactors = FALSE)
HHnFams_14 <-select(HHnFams_14, GEO.id2, 
                    HC01_EST_VC02, HC02_EST_VC02, HC05_EST_VC02,
                    HC01_EST_VC03, HC02_EST_VC03, 
                    HC01_EST_VC06, HC02_EST_VC06, 
                    HC01_EST_VC07, HC02_EST_VC07, 
                    HC01_EST_VC15, HC02_EST_VC15, HC05_EST_VC15,
                    HC01_EST_VC27, HC02_EST_VC27, HC05_EST_VC27,
                    HC01_EST_VC28, HC02_EST_VC28, HC05_EST_VC28,
                    HC01_EST_VC29, HC02_EST_VC29, HC05_EST_VC29) %>%
  rename(id=GEO.id2, 
         TotalHHs=HC01_EST_VC02,
         TotalHHs_MarriedCoupleFamily=HC02_EST_VC02,
         TotalHHs_Nonfamily=HC05_EST_VC02,
         HHs_AvgSize=HC01_EST_VC03,
         HHs_AvgSize_MarriedCoupleFamily=HC02_EST_VC03,
         HHs_TotalFamilies=HC01_EST_VC06,
         HHs_TotalFamilies_MarriedCoupleFamily=HC02_EST_VC06,
         HHs_AvgFamilySize=HC01_EST_VC07,
         HHs_AvgFamilySize_MarriedCoupleFamily=HC02_EST_VC07,
         HHs_Total=HC01_EST_VC15,
         HHs_Total_MarriedCoupleFamily=HC02_EST_VC15,
         HHs_Total_Nonfamily=HC05_EST_VC15,
         HHs_Total_PercOneUnitStructure=HC01_EST_VC27,
         HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_OneUnitStructure=HC02_EST_VC27,
         HHs_Total_UnitsinStructure_Nonfamily_Perc_OneUnitStructure=HC05_EST_VC27,
         HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure=HC01_EST_VC28,
         HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_TwoPlusUnitStructure=HC02_EST_VC28,
         HHs_Total_UnitsinStructure_Nonfamily_Perc_TwoPlusUnitStructure=HC05_EST_VC28,
         HHs_Total_UnitsinStructure_PercMobileHomeandSimilar=HC01_EST_VC29,
         HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_MobileHomeandSimilar=HC02_EST_VC29,
         HHs_Total_UnitsinStructure_Nonfamily_Perc_MobileHomeandSimilar=HC05_EST_VC29)

HHnFams_14 <- mutate(HHnFams_14,
                     id=as.character(id),
                     TotalHHs=as.numeric(TotalHHs),
                     TotalHHs_MarriedCoupleFamily=as.numeric(TotalHHs_MarriedCoupleFamily),
                     TotalHHs_Nonfamily=as.numeric(TotalHHs_Nonfamily),
                     HHs_AvgSize=as.numeric(HHs_AvgSize),
                     HHs_AvgSize_MarriedCoupleFamily=as.numeric(HHs_AvgSize_MarriedCoupleFamily),
                     HHs_TotalFamilies=as.numeric(HHs_TotalFamilies),
                     HHs_TotalFamilies_MarriedCoupleFamily=as.numeric(HHs_TotalFamilies_MarriedCoupleFamily),
                     HHs_AvgFamilySize=as.numeric(HHs_AvgFamilySize),
                     HHs_AvgFamilySize_MarriedCoupleFamily=as.numeric(HHs_AvgFamilySize_MarriedCoupleFamily),
                     HHs_Total=as.numeric(HHs_Total),
                     HHs_Total_MarriedCoupleFamily=as.numeric(HHs_Total_MarriedCoupleFamily),
                     HHs_Total_Nonfamily=as.numeric(HHs_Total_Nonfamily),
                     HHs_Total_PercOneUnitStructure=as.numeric(HHs_Total_PercOneUnitStructure),
                     HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_OneUnitStructure=as.numeric(HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_OneUnitStructure),
                     HHs_Total_UnitsinStructure_Nonfamily_Perc_OneUnitStructure=as.numeric(HHs_Total_UnitsinStructure_Nonfamily_Perc_OneUnitStructure),
                     HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure=as.numeric(HHs_Total_UnitsinStructure_PercTwoPlusUnitStructure),
                     HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_TwoPlusUnitStructure=as.numeric(HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_TwoPlusUnitStructure),
                     HHs_Total_UnitsinStructure_Nonfamily_Perc_TwoPlusUnitStructure=as.numeric(HHs_Total_UnitsinStructure_Nonfamily_Perc_TwoPlusUnitStructure),
                     HHs_Total_UnitsinStructure_PercMobileHomeandSimilar=as.numeric(HHs_Total_UnitsinStructure_PercMobileHomeandSimilar),
                     HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_MobileHomeandSimilar=as.numeric(HHs_Total_UnitsinStructure_MarriedCoupleFamily_Perc_MobileHomeandSimilar),
                     HHs_Total_UnitsinStructure_Nonfamily_Perc_MobileHomeandSimilar=as.numeric(HHs_Total_UnitsinStructure_Nonfamily_Perc_MobileHomeandSimilar))


HHnFams_14$HHs_Perc_MarriedCplFam <-round(((HHnFams_14$TotalHHs_MarriedCoupleFamily/HHnFams_14$TotalHHs) * 100), digits = 1)


# Marital status, education, and individual income

SCs_14<-read.csv('ACS_14_5YR_S0601_DHP.csv',  stringsAsFactors = FALSE)
SCs_14 <-select(SCs_14, GEO.id2, 
                HC01_EST_VC01, HC01_EST_VC12, HC01_EST_VC38,
                HC01_EST_VC39, HC01_EST_VC40, HC01_EST_VC41,
                HC01_EST_VC42, HC01_EST_VC45, HC01_EST_VC46,
                HC01_EST_VC47, HC01_EST_VC48, HC01_EST_VC49,
                HC01_EST_VC50, HC01_EST_VC53, HC01_EST_VC54,
                HC01_EST_VC55, HC01_EST_VC56, HC01_EST_VC57, 
                HC01_EST_VC58, HC01_EST_VC59, HC01_EST_VC60,
                HC01_EST_VC61, HC01_EST_VC63, HC01_EST_VC66,
                HC01_EST_VC67, HC01_EST_VC68) %>%
  rename(id=GEO.id2, 
         TotalPop=HC01_EST_VC01,
         MedianAge=HC01_EST_VC12,
         Marital_15YearsPlus_Total=HC01_EST_VC38,
         Marital_Perc_NeverMarried=HC01_EST_VC39,
         Marital_Perc_MarriedNotSeparated=HC01_EST_VC40, 
         Marital_Perc_DivorcedorSeparated=HC01_EST_VC41,
         Marital_Perc_Widowed=HC01_EST_VC42,
         EducationLevel_Total=HC01_EST_VC45,
         PercNotHSGrad=HC01_EST_VC46,
         PercHSGradOnly=HC01_EST_VC47,
         PercSomeCollegetoAA=HC01_EST_VC48,
         PercBADegreeOnly=HC01_EST_VC49,
         PercGradorProfDegree=HC01_EST_VC50,
         IndInc_Total=HC01_EST_VC53,
         IndInc_Perc1to10KorLess=HC01_EST_VC54,
         IndInc_Perc10to15K=HC01_EST_VC55,
         IndInc_Perc15to25K=HC01_EST_VC56,
         IndInc_Perc25to35K=HC01_EST_VC57,
         IndInc_Perc35to50K=HC01_EST_VC58,
         IndInc_Perc50to65K=HC01_EST_VC59,
         IndInc_Perc65to75K=HC01_EST_VC60,
         IndInc_Perc75KPlus=HC01_EST_VC61,
         IndInc_Median=HC01_EST_VC63,
         PovertyStatus_TotalPop=HC01_EST_VC66,
         PercUnderPov=HC01_EST_VC67,
         Perc100to150PercPoverty=HC01_EST_VC68)

SCs_14 <- mutate(SCs_14, id=as.character(id),
                 TotalPop=as.numeric(TotalPop),
                 MedianAge=as.numeric(MedianAge),
                 Marital_15YearsPlus_Total=as.numeric(Marital_15YearsPlus_Total),
                 Marital_Perc_NeverMarried=as.numeric(Marital_Perc_NeverMarried),
                 Marital_Perc_MarriedNotSeparated=as.numeric(Marital_Perc_MarriedNotSeparated),
                 Marital_Perc_DivorcedorSeparated=as.numeric(Marital_Perc_DivorcedorSeparated),
                 Marital_Perc_Widowed=as.numeric(Marital_Perc_Widowed),
                 EducationLevel_Total=as.numeric(EducationLevel_Total),
                 PercNotHSGrad=as.numeric(PercNotHSGrad),
                 PercHSGradOnly=as.numeric(PercHSGradOnly),
                 PercSomeCollegetoAA=as.numeric(PercSomeCollegetoAA),
                 PercBADegreeOnly=as.numeric(PercBADegreeOnly),
                 PercGradorProfDegree=as.numeric(PercGradorProfDegree),
                 IndInc_Total=as.numeric(IndInc_Total),
                 IndInc_Perc1to10KorLess=as.numeric(IndInc_Perc1to10KorLess),
                 IndInc_Perc10to15K=as.numeric(IndInc_Perc10to15K),
                 IndInc_Perc15to25K=as.numeric(IndInc_Perc15to25K),
                 IndInc_Perc25to35K=as.numeric(IndInc_Perc25to35K),
                 IndInc_Perc35to50K=as.numeric(IndInc_Perc35to50K),
                 IndInc_Perc50to65K=as.numeric(IndInc_Perc50to65K),
                 IndInc_Perc65to75K=as.numeric(IndInc_Perc65to75K),
                 IndInc_Perc75KPlus=as.numeric(IndInc_Perc75KPlus),
                 IndInc_Median=as.numeric(IndInc_Median),
                 PovertyStatus_TotalPop=as.numeric(PovertyStatus_TotalPop),
                 PercUnderPov=as.numeric(PercUnderPov),
                 Perc100to150PercPoverty=as.numeric(Perc100to150PercPoverty))


SCs_14$IndIncome_PercTotal <- SCs_14$IndInc_Perc1to10KorLess +
  SCs_14$IndInc_Perc10to15K +
  SCs_14$IndInc_Perc15to25K +  
  SCs_14$IndInc_Perc25to35K +
  SCs_14$IndInc_Perc35to50K +
  SCs_14$IndInc_Perc50to65K +
  SCs_14$IndInc_Perc65to75K +
  SCs_14$IndInc_Perc75KPlus


SCs_14$PercHSGradorMore <-SCs_14$PercHSGradOnly + 
  SCs_14$PercSomeCollegetoAA + 
  SCs_14$PercBADegreeOnly + 
  SCs_14$PercGradorProfDegree

SCs_14$PercBAorMore <- SCs_14$PercBADegreeOnly + 
  SCs_14$PercGradorProfDegree

SCs_14$IndInc_PercUpto25K <- SCs_14$IndInc_Perc1to10KorLess + 
  SCs_14$IndInc_Perc10to15K +
  SCs_14$IndInc_Perc15to25K

SCs_14$IndInc_Perc25Kto75K <- SCs_14$IndInc_Perc25to35K +
  SCs_14$IndInc_Perc35to50K +
  SCs_14$IndInc_Perc50to65K +
  SCs_14$IndInc_Perc65to75K

SCs_14$PercUnder150PercPov <-SCs_14$PercUnderPov + 
  SCs_14$Perc100to150PercPoverty


### Property Types

# Median year built
MedianHouseYr_14 <-read.csv('ACS_14_5YR_B25035_DHP.csv', stringsAsFactors = FALSE)
MedianHouseYr_14 <-select(MedianHouseYr_14, GEO.id2, HD01_VD01) %>%
  rename(id=GEO.id2, MedianHouseYr=HD01_VD01)
MedianHouseYr_14 <- mutate(MedianHouseYr_14, id=as.character(id),
                           MedianHouseYr=as.numeric(MedianHouseYr))

# Percent vacant
OSH_14 <-read.csv('ACS_14_5YR_B25002_DHP.csv', stringsAsFactors = FALSE, header=TRUE)
OSH_14 <-select(OSH_14, GEO.id2, 
                HD01_VD01, HD01_VD03) %>%
  rename(id=GEO.id2,
         HousingUnits_Total=HD01_VD01,
         HousingUnits_Vacant=HD01_VD03)
OSH_14 <- mutate(OSH_14, id=as.character(id),
                 HousingUnits_Total=as.numeric(HousingUnits_Total),
                 HousingUnits_Vacant=as.numeric(HousingUnits_Vacant))

OSH_14$Housing_Perc_Vacant <- round(((OSH_14$HousingUnits_Vacant/OSH_14$HousingUnits_Total) * 100), digits = 1)


# Percent owned, and renting
TvRO_14 <-read.csv('ACS_14_5YR_B25003_DHP.csv', stringsAsFactors = FALSE)
TvRO_14 <-select(TvRO_14, GEO.id2, 
                 HD01_VD01, HD01_VD02, HD01_VD03) %>%
  rename(id=GEO.id2, 
         TvRO_Total=HD01_VD01,
         TvRO_Owner=HD01_VD02,
         TvRO_Renter=HD01_VD03)

TvRO_14 <- mutate(TvRO_14, id=as.character(id),
                  TvRO_Total=as.numeric(TvRO_Total),
                  TvRO_Owner=as.numeric(TvRO_Owner),
                  TvRO_Renter=as.numeric(TvRO_Renter))

TvRO_14$Perc_Owned <- round(((TvRO_14$TvRO_Owner/TvRO_14$TvRO_Total) * 100), digits = 1)
TvRO_14$Perc_Rented <- TvRO_14$TvRO_Renter/TvRO_14$TvRO_Total * 100


### The Impact of Working

# Mean hours worked past 12 months

WorkPast12Mos_14 <-read.csv('ACS_14_5YR_S2303_DHP.csv', stringsAsFactors = FALSE)
WorkPast12Mos_14 <-select(WorkPast12Mos_14, GEO.id2, HC01_EST_VC22) %>%
  rename(id=GEO.id2, MeanHrsWkd=HC01_EST_VC22)
WorkPast12Mos_14 <- mutate(WorkPast12Mos_14, id=as.character(id),
                           MeanHrsWkd=as.numeric(MeanHrsWkd))

### The Impact of School Education

SchlDistStateRank_1213 <-read.csv('School_District_Rankings_1213.csv', stringsAsFactors = FALSE)
SchlDistStateRank_1213 <-select(SchlDistStateRank_1213, GEO.id2,
                                Schl_Dist_Rank_2012_13, Schl_Dist_Rank_2011_12) %>%
  rename(id=GEO.id2, 
         SchlDistRnkLYr=Schl_Dist_Rank_2012_13,
         SchlDistRnk2YrAgo=Schl_Dist_Rank_2011_12)
SchlDistStateRank_1213 <- mutate(SchlDistStateRank_1213, id=as.character(id),
                                 SchlDistRnkLYr=as.numeric(SchlDistRnkLYr),
                                 SchlDistRnk2YrAgo=as.numeric(SchlDistRnk2YrAgo))

# Joining files


ACSData1_14<-left_join(HouseValue_Median_14, Age_Sex_14, by=c("id")) 
ACSData2_14<-left_join(ACSData1_14, FinChars_14, by=c("id")) 
ACSData3_14<-left_join(ACSData2_14, HHnFams_14, by=c("id")) 
ACSData4_14<-left_join(ACSData3_14, SCs_14, by=c("id")) 
ACSData5_14<-left_join(ACSData4_14, MedianHouseYr_14, by=c("id")) 
ACSData6_14<-left_join(ACSData5_14, OSH_14, by=c("id")) 
ACSData7_14<-left_join(ACSData6_14, TvRO_14, by=c("id")) 
ACSData8_14<-left_join(ACSData7_14, WorkPast12Mos_14, by=c("id")) 
ACSData9_14<-left_join(ACSData8_14, SchlDistStateRank_1213, by=c("id")) 

save(ACSData9_14, file = "ACSData9_14.RData")

# Our first steps are complete
