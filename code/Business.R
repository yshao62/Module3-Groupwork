##read dataset
BreakfastData=read.table("BreakfastData.tsv",sep="\t",header=T)
BreakfastCare=BreakfastData[BreakfastData$is_open==1,c(1,2,3,4,5,9,10,11,13,14,17,20,25)]

##scale feature
unique(BreakfastCare$attributes.BusinessAcceptsCreditCards)
BreakfastCare[is.na(BreakfastCare$attributes.BusinessAcceptsCreditCards),"attributes.BusinessAcceptsCreditCards"]='False'
BreakfastCare[BreakfastCare$attributes.BusinessAcceptsCreditCards!="True","attributes.BusinessAcceptsCreditCards"]='False'
BreakfastCare[BreakfastCare$attributes.BusinessAcceptsCreditCards=="False","attributes.BusinessAcceptsCreditCards"]=0
BreakfastCare[BreakfastCare$attributes.BusinessAcceptsCreditCards=="True","attributes.BusinessAcceptsCreditCards"]=1

unique(BreakfastCare$attributes.BikeParking)
BreakfastCare[is.na(BreakfastCare$attributes.BikeParking),"attributes.BikeParking"]='False'
BreakfastCare[BreakfastCare$attributes.BikeParking=="None","attributes.BikeParking"]='False'
BreakfastCare[BreakfastCare$attributes.BikeParking=="False","attributes.BikeParking"]=0
BreakfastCare[BreakfastCare$attributes.BikeParking=="True","attributes.BikeParking"]=1

unique(BreakfastCare$attributes.RestaurantsTakeOut)
BreakfastCare[is.na(BreakfastCare$attributes.RestaurantsTakeOut),"attributes.RestaurantsTakeOut"]='False'
BreakfastCare[BreakfastCare$attributes.RestaurantsTakeOut=="None","attributes.RestaurantsTakeOut"]='False'
BreakfastCare[BreakfastCare$attributes.RestaurantsTakeOut=="False","attributes.RestaurantsTakeOut"]=0
BreakfastCare[BreakfastCare$attributes.RestaurantsTakeOut=="True","attributes.RestaurantsTakeOut"]=1

unique(BreakfastCare$attributes.WiFi)
BreakfastCare[is.na(BreakfastCare$attributes.WiFi),"attributes.WiFi"]='False'
BreakfastCare[BreakfastCare$attributes.WiFi=="None","attributes.WiFi"]='False'
BreakfastCare[BreakfastCare$attributes.WiFi=="uno","attributes.WiFi"]='False'
BreakfastCare[BreakfastCare$attributes.WiFi=="free","attributes.WiFi"]='True'
BreakfastCare[BreakfastCare$attributes.WiFi=="ufree","attributes.WiFi"]='True'
BreakfastCare[BreakfastCare$attributes.WiFi=="paid","attributes.WiFi"]='True'
BreakfastCare[BreakfastCare$attributes.WiFi=="upaid","attributes.WiFi"]='True'
BreakfastCare[BreakfastCare$attributes.WiFi=="no","attributes.WiFi"]='False'
BreakfastCare[BreakfastCare$attributes.WiFi=="yes","attributes.WiFi"]='True'
BreakfastCare[BreakfastCare$attributes.WiFi=="False","attributes.WiFi"]=0
BreakfastCare[BreakfastCare$attributes.WiFi=="True","attributes.WiFi"]=1

unique(BreakfastCare$attributes.HasTV)
BreakfastCare[is.na(BreakfastCare$attributes.HasTV),"attributes.HasTV"]='False'
BreakfastCare[BreakfastCare$attributes.HasTV=="None","attributes.HasTV"]='False'
BreakfastCare[BreakfastCare$attributes.HasTV=="False","attributes.HasTV"]=0
BreakfastCare[BreakfastCare$attributes.HasTV=="True","attributes.HasTV"]=1

##build model
library(MASS)
mod <- polr(as.factor(stars) ~  attributes.BusinessAcceptsCreditCards + attributes.BikeParking+attributes.RestaurantsTakeOut+attributes.WiFi+
              attributes.HasTV, data=BreakfastCare)
summary(mod)

library(AER)
coeftest(mod)



