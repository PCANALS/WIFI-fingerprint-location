#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####
library(dplyr)
library(ggplot2)

#### A._ SETTING FILES####

setwd("C:/Users/pilar/Google Drive/A_DATA/UBIQUM/TASK3.3_WIFI/task3-2-wifi-PCANALS")

# wifi_train_o<-read.csv(file = "trainingData.csv", header = TRUE, sep =",")
# save(wifi_train_o, file = "wifi_train_o.Rdata")
load(file = "wifi_train_o.Rdata")

# wifi_validation_o<-read.csv(file = "validationData.csv")
# save(wifi_validation_o, file = "wifi_validation_o.Rdata")
load(file = "wifi_validation_o.Rdata")


wifi_train<-wifi_train_o # to keep original#




#### B._ SUBSETS####


##changing values +100 to -110#

wifi_train[wifi_train==100]<--110


####subset by building####
wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 


#removing columns


waps<-grep("WAP", names(wifi_train), value = TRUE)  #WAP columns



wifi_t_b1_ns<-wifi_t_b1[!vapply(wifi_t_b1[waps], function(x) length(unique(x)) > 1, logical(1L))] #df waps wo signal

waps_t_b1_ns<-grep("WAP", names(wifi_t_b1_ns), value = TRUE) #WAP columns wo signal

wifi_t_b1_ws<-wifi_t_b1%>%
  select(-waps_t_b1_ns) #df waps w signal



#lo mismo por row#
####

ggplot(wifi_t_b1, aes(x=FLOOR),)+ 
         geom_jitter(aes(y=WAP400), na.rm = FALSE)


#tab<-tableplot(wifi_t_b1, plot = FALSE)

####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####
