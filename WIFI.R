#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)


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

####changing values +100 to -110####
# when we find values= to 100 means 
# not signal detection. In order to organize properly the bad and good signals 
# we modify this values with a value = to not signal

wifi_train[wifi_train==100]<--110


####subset by building####

# wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
# wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
# wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 


####removing columns and rows####


wifi_names<-names(wifi_train)

waps<-grep("WAP", names(wifi_train), value = TRUE)
wifi_names<-names(wifi_train)
nowaps<-setdiff(wifi_names, waps)

df_nowaps<-wifi_train[nowaps]
df_waps<-wifi_train[waps]


#remove rows without signal#

wifi_train<-wifi_train%>%filter(apply(wifi_train[waps],1, function(x) length(unique(x))) > 1)

##split df in with signal and without signal## BUENO######

wifi_t_signal<-cbind(df_waps[apply(df_waps,2, function(x) length(unique(x))) > 1], df_nowaps) #keeps waps with signal#
wifi_t_nosignal<-cbind(df_waps[!apply(df_waps,2, function(x) length(unique(x))) > 1], df_nowaps) #removes the waps with no signal#

rm(df_nowaps, df_waps)

waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)

#colum with max signal#

WAP_max<- apply(wifi_t_signal[waps_ws], 1, function(x) names(which.max(x)))
wifi_t_signal$WAP_max<-WAP_max #new colum with max values name colums


#### KNN with max###

partition<-createDataPartition(wifi_t_signal$BUILDINGID,times = 2, p = 0.01)

train<-wifi_t_signal[partition$Resample1,]
test<-wifi_t_signal[partition$Resample2,]

#check distribution#
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# qplot(x = LATITUDE, y = LONGITUDE, data = train)

ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 3)

knn<-train(BUILDINGID~WAP_max,data=train,
            method= "knn", trControl= ctrl,
            tuneLength = 10)

knn

summary(knn)


##by building##



wifi_t_b1_ns<-wifi_t_b1[!vapply(wifi_t_b1[waps], function(x) length(unique(x)) > 1, logical(1L))] #df waps wo signal

waps_t_b1_ns<-grep("WAP", names(wifi_t_b1_ns), value = TRUE) #WAP columns wo signal

wifi_t_b1_ws<-wifi_t_b1%>%
  select(-waps_t_b1_ns) #df waps w signal

#### PLOTS ####
# theme_set(theme_light())
# plot(wifi_train$LATITUDE, wifi_train$LONGITUDE)#
#      
# par(mfrow=c(1,2)) 
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# qplot(x = LATITUDE, y = LONGITUDE, data = train)
# 
# qplot(x = BUILDINGID, y = TIMESTAMP, data =wifi_train)
# 
# 
# ggplot(wifi_t_b1, aes(x=FLOOR),)+ 
#          geom_count(aes(y=WAP400), na.rm = FALSE)


#tab<-tableplot(wifi_t_b1, plot = FALSE)

####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####

