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


###changing values +100 to -110###
# when we find values= to 100 means 
# not signal detection. In order to organize properly the bad and good signals 
# we modify this values with a value = to not signal

wifi_train[wifi_train==100]<--110


####subset by building####
wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 


#removing columns

waps<-grep("WAP", names(wifi_train), value = TRUE)  #WAP columns

wifi_train$BUILDINGID<-as.factor(wifi_train$BUILDINGID)
wifi_train$FLOOR<-as.factor(wifi_train$FLOOR)


wifi_names<-names(wifi_train)

waps<-grep("WAP", names(wifi_train), value = TRUE) 

wifi_pos<-setdiff(wifi_names, waps)

#remove rows without signal#

wifi_train<-wifi_train%>%filter(apply(wifi_train[waps],1, function(x) length(unique(x))) > 1)

##split df in with signal and without signal##

#wifi_t_ns<-wifi_train[!vapply(wifi_train[waps], function(x) length(unique(x)) > 1, logical(1L))]
#wifi_t_ns<-wifi_train[!apply(wifi_train[waps],2, function(x) length(unique(x))) > 1]
wifi_t_ns<-wifi_train[!apply(wifi_train,2, function(x) length(unique(x))) > 1] #removes the waps with no signal#
wifi_t_ws<-wifi_train[apply(wifi_train,2, function(x) length(unique(x))) > 1]  #keeps waps with signal#

waps_ws<-grep("WAP", names(wifi_t_ws), value = TRUE)

#colum with max signal#

wifi_t_ws_max<- apply(wifi_t_ws[waps_ws], 1, function(x) names(which.max(x)))
wifi_t_ws$wifi_t_ws_max<-wifi_t_ws_max #new colum with max values name colums


#### KNN with max###

partition<-createDataPartition(wifi_t_ws$BUILDINGID,times = 2, p = 0.01)

train<-wifi_t_ws[partition$Resample1,]
test<-wifi_t_ws[partition$Resample2,]


ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 3)

knn<-train(BUILDINGID~.,data=train,
            method= "knn", trControl= ctrl,
            tuneLength = 10)

knn

summary(knn)

# d<-wifi_train[!(wifi_train[waps]=="-110"),]
# wifi_t_signal<-wifi_train[apply(wifi_train[waps],2, function(x) min(x) ==-110)] #con señal#
# wifi_t_nosignal<-wifi_train[apply(wifi_train,2, function(x) max(x) ==-110)]
#wifi_t_signal<-wifi_train[!apply(wifi_train[waps],2, function(x) min(unique(x))) > 1]
#df with all attributes except waps# NOT WORKING#
# wifi_nowaps_df <- setdiff(wifi_train,wifi_pos)


#ROW#

# x<-wifi_train[apply(wifi_train[,waps],1,function(x)(min(x==-110)))]
# > View(x)
# > x<-wifi_train[!apply(wifi_train[,waps],1,function(x)(min(x==-110)))]
# Error in `[.data.frame`(wifi_train, !apply(wifi_train[, waps], 1, function(x) (min(x ==  : 
#  undefined columns selected
# x<-wifi_train[!apply(wifi_train[,waps],1,function(x)(min(x <-110)))]
# x<-wifi_train[!apply(wifi_train[waps],1,function(x)(min(x <-110)))]
# x<-wifi_train[!apply(wifi_train[waps],1,function(x)(min(x=<-110)))]
# Error: unexpected assignment in "x<-wifi_train[!apply(wifi_train[waps],1,function(x)(min(x=<-"
# x<-wifi_train[!apply(wifi_train[1:520],1,function(x)(min==-110)))]
# Error: unexpected ')' in "x<-wifi_train[!apply(wifi_train[1:520],1,function(x)(min==-110)))"


##by building##



wifi_t_b1_ns<-wifi_t_b1[!vapply(wifi_t_b1[waps], function(x) length(unique(x)) > 1, logical(1L))] #df waps wo signal

waps_t_b1_ns<-grep("WAP", names(wifi_t_b1_ns), value = TRUE) #WAP columns wo signal

wifi_t_b1_ws<-wifi_t_b1%>%
  select(-waps_t_b1_ns) #df waps w signal

#### PLOTS ####
theme_set(theme_light())
plot(wifi_train$LATITUDE, wifi_train$LONGITUDE)#
     

qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)

qplot(x = BUILDINGID, y = TIMESTAMP, data =wifi_train)


ggplot(wifi_t_b1, aes(x=FLOOR),)+ 
         geom_count(aes(y=WAP400), na.rm = FALSE)


#tab<-tableplot(wifi_t_b1, plot = FALSE)

####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####

