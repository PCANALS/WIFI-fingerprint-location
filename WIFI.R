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
wifi_validation<-wifi_validation_o # to keep original#

rm(wifi_train_o, wifi_validation_o)

#### TYPES ATTRIBUTES ###

wifi_names<-names(wifi_train)
wifi_names_v<-names(wifi_validation)
all.equal(wifi_names, wifi_names_v) #columns in validation and in train are the same#

#splitting in groups the columns names#
waps<-grep("WAP", names(wifi_train), value = TRUE)
nowaps<-setdiff(wifi_names, waps)
fac<-c("FLOOR", "BUILDINGID", "SPACEID","RELATIVEPOSITION", "USERID", "PHONEID")

wifi_train[fac] <- lapply(wifi_train[fac], as.factor) 
wifi_validation[fac] <- lapply(wifi_validation[fac], as.factor) 


rm(fac)
#### B._ SUBSETS####

####B_remove duplicate rows####
wifi_train<-distinct(wifi_train)
wifi_validation<-distinct(wifi_validation)


####B_changing values +100 to -110####
# when we find values= to 100 means 
# not signal detection. In order to organize properly the bad and good signals 
# we modify this values with a value = to not signal

wifi_train[wifi_train==100]<--110
wifi_validation[wifi_validation==100]<--110


####B_removing columns and rows with same value/no unique####


#remove rows without signal#

wifi_train<-wifi_train%>%filter(apply(wifi_train[waps],1, function(x) length(unique(x))) > 1)


##split df in with signal and without signal##


df_nowaps<-wifi_train[nowaps]
df_waps<-wifi_train[waps]
df_nowaps_val<-wifi_validation[nowaps]
df_waps_val<-wifi_validation[waps]


wifi_t_signal<-cbind(df_waps[apply(df_waps,2, function(x) length(unique(x))) > 1], df_nowaps) #keeps waps with signal#
wifi_t_nosignal<-cbind(df_waps[!apply(df_waps,2, function(x) length(unique(x))) > 1], df_nowaps) #removes the waps with no signal#

wifi_t_signal_val<-cbind(df_waps_val[apply(df_waps_val,2, function(x) length(unique(x))) > 1], df_nowaps_val) #keeps waps with signal#

wifi_t_signal_names<-names(wifi_t_signal)
wifi_t_signal_val_names<-names(wifi_t_signal_val)


rm(df_nowaps, df_waps)

####intersect with validation###
wifi_t_signal_intersect<-intersect(wifi_t_signal_names, wifi_t_signal_val_names)

wifi_t_signal<-wifi_t_signal[,wifi_t_signal_intersect]

rm(wifi_t_signal_intersect, wifi_t_signal_names, wifi_t_signal_val_names)


#####colum with max signal####
waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)

WAP_max<- apply(wifi_t_signal[waps_ws], 1, function(x) names(which.max(x)))
wifi_t_signal$WAP_max<-WAP_max #new colum with max values name colums



####B_removing more -30####




####B_subset by building####

# wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
# wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
# wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 

####B_near 0 variance####




#### MODELING PARTITION####

#run models in parallel#
cl<-makeCluster(3)
doParallel:::registerDoParallel(cl)

set.seed(123)
partition<-createDataPartition(wifi_t_signal$BUILDINGID,times = 2, p = 0.01)

train<-wifi_t_signal[partition$Resample1,]
test<-wifi_t_signal[partition$Resample2,]

rm(cl, partition)

#check distribution#
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# qplot(x = LATITUDE, y = LONGITUDE, data = train)

ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)




#### KNN with max####

knn<-train(BUILDINGID~WAP_max,data=train,
           method= "knn", trControl= ctrl,
           preProcess= c("center","scale"),
           tuneLength = 10)

knn

#saveRDS(knn, file = "knn.rds")
# knn<-readRDS("knn.rds")

pred<-predict(knn, test) ###error##
confusionMatrix(pred, test$BUILDINGID)

#### SVM_ LINEAR with max#### 

# svm_tune <- tune(svm, train.x=train$WAP_max, 
#                  data = train, 
#                  kernel="radial", 
#                  ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))

svm_l<-train(BUILDINGID~WAP_max,data=wifi_t_signal, #aplicado en el train completo#
           method= "svmLinear", trControl= ctrl,
           tuneLength = 20)
svm_l


#### SVM_ Radial with max####

svm_r<-train(BUILDINGID~WAP_max,data=train,
             method= "svmRadial", trControl= ctrl,
             tuneLength = 20)
svm_r

#### SVM_ Polynominal with max####

svm_p<-train(BUILDINGID~WAP_max,data=train,
             method= "svmPoly", trControl= ctrl,
             tuneLength = 10, )
svm_p


#### Random Forest with max####

# rf<-train(BUILDINGID~WAP_max,data=train,
#           method= "rf", trControl= ctrl,
#           ntree=20,
#           tuneLength = 10)
# 
# rf

#### C.50 with max####
#### PCA with max####







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

