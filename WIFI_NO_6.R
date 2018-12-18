#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)
library(randomForest)
library(gg3D)

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

####NEW ATTRIBUTES####

wifi_train$BF<-paste0("B", wifi_train$BUILDINGID, "F", wifi_train$FLOOR)
wifi_validation$BF<-paste0("B", wifi_validation$BUILDINGID, "F", wifi_validation$FLOOR)


#### TYPES ATTRIBUTES ###

wifi_names<-names(wifi_train)
wifi_names_v<-names(wifi_validation)
all.equal(wifi_names, wifi_names_v) #columns in validation and in train are the same#

#splitting in groups the columns names#
waps<-grep("WAP", names(wifi_train), value = TRUE)
nowaps<-setdiff(wifi_names, waps)
fac<-c("FLOOR", "BUILDINGID", "SPACEID","RELATIVEPOSITION", "USERID", "PHONEID", "BF")
fac2<-c("FLOOR", "BUILDINGID", "SPACEID","RELATIVEPOSITION", "USERID", "PHONEID")

wifi_t_signal$WAP_max<-as.factor(wifi_t_signal$WAP_max)
wifi_t_signal_val$WAP_max_val<-as.factor(wifi_t_signal_val$WAP_max_val)


wifi_train[fac] <- lapply(wifi_train[fac], as.factor) 
wifi_validation[fac] <- lapply(wifi_validation[fac], as.factor) 

rm(fac,fac2, wifi_names, wifi_names_v)
#### B._ SUBSETS####


####B_remove duplicate rows####
wifi_train<-distinct(wifi_train)
wifi_validation<-distinct(wifi_validation)

#### kill the troll####
wifi_train<-filter(wifi_train, USERID!=6)


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


rm(df_nowaps, df_waps, df_nowaps_val, df_waps_val, wifi_t_nosignal)


####intersect with validation####

wifi_t_signal_intersect<-intersect(wifi_t_signal_names, wifi_t_signal_val_names)

wifi_t_signal<-wifi_t_signal[,wifi_t_signal_intersect]
wifi_t_signal_val<-wifi_t_signal_val[,wifi_t_signal_intersect]

rm(wifi_t_signal_intersect, wifi_t_signal_names, wifi_t_signal_val_names)
waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)



####Remove vaues with les more than -30 or -0,80  NOT WORKING and KILLED user 6####

# x<-wifi_t_signal%>%filter(apply(wifi_t_signal[waps_ws],1, function(x) any(x>= -0.30)))
# 
# y<-wifi_t_signal%>%filter(apply(wifi_t_signal[waps_ws],1, function(x) any(x-0.70)))
# summary(x[nowaps])#if not killed u can see the troll user#
# 
# wifi_t_signal%>%filter(USERID==6)%>%count()
# 
# 
# barplot(table(wifi_train$USERID))

###column BF####

#####colums with max signal MAX WAP & MAX SIGNAL####
#train#
waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)


WAP_max<- apply(wifi_t_signal[waps_ws], 1, function(x) names(which.max(x)))
WAP_max_value<-apply(wifi_t_signal[waps_ws], 1, function(x) max(x))

anyNA(WAP_max)

wifi_t_signal$WAP_max<-WAP_max #new colum with max values name colums
wifi_t_signal$WAP_max_value<-WAP_max_value

#validation#
new_waps<-setdiff(waps_ws, errors_max_value)
WAP_max_val<- apply(wifi_t_signal_val[new_waps], 1, function(x) names(which.max(x)))

WAP_max_value_val<-apply(wifi_t_signal_val[new_waps], 1, function(x) max(x))

errors_max_value%in%WAP_max_value_val_test

wifi_t_signal_val$WAP_max<-WAP_max_val #new colum with max values name colums
wifi_t_signal_val$WAP_max_value<-WAP_max_value_val

wifi_t_signal_val$WAP_max<-as.factor(wifi_t_signal_val$WAP_max)


####B_subset by building####

# wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
# wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
#wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 

####B_near 0 variance####


#### MODELING PARTITION####

#run models in parallel#
cl<-makeCluster(3)
doParallel:::registerDoParallel(cl)

set.seed(123)
partition<-createDataPartition(wifi_t_signal$BUILDINGID,times = 2, p = 0.1)

wifi_train<-wifi_t_signal[partition$Resample1,]
wifi_test<-wifi_t_signal[partition$Resample2,]

rm(cl, partition)

#check distribution#
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# qplot(x = LATITUDE, y = LONGITUDE, data = train)

ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)




#### KNN with max####

# system.time(knn_B_6<-train(BUILDINGID~WAP_max_value,data=wifi_t_signal,
#            method= "knn", trControl= ctrl,
#            tuneLength = 10))
# #           allowPallowParalel=TRUE))
# 
# knn_B_6
# 
#saveRDS(knn_B_6, file = "knn_B_6.rds")
knn_B_6<-readRDS("knn_B_6.rds")
# 
# knn_BF_6<-readRDS("knn_BF_6.rds")
# 
# knn_lat_6<-readRDS("knn_lat_6.rds")
# 
# saveRDS(knn_lon_6, file = "knn_lon_6.rds")
# knn_lon_6<-readRDS("knn_lon_6.rds")
# 
knn_pred_B_6<-predict(knn_B_6, wifi_t_signal_val)
confusionMatrix(knn_pred_B_6, wifi_t_signal_val$BUILDINGID)

#### SVM_ LINEAR with max#### 

# svm_tune <- tune(svm, train.x=train$WAP_max, 
#                  data = train, 
#                  kernel="radial", 
#                  ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))

# system.time(svm_l_B_6<-train(BUILDINGID~WAP_max,data=wifi_t_signal, #aplicado en el train completo#
#            method= "svmLinear", trControl= ctrl,
#            tuneLength = 20))
# svm_l_B_6

#e values not trained and detected as maximum in the validation data set#
errors_max_value<-c("WAP138", "WAP144", "WAP268", "WAP323", "WAP481")

#this waps are in the validation df not in the train#
eincluded<-errors_max_value %in% WAP_max

#rows with max values not trained as maximun in the validation data set#12#
validation_error<-wifi_t_signal_val%>%dplyr::filter(WAP_max%in%errors_max_value)

#add column with the columns error# and plot#
wifi_t_signal_val_e<-wifi_t_signal_val%>%dplyr::mutate(error= ifelse(WAP_max_val%in%errors_max_value, "error", "noerror"))

plot_ly(wifi_t_signal_val_e, x=~LATITUDE,y=~LONGITUDE, z= ~FLOOR, color=~error)

#dataframe with the rows with errors in the max values## #remove colums?

# training_error<--wifi_t_signal%>%dplyr::filter(WAP_max%in%errors_max_value)
# 
# setdiff()

saveRDS(svm_l_B_6, file = "svm_l_B_6.rds")
svm_l_B_6<-readRDS("svm_l_B_6.rds") #by BUILDING#

# #saveRDS(svm_l, file = "svm_l.rds")
# svm_l<-readRDS("svm_l.rds") #by building# 0,99
# 
# #saveRDS(svm_l_lat, file="svm_l_lat.rds")
# svm_l_lat<-readRDS("svm_l_lat.rds") #by latitude# RMSE 65.
# 
# #saveRDS(svm_l_lon, file="svm_l_lon.rds")
# svm_l_lon<-readRDS("svm_l_lon.rds") #by latitude# RMSE 65.

pred_svm_l_B_6<-predict(svm_l_B_6, wifi_t_signal_val)
confusionMatrix(pred_svm_l_B_6, wifi_t_signal_val$BUILDINGID)
# 
# pred_svm_l_f<-predict(svm_l_f, wifi_t_signal_val)
# confusionMatrix(pred_svm_l_f, wifi_t_signal_val$BF)

# pred_svm_l_lat<-predict(svm_l_lat, wifi_t_signal_val)
# confusionMatrix(pred_svm_l_lat, wifi_t_signal_val$LATITUDE)
# 
# #### SVM_ Radial with max####
# 
# svm_r<-train(BUILDINGID~WAP_max,data=train,
#              method= "svmRadial", trControl= ctrl,
#              tuneLength = 20)
# svm_r

#### SVM_ Polynominal with max####

# svm_p<-train(BUILDINGID~WAP_max,data=train,
#              method= "svmPoly", trControl= ctrl,
#              tuneLength = 10)
# svm_p


#### Random Forest with max####


# system.time(rf_BF <- randomForest(BUILDINGID~WAP_max, 
#                                   data = train, 
#                                   method="rf", trControl= ctrl,
#                                   ntree=10,
#                                   allowParalel=TRUE))



# rf<-train(BUILDINGID~WAP_max,data=train,
#           method= "rf", trControl= ctrl,
#           ntree=20,
#           tuneLength = 10)
# 
# rf

#### C.50 with max####
#### PCA with max####



stopCluster(cl)




##by building##


# 
# wifi_t_b1_ns<-wifi_t_b1[!vapply(wifi_t_b1[waps], function(x) length(unique(x)) > 1, logical(1L))] #df waps wo signal
# 
# waps_t_b1_ns<-grep("WAP", names(wifi_t_b1_ns), value = TRUE) #WAP columns wo signal
# 
# wifi_t_b1_ws<-wifi_t_b1%>%
#   select(-waps_t_b1_ns) #df waps w signal





#### PLOTS ####
# theme_set(theme_light())
# plot(wifi_train$LATITUDE, wifi_train$LONGITUDE)#
#      
# par(mfrow=c(1,2)) 
# qplot(x = LATITUDE, y = LONGITUDE, data = troll)
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_notroll)
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# scatterD3(x =LATITUDE, y = LONGITUDE, 
#         z=~FLOOR, data = wifi_notroll) %>% add_trace(x =~LATITUDE, y = ~LONGITUDE, 
#             z=~FLOOR, data = troll)
#   
# ggplot(troll, aes(x=LONGITUDE, y=LATITUDE, z=FLOOR, color=FLOOR)) + 
#   theme_void() +
#   axes_3D() +
#   stat_3D()
# 
# ggplot(wifi_train, aes(x=LONGITUDE, y=LATITUDE, z=FLOOR)) + 
#   theme_void() +
#   axes_3D() +
#   stat_3D()

# 
# qplot(x = BUILDINGID, y = TIMESTAMP, data =wifi_train)
# 
# 
# ggplot(wifi_t_signal, aes(x=LONGITUD))+
#          geom_count(aes(y=WAP323), na.rm = FALSE)
# 
# ggplot(wifi_t_signal_val, aes(x=BF))+
#   geom_count(aes(y=WAP323), na.rm = FALSE)
# 
# ggplot(wifi_t_signal, aes(x=BF))+
#   geom_count(aes(y=WAP508), na.rm = FALSE)
# 
# ggplot(wifi_t_signal_val, aes(x=BF))+
#   geom_count(aes(y=WAP508), na.rm = FALSE)


#268, 323

#tab<-tableplot(wifi_t_b1, plot = FALSE)

####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~
##restar los rows con ese valor del DF?
#unique values