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
library(plotly)

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

#Floor & Building combinated#

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


wifi_train[fac] <- lapply(wifi_train[fac], as.factor) 
wifi_validation[fac] <- lapply(wifi_validation[fac], as.factor) 

rm(fac,fac2, wifi_names, wifi_names_v)
#### B._ SUBSETS####


####B_remove duplicate rows####
wifi_train<-distinct(wifi_train)
wifi_validation<-distinct(wifi_validation)

#### kill the troll####user 6 is giving but signal in 60% of his signals, as we can't trust in 
#their registers will remove all the observations of this user#

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


####B_intersect with validation####

wifi_t_signal_intersect<-intersect(wifi_t_signal_names, wifi_t_signal_val_names)

wifi_t_signal<-wifi_t_signal[,wifi_t_signal_intersect]
wifi_t_signal_val<-wifi_t_signal_val[,wifi_t_signal_intersect]

rm(wifi_t_signal_intersect, wifi_t_signal_names, wifi_t_signal_val_names)
waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)



####B_Remove vaues with les more than -30 or -0,80  NOT WORKING and KILLED user 6####


#x<-wifi_t_signal %>% filter(apply(wifi_t_signal[waps_ws],1,function(x)any(x>=-30)))



##### ~~~~~~~C_Colums with max signal MAX WAP & MAX SIGNAL~~~~~||||||||####

#C_train max signal####
waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE)


WAP_max<- apply(wifi_t_signal[waps_ws], 1, function(x) names(which.max(x)))
WAP_max_value<-apply(wifi_t_signal[waps_ws], 1, function(x) max(x))

anyNA(WAP_max)

wifi_t_signal$WAP_max<-WAP_max #new colum with max values name colums
wifi_t_signal$WAP_max_value<-WAP_max_value

#y<-wifi_t_signal %>% filter(apply(wifi_t_signal["WAP_max"],1,function(x)max(x==-96)))

#### FILTER BAD SIGNAL####

bad_30<-wifi_t_signal %>%
  filter(WAP_max_value >= -30)

bad_80<-wifi_t_signal %>%
  filter(WAP_max_value <= -80)
  

bad_30_val<-wifi_t_signal_val %>%
  filter(WAP_max_value >= -30)

bad_80_val<-wifi_t_signal_val %>%
  filter(WAP_max_value <= -80)

summary(bad_80_val[nowaps])


# C_WAP Error detected modeling MAX VALUE####
#e values not trained and detected as maximum in the validation data set#
errors_max_value<-c("WAP138", "WAP144", "WAP268", "WAP323", "WAP481")

#creating a new vector of waps useful for the validation data set#

waps_ws_woe<-setdiff(waps_ws, errors_max_value)
#creating new max values for validation#


WAP_max_val<- apply(wifi_t_signal_val[waps_ws_woe], 1, function(x) names(which.max(x)))

WAP_max_value_val<-apply(wifi_t_signal_val[waps_ws_woe], 1, function(x) max(x))


#add column with the columns error# and plot#
wifi_t_signal_val_e<-wifi_t_signal_val%>%dplyr::mutate(error= ifelse(WAP_max_val%in%errors_max_value, "error", "noerror"))

#plot_ly(wifi_t_signal_val_e, x=~LATITUDE,y=~LONGITUDE, z= ~FLOOR, color=~error)
plot_ly(bad_80_val, x=~LATITUDE,y=~LONGITUDE, z= ~FLOOR, color=~BUILDINGID)

summary(bad_80_val[nowaps])

#this waps are in the validation df not in the train#
errors_max_value%in%WAP_max_val
errors_max_value %in% WAP_max

wifi_t_signal_val$WAP_max<-WAP_max_val #new colum with max values name colums
wifi_t_signal_val$WAP_max_value<-WAP_max_value_val

#as factor for train the models#
wifi_t_signal_val$WAP_max<-as.factor(wifi_t_signal_val$WAP_max)
wifi_t_signal$WAP_max<-as.factor(wifi_t_signal$WAP_max)



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

# system.time(knn_B_6<-train(BUILDINGID~WAP_max,
#                            data=wifi_t_signal,
#                            method= "knn", 
#                            trControl= ctrl))

#Error in e$fun(obj, substitute(ex), parent.frame(), e$data) : 
# worker initialization failed: list(Accuracy = NA, Kappa = NA, .cell1 = 0, .cell2 = 0, .cell3 = 0, .cell4 = 0, .cell5 = 0, .cell6 = 0, .cell7 = 0, .cell8 = 0, .cell9 = 0, k = 9, Resample = "Fold07.Rep1")NULLNULL
# Timing stopped at: 1.25 0.97 19

# knn_B_6
# 
# saveRDS(knn_B_6, file = "knn_B_6.rds")
#knn_B_6<-readRDS("knn_B_6.rds")
# 
# knn_BF_6<-readRDS("knn_BF_6.rds")
# 
# knn_lat_6<-readRDS("knn_lat_6.rds")
# 
# saveRDS(knn_lon_6, file = "knn_lon_6.rds")
# knn_lon_6<-readRDS("knn_lon_6.rds")
# 
# knn_pred_B_6<-predict(knn_B_6, wifi_t_signal_val)
# confusionMatrix(knn_pred_B_6, wifi_t_signal_val$BUILDINGID)

##### ~~~~ SVM_ LINEAR with max#### 

# svm_tune <- tune(svm, train.x=train$WAP_max, 
#                  data = train, 
#                  kernel="radial", 
#                  ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))

wifi_t_signal_val$LATITUDE<-as.integer(wifi_t_signal_val$LATITUDE)
wifi_t_signal$LATITUDE<-as.integer(wifi_t_signal$LATITUDE)


# system.time(svm_l_LAT_6<-train(y=wifi_t_signal$LATITUDE,
#                                x=wifi_t_signal[c("BUILDINGID", waps_ws)], #aplicado en el train completo#
#            method= "svmLinear"))
# svm_l_LAT_6
# 
# system.time(svm_r_LON_6<-train(LONGITUDE~WAP_max,data=wifi_t_signal, #aplicado en el train completo#
#                                method= "svmRadial"
#                                # ,trControl= ctrl
#                                ))
# svm_r_LON_6


# saveRDS(svm_l_B_6, file = "svm_l_B_6.rds")
svm_l_B_6<-readRDS("svm_l_B_6.rds") #by BUILDING#

#saveRDS(svm_l_F_6, file = "svm_l_F_6.rds")
svm_l_F_6<-readRDS("svm_l_F_6.rds")

#saveRDS(svm_l_BF_6, file = "svm_l_BF_6.rds")
svm_l_BF_6<-readRDS("svm_l_BF_6.rds")

#saveRDS(svm_l_LAT_6, file = "svm_l_LAT_6.rds")
svm_l_LAT_6<-readRDS("svm_l_LAT_6.rds")

#saveRDS(svm_l_LON_6, file = "svm_l_LON_6.rds")
svm_l_LON_6<-readRDS("svm_l_LON_6.rds")



#### Random Forest with max####

#bestmtry = tuneRF(x=wifi_t_signal[c("BUILDINGID", waps_ws)], y=wifi_t_signal$LONGITUDE, ntreeTry = 200, plot = F)

system.time(rf_lat<- randomForest(y=wifi_t_signal$LATITUDE,
                                 x=wifi_t_signal[c("BUILDINGID", waps_ws)],
                                 ntree=200, mtry = 104))

rf_lat


# system.time(rf_lon <- randomForest(y=wifi_t_signal$LONGITUDE,
#                                  x=wifi_t_signal[c("BUILDINGID", waps_ws)],
#                                  ntree=50, mtry = 104))
# 
# rf_lon


#saveRDS(rf_lon, file = "rf_lon.rds")
rf_lat<-readRDS("rf_lat.rds")


rf_lon<-readRDS("rf_lon.rds")



pred_lon_rf<-predict(rf_lon, wifi_t_signal_val)
postResample(pred_lon_rf, wifi_t_signal_val$LONGITUDE)

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
# plot_ly(wifi_t_signal, type="scatter3d", x=~LATITUDE, 
#         y=~LONGITUDE, z=~FLOOR, 
#         marker = list(
#           color="blue",
#           size= 4
#         ))%>%
#   add_trace(troll, type="scatter3d", x=~LATITUDE, y=~LONGITUDE, z=~FLOOR, marker = list(
#     color="red",
#     size= 2 ))


# theme_set(theme_light())
# plot(wifi_train$LATITUDE, wifi_train$LONGITUDE)#
#      
# par(mfrow=c(1,2)) 
qplot(x = LATITUDE, y = LONGITUDE, data = bad_80)
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_notroll)
# # qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# scatterD3(x =LATITUDE, y = LONGITUDE,
#         z=~FLOOR, data = bad_80) %>% add_trace(x =~LATITUDE, y = ~LONGITUDE,
            # z=~FLOOR, data = troll)

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