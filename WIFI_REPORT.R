#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")
options(scipen=999)

#### ._LIBRARIES####

pacman::p_load(caret, party, reshape, ggplot2, dplyr, doParallel, gg3D, plotly, randomForest, mlbench)
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


#### ATTRIBUTES ###

wifi_names<-names(wifi_train)
wifi_names_v<-names(wifi_validation)
all.equal(wifi_names, wifi_names_v) #columns in validation and in train are the same#

#splitting in groups the columns names####
waps<-grep("WAP", names(wifi_train), value = TRUE)
nowaps<-setdiff(wifi_names, waps)
fac<-c("FLOOR", "BUILDINGID", "SPACEID","RELATIVEPOSITION", "USERID", "PHONEID", "BF")


wifi_train[fac] <- lapply(wifi_train[fac], as.factor) 
wifi_validation[fac] <- lapply(wifi_validation[fac], as.factor) 

rm(fac, wifi_names, wifi_names_v)

#### B._ SUBSETS####


####B_remove duplicate rows####

wifi_train<-distinct(wifi_train)
wifi_validation<-distinct(wifi_validation)



####B_changing values +100 to -110####
# when we find values= to 100 means 
# not signal detection. In order to organize properly the bad and best signals 
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



##### ~~~~~~~C_Colums with max signal MAX WAP & MAX SIGNAL~~~~~||||||||####

#C_train max signal####

waps_ws<-grep("WAP", names(wifi_t_signal), value = TRUE) #wap columns#


WAP_max<- apply(wifi_t_signal[waps_ws], 1, function(x) names(which.max(x)))
WAP_max_value<-apply(wifi_t_signal[waps_ws], 1, function(x) max(x))

anyNA(WAP_max)

wifi_t_signal$WAP_max<-WAP_max #new colum with max values name colums
wifi_t_signal$WAP_max_value<-WAP_max_value

#creating new max values for validation#

WAP_max_val<- apply(wifi_t_signal_val[waps_ws], 1, function(x) names(which.max(x)))

WAP_max_value_val<-apply(wifi_t_signal_val[waps_ws], 1, function(x) max(x))




wifi_t_signal_val$WAP_max<-WAP_max_val #new colum with max values name colums
wifi_t_signal_val$WAP_max_value<-WAP_max_value_val

#### BAD SIGNAL ANALYSIS####

bad_30<-wifi_t_signal %>%
  filter(WAP_max_value >= -30)


summary(bad_30[nowaps]) #detected 2 users(6 -392and 14-54) not registering correct observations#
ggplot(bad_30, aes(x=USERID))+ geom_bar()

bad_80<-wifi_t_signal %>%
  filter(WAP_max_value <= -80)

summary(bad_80[nowaps])
ggplot(bad_80, aes(x=USERID))+ geom_bar()

summary(filter(bad_80[nowaps], USERID==6)) #only 2 bad observatios

summary(filter(wifi_t_signal[nowaps], USERID==6)) #977 observations of user 6#
summary(filter(wifi_t_signal[nowaps], USERID==14)) #1596 observations of user 14#

#As the user 6 40% of his observations are wrong,and we can not trust the reliability of his observations, 
#will remove this user for the nexts analysis. Because  will make noise for the final results#

# For the user 14 as only is the 3% of this observations will keep in it##

####validation bad signal#### 
#NO THERE ARE BAD SIGNAL IN THE VALIDATION SET
bad_30_val<-wifi_t_signal_val %>%
  filter(WAP_max_value >= -30)

bad_80_val<-wifi_t_signal_val %>%
  filter(WAP_max_value <= -80)


plot_ly(bad_80_val, x=~LATITUDE,y=~LONGITUDE, z= ~FLOOR, color=~BUILDINGID)

n_occur <- data.frame(table(bad_80_val$WAP_max))
n_occur[n_occur$Freq > 1,]

n_occur2 <- data.frame(table(bad_80$WAP_max))
n_occur2[n_occur2$Freq > 10,]


#### kill the troll####

#user 6 is giving but signal in 60% of his signals, as we can't trust in 
#their registers will remove all the observations of this user#

wifi_t_signal<-filter(wifi_t_signal, USERID!=6)


# C_WAP Error detected modeling MAX VALUE####
# values not trained and detected as maximum in the validation data set#

error<-wifi_t_signal_val$WAP_max%in%wifi_t_signal$WAP_max
df<-distinct(filter(data.frame(error, wifi_t_signal_val$WAP_max), error=="FALSE"))

errors_max_validation<-c("WAP138", "WAP144", "WAP268", "WAP323", "WAP481")

summary(wifi_t_signal[c("WAP138", nowaps)])

ggplot(wifi_t_signal, aes(x=LONGITUDE, y=LATITUDE))+
  geom_point(aes(y=WAP138), na.rm = FALSE)




#creating a new vector of waps useful for the validation data set#

waps_ws_woe<-setdiff(waps_ws, errors_max_validation)

#creating new max values for validation
# max values that don't uses the waps with issues in the validation

WAP_max_val<- apply(wifi_t_signal_val[waps_ws_woe], 1, function(x) names(which.max(x)))

WAP_max_value_val<-apply(wifi_t_signal_val[waps_ws_woe], 1, function(x) max(x))

wifi_t_signal_val$WAP_max<-WAP_max_val #new colum with max values name colums
wifi_t_signal_val$WAP_max_value<-WAP_max_value_val

#this waps are in the validation df not in the train#
errors_max_validation%in%WAP_max_val
errors_max_validation %in% WAP_max

wifi_t_signal_val$WAP_max<-WAP_max_val #new colum with max values name colums
wifi_t_signal_val$WAP_max_value<-WAP_max_value_val

#as factor for train the models#
wifi_t_signal_val$WAP_max<-as.factor(wifi_t_signal_val$WAP_max)
wifi_t_signal$WAP_max<-as.factor(wifi_t_signal$WAP_max)


##### ~~~~~~~~~~PLOTING ERROR NOT WORKING~~~~~~####

#add column with the columns error# and plot#

wifi_t_signal_val_e<-wifi_t_signal_val%>%dplyr::mutate(error= ifelse(WAP_max_val%in%errors_max_validation, "error", "noerror"))

plot_ly(wifi_t_signal_val_e, x=~LATITUDE,y=~LONGITUDE, z= ~FLOOR, color=~error)

plot_ly(type = "scatter3d", mode="markers", 
        wifi_t_signal_val_e,
        color = ~error, colors = c("#1289d7", "#ff0000"),
        x=~LATITUDE, y=~LONGITUDE, z= ~FLOOR)


####D._  Dataframes WHITHOUT BAD SIGNAL####

wifi_t_goodsignal<-setdiff(wifi_t_signal, bad_30)

summary(wifi_t_goodsignal$WAP_max_value)

wifi_t_bestsignal<-setdiff(wifi_t_goodsignal, bad_80)
summary(wifi_t_bestsignal$WAP_max_value)

#######~~~~~~~~ HASTA AQUÍ REVISADO ~~~~~~~~######


#### MODELING PARTITION####

#run models in parallel#
cl<-makeCluster(3)
doParallel:::registerDoParallel(cl)

set.seed(123)
# partition<-createDataPartition(wifi_t_signal$BUILDINGID,times = 2, p = 0.1)
# 
# wifi_train<-wifi_t_signal[partition$Resample1,]
# wifi_test<-wifi_t_signal[partition$Resample2,]

rm(cl, partition)

#check distribution#
# qplot(x = LATITUDE, y = LONGITUDE, data = wifi_train)
# qplot(x = LATITUDE, y = LONGITUDE, data = train)

#ctrl<-trainControl(method="repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)
wifi_t_signal_val$LATITUDE<-as.integer(wifi_t_signal_val$LATITUDE)
wifi_t_signal$LATITUDE<-as.integer(wifi_t_signal$LATITUDE)
wifi_t_bestsignal$LATITUDE<-as.integer(wifi_t_bestsignal$LATITUDE)
wifi_t_bestsignal$LONGITUDE<-as.integer(wifi_t_bestsignal$LONGITUDE)


#### ~~~~ SVM_ LINEAR~~~~### 

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

#WITHOUT USER 6 AND ERROR#
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

pred_svm_l_B_6<-predict(svm_l_B_6, wifi_t_signal_val)
pred_svm_l_F_6<-predict(svm_l_F_6, wifi_t_signal_val)
pred_svm_l_BF_6<-predict(svm_l_BF_6, wifi_t_signal_val)
confusionMatrix(pred_svm_l_BF_6, wifi_t_signal_val$BF)

#with user 6and errors

svm_l_b<-readRDS("svm_l.rds")
svm_l_f<-readRDS("svm_l_f.rds")
svm_l_lat<-readRDS("svm_l_lat.rds")
svm_l_lon<-readRDS("svm_l_lon.rds")

pred_svm_l_b<-predict(svm_l_b, wifi_t_signal_val)
confusionMatrix(pred_svm_l_b, wifi_t_signal_val$BUILDINGID)
#pred_svm_l_f<-predict(svm_l_f, wifi_t_signal_val) #not works
#confusionMatrix(pred_svm_l_f, wifi_t_signal_val$FLOOR)
#pred_svm_l_lat<-predict(svm_l_lat, wifi_t_signal_val)
#confusionMatrix(pred_svm_l_lat, wifi_t_signal_val$LATITUDE)



#### Random Forest with max####

# bestmtry = tuneRF(x=wifi_t_bestsignal[c("BUILDINGID", waps_ws)],
#                   y=wifi_t_bestsignal$BF, ntreeTry = 25, plot = F)


# system.time(rf_B_best<- randomForest(y=wifi_t_bestsignal$BUILDINGID,
#                                  x=wifi_t_bestsignal[waps_ws],
#                                  ntree=25, mtry = 17))

rf_B_6
#saveRDS(rf_B_6, file = "rf_B_6.rds")
rf_B_6<-readRDS("rf_B_6.rds")
rf_F_6<-readRDS("rf_F_6.rds") #FLOOR 4 WITHOUT ERROR#
rf_BF_6<-readRDS("rf_BF_6.rds") 
rf_BF_good<-readRDS("rf_BF_good.rds")
rf_F_good<-readRDS("rf_F_good.rds")
rf_B_good<-readRDS("rf_B_good.rds")
rf_BF_best<-readRDS("rf_BF_best.rds")
rf_F_best<-readRDS("rf_F_best.rds")
rf_B_best<-readRDS("rf_B_best.rds")

pred_rf_B_6<-predict(rf_B_6, wifi_t_signal_val)
confusionMatrix(pred_rf_B_6, wifi_t_signal_val$BUILDINGID)

pred_rf_F_6<-predict(rf_F_6, wifi_t_signal_val)
confusionMatrix(pred_rf_F_6, wifi_t_signal_val$FLOOR)

pred_rf_BF_6<-predict(rf_BF_6, wifi_t_signal_val)
confusionMatrix(pred_rf_BF_6, wifi_t_signal_val$BF)

pred_rf_BF_good<-predict(rf_BF_good, wifi_t_signal_val)
confusionMatrix(pred_rf_BF_good, wifi_t_signal_val$BF)

pred_rf_F_good<-predict(rf_F_good, wifi_t_signal_val)
confusionMatrix(pred_rf_F_good, wifi_t_signal_val$FLOOR)

pred_rf_B_good<-predict(rf_B_good, wifi_t_signal_val)
confusionMatrix(pred_rf_B_good, wifi_t_signal_val$BUILDINGID)

pred_rf_BF_best<-predict(rf_BF_best, wifi_t_signal_val)
confusionMatrix(pred_rf_BF_best, wifi_t_signal_val$BF)

pred_rf_F_best<-predict(rf_F_best, wifi_t_signal_val)
confusionMatrix(pred_rf_F_best, wifi_t_signal_val$FLOOR)

pred_rf_B_best<-predict(rf_B_best, wifi_t_signal_val)
confusionMatrix(pred_rf_B_best, wifi_t_signal_val$BUILDINGID)

# system.time(rf_lat_best<- randomForest(y=wifi_t_bestsignal$LATITUDE,
#                                        x=wifi_t_bestsignal[c("BUILDINGID", waps_ws)],
#                                        ntree=200, mtry = 104))
# 
# rf_lat_best

# system.time(rf_lon_good<- randomForest(y=wifi_t_goodsignal$LONGITUDE,
#                                        x=wifi_t_goodsignal[c("BUILDINGID", waps_ws)],
#                                        ntree=200, mtry = 104))

rf_lat_good
rf_lon_good

# system.time(rf_lon <- randomForest(y=wifi_t_signal$LONGITUDE,
#                                  x=wifi_t_signal[c("BUILDINGID", waps_ws)],
#                                  ntree=50, mtry = 104))
# 
# rf_lon


rf_lat<-readRDS("rf_lat.rds")
rf_lon<-readRDS("rf_lon.rds")


rf_lat_good<-readRDS("rf_lat_good.rds")
rf_lon_good<-readRDS("rf_lon_good.rds")
rf_lat_good
rf_lon_good

#saveRDS(rf_lon_best, file = "rf_lon_best.rds")
rf_lon_best<-readRDS("rf_lon_best.rds")
#saveRDS(rf_lat_best, file = "rf_lat_best.rds")
rf_lat_best<-readRDS("rf_lat_best.rds")


pred_lon_rf<-predict(rf_lon, wifi_t_signal_val)
pred_lat_rf<-predict(rf_lat, wifi_t_signal_val)
pred_lat_good_rf<-predict(rf_lat_good, wifi_t_signal_val)
pred_lon_good_rf<-predict(rf_lon_good, wifi_t_signal_val)
pred_lon_best_rf<-predict(rf_lon_best, wifi_t_signal_val)
pred_lat_best_rf<-predict(rf_lat_best, wifi_t_signal_val)



postResample(pred_lon_rf, wifi_t_signal_val$LONGITUDE)
postResample(pred_lat_rf, wifi_t_signal_val$LATITUDE)
postResample(pred_lat_good_rf, wifi_t_signal_val$LATITUDE)
postResample(pred_lon_good_rf, wifi_t_signal_val$LONGITUDE)
postResample(pred_lat_best_rf, wifi_t_signal_val$LATITUDE)
postResample(pred_lon_best_rf, wifi_t_signal_val$LONGITUDE)


####PLOTLY####

wifi_t_signal_val_pred<-wifi_t_signal_val

wifi_t_signal_val_pred$LATITUDE<-pred_lat_rf
wifi_t_signal_val_pred$LONGITUDE<-pred_lon_rf
wifi_t_signal_val_pred$FLOOR<-pred_svm_l_F_6

wifi_t_signal_val_pred$PREDICTED<-"pred"
wifi_t_signal_val$PREDICTED<-"real"

dim3d<-c("LATITUDE", "LONGITUDE", "FLOOR", "PREDICTED")

#with dplyr union we see that there are same values)#
df_plot<-rbind(wifi_t_signal_val[dim3d], wifi_t_signal_val_pred[dim3d])

pal <- c("#1289d7", "#ff0000", "#ffa500")
plot_ly(df_plot, x=~LATITUDE,y=~LONGITUDE, colors= pal,type = "scatter3d",
        z= ~FLOOR, color=~PREDICTED, mode="markers", size = 1, sizes=3)

pal <- c("#1289d7", "#00008b")



plot_ly(type = "scatter3d", colors = pal ) %>% 
  add_trace(data=wifi_t_signal_val,
            x = ~LATITUDE,
            y = ~LONGITUDE,
            z = ~FLOOR,
            mode="markers", sizes= 2) %>%
  add_trace(data=wifi_t_signal_val_pred,
            x = ~LATITUDE,
            y = ~LONGITUDE,
            z = ~FLOOR,
            mode="markers") %>%
  # add_trace(data=df_plot, 
  #           x = ~LATITUDE,
  #           y = ~LONGITUDE, 
  #           z = ~FLOOR, 
  #           mode = "lines",
  #           color = ~PREDICTED)




waps_pred<-grep("WAP", names(wifi_t_signal_val_pred), value = TRUE)

nowaps_pred<-setdiff(names(wifi_t_signal_val_pred), waps_pred)

summary(wifi_t_signal_val_pred[nowaps_pred])


# rf<-train(BUILDINGID~WAP_max,data=train,
#           method= "rf", trControl= ctrl,
#           ntree=20,
#           tuneLength = 10)
# 
# rf

#### C.50 with max####
#### PCA with max####

####LOGISTIC REGRESION####

glm_f_6 <- glm(BUILDINGID~WAP_max, data=wifi_t_signal)


glm_lat_6

saveRDS(glm_lat_6, file = "glm_lat_6.rds")
glm_lat_6<-readRDS("glm_lat_6.rds")
glm_lon_6<-readRDS("glm_lon_6.rds")


pred_glm_lon<-predict(glm_lon_6, wifi_t_signal_val)
pred_glm_lat<-predict(glm_lat_6, wifi_t_signal_val)

postResample(pred_glm_lon, wifi_t_signal_val$LONGITUDE)
postResample(pred_glm_lat, wifi_t_signal_val$LATITUDE)


glm_lat_good <- glm(LATITUDE~WAP_max, data=wifi_t_goodsignal)
glm_lon_good <- glm(LONGITUDE~WAP_max, data=wifi_t_goodsignal)


saveRDS(glm_lat_good, file = "glm_lat_good.rds")
glm_lat_6<-readRDS("glm_lat_6.rds")
glm_lon_6<-readRDS("glm_lon_6.rds")

pred_glm_lat_good<-predict(glm_lat_good, wifi_t_signal_val)
postResample(pred_glm_lat_good, wifi_t_signal_val$LATITUDE)


#### KNN with max####

#system.time(knn_lon_6<-train(LONGITUDE~WAP_max_value,data=wifi_t_signal,
#           method= "knn"))

#knn_lon_6

saveRDS(knn_lon_6, file = "knn_lon_6.rds")

knn_F_6<-readRDS("knn_F_6.rds")

knn_B_6<-readRDS("knn_B_6.rds")

knn_BF_6<-readRDS("knn_BF_6.rds")

knn_lat_6<-readRDS("knn_lat_6.rds")

knn_lon_6<-readRDS("knn_lon_6.rds")


knn_pred_B_6<-predict(knn_B_6, wifi_t_signal_val)
confusionMatrix(knn_pred_B_6, wifi_t_signal_val$BUILDINGID)
knn_pred_F_6<-predict(knn_F_6, wifi_t_signal_val)
confusionMatrix(knn_pred_F_6, wifi_t_signal_val$FLOOR)
knn_pred_BF_6<-predict(knn_BF_6, wifi_t_signal_val)
confusionMatrix(knn_pred_BF_6, wifi_t_signal_val$BF)
knn_pred_lat_6<-predict(knn_lat_6, wifi_t_signal_val)
confusionMatrix(knn_pred_lat_6, wifi_t_signal_val$LATITUDE)
knn_pred_lon_6<-predict(knn_lon_6, wifi_t_signal_val)
confusionMatrix(knn_pred_lon_6, wifi_t_signal_val$LONGITUDE)

knn_F<-readRDS("knn.rds")

knn_B<-readRDS("knn_B.rds")

knn_BF<-readRDS("knn_BF.rds")

knn_lat<-readRDS("knn_lat.rds")

knn_lon<-readRDS("knn_lon.rds")

knn_pred_B<-predict(knn_B, wifi_t_signal_val)
confusionMatrix(knn_pred_B, wifi_t_signal_val$BUILDINGID)
knn_pred_F<-predict(knn_F, wifi_t_signal_val)
confusionMatrix(knn_pred_F, wifi_t_signal_val$FLOOR)
knn_pred_BF<-predict(knn_BF, wifi_t_signal_val)
confusionMatrix(knn_pred_BF, wifi_t_signal_val$BF)
knn_pred_lat<-predict(knn_lat, wifi_t_signal_val)
confusionMatrix(knn_pred_lat, wifi_t_signal_val$LATITUDE)
knn_pred_lon<-predict(knn_lon, wifi_t_signal_val)
confusionMatrix(knn_pred_lon, wifi_t_signal_val$LONGITUDE)








stopCluster(cl)

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
ggplot(wifi_t_signal, aes(x=LONGITUDE, y=LATITUDE))+
         geom_point(aes(y=WAP144), na.rm = FALSE)


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

#FOLLOW USERID
#GEOM WAP508
# DELETE FLOOR 4
#BY BUILDING

####B_subset by building####

# wifi_t_b0<-wifi_train%>%dplyr:::filter(BUILDINGID == 0) #floors 0 to 3
# wifi_t_b1<-wifi_train%>%dplyr:::filter(BUILDINGID == 1) #floors 0 to 3
#wifi_t_b2<-wifi_train%>%dplyr:::filter(BUILDINGID == 2) #floors 0 to 4 


# 
# wifi_t_b1_ns<-wifi_t_b1[!vapply(wifi_t_b1[waps], function(x) length(unique(x)) > 1, logical(1L))] #df waps wo signal
# 
# waps_t_b1_ns<-grep("WAP", names(wifi_t_b1_ns), value = TRUE) #WAP columns wo signal
# 
# wifi_t_b1_ws<-wifi_t_b1%>%
#   select(-waps_t_b1_ns) #df waps w signal
####B_near 0 variance####