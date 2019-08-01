# test_sample <- Combi_data_clean %>% group_by(BUILDINGID,FLOOR) %>%  sample_n(100)
require(randomForest)
Real_Test <- read.csv("D:/Ubiqum/Module 3/Task 3.3_WiFi/WiFi v.1/TestSet/testData.csv",header = TRUE, sep = ",")
# tuneRF check best mtry, with doBEST - creatre a model

t_0 <- proc.time()

fit<- tuneRF(x=trainSet[,1:359], y= trainSet$BUILDINGID, doBest = TRUE)

fit_B<- tuneRF(x=trainSet_new[,1:359], y= trainSet_new$BUILDINGID, doBest = TRUE)

saveRDS(fit_B,file= "tuneRF model_acc_0.99_kappa_0.98_new.rds")
fit_B <- readRDS("Models/Random Forest (tuneRF)/tuneRF model_acc_0.99_kappa_0.98_new.rds", refhook = NULL)

t_1 <- proc.time()
time_rf <- t_1-t_0
print(time_rf/60)

fit_B$results
fit
plot(fit)
fit$results
summary(fit_B)

varImp(fit_B)

# fit_TunrRF_acc1_kappa1 <- fit

# DO NOT TOUCH this variable  fit_TunrRF_acc1_kappa1
 RF_pred_B <- predict(fit_B,testSet_1[,1:359])
RF_pred_B_valid <- predict(fit_B,valid_data_orig[,1:520])
  RF_pred_post<- postResample(RF_pred_B, as.factor(testSet_1$BUILDINGID))
 RF_pred_post
 
 readRDS("Models/Random Forest (tuneRF)/tuneRF model_acc_0.99_kappa_0.98_new.rds", refhook = NULL)
 
 testSet_1$Predict_B_tuneRF <-RF_pred_B
 
 confusionMatrix(RF_pred_B, as.factor(testSet_new$BUILDINGID))
 write.csv(testSet,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- testSet_new[which(testSet_new$Predict_B_samp60_tuneRF==testSet_new$BUILDINGID),] 
 RF_mis <- testSet_new[which(testSet_new$Predict_B_samp60_tuneRF!=testSet_new$BUILDINGID),] 
 RF_cor
 RF_mis
 
 #  Validation set
 valid_data_orig$Predict_B_RF <-RF_pred_B_valid
 confusionMatrix(RF_pred_B_valid, as.factor(valid_data_orig$BUILDINGID))
 
 RF_cor <- testSet_new[which(valid_data_orig$Predict_B_RF==valid_data_orig$BUILDINGID),] 
 RF_mis <- testSet_new[which(valid_data_orig$Predict_B_RF!=valid_data_orig$BUILDINGID),] 
 
 # *******************************TRAININg SET ****************************************************
 # Apply model to the training set to have predicted building in training set for the next predictions
 
 RF_pred_B_train <- predict(fit_B,trainSet_new[,1:359])
 
 RF_pred_post_train<- postResample(RF_pred_B_train, as.factor(valid_data_orig$BUILDINGID))
 RF_pred_post
 trainSet_new$Predict_B_samp60_tuneRF <-RF_pred_B_train
 confusionMatrix(RF_pred_B_train, as.factor(trainSet_new$BUILDINGID))

 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- trainSet[which(trainSet$Predict_B_samp60_tuneRF==trainSet$BUILDINGID),] 
 RF_mis <- trainSet[which(trainSet$Predict_B_samp60_tuneRF!=trainSet$BUILDINGID),] 
 RF_cor
 RF_mis
 
 # *******************************TRAINING SET ****************************************************
 # Apply model to the training set to have predicted building in training set for the next predictions
 
 RF_pred_B_train <- predict(fit,trainSet[,1:359])
 
 RF_pred_post_train<- postResample(RF_pred_B_train, as.factor(trainSet$BUILDINGID))
 RF_pred_post
 
 
 
 
 trainSet_new$Predict_B_samp60_tuneRF <-RF_pred_B_train
 
 confusionMatrix(RF_pred_B_train, as.factor(trainSet$BUILDINGID))
 
 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- trainSet[which(trainSet$Predict_B_samp60_tuneRF==trainSet$BUILDINGID),] 
 RF_mis <- trainSet[which(trainSet$Predict_B_samp60_tuneRF!=trainSet$BUILDINGID),] 
 RF_cor
 RF_mis
 
 #### !!!!!!!!!!! PREDICTING FLOORs !!!!!!!!!!!  ####
 
 #### ******************* Building 1 -Floor ******************* ####
 # Training set: Train_Build_1
 
 Train_Build_1 <-testSet_1 %>% filter(Predict_B_tuneRF==1)
 #Train_Build_1 <-  as_tibble(Train_Build_1)
 
 fit_floor <- tuneRF(x=Train_Build_1[,c(1:359)], y = as.factor(as.vector.factor(Train_Build_1$FLOOR)) , doBest = TRUE, importance=TRUE)
 plot(fit_floor)
 
 
 saveRDS(fit_floor,file= "Model for predicting floor in the Building 1.rds")
 fit_floor <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting floor in the Building 1.rds", refhook = NULL)
 
  # Testing set: testSet_Build_1
 testSet_Build_1_1 <-testSet_1 %>% filter(Predict_B_tuneRF==1)
 
 RF_pred_F <- predict(fit_floor,testSet_Build_1_1[,c(1:359)])
 
 RF_pred_post_F<- postResample(RF_pred_F, as.factor(testSet_Build_1$FLOOR))
 RF_pred_post_F
 levels(testSet_Build_1$Predict_Floor_samp60_tuneRF) <- c(levels(testSet_Build_1$Predict_Floor_samp60_tuneRF), "5")    # add new level
 
 testSet_Build_1$FLOOR <- factor(testSet_Build_1$FLOOR)
 
 testSet_Build_1_1$Predict_Floor_tuneRF <-RF_pred_F
 confusionMatrix(RF_pred_F, testSet_Build_1$FLOOR)
 # write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
 # Evaluating
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor_F <- testSet_Build_1[which(testSet_Build_1$Predict_Floor_tuneRF==testSet_Build_1$FLOOR),] 
 RF_mis_F <- testSet_Build_1[which(testSet_Build_1$Predict_Floor_tuneRF!=testSet_Build_1$FLOOR),] 

 # testSet_Build_1 <- testSet_Build_1[,c(1:367,369,368)]

 #### ******************* Building 1 -LATITUDE ******************* ####
 
 # Training set: Train_Build_1
 # Testing set: testSet_Build_1
 t_0 <- proc.time()
 fit_Lat <- tuneRF(x=Train_Build_1[,c(1:359)],
                     y = Train_Build_1$LATITUDE ,
                     doBest = TRUE,
                     importance=TRUE,
                     ntree = 300,)
                     
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lat)
 
 saveRDS(fit_Lat,file= "Model for predicting LATITUDE in the Building 1.rds")
 fit_Lat <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting LATITUDE in the Building 1.rds", refhook = NULL)
 
 
# Predicting
 RF_pred_Lat <- predict(fit_Lat,testSet_Build_1[,c(1:359)])
 
 RF_pred_post_Lat<- postResample(RF_pred_Lat, testSet_Build_1$LATITUDE)
 RF_pred_post_Lat
 
 testSet_Build_1$Pr_LAT_tuneRF <-RF_pred_Lat
 
 #### ******************* Building 1 -LONGITUDE ******************* ####
 
 # Training set: Train_Build_1
 # Testing set: testSet_Build_1
 t_0 <- proc.time()
 fit_Lon <- tuneRF(x=Train_Build_1[,c(1:359)],
                   y = Train_Build_1$LONGITUDE ,
                   doBest = TRUE,
                   importance=TRUE,
                   ntree = 300,)
 
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lon)
 
 saveRDS(fit_Lon,file= "Model for predicting Longitute in the Building 1.rds")
 # fit_Lon <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting Longitute in the Building 1.rds", refhook = NULL)
 
 # Predicting
 RF_pred_Lon <- predict(fit_Lon,testSet_Build_1[,c(1:359)])
 
 RF_pred_post_Lon<- postResample(RF_pred_Lon, testSet_Build_1$LONGITUDE)
 RF_pred_post_Lon
 
 testSet_Build_1$Pr_Lon_tuneRF <-RF_pred_Lon
 
 ####  plot testSet_Build_1 ####
 
 plot_ly(testSet_Build_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
   add_markers(color = "red",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' )))
 
 
 ####  plot testSet_Build_1 ####
 
 plot_ly(testSet_Build_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
   add_markers(color = "red",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' )))
 
 plot_ly(testSet_Build_1, x = ~ Pr_Lon_tuneRF, y = ~Pr_LAT_tuneRF, z = ~unclass(Predict_Floor_samp60_tuneRF),color =I("blue"),marker = list(size = 5)) %>%
   add_markers(color = "blue",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' )))
 
 # *******************************TRAINING SET - Building 1 -Floor  ****************************************************
 # Apply model to the training set to have predicted building in training set for the next predictions
 

 RF_pred_B1f_train <- predict(fit_floor,Train_Build_1[,1:359])
 
 RF_pred_post_train_B1f<- postResample(RF_pred_B1f_train, as.factor(Train_Build_1$FLOOR))
 RF_pred_post_train_B1f
 
 Train_Build_1$Predict_Floor_samp60_tuneRF <-RF_pred_B1f_train

 bb <- confusionMatrix(RF_pred_B_train, as.factor(trainSet$BUILDINGID))
 plt.matshow(bb$table)
 
 
 bb <-  as.data.frame(bb$table)
 plot <- ggplot( bb)
 draw_confusion_matrix(bb$table)
 
 plot + geom_tile(aes(x=Prediction, y=Reference, fill=Freq)) + scale_x_discrete(name="Actual Class")
 + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
   labs(fill="Normalized\nFrequency")
 
 plt.show()
 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- Train_Build_1[which(Train_Build_1$Predict_Floor_samp60_tuneRF==Train_Build_1$FLOOR),] 
 RF_mis <- Train_Build_1[which(Train_Build_1$Predict_Floor_samp60_tuneRF!=Train_Build_1$FLOOR),] 
 
 #### ******************* Building 2 -Floor ******************* ####
 
 Train_Build_2_1 <-trainSet_1 %>% filter(Predict_B_tuneRF==2)
 #Train_Build_1 <-  as_tibble(Train_Build_1)
 
 fit_floor <- tuneRF(x=Train_Build_2[,c(1:359)], y = as.factor(as.vector.factor(Train_Build_2$FLOOR)) , doBest = TRUE, importance=TRUE)
 plot(fit_floor)
 
 
 saveRDS(fit_floor,file= "Model for predicting floor in the Building 2.rds")
 fit_floor <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting floor in the Building 2.rds", refhook = NULL)
 
 testSet_Build_2_1 <-testSet_1 %>% filter(Predict_B_tuneRF==2)
 
 RF_pred_F <- predict(fit_floor,testSet_Build_2_1[,c(1:359)])
 
 RF_pred_post_F<- postResample(RF_pred_F, as.factor(testSet_Build_2$FLOOR))
 RF_pred_post_F
 
 testSet_Build_2_1$Predict_Floor_tuneRF <-RF_pred_F
 confusionMatrix(as.factor(RF_pred_F), as.factor(testSet_Build_2$FLOOR))
 # write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
 # Evaluating
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor_F <- testSet_Build_2[which(testSet_Build_2$Predict_Floor_samp60_tuneRF==testSet_Build_2$FLOOR),] 
 RF_mis_F <- testSet_Build_2[which(testSet_Build_2$Predict_Floor_samp60_tuneRF!=testSet_Build_2$FLOOR),] 
 
 
 # Evaluate correctly / incorrectly predicted combinations B_F
 testSet_Build_2$ID <- paste(testSet_Build_2$BUILDINGID,testSet_Build_2$FLOOR,sep = "B_F")
 testSet_Build_2$ID_p <- paste(testSet_Build_2$Predict_B_samp60_tuneRF,testSet_Build_2$Predict_Floor_samp60_tuneRF,sep = "B_F")
 
 RF_cor_ID <- testSet_Build_2[which(testSet_Build_2$ID==testSet_Build_2$ID_p),] 
 RF_mis_ID <- testSet_Build_2[which(testSet_Build_2$ID!=testSet_Build_2$ID_p),] 
 
 #### ******************* Building 2 -LATITUDE ******************* ####
 
 # Training set: Train_Build_2
 # Testing set: testSet_Build_2
 
 t_0 <- proc.time()
 fit_Lat_2 <- tuneRF(x=Train_Build_2[,c(1:359)],
                   y = Train_Build_2$LATITUDE ,
                   doBest = TRUE,
                   importance=TRUE,
                   ntree = 300,)
 
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lat_2)
 
 saveRDS(fit_Lat_2,file= "Model for predicting LATITUDE in the Building 2.rds")
 fit_Lat_2 <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting LATITUDE in the Building 2.rds", refhook = NULL)
 
 # Predicting
 RF_pred_Lat_2 <- predict(fit_Lat_2,testSet_Build_2[,c(1:359)])
 
 RF_pred_post_Lat<- postResample(RF_pred_Lat_2, testSet_Build_2$LATITUDE)
 RF_pred_post_Lat
 
 testSet_Build_2$Pr_LAT_tuneRF <- RF_pred_Lat_2
 
 #### ******************* Building 2 -LONGITUDE ******************* ####
 

 t_0 <- proc.time()
 fit_Lon_2 <- tuneRF(x=Train_Build_2[,c(1:359)],
                   y = Train_Build_2$LONGITUDE ,
                   doBest = TRUE,
                   importance=TRUE,
                   ntree = 300,)
 
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lon_2)
 
 saveRDS(fit_Lon_2,file= "Model for predicting Longitute in the Building 2.rds")
 fit_Lon_2 <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting Longitute in the Building 2.rds", refhook = NULL)
 
 # Predicting
 RF_pred_Lon_2 <- predict(fit_Lon_2,testSet_Build_2[,c(1:359)])
 
 RF_pred_post_Lon_2<- postResample(RF_pred_Lon_2, testSet_Build_2$LONGITUDE)
 RF_pred_post_Lon_2
 
 testSet_Build_2$Pr_Lon_tuneRF <-RF_pred_Lon_2
 
 ####  plot testSet_Build_2 ####
 
 plot_ly(testSet_Build_2, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
   add_markers(color = "red",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' )))
 
 plot_ly(testSet_Build_2, x = ~ Pr_Lon_tuneRF, y = ~Pr_LAT_tuneRF, z = ~unclass(Predict_Floor_samp60_tuneRF),color =I("blue"),marker = list(size = 5)) %>%
   add_markers(color = "blue",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' )))
 
 
 # *******************************TRAINING SET - Building 2 - Floor  ****************************************************

  # Apply model to the training set to have predicted building in training set for the next predictions
 
 RF_pred_B2f_train <- predict(fit_floor,Train_Build_2[,1:359])
 
 RF_pred_post_train_B2f<- postResample(RF_pred_B2f_train, as.factor(Train_Build_2$FLOOR))
 RF_pred_post_train_B2f
 
 Train_Build_2$Predict_Floor_samp60_tuneRF <-RF_pred_B2f_train
 
 confusionMatrix(RF_pred_B2f_train, as.factor(Train_Build_2$FLOOR))
 
 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- Train_Build_2[which(Train_Build_2$Predict_Floor_samp60_tuneRF==Train_Build_2$FLOOR),] 
 RF_mis <- Train_Build_2[which(Train_Build_2$Predict_Floor_samp60_tuneRF!=Train_Build_2$FLOOR),] 
 RF_cor
 RF_mis
 
 #### ******************* Building 3 - Floor ******************* ####
 
 Train_Build_3 <-trainSet_new %>% filter(Predict_B_samp60_tuneRF==3)
 #Train_Build_1 <-  as_tibble(Train_Build_1)
 
 fit_floor <- tuneRF(x=Train_Build_3[,c(1:359)], y = as.factor(as.vector.factor(Train_Build_3$FLOOR)) , doBest = TRUE, importance=TRUE)
 plot(fit_floor)
 
 
 saveRDS(fit_floor,file= "Model for predicting floor in the Building 3.rds")
 fit_floor <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting floor in the Building 3.rds", refhook = NULL)
 
 testSet_Build_3_1 <-testSet_1 %>% filter(Predict_B_tuneRF==3)
 
 RF_pred_F <- predict(fit_floor,testSet_Build_3_1[,c(1:359)])
 
 RF_pred_post_F<- postResample(RF_pred_F, as.factor(testSet_Build_3$FLOOR))
 RF_pred_post_F
 
 testSet_Build_3_1$Predict_Floor_tuneRF <-RF_pred_F
 levels(testSet_Build_3$Predict_Floor_samp60_tuneRF) <- c(levels(testSet_Build_3$Predict_Floor_samp60_tuneRF), "5")    # add new level
 
 confusionMatrix(as.factor(RF_pred_F), testSet_Build_3$FLOOR)
 # write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
 # Evaluating
 #-Select instances that were correctly/incorrectly classifed in rf model
 
 RF_cor_F <- testSet_Build_3[which(testSet_Build_3$Predict_Floor_samp60_tuneRF==testSet_Build_3$FLOOR),] 
 RF_mis_F <- testSet_Build_3[which(testSet_Build_3$Predict_Floor_samp60_tuneRF!=testSet_Build_3$FLOOR),] 
 
 #### ******************* Building 3 -LATITUDE ******************* ####
 
 # Training set: Train_Build_3
 # Testing set: testSet_Build_3
 
 t_0 <- proc.time()
 fit_Lat_3 <- tuneRF(x=Train_Build_3[,c(1:359)],
                     y = Train_Build_3$LATITUDE ,
                     doBest = TRUE,
                     importance=TRUE,
                     ntree = 300,)
 
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lat_3)
 
 saveRDS(fit_Lat_3,file= "Model for predicting LATITUDE in the Building 3.rds")
 fit_Lat_3 <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting LATITUDE in the Building 3.rds", refhook = NULL)
 
 # Predicting
 RF_pred_Lat_3 <- predict(fit_Lat_3,testSet_Build_3[,c(1:359)])
 
 RF_pred_post_Lat<- postResample(RF_pred_Lat_3, testSet_Build_3$LATITUDE)
 RF_pred_post_Lat
 
 testSet_Build_3$Pr_LAT_tuneRF <-RF_pred_Lat_3
 
 #### ******************* Building 3 -LONGITUDE ******************* ####
 
 
 t_0 <- proc.time()
 fit_Lon_3 <- tuneRF(x=Train_Build_3[,c(1:359)],
                     y = Train_Build_3$LONGITUDE ,
                     doBest = TRUE,
                     importance=TRUE,
                     ntree = 300,)
 
 t_1 <- proc.time()
 time_rf <- t_1-t_0
 print(time_rf/60)
 plot(fit_Lon_3)
 
 saveRDS(fit_Lon_3,file= "Model for predicting Longitute in the Building 3.rds")
 fit_Lon_3 <-  readRDS("Models/Random Forest (tuneRF)/Model for predicting Longitute in the Building 3.rds", refhook = NULL)
 
 # Predicting
 RF_pred_Lon_3 <- predict(fit_Lon_3,testSet_Build_3[,c(1:359)])
 
 RF_pred_post_Lon_3<- postResample(RF_pred_Lon_3, testSet_Build_3$LONGITUDE)
 RF_pred_post_Lon_3
 
 testSet_Build_3$Pr_Lon_tuneRF <-RF_pred_Lon_3
 
 
 # *******************************TRAINING SET - Building 3 - Floor  ****************************************************
 
 # Apply model to the training set to have predicted building in training set for the next predictions
 
 RF_pred_B3f_train <- predict(fit_floor,Train_Build_3[,1:359])
 
 RF_pred_post_train_B3f<- postResample(RF_pred_B3f_train, as.factor(Train_Build_3$FLOOR))
 RF_pred_post_train_B3f
 
 Train_Build_3$Predict_Floor_samp60_tuneRF <-RF_pred_B3f_train
 levels(Train_Build_3$Predict_Floor_samp60_tuneRF) <- c(levels(Train_Build_3$Predict_Floor_samp60_tuneRF), "5")    # add new level
 
 confusionMatrix(RF_pred_B3f_train, as.factor(Train_Build_3$FLOOR))
 
 # Evaluating
 
 #-Select instances that were correctly/incorrectly classifed in rf model
 RF_cor <- Train_Build_3[which(Train_Build_3$Predict_Floor_samp60_tuneRF==Train_Build_3$FLOOR),] 
 RF_mis <- Train_Build_3[which(Train_Build_3$Predict_Floor_samp60_tuneRF!=Train_Build_3$FLOOR),] 
 
 
 # PLots
 
 Train_Build_2_plot <- Train_Build_2
 
 
 
 Train_Build_2_plot[Train_Build_2_plot==100]<-NA
 
 plot_ly(Train_Build_2_plot, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
   add_markers(color = "red",colors = "Set1") %>%
   layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                 yaxis = list(title = 'LATITUDE'),
                                                                 zaxis = list(title = 'Floor' ))) 
 
 # 3D plot for signal's distribution by correct / wrong building 1
 All_testSet <- rbind(testSet_Build_1[,1:377],testSet_Build_2,testSet_Build_3)
 All_testSet_1 <- rbind(testSet_Build_1_1,testSet_Build_2_1,testSet_Build_3_1)
 
 All_testSet_1$ID <- paste(All_testSet_1$BUILDINGID,All_testSet_1$FLOOR,sep = "B_f")
 All_testSet_1$PredictID <- paste(All_testSet_1$Predict_B_tuneRF,All_testSet_1$Predict_Floor_tuneRF,sep = "B_f")
 
 plot_ly(new_dataset, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
   add_markers(color = ~as.factor(Origin),colors = "Set1") %>%
   layout(title = "Signals distribution - trainig and validation sets",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                                    yaxis = list(title = 'LATITUDE'),
                                                                                    zaxis = list(title = 'Floor' )))
 
 All_testSet_1<-All_testSet_1 %>%
   dplyr::mutate(Build_check1 = as.numeric(BUILDINGID)  - as.numeric(Predict_B_tuneRF))
 
 All_testSet_1<-All_testSet_1 %>%
   dplyr::mutate(Floor_check1 = as.numeric(FLOOR)  - as.numeric(Predict_Floor_tuneRF))
 
 #  Creating Build_check column 
 All_testSet_1<-All_testSet_1 %>%
   mutate(
     Build_check = case_when(
       Build_check1== 0  ~ "Correct",
       Build_check1!= 0  ~ "Wrong"))
 
 All_testSet_1<-All_testSet_1 %>%
   mutate(
     Floor_check = case_when(
       Floor_check1== 0  ~ "Correct",
       Floor_check1!= 0  ~ "Wrong"))
 
 All_testSet_1<-All_testSet_1 %>%
   mutate(
     ID_check = case_when(
       Build_check == "Correct" & Floor_check == "Correct" ~ "Correct",
       TRUE  ~ "Wrong"))
 
 All_testSet_1$Build_check1 <-NULL
 All_testSet_1$Floor_check1 <-NULL
 
 w <- ggplot(All_testSet_1, aes(All_testSet_1$PHONEID, fill='Correct')) + 
   geom_histogram(binwidth = 2) +
   geom_histogram(data=All_testSet_1, aes(All_testSet_1$PHONEID, fill='Wrong'),
                  alpha=0.7, binwidth = 2) +
   scale_fill_manual(values=c('Correct'='green', 'Wrong'='blue')) +
   labs(fill='Prediction') +
   theme(text = element_text(size=14)) +
   theme(panel.border=element_rect(colour='black', fill=NA)) +
   ggtitle('Distribution of wrong predictions for Building and floor by phone ID') +
   xlab('Percent of Incorrectly Classified Instances by PhoneID')
 
 ggplotly(w)
 
 All_testSet_cor <- All_testSet_1[which(All_testSet_1$ID_check== "Correct"),] 
 All_testSet_mis <- All_testSet_1[which(All_testSet_1$ID_check== "Wrong"),]  
 
 KNN_cor1 <- testSet_1[which(testSet_1$Predict_B_tuneRF==testSet_1$BUILDINGID),] 
 KNN_mis1 <- testSet_1[which(testSet_1$Predict_B_tuneRF !=testSet_1$BUILDINGID),] 
 All_testSet_cor$WAP_num <- apply(KNN_cor1[,1:359], 1, function(x) length(which(x !=100)))
 All_testSet_mis$WAP_num <- apply(KNN_mis1[,1:259], 1, function(x) length(which(x !=100)))
 
 write.csv(All_testSet_cor,file = "All_testSet_cor.csv", row.names = FALSE)
 write.csv(All_testSet_mis,file = "All_testSet_mis.csv", row.names = FALSE)
 
 w <- ggplot(All_testSet_cor, aes(All_testSet_cor$PHONEID, fill='Correct')) + 
   geom_histogram(binwidth = 2) +
   geom_histogram(data=All_testSet_mis, aes(All_testSet_mis$PHONEID, fill='Incorrect'),
                  alpha=0.7, binwidth = 2) +
   scale_fill_manual(values=c('Correct'='green', 'Incorrect'='blue')) +
   labs(fill='Prediction') +
   theme(text = element_text(size=14)) +
   theme(panel.border=element_rect(colour='black', fill=NA)) +
   ggtitle('Quantity of correctly / incorrectly classified Building_Floor by PhoneID') +
   xlab('Phone numbers')

 ggplotly(w)
 
 #-Make data frame of counts for correctly classified by phoneID
 require(broom)
 phone_hit <- tidy(table(All_testSet_cor$PHONEID))
 #-Assign new column names
 names(phone_hit) <- c('PHONEID', 'Count_hit') 
 
 #-Make data frame of counts for incorrectly classified by phoneID
 phone_mis <- tidy(table(rf_mis$PHONEID))
 #-Assign new column names
 names(phone_mis) <- c('PhoneID', 'Count_miss')
 
 #-Merge data frames
 x <- merge(x=phone_hit, y=phone_mis, by = 'PhoneID')
 
 #-Add column for percent missed by phoneID
 y <- mutate(x, Percent_miss = round(Count_miss/(Count_miss + Count_hit)*100,3))
 
 #-Plot percent miscalssified by phone ID
 ggplot(All_testSet_1, aes(x=factor(PHONEID), y= Percent_miss)) +
   geom_bar(stat='identity', fill='blue', colour='black') + 
   ggtitle('Percent of Incorrectly Classified Instances by PhoneID')+
   xlab('Phone ID') +
   ylab('Percent Misclassified') +
   theme(text = element_text(size=14)) +
   ylim(0,100) +
   theme(panel.border=element_rect(colour='black', fill=NA))
 
 