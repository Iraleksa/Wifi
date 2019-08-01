#registerDoMC(cores=16)   #-for parallel processing as AWS EC2 instance
set.seed(7811)

#-Grid of k values to search 
knn_grid <- expand.grid(.k=c(1:5))

#-Train kNN model
t_0 <- proc.time()
knn_fit <- train(BUILDINGID~., data=  trainSet[,1:360], 
                 method='knn',
                 preProcess = c('zv'),
                 tuneGrid=knn_grid,
                 trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

# tuneGrid=knn_grid,

# fit_KNN_acc0.9999_kappa0.9999 <- knn_fit

# DO NOT TOUCH this variable fit_KNN_acc0.9999_kappa0.9999


saveRDS(knn_fit, file= "knn_model_acc_0.9983_kappa_0.9973.rds") # Why it does not work?
knn_fit <-  readRDS("Models/KNN/knn_model_acc_0.9983_kappa_0.9973.rds", refhook = NULL)

#  Assesment KNN

knn_fit$results

#-Save model
plot(knn_fit)
print(knn_fit)

# Prediction

knn_fit_pred_test<-predict(knn_fit,testSet_new[,1:359])

confusionMatrix(knn_fit_pred_test, as.factor(testSet_new$BUILDINGID))
knn_post<- postResample(knn_fit_pred_test, as.factor(testSet_new$BUILDINGID)) # DOES NOT WORK
knn_post
# Performance of model on testing samples

summary(knn_fit_pred_test)

testSet_new$Predict_B_KNN1 <-knn_fit_pred_test

write.csv(testSet_new,file = "testSet_new_predict Build (tuneRF and KNN) Version 7.csv", row.names = FALSE)

# valid_data_prediction$Predict_B_KNN_20 <- NULL

# Evaluating

#-Select instances that were correctly/ incorrectly classifed in rf model
KNN_cor <- testSet_new[which(testSet_new$Predict_B_KNN1==testSet$BUILDINGID),] 
KNN_mis <- testSet_new[which(testSet_new$Predict_B_KNN1 !=testSet$BUILDINGID),] 

#-Create data frames of instance counts at locations for correctly and incorrectly classified
KNN_cor_Freq <- as.data.frame(table(KNN_cor$BUILDINGID))
KNN_mis_Freq <- as.data.frame(table(KNN_mis$BUILDINGID))

# *******************************TRAININg SET ****************************************************
# Apply model to the training set to have predicted building in training set for the next predictions

knn_fit_pred_train<-predict(knn_fit,trainSet_new[,1:359])

confusionMatrix(knn_fit_pred_train, as.factor(trainSet_new$BUILDINGID))
knn_post<- postResample(knn_fit_pred_train, as.factor(trainSet_new$BUILDINGID)) # DOES NOT WORK
knn_post
# Performance of model on testing samples

summary(knn_fit_pred_train)

trainSet_new$Predict_B_KNN <-knn_fit_pred_train

write.csv(trainSet_new,file = "trainSet_new Version 7.csv", row.names = FALSE)

# Evaluating

#-Select instances that were correctly/ incorrectly classifed in rf model
KNN_cor <- trainSet_new[which(trainSet_new$Predict_B_KNN==trainSet_new$BUILDINGID),] 
KNN_mis <- trainSet_new[which(trainSet_new$Predict_B_KNN !=trainSet_new$BUILDINGID),] 

#### !!!!!!!!!!! PREDICTING FLOOR !!!!!!!!!!!  ####
Train_Build_1 <-trainSet %>% filter(Predict_B_samp60_KNN==1)

set.seed(7811)

#-Grid of k values to search 
knn_grid <- expand.grid(.k=c(1:5))

#-Train kNN model
t_0 <- proc.time()
knn_fit <- train(BUILDINGID~., data =  Train_Build_1[,c(1:359)], 
                 method='knn',
                 preProcess = c('zv'),
                 tuneGrid=knn_grid,
                 trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

#### !!!!!!!!!!! PREDICTING FLOORs !!!!!!!!!!!  ####

#### ******************* Building 1 -Floor ******************* ####

Train_Build_1 <-trainSet_new %>% filter(Predict_B_samp60_tuneRF==1)
#Train_Build_1 <-  as_tibble(Train_Build_1)
Train_Build_1$FLOOR <- factor(Train_Build_1$FLOOR)
# testSet_Build_1$Predict_Floor_KNN <- factor(testSet_Build_1$Predict_Floor_KNN)

t_0 <- proc.time()
knn_fit_floor <- train(FLOOR ~., data=  Train_Build_1[,c(1:360,366)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

saveRDS(knn_fit_floor,file= "Model for predicting floor in the Building 1_KNN.rds")
knn_fit_floor <-  readRDS("Models/KNN/Model for predicting floor in the Building 1_KNN.rds", refhook = NULL)

testSet_Build_1 <-testSet_new %>% filter(Predict_B_samp60_tuneRF==1)

KNN_pred_F <- predict(knn_fit_floor,testSet_Build_1[,c(1:359,366)])
testSet_Build_1$Predict_B_samp60_tuneRF <- as.factor(testSet_Build_1$Predict_B_samp60_tuneRF)

KNN_pred_post_F<- postResample(KNN_pred_F, as.factor(testSet_Build_1$FLOOR))
KNN_pred_post_F

testSet_Build_1$Predict_Floor_KNN <-KNN_pred_F
levels(testSet_Build_1$Predict_Floor_KNN) <- c((testSet_Build_1$Predict_Floor_KNN), "5")   # add new level

confusionMatrix(KNN_pred_F, testSet_Build_1$FLOOR)
# write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
# Evaluating
#-Select instances that were correctly/incorrectly classifed in rf 
testSet_Build_1$FLOOR <- factor(testSet_Build_1$FLOOR)

RF_cor_F <- testSet_Build_1[which(testSet_Build_1$Predict_Floor_KNN==testSet_Build_1$FLOOR),] 
RF_mis_F <- testSet_Build_1[which(testSet_Build_1$Predict_Floor_KNN!=testSet_Build_1$FLOOR),] 



#### ******************* Building 1 -LATITUDE ******************* ####

# Training set: Train_Build_1
# Testing set: testSet_Build_1

t_0 <- proc.time()
KNN_fit_Lat_1 <- train(LATITUDE ~., data =  Train_Build_1[,c(1:359,363)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lat_1)

saveRDS(KNN_fit_Lat_1,file= "Model for predicting LATITUDE in the Building 1 (KNN_fit_Lat_1).rds")
# KNN_fit_Lat_1 <-  readRDS("Models/KNN/Model for predicting LATITUDE in the Building 1 (KNN_fit_Lat_1).rds", refhook = NULL)

# Predicting
KNN_pred_Lat_1 <- predict(KNN_fit_Lat_1,testSet_Build_1[,c(1:359)])

KNN_pred_post_Lat<- postResample(KNN_pred_Lat_1, testSet_Build_1$LATITUDE)
KNN_pred_post_Lat

testSet_Build_1$Pr_LAT_KNN <-KNN_pred_Lat_1

#### ******************* Building 1 -LONGITUDE ******************* ####


t_0 <- proc.time()
KNN_fit_Lon_1 <- train(LONGITUDE ~., data =  Train_Build_1[,c(1:359,362)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lon_1)

saveRDS(KNN_fit_Lon_1,file= "Model for predicting Longitute in the Building 1 (KNN_fit_Lon_1).rds")
# KNN_fit_Lon_1 <-  readRDS("Models/KNN/Model for predicting Longitute in the Building 1 (KNN_fit_Lon_1).rds", refhook = NULL)

# Predicting
KNN_pred_Lon_1 <- predict(KNN_fit_Lon_1,testSet_Build_1[,c(1:359)])

KNN_pred_post_Lon_1<- postResample(KNN_pred_Lon_1, testSet_Build_1$LONGITUDE)
KNN_pred_post_Lon_1

testSet_Build_1$Pr_Lon_KNN <-KNN_pred_Lon_1

write.csv(testSet_Build_1,file = "testSet_Build_1 Build (tuneRF and KNN) Version 8.csv", row.names = FALSE)


#### ******************* Building 2 -Floor ******************* ####

Train_Build_2 <-trainSet_new %>% filter(Predict_B_samp60_tuneRF==2)
#Train_Build_1 <-  as_tibble(Train_Build_1)
Train_Build_2$FLOOR <- factor(Train_Build_2$FLOOR)
testSet_Build_2$Predict_Floor_KNN <- factor(testSet_Build_2$Predict_Floor_KNN)

t_0 <- proc.time()
knn_fit_floor <- train(FLOOR ~., data=  Train_Build_2[,1:360], 
                 method='knn',
                 preProcess = c('zv'),
                 tuneGrid=knn_grid,
                 trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

saveRDS(knn_fit_floor,file= "Model for predicting floor in the Building 2_KNN.rds")
knn_fit_floor <-  readRDS("Models/KNN/Model for predicting floor in the Building 2_KNN.rds", refhook = NULL)

testSet_Build_2 <-testSet_new %>% filter(Predict_B_samp60_tuneRF==2)

KNN_pred_F <- predict(knn_fit_floor,testSet_Build_2[,c(1:359)])

KNN_pred_post_F<- postResample(KNN_pred_F, as.factor(testSet_Build_2$FLOOR))
KNN_pred_post_F

testSet_Build_2$Predict_Floor_KNN <-KNN_pred_F
levels(testSet_Build_2$Predict_Floor_KNN) <- c((testSet_Build_2$Predict_Floor_KNN), "5")   # add new level

confusionMatrix(as.factor(KNN_pred_F), testSet_Build_2$FLOOR)
# write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
# Evaluating
#-Select instances that were correctly/incorrectly classifed in rf model
RF_cor_F <- testSet_Build_2[which(testSet_Build_2$Predict_Floor_KNN==testSet_Build_2$FLOOR),] 
RF_mis_F <- testSet_Build_2[which(testSet_Build_2$Predict_Floor_KNN!=testSet_Build_2$FLOOR),] 



#### ******************* Building 2 -LATITUDE ******************* ####

# Training set: Train_Build_2
# Testing set: testSet_Build_2

t_0 <- proc.time()
KNN_fit_Lat_2 <- train(LATITUDE ~., data =  Train_Build_2[,c(1:359,363)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lat_2)

saveRDS(KNN_fit_Lat_2,file= "Model for predicting LATITUDE in the Building 2 (KNN_fit_Lat_2).rds")
KNN_fit_Lat_2 <-  readRDS("Models/KNN/Model for predicting LATITUDE in the Building 2 (KNN_fit_Lat_2).rds", refhook = NULL)

# Predicting
KNN_pred_Lat_2 <- predict(KNN_fit_Lat_2,testSet_Build_2[,c(1:359)])

KNN_pred_post_Lat<- postResample(KNN_pred_Lat_2, testSet_Build_2$LATITUDE)
KNN_pred_post_Lat

testSet_Build_2$Pr_LAT_KNN <-KNN_pred_Lat_2

#### ******************* Building 2 -LONGITUDE ******************* ####


t_0 <- proc.time()
KNN_fit_Lon_2 <- train(LONGITUDE ~., data =  Train_Build_2[,c(1:359,362)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lon_2)

saveRDS(KNN_fit_Lon_2,file= "Model for predicting Longitute in the Building 2 (KNN_fit_Lon_2).rds")
# KNN_fit_Lon_2 <-  readRDS("Models/KNN/Model for predicting Longitute in the Building 2 (KNN_fit_Lon_2).rds", refhook = NULL)

# Predicting
KNN_pred_Lon_2 <- predict(KNN_fit_Lon_2,testSet_Build_2[,c(1:359)])

KNN_pred_post_Lon_2<- postResample(KNN_pred_Lon_2, testSet_Build_2$LONGITUDE)
KNN_pred_post_Lon_2

testSet_Build_2$Pr_Lon_KNN <-KNN_pred_Lon_2

write.csv(testSet_Build_2,file = "testSet_Build_2 Build (tuneRF and KNN) Version 8.csv", row.names = FALSE)

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

#### ******************* Building 3 -Floor ******************* ####

Train_Build_3 <-trainSet_new %>% filter(Predict_B_samp60_tuneRF==3)
#Train_Build_1 <-  as_tibble(Train_Build_1)
Train_Build_3$FLOOR <- factor(Train_Build_3$FLOOR)
testSet_Build_3$Predict_Floor_KNN <- factor(testSet_Build_3$Predict_Floor_KNN)

t_0 <- proc.time()
knn_fit_floor_3 <- train(FLOOR ~., data=  Train_Build_3[,1:360], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)




saveRDS(knn_fit_floor_3,file= "Model for predicting floor in the Building 3_KNN.rds")
knn_fit_floor_3 <-  readRDS("Models/KNN/Model for predicting floor in the Building 3_KNN.rds", refhook = NULL)

# testSet_Build_3 <-testSet_new %>% filter(Predict_B_samp60_tuneRF==3)

KNN_pred_F_3 <- predict(knn_fit_floor_3,testSet_Build_3[,c(1:359)])

KNN_pred_post_F <- postResample(KNN_pred_F_3, as.factor(testSet_Build_3$FLOOR))
KNN_pred_post_F

testSet_Build_3$Predict_Floor_KNN <-KNN_pred_F_3
levels(testSet_Build_3$Predict_Floor_KNN) <- c((testSet_Build_3$Predict_Floor_KNN), "5")   # add new level

confusionMatrix(as.factor(testSet_Build_3$Predict_Floor_KNN), testSet_Build_3$FLOOR)

levels(KNN_pred_F) <- c((KNN_pred_F), "5")   # add new level

# write.csv(testSet_new,file = "testSet(samp 40) Perfect Version 6.csv", row.names = FALSE)
# Evaluating
#-Select instances that were correctly/incorrectly classifed in rf model
RF_cor_F <- testSet_Build_3[which(testSet_Build_3$Predict_Floor_KNN==testSet_Build_3$FLOOR),] 
RF_mis_F <- testSet_Build_3[which(testSet_Build_3$Predict_Floor_KNN!=testSet_Build_3$FLOOR),] 



#### ******************* Building 3 -LATITUDE ******************* ####

# Training set: Train_Build_3
# Testing set: testSet_Build_3

t_0 <- proc.time()
KNN_fit_Lat_3 <- train(LATITUDE ~., data =  Train_Build_3[,c(1:359,363)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lat_3)

saveRDS(KNN_fit_Lat_3,file= "Model for predicting LATITUDE in the Building 3 (KNN_fit_Lat_3).rds")
KNN_fit_Lat_3 <-  readRDS("Models/KNN/Model for predicting LATITUDE in the Building 3 (KNN_fit_Lat_3).rds", refhook = NULL)

# Predicting
KNN_pred_Lat_3 <- predict(KNN_fit_Lat_3,testSet_Build_3[,c(1:359)])

KNN_pred_post_Lat<- postResample(KNN_pred_Lat_3, testSet_Build_3$LATITUDE)
KNN_pred_post_Lat

testSet_Build_3$Pr_LAT_KNN <-KNN_pred_Lat_3

#### ******************* Building 3 - LONGITUDE ******************* ####


t_0 <- proc.time()
KNN_fit_Lon_3 <- train(LONGITUDE ~., data =  Train_Build_3[,c(1:359,362)], 
                       method='knn',
                       preProcess = c('zv'),
                       tuneGrid=knn_grid,
                       trControl = fitControl)
t_1 <- proc.time()
time_knn <- t_1-t_0
print(time_knn/60)

plot(KNN_fit_Lon_3)

saveRDS(KNN_fit_Lon_3,file= ("Model for predicting Longitute in the Building 3 (KNN_fit_Lon_3).rds"))
# KNN_fit_Lon_3 <-  readRDS("Models/KNN/Model for predicting Longitute in the Building 3 (KNN_fit_Lon_3).rds", refhook = NULL)

# Predicting
KNN_pred_Lon_3 <- predict(KNN_fit_Lon_3,testSet_Build_3[,c(1:359)])

KNN_pred_post_Lon_3<- postResample(KNN_pred_Lon_3, testSet_Build_3$LONGITUDE)
KNN_pred_post_Lon_3

testSet_Build_3$Pr_Lon_KNN <-KNN_pred_Lon_3

write.csv(testSet_Build_3,file = "testSet_Build_3 Build (tuneRF and KNN) Version 8.csv", row.names = FALSE)


testSet_Build_3 <-testSet_Build_3[,c(1:367,373,370,371:372,368:369)]
# *******************************TRAINING SET - Building 3 - Floor  ****************************************************

# Apply model to the training set to have predicted building in training set for the next predictions

RF_pred_B2f_train <- predict(fit_floor,Train_Build_3[,1:359])

RF_pred_post_train_B3f<- postResample(RF_pred_B3f_train, as.factor(Train_Build_3$FLOOR))
RF_pred_post_train_B3f

Train_Build_3$Predict_Floor_samp60_tuneRF <-RF_pred_B3f_train

confusionMatrix(RF_pred_B3f_train, as.factor(Train_Build_3$FLOOR))

# Evaluating

#-Select instances that were correctly/incorrectly classifed in rf model
RF_cor <- Train_Build_2[which(Train_Build_2$Predict_Floor_samp60_tuneRF==Train_Build_2$FLOOR),] 
RF_mis <- Train_Build_2[which(Train_Build_2$Predict_Floor_samp60_tuneRF!=Train_Build_2$FLOOR),] 




# PLOTSSSSSS

#-Plot distribution of Instance Counts for Locations
ggplot(KNN_cor_Freq, aes(x=Freq, fill='Correct')) +
  geom_histogram(binwidth = 2) +
  geom_histogram(data=KNN_mis_Freq, aes(x=Freq, fill='Incorrect'),
                 alpha=0.4, binwidth=2) +
  scale_fill_manual(values=c('Correct'='green', 'Incorrect'='blue')) +
  labs(fill='Prediction') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  ggtitle('Distribution of Instances per Location') +
  xlab('Number of Instances per Location')

#### WAPs Detected ####
#-Plot distribution of WAP counts for correctly/incorrectly classified

KNN_cor1 <- testSet_1[which(testSet_1$Predict_B_tuneRF==testSet_1$BUILDINGID),] 
KNN_mis1 <- testSet_1[which(testSet_1$Predict_B_tuneRF !=testSet_1$BUILDINGID),] 
KNN_cor1$WAP_num <- apply(KNN_cor1[,1:367], 1, function(x) length(which(x !=100)))
KNN_mis1$WAP_num <- apply(KNN_mis1[,1:367], 1, function(x) length(which(x !=100)))

w <- ggplot(KNN_cor1, aes(WAP_num, fill='Correct')) + 
  geom_histogram(binwidth = 2) +
  geom_histogram(data=KNN_mis1, aes(WAP_num, fill='Incorrect'),
                 alpha=0.7, binwidth = 2) +
  scale_fill_manual(values=c('Correct'='green', 'Incorrect'='blue')) +
  labs(fill='Prediction') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  ggtitle('Distribution of WAPs detected per Location, KNN') +
  xlab('Number of WAPs per Location')

ggplotly(w)





# Total results

results <- resamples(list(kNN=knn_fit,
                          RF=rfFit1))
                         

