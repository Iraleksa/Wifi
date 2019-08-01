# 1.Combine train and validation sets   - > Combi_data
# 2.To replace all values higher, than - 30  to == - 30
# 3.To replace all values lower, than - 80 to == 100
# 4.Remove totally duplicated rows 
# 5.Remove  rows & columns for WAPS [,1:520] which contain ONLY 100 (no signal)
# 6.Check those lines which have wap_num less than 3, maybe we need to remove it.
# 7.To check WAPs, which are receiving signals from several buildings. How much of them after previous steps.
# 8.Devide dataset in to training and testing sets
# 9.Cross the fingers and check Gabi`s tuneRF
# 10. If it is ok, think about discretizig LONG and LAT, and repeat 9 with new groub_by


# wifi_data ->19 937 obs x 530 col
# valid_data ->1 111 obs x 530 col
# Combi_data -> 21 048 obs x 530 

# No_signal observations  -> 76 obs
# Duplicated  -> 772 obs
# Waps do not receive any signal though all observation  -> 55 columns

#### 1. Combine train and validation sets   - > Combi_data ####


wifi_data_orig$Origin <- "Training"
valid_data_orig$Origin <- "Validation"

Combi_data <- rbind(wifi_data_orig,valid_data_orig)

#  Rename Floor and building values
Combi_data$FLOOR <- recode(Combi_data$FLOOR, '0'=1, '1'=2, '2'=3, '3'=4, '4'=5)
Combi_data$BUILDINGID <- recode(Combi_data$BUILDINGID, '0'=1, '1'=2, '2'=3)

# Add ID column

Combi_data$ID <- paste(Combi_data$BUILDINGID,Combi_data$FLOOR,sep = "B_f")

#-Convert ID variable to categorical
Combi_data$ID <- as.factor(Combi_data$ID)

#### 2. To replace all values higher, than - 30  to == - 30  - > Combi_data_30 ####

Combi_data_30 <- Combi_data
set_replace <- Combi_data_30[,1:520] # we do replacing only in WAPS columns
set_replace[set_replace > -30 & set_replace !=100 ]<- -30
Combi_data_30 <- cbind(set_replace,Combi_data_30[,521:530])

# PLot all signals from all pfones


# Preàre melted dataset with only WAPS and phone ID
phones<-Combi_data_80_NA
a<-phones %>% select(WAP001:WAP520)
a[a==100]<-NA
phones<-cbind(a,phones$PHONEID)
colnames(phones)[521] <-"PHONEID"

require(reshape2)
# phones_melt <- melt(phones, id.vars = "ID",na.rm = TRUE)

phones_melt <- melt(phones, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt)[2] <-"WAPs"

freq_WAP <-phones_melt %>%
  group_by(WAPs, value) %>%
  summarise(n = n()) %>% arrange(desc(value))

head(freq_WAP, n=30)
# PLot Signal frequency per phone ID
ph<- ggplot(phones_melt, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency per phone ID for signals between (-30 & - 80) without no signals")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~PHONEID,scales = "free_x")

ph<- ph + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(ph)


#### 1. To replace all values lower, than - 80 to == 100  - > Combi_data_80 ####


Combi_data_80 <- Combi_data_30
set_replace <- Combi_data_30[,1:520] # we do replacing only in WAPS columns
set_replace[set_replace < -80 & set_replace !=100 ]<- -100
Combi_data_80 <- cbind(set_replace,Combi_data_30[,521:530])

#  Converting no signals (100 & -100) into NA
Combi_data_80_NA <- Combi_data_80
Combi_data_80_NA[Combi_data_80_NA==100]<-NA
Combi_data_80_NA[Combi_data_80_NA== -100]<-NA

#### 4.Remove totally duplicated rows    - > Combi_data_NoRow (772 obs)####

Combi_data_NoRow <- Combi_data_80_NA


dup<-duplicated(Combi_data_NoRow)
only_dup <-   Combi_data_NoRow[dup,]
no_dup <-   Combi_data_NoRow[!dup,]
Combi_data_NoRow <-no_dup

#### 5.Remove  rows & columns for WAPS [,1:520] which contain ONLY 100 (no signal)- > Combi_data_OnlySig ####

Combi_data_OnlySig <- Combi_data_NoRow

Combi_data_OnlySig[is.na(Combi_data_OnlySig)] <- 100

#   Identify which columns contain ONLY 100 -> 161 WAPS dont receive any signal

rest_cols<-c(names(Combi_data_OnlySig[,521:530])) # names of columns at the end of the table, which is NO WAP signals

Combi_data_OnlySig_if_100_col<-apply(Combi_data_OnlySig[,1:520], 2,function(x) any(x!=100)) #Here we check if column contains only 100 values. TRUE= column have at least one value !=100, FALSE  - all values is = 100  
Combi_data_OnlySig_no_100_col<-c(names(which(Combi_data_OnlySig_if_100_col == TRUE))) # Subset only WAPs , which do not contain only 100 -> have at least one signal
Combi_data_OnlySig_only_100_col<-c(names(which(Combi_data_OnlySig_if_100_col == FALSE))) # Sunbset only WAPs , which  contain only 100 -> have no signal
# ! WARNING: check previous value, IF NULL - NO ACTIONS is needed! - run it in console names(which(wifi_data_clean_if_100_col == TRUE))
Combi_data_OnlySig_final_cols<-c(Combi_data_OnlySig_no_100_col,rest_cols)# names of ALL columns, which is no WAP & WAP, which have at least one signal
Combi_data_OnlySig_sign <- Combi_data_OnlySig[,Combi_data_OnlySig_final_cols] # subset of only those columns with at least one signal 

#  Identify which raws in modified dataset contain only siugnal== 100 - 812 observations

Combi_data_OnlySig_sign$No_Signal <-apply(Combi_data_OnlySig_sign[,1:329],1,function(x) length(which( x==100)))
Combi_data_OnlySig_NoSign <- Combi_data_OnlySig_sign %>% filter(No_Signal ==329)
Combi_data_OnlySig_sign <- Combi_data_OnlySig_sign %>% filter(No_Signal !=329)  # removing NO Signal rows

rm(Combi_data_OnlySig_no_100_col,n,str,phones, phones_melt, a, Combi_data_80, Combi_data_30, ph, dup, Combi_data_OnlySig_NoSign,Combi_data_OnlySig_final_cols, Combi_data_OnlySig_only_100_col, Combi_data_OnlySig_if_100_col)


#### 6.Check those lines which have wap_num less than 3, maybe we need to remove it.  ####
#  1385 observations have less, thans 3 signals.
# I am not going to remove it yet.
Combi_data_clean <- Combi_data_OnlySig_sign
Combi_data_clean$WAP_num <- apply(Combi_data_clean[,1:359], 1, function(x) length(which(x !=100)))
Less_than_3_signals_inRow <- Combi_data_clean %>% filter(WAP_num < 3)

Combi_data_clean_0 <- Combi_data_clean %>% filter(WAP_num == 0)

Combi_data_clean$BUILDINGID <- as.factor(Combi_data_clean$BUILDINGID)
Combi_data_clean$FLOOR <- as.factor(Combi_data_clean$FLOOR)

write.csv(Combi_data_clean,file = "Combi_data_clean Version 5.csv", row.names = FALSE)


# Ading discretised columns

Combi_data_clean<-Combi_data_clean %>%
  mutate(
    Long_disc = case_when(
      
      round(LONGITUDE,digits=0) %in%  -7696:-7652  ~ "Lo10",
      round(LONGITUDE,digits=0) %in%  -7652:-7613  ~ "Lo9",
      round(LONGITUDE,digits=0) %in%  -7613:-7574  ~ "Lo8",
      round(LONGITUDE,digits=0) %in%  -7574:-7535  ~ "Lo7",
      round(LONGITUDE,digits=0) %in%  -7535:-7496  ~ "Lo6",
      round(LONGITUDE,digits=0) %in%  -7496:-7457  ~ "Lo5",
      round(LONGITUDE,digits=0) %in%  -7457:-7418  ~ "Lo4",
      round(LONGITUDE,digits=0) %in%  -7418:-7379  ~ "Lo3",
      round(LONGITUDE,digits=0) %in%  -7379:-7340  ~ "Lo2",
      round(LONGITUDE,digits=0) %in%  -7340:-7298  ~ "Lo1"
      
      ))


Combi_data_clean<-Combi_data_clean %>%
  mutate(
    Lat_disc = case_when(
      
      round(LATITUDE,digits=0) %in%  4864989:4865019  ~ "Lo10",
      round(LATITUDE,digits=0) %in%  4864962:4864989  ~ "Lo9",
      round(LATITUDE,digits=0) %in%  4864935:4864962  ~ "Lo8",
      round(LATITUDE,digits=0) %in%  4864908:4864935  ~ "Lo7",
      round(LATITUDE,digits=0) %in%  4864881:4864908  ~ "Lo6",
      round(LATITUDE,digits=0) %in%  4864854:4864881  ~ "Lo5",
      round(LATITUDE,digits=0) %in%  4864827:4864854  ~ "Lo4",
      round(LATITUDE,digits=0) %in%  4864800:4864827  ~ "Lo3",
      round(LATITUDE,digits=0) %in%  4864773:4864800  ~ "Lo2",
      round(LATITUDE,digits=0) %in%  4864746:4864773  ~ "Lo1"
      
    ))

Combi_data_clean$Long_disc<- as.factor(Combi_data_clean$Long_disc)
Combi_data_clean$Lat_disc<- as.factor(Combi_data_clean$Lat_disc)

Combi_data_clean <-Combi_data_clean[,c(1:359,363,362,360:361,372:373,364:371)]

#### 7.To check WAPs, which are receiving signals from several buildings.####
# How much of them after previous steps.

Combi_data_clean_dup <- Combi_data_clean %>% filter(WAPs %in% names_dup) #All duplicated WAPS
length(unique(All_buildings_dup$WAPs))

Combi_data_clean_dup <- Combi_data_clean[,c(names_dup,360,361,364,365)]
Combi_data_clean_dup$WAP_num <- apply(Combi_data_clean_dup[,1:142], 1, function(x) length(which(x !=100)))
Combi_data_clean_dup[Combi_data_clean_dup==100]<-NA
Combi_data_clean_dup_0 <- Combi_data_clean_dup %>% filter(WAP_num !=0)

WAP341<-Combi_data_clean_dup_0 %>% select("WAP341",
                           "LONGITUDE",
                           "LATITUDE",
                           "FLOOR",
                           "BUILDINGID"
                          ) %>% filter(WAP341!=100)


Combi_data_clean_Nodup <- Combi_data_clean[,-c(names_dup)]
