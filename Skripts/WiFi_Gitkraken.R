pacman::p_load(readr,autoplotly,ggplot2,plotly,tidyverse,party,lubridate, caret,dplyr)

#### 1. Loading Data ####
wifi_data <- read_csv("D:/Ubiqum/Module 3/Task 3.3_WiFi/Data/Raw/trainingData.csv")
valid_data <- read_csv("D:/Ubiqum/Module 3/Task 3.3_WiFi/Data/Raw/validationData.csv")

# Test Gitkraken
dim(wifi_data) # The dim() function tells us the dimensions of the data 
dim(valid_data)
#### 2.INSPECT DATA ####

glimpse(wifi_data[,510:532]) #View last 20 features of data set
sum(is.na(wifi_data)) #   - 0 NA`s



#### 3. PROCESS THE DATA ####

#-Convert features to categorical
wifi_data$BUILDINGID <- factor(wifi_data$BUILDINGID)
wifi_data$SPACEID <- factor(wifi_data$SPACEID)
wifi_data$RELATIVEPOSITION <- factor(wifi_data$RELATIVEPOSITION)
wifi_data$FLOOR <- factor(wifi_data$FLOOR)

#-Recode floor factor level names
wifi_data$FLOOR <- recode(wifi_data$FLOOR, '0'=1, '1'=2, '2'=3, '3'=4, '4'=5)
wifi_data$BUILDINGID <- recode(wifi_data$BUILDINGID, '0'=1, '1'=2, '2'=3)

#-Add count of WAP's detected as a feature
# B1_F3<-wifi_data %>% filter(BUILDINGID==1,FLOOR==3)
wifi_data$WAP_num <- apply(wifi_data[,1:520], 1, function(x) length(which(x !=100)))

# Next, we’ll filter the data set such that it only contains locations outside of spaces (i.e. corridor).
wifi_outside <- filter(wifi_data,wifi_data$RELATIVEPOSITION==2)

#-Consolidate position identifiers to create location ID feature
wifi_data$ID <- paste(wifi_data$BUILDINGID,wifi_data$FLOOR,sep = "B_f")

#-Convert ID variable to categorical
wifi_data$ID <- factor(wifi_data$ID)

#-Count of ID classes for classification
nlevels(wifi_data$ID)
unique(wifi_data$ID)


h<- histogram(Combi_data_clean$LONGITUDE)
histogram(Combi_data_clean$LATITUDE)
histogram(factor(Combi_data_clean$Lat_disc))
histogram(factor(Combi_data_clean$Long_disc))
plot_ly(h)
# Converting TIMESTAMP into POSIXct
#dataset$DateTime <- as.POSIXct(strptime(dataset$DateTime,"%d/%m/%Y %H:%M:%S"))
wifi_data$DateTime <- as.POSIXct(wifi_data$TIMESTAMP,tz = "UTC",origin = "1970/01/01 00:00:00", "%Y/%m/%d %H:%M:%S")
# Creating separate datasets for different buildings

Build_1<-wifi_data %>% filter(BUILDINGID==1)
Build_2<-wifi_data %>% filter(BUILDINGID==2)
Build_3<-wifi_data %>% filter(BUILDINGID==3)

# To check means for each column
colMeans(Build_1[,1:520])
summary(colMeans(Build_1[,1:520]))

rowMeans(new_Build2[,1:200])
rowMeans(Build_2[,847])

#### Creating datasets for different buildings, ONLY with signal ####

rest_cols<-c(names(wifi_data[,521:532])) # names of columns at the end of the table, which is NO WAP signals

# BUILDING 1
#  Identify which columns contain only 100
Build_1_if_100_col<-apply(Build_1[,1:520], 2,function(x) any(x!=100)) #Here we check if column contains only 100 values. TRUE= column have at least one value !=100, FALSE  - all values is = 100  
Build_1_no_100_col<-c(names(which(Build_1_if_100_col == TRUE))) # Sunbset ony WAPs , which do not contain only 100 -> have at least one signal

# ! WARNING: check previous value, IF NULL - NO ACTIONS is needed! which(Build_1_if_100_col == TRUE)

B1_final_cols<-c(Build_1_no_100_col,rest_cols)# names of ALL columns, which is no WAP & WAP, which have at least one signal

Build1_sign <- Build_1[,B1_final_cols] # subset of only those columns with at least one signal 
waps_1<-ncol(Build1_sign)-12 # How much WAPs were detected in this building
waps_1

#  Identify which raws in modified dataset contain only 100
Build_1_if_100_row<-apply(Build1_sign[,1:waps_1], 1,function(x) any(x!=100))  # Here we check if rows contains only 100 values.
Build_1_no_100_rowc<-c(names(which(Build_1_if_100_row == TRUE))) # Sunbset ony WAPs , which do not contain only 100. Check result. IF NULL, no modification is needed
Build_1_no_100_rowc

# Clean created temporal variables

# BUILDING 2
#  Identify which columns contain only 100
Build_2_if_100_col<-apply(Build_2[,1:520], 2,function(x) any(x!=100)) #Here we check if column contains only 100 values. TRUE= column have at least one value !=100, FALSE  - all values is = 100  
Build_2_no_100_col<-c(names(which(Build_2_if_100_col == TRUE))) # Sunbset ony WAPs , which do not contain only 100 -> have at least one signal

final_cols<-c(Build_2_no_100_col,rest_cols)# names of ALL columns, which is no WAP & WAP, which have at least one signal

Build2_sign <- Build_2[,final_cols] # subset of only those columns with at least one signal 
waps_2<-ncol(Build2_sign)-12 # How much WAPs were detected in this building
waps_2

#  Identify which raws in modified dataset contain only 100
Build_2_if_100_row<-apply(Build2_sign[,1:waps_2], 1,function(x) any(x!=100))  # Here we check if rows contains only 100 values.
Build_2_no_100_rowc<-c(names(which(Build_2_if_100_row == TRUE))) # Sunbset ony WAPs , which do not contain only 100. Check result. IF NULL, no modification is needed
Build_2_no_100_rowc

# BUILDING 3
#  Identify which columns contain only 100
Build_3_if_100_col<-apply(Build_3[,1:520], 2,function(x) any(x!=100)) #Here we check if column contains only 100 values. TRUE= column have at least one value !=100, FALSE  - all values is = 100  
Build_3_no_100_col<-c(names(which(Build_3_if_100_col == TRUE))) # Sunbset ony WAPs , which do not contain only 100 -> have at least one signal

# ! WARNING: check previous value, IF NULL - NO ACTIONS is needed!

B3_final_cols<-c(Build_3_no_100_col,rest_cols)# names of ALL columns, which is no WAP & WAP, which have at least one signal

Build3_sign <- Build_3[,B3_final_cols] # subset of only those columns with at least one signal 
waps_3<-ncol(Build3_sign)-12 # How much WAPs were detected in this building
waps_3

#  Identify which raws in modified dataset contain only 100
Build_3_if_100_row<-apply(Build3_sign[,1:waps_3], 1,function(x) any(x!=100))  # Here we check if rows contains only 100 values.
Build_3_no_100_rowc<-c(names(which(Build_3_if_100_row == TRUE))) # Sunbset ony WAPs , which do not contain only 100. Check result. IF NULL, no modification is needed
Build_3_no_100_rowc


# NOw we have our new datasets:
# Build1_sign - 200 WAPs
# Build2_sign - 207 WAPs
# Build3_sign - 203 WAPs

# Build1_valid_sign - 183 WAPs
# Build2_valid_sign - 170 WAPs
# Build3_valid_sign - 125 WAPs


# Clean created temporal variables
rm("B1_final_cols","B3_final_cols","Build_1_if_100_col",Build_1_if_100_row,Build_1_no_100_col,Build_1_no_100_rowc,Build_2_if_100_col,Build_2_if_100_row,Build_2_no_100,Build_2_no_100,Build_2_no_100_col,Build_2_no_100_rowc,Build_3_if_100_col,Build_3_if_100_row,Build_3_no_100_col,Build_3_no_100_rowc)


#### ******* THIS IS did not WORK, BUT I would like to find solution, some day... ******* ####

# colMeans(Build_2[,1:520])
# test<-Build_2%>% filter_at(vars(contains("WAP")), all_vars(.!=100))# does not work
# 
# test<-Build_2 %>% filter_at(vars(Build_2[,1:520]), all_vars(.!=100))# how to specify which columns I want to put to vars?
# wifi_data[,1:520] %>% filter_all(any_vars(.== 100))
# which(wifi_data[,1:520] !=100)


#### *******************************************************************************************####
# Exclude columns with all 100

#### 4. Explore the Data ####

#-Create data frame of instance counts per location 
ID_freq <- as.data.frame(table(wifi_data$ID))

#-Plot histogram of instance counts at locations
loc<- ggplot(ID_freq, aes(x = Freq)) +
  geom_histogram(fill='green', binwidth = 2, color='black')+
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency Count of Location ID Instances') +
  xlab('Number of Instances for a Location ID') +
  ylab('Frequency of Observed Instance Count') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))
ggplotly(loc)

# Visualize how the number of WAPs detected varies across the 3 buildings and across floors

#-Distribution of WAP count by building- boxplot

dist<-ggplot(wifi_data, aes(x=BUILDINGID, y=WAP_num)) + 
  geom_boxplot(fill='lightblue') +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Detected Wireless Access Points by Building') +
  labs(x="Building Number", y= 'WAP Counts' ) +
  theme(panel.border=element_rect(colour='black', fill=NA))
ggplotly(dist)

#Distribution of WAP count by building and floor
h<-ggplot(wifi_data, aes(x=WAP_num, fill=FLOOR)) + geom_bar() +
  facet_grid(BUILDINGID~.) +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Detected Wireless Access Points by Building') +
  labs(x="Number of WAP's Detected by Building", y= 'Counts by Building Floor') +
  theme(panel.border=element_rect(colour='black', fill=NA))
ggplotly(h)

#3D image of reference point locations in data set
require(scatterplot3d)
scatterplot3d(wifi_data$LONGITUDE, wifi_data$LATITUDE, wifi_data$FLOOR,wifi_data$USERID,
              type='p',
              highlight.3d = FALSE,
              color=wifi_data$USERID,
              angle=155,
              pch=16,
              box=FALSE,
              main = "Location Reference Points Across Three Buildings of UJIIndoorLoc Data Set",
              cex.lab = 1,
              cex.main=1,
              cex.sub=1,
              col.sub='blue',
              xlab='Longitude', ylab='Latitude',zlab = 'Building Floor') 

# 3D plot for signal's distribution by User ID
wifi_data_clean_check <- wifi_data_clean
wifi_data_clean_check<- as.tibble(wifi_data_clean_check)


wifi_data_clean_check[wifi_data_clean_check==100]<-NA


write.csv(Clean_data_subset_NA,file = "Clean_data_subset_NA.csv", row.names = FALSE)

sign_distr_train <- plot_ly(Clean_data_subset_NA, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
  add_markers(color = ~as.factor(USERID),colors = "Set1") %>%
  layout(title = "Signals distribution_clean data",scene = list(xaxis = list(title = 'LONGITUDE'),
                      yaxis = list(title = 'LATITUDE'),
                      zaxis = list(title = 'Floor' )))

# 3D plot for signal's distribution by Phone ID
# wifi_data[is.na(wifi_data)] <- 100
wifi_data_NA <- wifi_data
wifi_data_NA[wifi_data_NA==100]<-NA

plot_ly(wifi_data_NA, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 5)) %>%
  add_markers(color = ~as.factor(USERID),colors = "Set1") %>%
  layout(title = "Signals distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                yaxis = list(title = 'LATITUDE'),
                                                                zaxis = list(title = 'Floor' )))


# autosize = F, width = 1500, height = 1000

# 3D plot for signal's distribution for Building 1
B1<-wifi_data %>% filter(BUILDINGID==1)

plot_ly(B1, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 7)) %>%
  add_markers(color = ~USERID) %>%
  layout(scene = list(xaxis = list(title = 'LONGITUDE'),
                      yaxis = list(title = 'LATITUDE'),
                      zaxis = list(title = 'Floor' )))


# 3D plot for signal's distribution for Building 2
B2<-wifi_data %>% filter(BUILDINGID==2)

plot_ly(B2, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 7)) %>%
  add_markers(color = ~USERID) %>%
  layout(scene = list(xaxis = list(title = 'LONGITUDE'),
                      yaxis = list(title = 'LATITUDE'),
                      zaxis = list(title = 'Floor' )))

# 3D plot for signal's distribution for Building 2 No 100
B2<-Build2_sign 

plot_ly(B2, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 7)) %>%
  add_markers(color = ~USERID) %>%
  layout(scene = list(xaxis = list(title = 'LONGITUDE'),
                      yaxis = list(title = 'LATITUDE'),
                      zaxis = list(title = 'Floor' )))

# 3D plot for signal's distribution for Building 0
B1<-wifi_data %>% filter(BUILDINGID==1)

plot_ly(B1, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 7)) %>%
  add_markers(color = ~USERID) %>%
  layout(scene = list(xaxis = list(title = 'LONGITUDE'),
                      yaxis = list(title = 'LATITUDE'),
                      zaxis = list(title = 'Floor' )))



# Histogram of "Quantity of WAPs per building_floor

wap_q<-ggplot(wifi_data, aes(x=ID)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Qty of WAPs") + ylab("Building_floor")+ggtitle("Quantity of WAPs per building_floor")+
theme(plot.title = element_text(hjust = 0.5))

ggplotly(wap_q)

# Frequency of signals for different phoneID´s
# Preàre melted dataset with only WAPS and phone ID
phones<-wifi_data
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
# PLot Signal frequency per  ID

write.csv(phones_melt,file = "Signal frequency per phone ID.csv", row.names = FALSE)
phones_melt
ph<- ggplot(phones_melt, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency per phone ID")+
  theme(plot.title = element_text(hjust = 0.5))+
facet_wrap(~PHONEID,scales = "free_x")

ph<- ph + theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=8,angle=45),
          axis.text.y = element_text(face="plain", color="darkgrey", 
                                     size=10, angle=0))
 
ggplotly(ph)

# Preàre melted dataset with only WAPS and ID
phones<-wifi_data
a<-phones %>% select(WAP001:WAP520)
a[a==100]<-NA
phones<-cbind(a,phones$ID)
colnames(phones)[521] <-"ID"

require(reshape2)
phones_melt <- melt(phones, id.vars = "ID",na.rm = TRUE)

# phones_melt <- melt(phones, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt)[2] <-"WAPs"

freq_WAP <-phones_melt %>%
  group_by(WAPs, value) %>%
  summarise(n = n()) %>% arrange(desc(value))

head(freq_WAP, n=30)
# PLot Signal frequency per phone ID

write.csv(phones_melt,file = "Signal frequency per Build_Floor ID.csv", row.names = FALSE)
phones_melt
ph<- ggplot(phones_melt, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency per Building_Floor")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ID,scales = "free_x")

ph<- ph + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                     size=10, angle=0))
 
ggplotly(ph)

# PLot Signal frequency per phone ID 17
phones_melt_17 <- phones_melt   %>% filter(PHONEID == 17, value >=-74 , value <=-73)

ph_17<- ggplot(phones_melt_17, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency per phone ID 17")+
  theme(plot.title = element_text(hjust = 0.5))

ph_17<- ph_17 + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(ph_17)

# PLot Signal frequency per phone ID 11

phones_melt_11 <- phones_melt   %>% filter(PHONEID == 11)

phones_melt_11 %>%
  group_by(WAPs, value) %>%
  summarise(n = n()) %>% arrange(desc(n))

ph_11<- ggplot(phones_melt_11, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency per phone ID 11")+
  theme(plot.title = element_text(hjust = 0.5))

ph_11<- ph_11 + theme(axis.text.x = element_text(face="plain", color="black", 
                                                 size=8,angle=45),
                      axis.text.y = element_text(face="plain", color="darkgrey", 
                                                 size=10, angle=0))

ggplotly(ph_11)

#Signal frequency for all phones

all_ph<-ggplot(phones_melt, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Phones") + ylab("Frequency")+ggtitle("Signal frequency for all phones")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph)


#Signal frequency for all phones only Amazing signals

Amaz<-wifi_data %>% filter(Amazing !=0)

phones_amaz<-Amaz
am<-phones_amaz %>% select(WAP001:WAP520)
am[am==100]<-NA
am[am< -30]<-NA
phones_amaz<-cbind(am,phones_amaz$PHONEID)
colnames(phones_amaz)[521] <-"PHONEID"

require(reshape2)

phones_melt_am <- melt(phones_amaz, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt_am)[2] <-"WAPs"

all_ph_amaz<-ggplot(phones_melt_am, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Signal intencity") + ylab("Frequency")+ggtitle("Signal frequency for all phones only Amazing signals (more, than -30 dBm)")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph_amaz)








