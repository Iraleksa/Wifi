# x>=-30 - Amazing
# x>=-67 & x<-30 - Very Good
# x>=-75 & x<-67 - Okay	
#  x<-75 - Not Good	

apply(wifi_data[,1:520],1,function(x) length(which(x<-75 & x!=100)))
apply(wifi_data[1,1:520],1,function(x) length(which(x<-75 & x!=100)))

apply(wifi_data[,1:520],1,function(x) length(which(x>=-30 & x!=100)))


wifi_data<-wifi_data %>%
  dplyr::mutate(Amazing = apply(wifi_data[,1:520],1,function(x) length(which(x>=-30 & x!=100))))
wifi_data$Amazing<-as.numeric(wifi_data$Amazing)

#  This is not finished

wifi_data$Amazing<-apply(wifi_data[,1:520],1,function(x) length(which(x>=-30 & x!=100)))
wifi_data$Very_Good <-apply(wifi_data[,1:520],1,function(x) length(which(x>=-67 & x < -30 & x!=100)))
wifi_data$Okay <-apply(wifi_data[,1:520],1,function(x) length(which(x>= -75 & x< -67 & x!=100)))
wifi_data$Not_Good <-apply(wifi_data[,1:520],1,function(x) length(which(x< -75 & x!=100)))

wifi_data$No_Signal <-apply(wifi_data[,1:520],1,function(x) length(which( x==100)))


# 3D plot for signal's distribution by Quality of signal
####  Amazing signals ####

Amaz<-wifi_data %>% filter(Amazing !=0)

l <- list( 
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

write.csv(Amaz,file = "Amazing by user ID.csv", row.names = FALSE)

plot_ly(Amaz, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(USERID),colors = "Set1") %>%
  layout(title = "Amazing signals (more, than -30 dBm) distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                yaxis = list(title = 'LATITUDE'),
                                                                zaxis = list(title = 'Floor' ))) %>%   layout(legend = l)


####  Very Good signals ####

write.csv(Very_G,file = "Very Good and OKay signals.csv", row.names = FALSE)

Very_G<-wifi_data %>% filter(Very_Good !=0 & Okay !=0)

l <- list(
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

plot_ly(Very_G, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(Very_Good),colors = "Set1") %>%
  layout(title = "Very Good and OKay signals (between -75 & - 30 dBm) distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                        yaxis = list(title = 'LATITUDE'),
                                                                        zaxis = list(title = 'Floor' ))) %>% 
                                                                        layout(legend = l)



#### Okay signals ####

Ok<-wifi_data %>% filter(Okay !=0)

l <- list(
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

plot_ly(Ok, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(Okay),colors = "Set1") %>%
  layout(title = "Okay signals (between -75 & -67 dBm) distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                        yaxis = list(title = 'LATITUDE'),
                                                                        zaxis = list(title = 'Floor' ))) %>% 
  layout(legend = l)

#### Not_Good signals ####

N_Good<-wifi_data %>% filter(Not_Good !=0)

l <- list(
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

plot_ly(N_Good, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(Not_Good),colors = "Set1") %>%
  layout(title = "Okay signals (less than -75 dBm) distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                                             yaxis = list(title = 'LATITUDE'),
                                                                                             zaxis = list(title = 'Floor' ))) %>% 
  layout(legend = l)


# No signals

No_Sig<-wifi_data %>% filter(No_Signal ==520)

l <- list(
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

plot_ly(No_Sig, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(No_Signal),colors = "Set1") %>%
  layout(title = "No signals  distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                                         yaxis = list(title = 'LATITUDE'),
                                                                                         zaxis = list(title = 'Floor' ))) %>% 
  layout(legend = l)


#Signal frequency for all phones only Amazing signals
require(reshape2)

# Preparing dataset

Amaz<-wifi_data %>% filter(Amazing !=0)

phones_amaz<-Amaz
am<-phones_amaz %>% select(WAP001:WAP520)
am[am==100]<-NA
am[am< -30]<-NA
phones_amaz<-cbind(am,phones_amaz$PHONEID)
colnames(phones_amaz)[521] <-"PHONEID"
phones_melt_am <- melt(phones_amaz, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt_am)[2] <-"WAPs"

all_ph_amaz<-ggplot(phones_melt_am, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Signal intencity") + ylab("Frequency")+ggtitle("Signal frequency for all phones only Amazing signals (more, than -30 dBm)")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph_amaz)

write.csv(phones_melt_am,file = "Amazing signals his.csv", row.names = FALSE)

#Signal frequency for all phones only Very Good signals
require(reshape2)

# Preparing dataset

Very_G<-wifi_data %>% filter(Very_Good !=0)

phones_vg<-Very_G
avg<-phones_vg %>% select(WAP001:WAP520)
avg[avg==100]<-NA
avg[avg < -67]<-NA
avg[avg >=- 30]<-NA

phones_vg<-cbind(avg,phones_vg$PHONEID)
colnames(phones_vg)[521] <-"PHONEID"
phones_melt_avg <- melt(phones_vg, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt_avg)[2] <-"WAPs"

all_ph_vg<-ggplot(phones_melt_avg, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Signal intencity") + ylab("Frequency")+ggtitle("Signal frequency for all phones only Very Good signals(between -67 & - 30 dBm)")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph_vg)


#Signal frequency for all phones only Okay signals

# Preparing dataset

Ok<-wifi_data %>% filter(Okay !=0)

phones_ok<-Ok
aok<-phones_ok %>% select(WAP001:WAP520)
aok[aok==100]<-NA
aok[aok < -75]<-NA
aok[aok >= -67]<-NA

phones_ok<-cbind(aok,phones_ok$PHONEID)
colnames(phones_ok)[521] <-"PHONEID"
phones_melt_aok <- melt(phones_ok, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt_aok)[2] <-"WAPs"

all_ph_ok<-ggplot(phones_melt_aok, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Signal intencity") + ylab("Frequency")+ggtitle("Signal frequency for all phones only Okay signals (between -75 & -67 dBm)")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph_ok)


#Signal frequency for all phones only Not Good signals

# Preparing dataset
N_Good<-wifi_data %>% filter(Not_Good !=0)


phones_ng<-N_Good
ang<-phones_ng %>% select(WAP001:WAP520)
ang[ang==100]<-NA
ang[ang >= -75]<-NA

phones_ng<-cbind(ang,phones_ng$PHONEID)
colnames(phones_ng)[521] <-"PHONEID"
phones_melt_ang <- melt(phones_ng, na.rm = TRUE, id.vars = "PHONEID")
colnames(phones_melt_ang)[2] <-"WAPs"

all_ph_ng<-ggplot(phones_melt_ang, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Signal intencity") + ylab("Frequency")+ggtitle("Signal frequency for all phones only Not Good signals (less than -75 dBm) ")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(all_ph_ng)


Too_good<-wifi_data$Too_good<-apply(wifi_data[,1:520],1,function(x) length(which(x >= -3 & x!=100)))
Too_good_df<- wifi_data %>% filter(Too_good  >= -3)
Too_good_susp<- wifi_data %>% filter(Too_good  > 1)



TG<-wifi_data %>% filter(Too_good  >= -30 )

l <- list(
  font = list(
    family = "sans-serif",
    size = 30,
    color = "#000"))

plot_ly(TG, x = ~LONGITUDE, y = ~LATITUDE, z = ~unclass(FLOOR),marker = list(size = 6)) %>%
  add_markers(color = ~as.factor(Too_good),colors = "Set1") %>%
  layout(title = "Too good signals (more, than -3 dBm) distribution_train data",scene = list(xaxis = list(title = 'LONGITUDE'),
                                                                                             yaxis = list(title = 'LATITUDE'),
                                                                                             zaxis = list(title = 'Floor' ))) %>% 
  layout(legend = l)


