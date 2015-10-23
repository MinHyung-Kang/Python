
#Load the data
mini_data <- read.csv(file="mini_contest.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#Change the names to understandable names
names <- colnames(mini_data)
names <- gsub("tempm","Temperature(celsius)",names)
names <- gsub("hum","Humidity(percent)",names)
names <- gsub("wspdm","WindSpeed(kph)",names)
names <- gsub("wdird","WindDirection(degree)",names)
names <- gsub("wdire","WindDirection(nwse)",names)
names <- gsub("pressurem","Pressure(mBar)",names)
names <- gsub("vism","Visibility(Km)",names)
names <- gsub("windchillm","WindChill(celsius)",names)
names <- gsub("tempm","Temperature(cel)",names)
names <- gsub("fog","Fog(binary)",names)
names <- gsub("rain","Rain(binary)",names)
names <- gsub("snow","Snow(binary)",names)
names <- gsub("hail","Hail(binary)",names)
names <- gsub("thunder","Thunder(binary)",names)
names <- gsub("target","Day_0",names)
colnames(mini_data) = names
str(mini_data)


#Collect the dataset for past days
n = length(mini_data$Day_0)
l = length(colnames(mini_data))
mini_data2 = mini_data[8:n,0:(l-1)]


#Combine the past dates
for(i in -7:0){ 
   temp = mini_data$Day_0[(8+i):(n+i)]
   mini_data2 = cbind(mini_data2,temp)
   colnames(mini_data2)[l+(7+i)]<-sprintf("Day_%d",i)
}


#Get the weekday feature
Sys.setlocale("LC_TIME", "English") 
dates <- as.Date(mini_data2$time,"%Y-%m-%d")
weekday <- as.numeric(format(dates, format="%w"))
dayofyear <- as.numeric(format(dates,format="%j"))
month <- as.numeric(format(dates,format="%m"))
mini_data2 = cbind(weekday,dayofyear,month,mini_data2[,-1],stringsAsFactors=FALSE)

#Get the header
mini_header = colnames(mini_data2)

#Get datatypes
data_type = sapply(mini_data2, class) 


#Initial Output files
# write.table(mini_header,file="mini_clean_header.csv", row.names=FALSE,col.names=FALSE,sep=",")
# write.table(mini_data2,file="mini_clean.csv", row.names=FALSE,col.names=FALSE,sep=",")
# write.table(data_type,file="mini_clean_type.csv", row.names=FALSE,col.names=FALSE,sep=",")


#Filter out data so that we do not look at winddirection or pressure look at filtered data
keyword= !(grepl('WindDirection',mini_header, ignore.case=TRUE) 
          | grepl('Pressure',mini_header,ignore.case=TRUE))
small_header = mini_header[keyword]
small_data = mini_data2[,keyword]
small_type = data_type[keyword]

# write.table(small_header,file="small_header.csv", row.names=FALSE,col.names=FALSE,sep=",")
# write.table(small_data,file="small.csv", row.names=FALSE,col.names=FALSE,sep=",")
# write.table(small_type,file="small_type.csv", row.names=FALSE,col.names=FALSE,sep=",")

#Average all the values
value_list <- c("Temperature", "Humidity", "WindSpeed", 
                "Visibility", "WindChill", "Fog",
                "Rain","Snow","Hail","Thunder") 

keyword_Temp <- lapply(value_list,function(x){
   grepl(as.name(x),small_header, ignore.case=TRUE)
}
)

m=length(small_data$weekday)
mean_data = seq(1,m)

#For each given keyword, average the value
for(i in 1:length(value_list)){
   data_temp <- small_data[,keyword_Temp[[i]]]   
   mean_temp <- rowMeans(data_temp)
   mean_data <- cbind(mean_data,mean_temp)
}

mean_data = data.frame(mean_data[,-1])
colnames(mean_data)=value_list

smaller_data = cbind(small_data[,1:5],mean_data,small_data[,56:63])
smaller_header = colnames(smaller_data)
smaller_type = sapply(smaller_data, class) 

# Write the datafile
write.table(smaller_header,file="smaller_header.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(smaller_data,file="smaller_data.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(smaller_type,file="smaller_type.csv", row.names=FALSE,col.names=FALSE,sep=",")
