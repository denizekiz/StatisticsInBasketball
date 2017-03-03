data.acc <- read.table(header = TRUE,"../26_1_2017/IMU.txt")
# Video sync value
val <- 1488128623000 - 10800000 #sync
begin <- 0
end <- 10000
#Create sync time series
data_sync.acc <- data.acc
data_sync.acc$TimeStamp <- data.acc$TimeStamp - val
#Plot the data
plot((data.acc[(data.acc$TimeStamp-val)/1000 <end & (data.acc$TimeStamp-val)/1000 > begin,]$TimeStamp - val)/1000,data.acc[(data.acc$TimeStamp-val)/1000 <end & (data.acc$TimeStamp-val)/1000 >begin ,]$mag,type = "l", xlab="Seconds", ylab="Acceleration magnitude", main = "Acceleration Magnitude",xlim=c(begin,end))
