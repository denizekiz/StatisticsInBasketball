data.acc <- read.table(header = TRUE,"../26_1_2017/IMU.txt")
data.hit <- read.table(header = TRUE,"../Codes/video_1_ann_txt")
data.hit$Begin
# Video sync value
val <- 1488128623000 - 10800000 #sync
begin <- 0
end <- 2000
#Create sync time series
data_sync.acc <- data.acc
data_sync.acc$TimeStamp <- data.acc$TimeStamp - val
#Plot the data
plot((data.acc[(data.acc$TimeStamp-val)/1000 <end & (data.acc$TimeStamp-val)/1000 > begin,]$TimeStamp - val)/1000,data.acc[(data.acc$TimeStamp-val)/1000 <end & (data.acc$TimeStamp-val)/1000 >begin ,]$mag,type = "l", xlab="Seconds", ylab="Acceleration magnitude", main = "Acceleration Magnitude",xlim=c(begin,end))

plot((data.acc[(data.acc$TimeStamp-val)/1000 < as.numeric(seconds(ms(data.hit$End))) & (data.acc$TimeStamp-val)/1000 > as.numeric(seconds(ms(data.hit$Begin))),]$TimeStamp - val)/1000,data.acc[(data.acc$TimeStamp-val)/1000 <as.numeric(seconds(ms(data.hit$End))) & (data.acc$TimeStamp-val)/1000 >as.numeric(seconds(ms(data.hit$Begin))) ,]$mag,type = "l", xlab="Seconds", ylab="Acceleration magnitude", main = "Acceleration Magnitude",xlim=c(begin,end))

segment.manually <- function(time1, time2, xplot = timestamp_in_seconds, yplot) {
  return (yplot[xplot > time1 & xplot < time2,])
}
segments <- NULL



for(i in 1:length(data.hit$Begin))
{
  
  segments[[i]] <- segment.manually(time1 = as.numeric(seconds(ms(data.hit$Begin[i]))),time2=as.numeric(seconds(ms(data.hit$End[i]))),yplot = data_sync.acc,xplot = (data_sync.acc$TimeStamp)/1000)
  segments[[i]]$hit <- data.hit$Hit[i]
}
segments_matrix <- NULL
for(i in 1:length(data.hit$Begin))
{
  
  segments_matrix<-rbind(segments_matrix,segments[[i]])
}
plot(segments_matrix[segments_matrix$hit,]$TimeStamp/1000,segments_matrix[segments_matrix$hit,]$mag,type="l")


my.filter <- 22
hits <- segments_matrix[!segments_matrix$hit& segments_matrix$mag>my.filter,]$mag
no_hits <- segments_matrix[!segments_matrix$hit& segments_matrix$mag>my.filter,]$mag
all <- segments_matrix[segments_matrix$mag>my.filter,]$mag
plot(ecdf(hits))
qqnorm(hits)
hist(hits,breaks =5,probability = T)
curve(dnorm(x, mean=mean(hits), sd=sd(hits)), add=TRUE)

plot(ecdf(no_hits))
qqnorm(no_hits)
hist(no_hits,breaks =5,probability = T)
curve(dnorm(x, mean=mean(no_hits), sd=sd(no_hits)), add=TRUE)
boxplot(no_hits)
boxplot(hits)
boxplot(all)

plot(ecdf(all))
qqnorm(all)
hist(all,breaks =5,probability = T)
curve(dnorm(x, mean=mean(all), sd=sd(all)), add=TRUE)




