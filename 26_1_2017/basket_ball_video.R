### create jpegs
create.jpegs <- function(data.in, fps=100, time.window=15)
{
  min.ts <- min(data.in$TimeStamp)
  max.ts <- max(data.in$TimeStamp)
  print(paste("min / max timestamp:", min.ts, "/", max.ts))
  
  jpeg(file = "myplot%d.jpeg")
  
  for(j in 0:45)
  {
    time.start <- min.ts + j*time.window
    ylim.min <- min(data.in[data.in$TimeStamp>=time.start & data.in$TimeStamp<=(time.start+time.window),2])
    ylim.max <- max(data.in[data.in$TimeStamp>=time.start & data.in$TimeStamp<=(time.start+time.window),2])
    
    for(i in 1:(time.window*fps))
    {
      time.end <- time.start + i/fps
      matplot(data.in[data.in$TimeStamp>=time.start & data.in$TimeStamp<=time.end,1],
              data.in[data.in$TimeStamp>=time.start & data.in$TimeStamp<=time.end,2],
              type="l",
              xlim=c(time.start, time.start+time.window),
              ylim=c(ylim.min, ylim.max),
              xlab="Seconds", ylab="HRM_RAW",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5
              )
    }
  }
  dev.off()
}

data.tmp <- data_sync.acc[,c("TimeStamp", "mag")]
hr <- data.heart_raw[,c("TimeStamp","HRM_RAW")]
hr$TimeStamp <- hr$TimeStamp/1000
data.tmp$TimeStamp <- (data.tmp$TimeStamp)/1000

create.jpegs(data.tmp)
create.jpegs(hr)
