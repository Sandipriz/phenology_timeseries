##load libraries
library(lubridate)
library(tidyr)
library(Hmisc)
library(tidyverse)
library(xts)
library(dplyr)
library(bfast)
library(tseries)
library(stats)

##load data
data<-  read.csv2("E:/3_sem/609_envn_data/raw_data/S2_18_22.csv", sep=",")
data <- data.frame(data)
#colnames(C1_C)[1] <- 'Date'
#colnames(C1_C)[2] <- 'NDVI'

##explore data
head(data)
tail(data)
str(data)

#change data types
data[2:37] <-lapply(data[2:37], FUN = function(y){as.numeric(y)})
data<-data %>%
  mutate(Date = as.Date(Date, format="%d-%b-%y"))

#remove duplicate
data<- data[!duplicated(data$Date), ]

min(data$Date)
max(data$Date)

##check summary of column to impute the value
summary(data$C1)
plot(data$Date, data$C1, type="line", main="Original data for C1_C", xlab='Date', ylab="NDVI")
points(data$Date, data$C1, col="blue", pch=16)

##Impute the missing values
data<-data %>%
  complete(Date = seq.Date(min(Date), max(Date), by="1 day"))
head(data,10)
library(imputeTS)
head(data)
data1<- na_interpolation(data, option = "stine")
head(data1)
print(data1[9:15,])
data1 <- data1[-c(1:13),]

print(data1[1092:1100,])
data1 <- data1[-c(1097:1100),]
tail(data1)
##Keep legend outside the plot
plot_C1 <- data1[,c(1,2)]
plot_C4 <- data1[,c(1,5)]
C1_plot <- data[,c(1,2)]
head(plot_C4)
head(plot_C1)
par(mar = c(5, 4, 4, 8),                                  # Specify par parameters
    xpd = TRUE)
plot(plot_C1, type="l", lwd=1,  main="C1_C With Stine interpolation",xlab='Date', ylab="NDVI")
points(plot_C1, col="red", pch=10, cex=0.6)
points(C1_plot, col="blue", pch=16, cex=1.2)


legend("topright", inset = c(- 0.25, 0),                   # Create legend outside of plot
       legend = c("NDVI","Interpolated values"),
       pch = c(16,10),
       col = 1:2,
       cex=0.8)
#legend(x = "topleft", box.col = "brown",
#      bg ="white", box.lwd = 2 , title="Legends", 
#      legend=c("Stine Interpolation", "Available timeseries data"), 
#     fill = c("blue","red"), cex=0.65)
##explore data
which(data$C1>1)
str(plot_C1)
plot(plot_C1)
## check for min and max dates if they are correctly taken by R
min_date <- min(data1$Date)
max_date <- max(data1$Date)
str(plot_C1)
ndvi.C1 <- ts(plot_C1$C1,  start = c(2019, as.numeric(format(plot_C1$Date[1], "%j"))), ##assiining the day for the data
              frequency = 365) 
ndvi.C4 <- ts(plot_C4$C4,  start = c(2019, as.numeric(format(plot_C4$Date[1], "%j"))),
              frequency = 365)
head(ndvi.C1)
tail(ndvi.C1)
plot(ndvi.C1)
str(min_date)

##decompose data
fit_add=decompose(ndvi.C4, type="additive")
plot(fit_add)
##fit_add1 = HoltWinters(ndvi.C4,beta = FALSE, 
##                     gamma = FALSE)
#plot(fit_add1)

library(signal)
##trial for each savitzky golay smoothering across parameter
par(mfrow=c(4,1))
dev.off()

sg_C1 <- filter(filt=sgolay(plot_C1$C1, p=5,n=11))
x=sgolayfilt(plot_C1$C1, p=2, n=133, ts=30) 
plot(x, col="red")
sg_C1 <- ts(x, start= c(2019, as.numeric(format(plot_C4$Date[1], "%j"))),
            frequency = 365) 
plot(plot_C1$C1)
(sg_C1, col="red", lwd=2 main= "C1 with Savitzky Golay Smoothing")

plot(plot_C1$Date, plot_C1$C1, type="line", main="Timeseries data plotting for C1_C", xlab='Date', ylab="NDVI")
points(plot_C1$Date, plot_C1$C1, col="blue", pch=16)
points(data$Date, data$C1, col="red", pch=16, cex=2)
plot(sg_C1, col="darkgoldenrod1", type="line", lwd=2, main="Savitzky-Golay Filtering for C1_C", xlab='Date', ylab="NDVI")
plot(plot_C1$Date, plot_C1$C1, type="line", main="Timeseries data plotting for C1_C", xlab='Date', ylab="NDVI")

class(sg_C1)
head(sg_C1)

##Check for stationery vs non-stationery data
adf.test(plot_C4$C4)
acf(plot_C1, )

##check for autocorrelation
acf(ndvi.C1, type ="correlation",
    plot = TRUE, xlim=c(-1,365),main= "Autocorrelation", na.action = na.fail, demean = TRUE)


class(sg1)
head(sg1)
dev.off()

bfast_output <- bfast(
  sg_C1,
  h = 0.18, #keep on twirking the h value for break points
  season = "harmonic",
  max.iter = 1,
  breaks = 6,
  hpc = "none",
  level = 0.01,
  decomp = c("stl"),
  type = "OLS-MOSUM")
plot(bfast_output)
summary(bfast_output)
bfast_output$output


##Rbeast algorith for breakpoint detection
library(Rbeast)
Abeast <- beast(ndvits1,
  start = as.Date('2019-01-01'),
  deltat = 1/365,
  season = c('harmonic'),
  freq = 365,
  scp.minmax = c(0,10), sorder.minmax = c(0,5), sseg.min = NULL,
  tcp.minmax = c(0,10), torder.minmax = c(0,1), tseg.min = NULL,)
  
summary(Abeast)  
plot(Abeast)  
  
beast123(data1[,2:37], )