library(readr)
library(ggplot2)
library(stats)

raw_data <- read_csv("raw_data.csv")
result <- data.frame(matrix(vector(), 221, 5, dimnames=list(c(), c("time", "total_area", "total_anomaly_area", "average", "st_error"))))

for(i in 6:226){
  total_area <- 0
  sum_anomaly <- 0
  this_anomaly <- 0
  weighted_result <- 0
  working <- data.frame(matrix(vector(), nrow(raw_data), 4, dimnames=list(c(),c("grid","area","raw","weighted"))))
  for(j in 1:nrow(raw_data)){
    working$grid[j] <- raw_data$PageName[j]
    working$raw[j] <- raw_data[j,i]
    working$area[j] <- raw_data$area[j]
    if(raw_data[j,i]!=0){
      this_anomaly <- raw_data$area[j] * raw_data[j,i]
      total_area <- total_area + raw_data$area[j]
      sum_anomaly <- sum_anomaly + this_anomaly
    }
  }
  anomaly_result <- sum_anomaly/total_area
  result$time[i-5] <- i-6
  result$total_area[i-5] <- total_area
  result$total_anomaly_area[i-5] <- sum_anomaly
  result$average[i-5] <- anomaly_result
  
  for(x in 1:nrow(working)){
    
    working$weighted[x] <- as.numeric(working$raw[x]) * (working$area[x]/total_area)
    if(working$weighted[x] == 0){
      working$weighted[x]<- NA
    }
  }
  weighted_result <- sd(working$weight, na.rm = TRUE)
  result$st_error[i-5] <- weighted_result
  cat("Processing t", i-6, "\n")
}