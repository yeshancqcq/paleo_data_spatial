library(plyr)
library(readr)

model_anomaly <- read_csv("Feng_model_anomaly.csv")
proxy_anomaly <- read_csv("raw_data_anomaly_cal.csv")

diff_anomaly <- proxy_anomaly

for(row in 1:nrow(model_anomaly)){
  for(col in 7:ncol(model_anomaly)){
    if(as.numeric(proxy_anomaly[row,col])!= 0 || is.na(proxy_anomaly[row,col])!= 0){
      diff_anomaly[row,col] <- as.numeric(model_anomaly[row,col]) - as.numeric(proxy_anomaly[row,col])
    } else {
      diff_anomaly[row,col] <- NA
    }
  }
  cat("finishing row", row, " ;")
}

write.csv(diff_anomaly, file = "data_model_anomaly_diff.csv")
