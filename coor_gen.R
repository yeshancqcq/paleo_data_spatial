library(readr)

proxy_data <- read_csv("data/proxy_anomaly.csv")
proxy_data$lat <- NA
proxy_data$lon <- NA

this_lat <- 90 - 2.5
this_lon <- -180 + 2.5
count <- 1

for(row in 1:nrow(proxy_data)){
  if(count <=72){
    proxy_data$lat[row] <- this_lat
    proxy_data$lon[row] <- this_lon
    this_lon <- this_lon + 5
    count <- count + 1
  } else {
    count <- 1
    this_lon <- -180 + 2.5
    this_lat <- this_lat - 5
    proxy_data$lat[row] <- this_lat
    proxy_data$lon[row] <- this_lon
    this_lon <- this_lon + 5
    count <- count + 1
  }
}

write.csv(proxy_data,"data/proxy_anomaly_coordinates.csv")
