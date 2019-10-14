library(readr)
library(ggplot2)
library(stats)

# Set working directory to the source folder
model_anomaly <- read_csv("Feng_model_anomaly.csv")
model_anomaly <- model_anomaly[-1]
raw_data <- read_csv("proxy_anomaly.csv")
result <- data.frame(matrix(vector(), 220, 5, dimnames=list(c(), c("time", "total_area", "total_anomaly_area", "proxy_average", "model_average"))))

for(i in 6:225){
  total_area <- 0
  model_total_area <- 0
  sum_anomaly <- 0
  this_anomaly <- 0
  model_sum_anomaly <- 0
  model_this_anomaly <- 0
  weighted_result <- 0
  model_weighted_restul <- 0
  working <- data.frame(matrix(vector(), nrow(raw_data), 4, dimnames=list(c(),c("grid","area","raw","weighted"))))
  for(j in 1:nrow(raw_data)){
    working$grid[j] <- raw_data$PageName[j]
    working$raw[j] <- raw_data[j,i]
    working$area[j] <- raw_data$area[j]
    if(raw_data[j,i]!=0){
      this_anomaly <- raw_data$area[j] * raw_data[j,i]
      total_area <- total_area + raw_data$area[j]
      sum_anomaly <- sum_anomaly + this_anomaly
      model_this_anomaly <- model_anomaly[j,i]*raw_data$area[j]
      model_sum_anomaly <- model_sum_anomaly + model_this_anomaly
      model_total_area <- model_total_area + raw_data$area[j]
    }
  }
  anomaly_result <- sum_anomaly/total_area
  model_anomaly_result <- model_sum_anomaly/model_total_area
  result$time[i-5] <- i-5
  result$total_area[i-5] <- total_area
  result$total_anomaly_area[i-5] <- sum_anomaly
  result$proxy_average[i-5] <- anomaly_result
  result$model_average[i-5] <- model_anomaly_result
  
  cat("Processing t", i-6, "\n")
}

for(i in 1:nrow(result)){
  result$time_plot[i] <- result$time[i] * 100
  result$proxy_average_plot[i] <- as.numeric(result$proxy_average[i])
  result$model_average_plot[i] <- as.numeric(result$model_average[i])
}

plot_data <- data.frame(Years = result$time_plot, Model = result$model_average_plot, Proxy = result$proxy_average_plot)

ave <- ggplot()+
  geom_line(data=plot_data, aes(x=Years, y=Proxy, colour = "Proxy Data"), size = 0.5)+
  geom_line(data=plot_data, aes(x=Years, y=Model, colour = "Model Data"), size = 0.5)+
  labs(y = "Anomaly (Celsius)",
       x = "Time (Years BP)",
       colour = "Data Type") +
  ggtitle("Global Avereage Temperature Anomaly, Area Weighted","Anomaly reference: the average temperature from 8 ka to 12 ka")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.key= element_rect(colour="white", fill=NA)
  ) +
  scale_colour_manual(values=c(
    "Proxy Data" = "blue",
    "Model Data" = "red"
  ))+
  scale_x_reverse(limits = c(22000, 0))+
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

ave

write.csv(plot_data,"average_data_model_proxygrids.csv")
