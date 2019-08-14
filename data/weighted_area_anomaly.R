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

for(i in 1:nrow(result)){
  result$time_plot[i] <- result$time[i] * 100
  result$average_plot[i] <- as.numeric(result$average[i])
  result$upper[i] <- as.numeric(result$average[i]) + result$st_error[i]
  result$lower[i] <- as.numeric(result$average[i]) - result$st_error[i]
}

plot_data <- data.frame(Years = result$time_plot, Average = result$average_plot, Lower = result$lower, Upper = result$upper)

ave <- ggplot()+
  geom_ribbon(data=plot_data, aes(x=Years, ymin=Lower, ymax=Upper),fill="#71afd1", alpha=0.5)+
  geom_line(data=plot_data, aes(x=Years, y=Average), size = 0.5, colour ="#6868a8")+
  labs(y = "Anomaly (Celsius)",
       x = "Time (Years BP)",
       colour = "Legend") +
  ggtitle("Global Avereage Temperature Anomaly, Area Weighted","Anomaly reference: the average temperature from 8ka to 12 ka")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)
  ) +
  scale_x_reverse(limits = c(22000, 0))+
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1))+
  annotate("text", x = 20000, y = -5, label = "1-Sigma Uncertainty", size = 3 ,colour = "#457a96")+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=12000, linetype="dashed", color = "red")+
  geom_vline(xintercept=8000, linetype="dashed", color = "red")

ave
