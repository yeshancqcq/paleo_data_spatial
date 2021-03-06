library(astrochron)
library(ggplot2)
library(grid)
library(dplyr)
library(readr)
data <- read_csv("C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/data/plot_ave_sigma.csv")

plot_data <- data.frame(Years = data$`Years BP`, Average = data$Weighted_Average, Lower = data$Weighted_lower, Upper = data$Weighted_upper)

ave <- ggplot()+
  geom_ribbon(data=plot_data, aes(x=Years, ymin=Lower, ymax=Upper),fill="#71afd1", alpha=0.5)+
  geom_line(data=plot_data, aes(x=Years, y=Average), size = 1.2, colour ="#6868a8")+
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
  annotate("text", x = 20000, y = -5, label = "1-Sigma Uncertainty", size = 3 ,colour = "#457a96")+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=12000, linetype="dashed", color = "red")+
  geom_vline(xintercept=8000, linetype="dashed", color = "red")
  
ave
  