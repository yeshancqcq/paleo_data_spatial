library(readr)
library(ggplot2)
library(gtable)
library(grid)
library(dplyr)
library(gridExtra)
library(cowplot)
library(maps)
library(ggmap)
data <- read_csv("~/Documents/github/paleo_data_spatial/metadata_lipd.csv")

data$res_class <- NA
for(i in 1:nrow(data)){
  if(data$resolution[i] == 0){
    data$res_class[i] <- "0 or NA"
  } else if(data$resolution[i] <= 5){
    data$res_class[i] <- "0.1 to 5"
  } else if(data$resolution[i] <= 10){
    data$res_class[i] <- "5.1 to 10"
  } else if(data$resolution[i] <= 15){
    data$res_class[i] <- "10.1 to 15"
  } else if(data$resolution[i] <= 20){
    data$res_class[i] <- "15.1 to 20"
  } else {
    data$res_class[i] <- "more than 20"
  }
  cat("finishing", i,"\n")
}

data$proxy_class <- NA
for(i in 1:nrow(data)){
  if(is.na(data$proxy[i])){
    data$proxy_class[i] <- "others"
  } else if(data$proxy[i] == "pollen"){
    data$proxy_class[i] <- "pollen"
  } else if (data$proxy[i] == "d18O"){
    data$proxy_class[i] <- "d18O"
  } else if (data$proxy[i] == "alkenone"){
    data$proxy_class[i] <- "alkenone"
  } else if (data$proxy[i] == "chironomid"){
    data$proxy_class[i] <- "chironomid"
  } else if (data$proxy[i] == "foraminifera"){
    data$proxy_class[i] <- "foraminifera"
  } else if (data$proxy[i] == "GDGT"){
    data$proxy_class[i] <- "GDGT"
  } else if (data$proxy[i] == "Mg/Ca"){
    data$proxy_class[i] <- "Mg/Ca"
  } else {
    data$proxy_class[i] <- "others"
  }
  cat("finishing", i,"\n")
}

data$age_point <- NA
for(i in 1:nrow(data)){
  if(is.na(data$num_control[i])){
    data$age_point[i] <- "0 or NA"
  } else if (data$num_control[i] <= 3){
    data$age_point[i] <- "1 to 3"
  } else if (data$num_control[i] <= 8){
    data$age_point[i] <- "4 to 8"
  } else {
    data$age_point[i] <- "more than 8"
  }
  cat("finishing", i,"\n")
}

data$res_class <- factor(data$res_class,levels = c("0 or NA","0.1 to 5","5.1 to 10","10.1 to 15","15.1 to 20","more than 20"))
plot1 <- ggplot(data = data) + 
  geom_rect(aes(xmin = min_yr/1000, xmax = max_yr/1000, ymin = u_id, ymax = u_id + 1, fill = res_class, color = res_class))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.justification = c(0, 0),
        legend.position = "right",
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  scale_color_manual(values = c(
    "0 or NA" = "dark blue",
    "0.1 to 5" = "blue",
    "5.1 to 10" = "light blue",
    "10.1 to 15" = "pink",
    "15.1 to 20" = "salmon",
    "more than 20" = "red"
  ))+
  scale_fill_manual(values = c(
    "0 or NA" = "dark blue",
    "0.1 to 5" = "blue",
    "5.1 to 10" = "light blue",
    "10.1 to 15" = "pink",
    "15.1 to 20" = "salmon",
    "more than 20" = "red"
  ))+
  ggtitle("Resolution")+
  labs(y = "",
       x = "Ka",
       color = "record per kyr",
       fill = "record per kyr")+
  scale_x_reverse(limits = c(22,0), breaks = scales::pretty_breaks(n = 10))

plot1

data$proxy_class <- factor(data$proxy_class,levels = c("alkenone","GDGT","d18O","foraminifera","chironomid","pollen", "Mg/Ca","others"))
plot2 <- ggplot(data = data) + 
  geom_rect(aes(xmin = min_yr/1000, xmax = max_yr/1000, ymin = u_id, ymax = u_id + 1, fill = proxy_class, color = proxy_class))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.justification = c(0, 0),
        legend.position = "right",
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  ggtitle("Proxy")+
  scale_fill_manual(values = c(
    "alkenone" = "dark green",
    "GDGT" = "orange",
    "d18O" = "blue",
    "foraminifera" = "tan",
    "chironomid" = "purple",
    "pollen" = "pink", 
    "Mg/Ca" = "salmon",
    "others" = "gray"
  ))+
  scale_color_manual(values = c(
    "alkenone" = "dark green",
    "GDGT" = "orange",
    "d18O" = "blue",
    "foraminifera" = "tan",
    "chironomid" = "purple",
    "pollen" = "pink", 
    "Mg/Ca" = "salmon",
    "others" = "gray"
  ))+
  labs(y = "",
       x = "Ka",
       color = "proxy",
       fill = "proxy")+
  scale_x_reverse(limits = c(22,0), breaks = scales::pretty_breaks(n = 10))

plot2

plot3 <- ggplot(data = data) + 
  geom_rect(aes(xmin = min_yr/1000, xmax = max_yr/1000, ymin = u_id, ymax = u_id + 1, fill = age_point, color = age_point))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.justification = c(0, 0),
        legend.position = "right",
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  ggtitle("Age control points")+
  scale_color_manual(values = c(
    "0 or NA" = "black",
    "1 to 3" = "tan",
    "4 to 8" = "salmon",
    "more than 8" = "light blue"
  ))+
  scale_fill_manual(values = c(
    "0 or NA" = "black",
    "1 to 3" = "tan",
    "4 to 8" = "salmon",
    "more than 8" = "light blue"
  ))+
  labs(y = "",
       x = "Ka",
       color = "age points",
       fill = "age points")+
  scale_x_reverse(limits = c(22,0), breaks = scales::pretty_breaks(n = 10))

plot3
gp <- plot_grid(plot1, plot2, plot3, align = "h", nrow = 1, ncol = 3)
gp


base <- map_data("world")

# plot the map:

map1 <- ggplot() + 
  geom_polygon(data = base, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", size = 0.05) +
  coord_fixed(xlim = c(-180, 180),  ylim = c(-89, 89), ratio = 1.4)+
  geom_point(data = data, aes(x = lon, y = lat, colour=res_class), size = 2.2) +
  scale_color_manual(values = c(
    "0 or NA" = "dark blue",
    "0.1 to 5" = "blue",
    "5.1 to 10" = "light blue",
    "10.1 to 15" = "pink",
    "15.1 to 20" = "salmon",
    "more than 20" = "red"
  ))+
  #geom_text(data = bigten, aes(x = lon, y = lat, label = paste("  ", as.character(University), sep="")), fontface = "bold",angle = 0, hjust = 0, color = "black", size = 3.5)+
  theme(
    axis.line = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "right", 
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA, fill = NA)
  )+
  labs(colour = "record per kyr")+
  ggtitle("Resolution") +
  scale_x_continuous(limits = c(-180,180), breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(limits = c(-89,89), breaks = scales::pretty_breaks(n = 12))

map1

map2 <- ggplot() + 
  geom_polygon(data = base, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", size = 0.05) +
  coord_fixed(xlim = c(-180, 180),  ylim = c(-89, 89), ratio = 1.4)+
  geom_point(data = data, aes(x = lon, y = lat, colour=proxy_class), size = 2.2) +
  scale_color_manual(values = c(
    "alkenone" = "dark green",
    "GDGT" = "orange",
    "d18O" = "blue",
    "foraminifera" = "tan",
    "chironomid" = "purple",
    "pollen" = "pink", 
    "Mg/Ca" = "salmon",
    "others" = "gray"
  ))+
  #geom_text(data = bigten, aes(x = lon, y = lat, label = paste("  ", as.character(University), sep="")), fontface = "bold",angle = 0, hjust = 0, color = "black", size = 3.5)+
  theme(
    axis.line = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "right", 
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA, fill = NA)
  )+
  labs(colour = "proxy")+
  ggtitle("Proxy") +
  scale_x_continuous(limits = c(-180,180), breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(limits = c(-89,89), breaks = scales::pretty_breaks(n = 12))

map2

map3 <- ggplot() + 
  geom_polygon(data = base, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", size = 0.05) +
  coord_fixed(xlim = c(-180, 180),  ylim = c(-89, 89), ratio = 1.4)+
  geom_point(data = data, aes(x = lon, y = lat, colour=age_point), size = 2.2) +
  scale_color_manual(values = c(
    "0 or NA" = "black",
    "1 to 3" = "tan",
    "4 to 8" = "salmon",
    "more than 8" = "light blue"
  ))+
  #geom_text(data = bigten, aes(x = lon, y = lat, label = paste("  ", as.character(University), sep="")), fontface = "bold",angle = 0, hjust = 0, color = "black", size = 3.5)+
  theme(
    axis.line = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "right", 
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA, fill = NA)
  )+
  labs(colour = "age control")+
  ggtitle("Age control points") +
  scale_x_continuous(limits = c(-180,180), breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(limits = c(-89,89), breaks = scales::pretty_breaks(n = 12))

map3

gp2 <- plot_grid(map1, map2, map3, align = "v", nrow = 3, ncol = 1)
gp2
