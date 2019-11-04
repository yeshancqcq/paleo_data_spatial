#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(geojsonio)
library(broom)
library(maptools)
library(rgdal)
library(readr)
library(RColorBrewer)
library(gtable)
library(grid)
library(dplyr)
require(gridExtra)
library(ggpubr)

my.cols <- brewer.pal(9, "Blues")

data_model_diff <- read_csv("data/data_model_anomaly_diff.csv")

world <- map_data("world")

regions <- read_csv("data/diff_regional.csv")

#spdf <- geojson_read("GitHub/yeshancqcq.github.io/anomaly/data/diff_anomaly.json",  what = "sp")
#spdf_anomaly_diff <- tidy(spdf)
anomal_diff=readShapePoly("data/diff_anomaly.shp",verbose=TRUE)
ad_df <- fortify(anomal_diff)
ad_df$X1_1 <- factor(ad_df$id)
ad_df$id <- NULL
plot.data <- merge(ad_df, data_model_diff, by = "X1_1")


for(i in seq(233, 14, -1)){
  yr <- (i-13)*100
  gg.data <- data.frame(long=plot.data[,2], lat=plot.data[,3], group=plot.data[,7], time=plot.data[,i])
  plot1 <- ggplot() + 
    geom_polygon(data = world, aes(x=long, y = lat, group = group),color = NA, fill = 'gray80', alpha=0.5) +
    coord_fixed(xlim = c(-180, 180),  ylim = c(-90,90), ratio = 1.5)+
    geom_polygon(data = gg.data, 
                 aes(x = long, y = lat, group = group, fill= time),
                 color = NA, size = .2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", fill="gray65"), 
          axis.line = element_line(colour = "black"))+
    scale_fill_gradient2(low="#1c579e",high="#9e1c1c",mid="white",limits=c(-10,10), na.value = NA)+
    #scale_x_continuous(limits = c(-180,180), breaks = scales::pretty_breaks(n = 10))+
    #scale_y_continuous(limits = c(-90,90), breaks = scales::pretty_breaks(n = 10))+
    annotate("text", x = -160, y = 85, label = paste(as.character(yr), " Years BP"), size = 3, colour = "black", alpha = 0.6) +
    labs(y = "Latitude",
         x = "Longitude",
         fill = "°C")+
    ggtitle("Temperature Anomaly Difference") 
  
  
  regions.plot <-  subset(regions, time >= yr)
  plot2 <- ggplot()+
    geom_line(data=regions.plot,aes(time, gulf_of_mexico, colour = "Caribbean and Gulf of Mexico"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, china, colour = "North Central China"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, new_zealand, colour = "New Zealand"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, alaska, colour = "Alaska"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, antarctica, colour = "Antarctica"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, japan, colour = "Japan and E. China Sea"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, indonesia, colour = "Indonesia"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, mediterranean, colour = "Mediterranean Sea"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, philippines, colour = "Philippines and S. China Sea"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, s_atlantic, colour = "S. Atlantic"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, e_pacific, colour = "Tropical E. Pacific"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, n_atlantic, colour = "N. Atlantic"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, s_australia, colour = "S. Australia"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, oregon, colour = "Offshore Oregon"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, scandinavia, colour = "Scandinavia and Svalbard"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, iceland, colour = "Iceland and British Isles"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, greenland, colour = "Greenland"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, congo, colour = "Congo"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, w_africa, colour = "Tropical W. African Coast"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, arabean_sea, colour = "Arabian Sea"), size = 0.5)+
    geom_line(data=regions.plot,aes(time, s_america, colour = "W. South American Coast"), size = 0.5)+
    scale_y_continuous(name = expression("Anomaly Differences (°C)"), 
                       limits = c(-10, 20),
                       breaks = scales::pretty_breaks(n = 10))+
    scale_colour_manual(values = c(
      'North Central China' = '#7CA0A6',
      'Caribbean and Gulf of Mexico' = 'blue',
      'Japan and E. China Sea' = 'green',
      'Antarctica' = 'light blue',
      'Congo' = '#aceb86',
      'Arabian Sea' = '#7b7ba8',
      'Indonesia' = 'orange',
      'New Zealand' = 'dark blue',
      'W. South American Coast' = '#c244a7',
      'Tropical W. African Coast' = '#c24470',
      'Alaska' = 'purple',
      'Philippines and S. China Sea' = 'gray30',
      'Mediterranean Sea' = 'gray70',
      'S. Atlantic' = 'red',
      'Greenland' = 'dark green',
      'Offshore Oregon' = 'pink',
      'Scandinavia and Svalbard' = '#0095ff',
      'Iceland and British Isles' = 'yellow',
      'S. Australia' = 'tan',
      'Tropical E. Pacific' = 'black',
      'N. Atlantic' = '#27d9d9'
    )) +
    labs(y = "Counts",
         x = "Time (Years BP)",
         colour = "Locations") +
    ggtitle("Anomaly Difference = Model Anomaly - Proxy Anomaly", "Weighted to Grid Area") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line.y = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black"),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.justification = c(0, 0),
          legend.position = "right",
          legend.background = element_rect(colour = NA, size = 0.2),
          legend.key = element_rect(colour = "white", fill = NA),
          legend.title = element_blank(),
          legend.text=element_text(size=rel(0.9))
    ) +
    geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
    scale_x_reverse(limits = c(22000, 0),breaks = scales::pretty_breaks(n = 10))+
    #annotate("text", x = 10000, y = 18, label = "Temperature Anomaly Difference = Model Anomaly - Proxy Anomaly (Weighted to Grid Area)", size = 3, colour = "black", alpha = 1)+
    annotate("text", x = 20000, y = 18, label = paste(as.character(yr), " Years BP"), size = 3, colour = "black", alpha = 1)
 
   cat(as.character(yr), " Years BP", "\n")
   
   #grid.arrange(plot2, plot1, ncol=1, nrow=2)
   ggarrange(plot1, plot2, heights = c(2.4, 1.2),
             ncol = 1, nrow = 2, align = "v")
   
   ggsave(paste(as.character(yr), ".jpg"), plot = last_plot(), device = NULL, path = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/img/int_data",
          width = 32, height = 28, units = "cm",
          dpi = 300)

}

plot1
plot2

#grid.arrange(plot2, plot1, ncol=1, nrow=2)
ggarrange(plot1, plot2, heights = c(2.4, 1.2),
          ncol = 1, nrow = 2, align = "v")

ggsave(paste(as.character(yr), ".jpg"), plot = last_plot(), device = NULL, path = "C:/Users/Shan Ye/Documents/GitHub/paleo_data_spatial/img/int_data",
       width = 32, height = 28, units = "cm",
       dpi = 300)





  


