# plot NH ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$NH, model=model_hemi$NH, diff=diff_hemi$NH)
plotNH <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Proxy"))+
  geom_line(data=plot_data,aes(time, model, colour = "Model"))+
  #geom_line(data=plot_data,aes(time, diff+10, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Proxy' = 'blue',
    'Model' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 10, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 2),
                     breaks = scales::pretty_breaks(n = 10))+
  annotate("text", x = 20500, y = 1.7, label = "30°N ~ 90°N")+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 30°N ~ 90°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.y= element_line(colour = "black"),
        axis.line.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.8, 0.1),
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plotNH
ggsave("../img/regional_plots/NH1.jpg", width = 6, height = 4)


# Tropical plot +++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$Tropical, model=model_hemi$Tropical, diff=diff_hemi$Tropical)
plotTrop <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Proxy"))+
  geom_line(data=plot_data,aes(time, model, colour = "Model"))+
  #geom_line(data=plot_data,aes(time, diff+10, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Proxy' = 'blue',
    'Model' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 10, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 2),
                     breaks = scales::pretty_breaks(n = 10))+
  annotate("text", x = 20500, y = 1.7, label = "30°N ~ 30°S")+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 30°N ~ 90°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.y= element_line(colour = "black"),
        axis.line.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.8, 0.1),
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plotTrop
ggsave("../img/regional_plots/Trop1.jpg", width = 6, height = 4)

# SH Plot +++++++++++++++++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$SH, model=model_hemi$SH, diff=diff_hemi$SH)
plotSH <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Proxy"))+
  geom_line(data=plot_data,aes(time, model, colour = "Model"))+
  #geom_line(data=plot_data,aes(time, diff+10, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Proxy' = 'blue',
    'Model' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 10, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 2),
                     breaks = scales::pretty_breaks(n = 10))+
  annotate("text", x = 20500, y = 1.7, label = "30°S ~ 90°S")+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 30°N ~ 90°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = c(0.8, 0.1),
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plotSH
ggsave("../img/regional_plots/SH1.jpg", width = 6, height = 4)


# Global +++++++++++++++++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$Global, model=model_hemi$Global, diff=diff_hemi$Global)
plotGlobal <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Proxy"))+
  geom_line(data=plot_data,aes(time, model, colour = "Model"))+
  #geom_line(data=plot_data,aes(time, diff+10, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Proxy' = 'blue',
    'Model' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 10, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 2),
                     breaks = scales::pretty_breaks(n = 10))+
  annotate("text", x = 20500, y = 1.7, label = "Global")+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 30°N ~ 90°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.y= element_line(colour = "black"),
        axis.line.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.8, 0.1),
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plotGlobal
ggsave("../img/regional_plots/Global1.jpg", width = 6, height = 4)

#++++++++++++ stack ++++++++

library(gtable)
library(grid)
library(dplyr)
g2 <- ggplotGrob(plotGlobal)
g3 <- ggplotGrob(plotNH)
g4 <- ggplotGrob(plotTrop)
g5 <- ggplotGrob(plotSH)
g <- rbind(g2, g3, g4, g5, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)


