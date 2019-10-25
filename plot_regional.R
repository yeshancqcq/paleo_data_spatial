library(readr)
library(ggplot2)

proxy <- read_csv("proxy_region.csv")
model <- read_csv("model_region.csv")
diff <- read_csv("../regional_all.csv")


#new zealand
plot_data <- data.frame(time=proxy$time, proxy=proxy$new_zealand, model=model$new_zealand, diff=diff$New_Zealand)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at New Zealand", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/new_zealand.jpg", width = 6, height = 4)

#greenland
plot_data <- data.frame(time=proxy$time, proxy=proxy$greenland, model=model$greenland, diff=diff$Greenland)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Greenland", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/greenland.jpg", width = 6, height = 4)

#alaska
plot_data <- data.frame(time=proxy$time, proxy=proxy$alaska, model=model$alaska, diff=diff$Alaska)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Alaska", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/alaska.jpg", width = 6, height = 4)

#north atlantic
plot_data <- data.frame(time=proxy$time, proxy=proxy$n_atlantic, model=model$n_atlantic, diff=diff$N_Atlantic)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at North Atlantic", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/north_atlantic.jpg", width = 6, height = 4)

#indonesia
plot_data <- data.frame(time=proxy$time, proxy=proxy$indonesia, model=model$indonesia, diff=diff$Indonesia)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Indonesia", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/indonesia.jpg", width = 6, height = 4)

#antarctica
plot_data <- data.frame(time=proxy$time, proxy=proxy$antarctica, model=model$antarctica, diff=diff$Antarctica)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Antarctica", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/antarctica.jpg", width = 6, height = 4)

#china
plot_data <- data.frame(time=proxy$time, proxy=proxy$china, model=model$china, diff=diff$China)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at North-Central China", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/indonesia.jpg", width = 6, height = 4)

#japan
plot_data <- data.frame(time=proxy$time, proxy=proxy$japan, model=model$japan, diff=diff$Japan)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Japan and E. China Sea", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/japan.jpg", width = 6, height = 4)

#philippines
plot_data <- data.frame(time=proxy$time, proxy=proxy$philippines, model=model$philippines, diff=diff$Philippines)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Philippines and S. China Sea", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/philippines.jpg", width = 6, height = 4)

#south australia
plot_data <- data.frame(time=proxy$time, proxy=proxy$s_australia, model=model$s_australia, diff=diff$S_Australia)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Offshore Southern Australia", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/s_australia.jpg", width = 6, height = 4)

#oregon
plot_data <- data.frame(time=proxy$time, proxy=proxy$oregon, model=model$oregon, diff=diff$Offshore_oregon)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Offshore Oregon", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/oregon.jpg", width = 6, height = 4)

#eastern pacific
plot_data <- data.frame(time=proxy$time, proxy=proxy$e_pacific, model=model$e_pacific, diff=diff$Tropical_E_Pacific)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Tropical E. Pacific", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/tropical_e_pacific.jpg", width = 6, height = 4)

#caribbean and g of mexic
plot_data <- data.frame(time=proxy$time, proxy=proxy$gulf_of_mexico, model=model$gulf_of_mexico, diff=diff$Caribbean)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  geom_line(data=plot_data,aes(time, diff+20, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red',
    'Model - Proxy' = 'purple'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  geom_hline(yintercept = 20, size = 0.6, linetype='dotted', colour="purple") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-25, 40), 
                     sec.axis = sec_axis(~.-20, name = "Anomaly Differences (°C)"))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly at Caribbean and Gulf of Mexico", "Proxy Metadata and Model Outputs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot
ggsave("../img/regional_plots/caribbean.jpg", width = 6, height = 4)
