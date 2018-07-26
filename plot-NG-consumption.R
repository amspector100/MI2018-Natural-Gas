# C:/Users/amspe/Documents/R/MI2018/Natural Gas or
# C:/Users/aspector/Documents/R Projects/Natural Gas

# ------------------------------------------------------------------
# This file graphs a various statistics related to NG consumption of the world
# and of the US. There's another R script that deals purely with US production
# and consumption.
# ------------------------------------------------------------------

library(maggritr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)

ng_data <- read_csv("final_ng_cons_data.csv")

# Global color and sizing variables
grid_color <- 'gray80'
grid_size <- 0.35
line_colors <- c('black', '#619CFF', '#F8766D')

# Reshape to make it easier for ggplot2 ----------------------------------------

# Percent data
percent_data <- ng_data %>% 
  select(c("Year", "us_perc_from_ng", "world_perc_from_ng")) %>%
  gather("place", "consumption", us_perc_from_ng, world_perc_from_ng) %>%
  mutate(place = ifelse(place == 'us_perc_from_ng', 'US', 'World (Including US)'))

# Create unscaled version of consumption data 
cons_data <- ng_data %>% 
  mutate(total_world_cons = total_world_cons - total_us_cons) %>%
  select(c("Year", "total_us_cons", "total_world_cons")) %>%
  gather("place", "consumption", total_us_cons, total_world_cons) %>%
  mutate(place = ifelse(place == 'total_us_cons', 'US', 'World (Excluding US)'))

# Create scaled version of consumption data 

scaled_cons_data <- cons_data %>%
  group_by(place) %>%
  mutate(consumption = 100*consumption/consumption[Year == 1990])

# Update scaled version to include total world energy consumption (not just NG)
old_cols_v2 <- c("total_us_cons", "total_world_cons", "total_world_energy_cons")
new_cols_v2 <- c("US Natural Gas Consumption", "World Natural Gas Consumption",
                 "Total World Energy Consumption")

scaled_cons_data_v2 <- ng_data %>%
  # Make sure world totals don't include US
  mutate(total_world_cons = total_world_cons - total_us_cons) %>%
  mutate(total_world_energy_cons = total_world_energy_cons - total_us_energy_cons) %>%
  select(c("Year", "total_us_cons", 
           "total_world_cons", "total_world_energy_cons")) %>%
  rename_at(vars(old_cols_v2), ~ new_cols_v2) %>%
  gather("place", "consumption", -Year) %>%
  group_by(place) %>%
  mutate(consumption = 100*consumption/consumption[Year == 1990]) %>% 
  mutate(Source = ifelse(grepl('Total', place), 'Total', 'NG'))

  
# Graph 1 - percent of electricity production for NG/US

perc_plot <- ggplot(percent_data) + 
  geom_line(aes(x = Year, y=consumption, group=place, color = place)) + 
  guides(color=F) +
  theme_bw() + 
  theme(axis.line.x=element_line(color='black', size=0.5),
        axis.text=element_text(color='black'),
        axis.text.x=element_text(margin=margin(0, 0, 0, 0)),
        axis.text.y=element_text(margin=margin(0, 0, 0, 0)),
        axis.ticks=element_blank(),
        axis.title.y.right=element_text(margin=margin(0, 0, 0, 0)),
        #axis.title.y=element_text(margin=c(0, 0, 0, 0)),
        panel.grid.major.y=element_line(color=grid_color, size=grid_size),
        panel.grid.major.x=element_blank(),
        # geom_segment() will add vertical grid lines in later (minus 1990)
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.caption=element_text(margin=margin(6, 0, 0, 0), size=8)
  ) +
  geom_vline(xintercept=seq(1990, 2015, 5), color=grid_color, size=grid_size) +
  

  labs(title="Percent of Total Electricity Production from Natural Gas Sources",
       x=NULL,
       y='Percent',
       caption= 'Based on IEA data from  www.iea.org/statistics, © OECD/IEA') +
  scale_x_continuous(limits=c(1990, 2015),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(10, 35),
                     expand = c(0,0),
                     breaks=seq(15, 35, 5)) + 
  geom_text(data = percent_data %>% 
            filter(Year <= 2013) %>%
              group_by(place) %>%
              summarize(Y = 0.95*max(consumption, na.rm=T), X = 2008),
            aes(x = X, y = Y, label = place, color = place),
            hjust = .05, vjust = 5)

png('Figures/natgas-electricity-graph.png', w=800, h=500)
print(perc_plot)
dev.off()

svg('Figures/natgas-electricity-graph.svg', w=8, h=5)
print(perc_plot)
dev.off()

# Graph 2 - Total consumption for US/World

cons_plot <- ggplot(cons_data) + 
  geom_line(aes(x = Year, y=consumption, group=place, color = place)) + 
  guides(color=F) +
  theme_bw() + 
  theme(axis.line.x=element_line(color='black', size=0.5),
        axis.text=element_text(color='black'),
        axis.text.x=element_text(margin=margin(0, 0, 0, 0)),
        axis.text.y=element_text(margin=margin(0, 0, 0, 0)),
        axis.ticks=element_blank(),
        axis.title.y.right=element_text(margin=margin(0, 0, 0, 0)),
        #axis.title.y=element_text(margin=c(0, 0, 0, 0)),
        panel.grid.major.y=element_line(color=grid_color, size=grid_size),
        panel.grid.major.x=element_blank(),
        # geom_segment() will add vertical grid lines in later (minus 1990)
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.caption=element_text(margin=margin(6, 0, 0, 0), size=8)
  ) +
  geom_vline(xintercept=seq(1990, 2017, 5), color=grid_color, size=grid_size) +
  
  
  labs(title="Total Consumption of Natural Gas",
       x=NULL,
       y='Billions of Cubic Meters',
       caption=expression(paste('Source: BP, ',
                                italic('Statistical Review of World Energy 2017')))) +
  scale_x_continuous(limits=c(1990, 2017),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(0, 3000),
                     expand = c(0,0),
                     breaks=seq(500, 3000, 500)) + 
  # Add labels
  geom_text(data = cons_data %>% 
              filter(Year <= 2005) %>%
              group_by(place) %>%
              summarize(Y = max(consumption, na.rm=T) + 0.32, X = 1992.5),
            aes(x = X, y = Y, label = place, color = place),
            hjust = 0.2)


png('Figures/natgas-consumption-graph.png', w=800, h=500)
print(cons_plot)
dev.off()

svg('Figures/natgas-consumption-graph.svg', w=8, h=5)
print(cons_plot)
dev.off()


# Graph 3 - Scaled consumption for US/World

scaled_cons_plot <- ggplot(scaled_cons_data) + 
  geom_line(aes(x = Year, y=consumption, group=place, color = place)) + 
  guides(color=F) +
  theme_bw() + 
  theme(axis.line.x=element_line(color='black', size=0.5),
        axis.text=element_text(color='black'),
        axis.text.x=element_text(margin=margin(0, 0, 0, 0)),
        axis.text.y=element_text(margin=margin(0, 0, 0, 0)),
        axis.ticks=element_blank(),
        axis.title.y.right=element_text(margin=margin(0, 0, 0, 0)),
        #axis.title.y=element_text(margin=c(0, 0, 0, 0)),
        panel.grid.major.y=element_line(color=grid_color, size=grid_size),
        panel.grid.major.x=element_blank(),
        # geom_segment() will add vertical grid lines in later (minus 1990)
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.caption=element_text(margin=margin(6, 0, 0, 0), size=8)
  ) +
  geom_vline(xintercept=seq(1990, 2017, 5), color=grid_color, size=grid_size) +
  
  
  labs(title="Consumption of Natural Gas, Scaled to 1990 Levels",
       x=NULL,
       y='Percent of 1990 Levels',
       caption=expression(paste('Source: BP, ',
                                italic('Statistical Review of World Energy 2017')))) +
  scale_x_continuous(limits=c(1990, 2017),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(100, 200),
                     expand = c(0,0),
                     breaks=seq(100, 200, 20)) + 
  geom_text(data = scaled_cons_data %>% 
              filter(Year <= 2003) %>%
              group_by(place) %>%
              summarize(Y = max(consumption, na.rm=T), X = 2005),
            aes(x = X, y = Y, label = place, color = place),
            hjust = 0.2, vjust = 1)


png('Figures/natgas-scaled-consumption-graph.png', w=800, h=500)
print(scaled_cons_plot)
dev.off()

svg('Figures/natgas-scaled-consumption-graph.svg', w=8, h=5)
print(scaled_cons_plot)
dev.off()



# Graph 4 - Scaled consumption for US/World with world energy cons included too

scaled_cons_plot_v2 <- ggplot(scaled_cons_data_v2) + 
  geom_line(aes(x = Year, y=consumption, group=place, color = place, linetype = Source)) + 
  scale_color_manual(values = line_colors) + 
  guides(color=F) +
  theme_bw() + 
  theme(axis.line.x=element_line(color='black', size=0.5),
        axis.text=element_text(color='black'),
        axis.text.x=element_text(margin=margin(0, 0, 0, 0)),
        axis.text.y=element_text(margin=margin(0, 0, 0, 0)),
        axis.ticks=element_blank(),
        axis.title.y.right=element_text(margin=margin(0, 0, 0, 0)),
        #axis.title.y=element_text(margin=c(0, 0, 0, 0)),
        panel.grid.major.y=element_line(color=grid_color, size=grid_size),
        panel.grid.major.x=element_blank(),
        # geom_segment() will add vertical grid lines in later (minus 1990)
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.caption=element_text(margin=margin(6, 0, 0, 0), size=8),
        legend.position = "none"
  ) +
  geom_vline(xintercept=seq(1990, 2017, 5), color=grid_color, size=grid_size) +
  
  
  labs(title="Natural Gas and Total Energy Consumption, Scaled to 1990 Levels",
       x=NULL,
       y='Consumption, Percent of 1990 Levels',
       caption=expression(paste('Source: BP, ',
                                italic('Statistical Review of World Energy 2017.'),
                                       ' Note that world totals exclude the US.'))) +
  scale_x_continuous(limits=c(1990, 2017),
                     expand = c(0,0),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(100, 200),
                     expand = c(0.0025,0),
                     breaks=seq(100, 200, 20)) + 
  geom_text(data = scaled_cons_data_v2 %>% 
              filter(Year <= 2007) %>%
              group_by(place) %>%
              summarize(Y = max(consumption, na.rm=T) + 0.32, X = 2007),
            aes(x = X, y = Y, label = place, color = place),
            hjust = 0.2, vjust = -1.3, angle = 30, size = 3.5)


png('Figures/natgas-scaled-consumption-graph-v2.png', w=800, h=500)
print(scaled_cons_plot_v2)
dev.off()

svg('Figures/natgas-scaled-consumption-graph-v2.svg', w=8, h=5)
print(scaled_cons_plot_v2)
dev.off()

# Graph 5 - Scaled consumption for US/World with US/Word energy cons included too

line_colors <- c('#619CFF', '#F8766D')


# Create new labels and a new color
old_cols_v3 <- append(old_cols_v2, 'total_us_energy_cons')
new_cols_v3 <- append(new_cols_v2, 'Total US Energy Consumption')
line_colors <- append(line_colors, 'black')

# Create data.frame
scaled_cons_data_v3 <- ng_data %>%
  # Make sure world totals don't include US
  mutate(total_world_cons = total_world_cons - total_us_cons) %>%
  mutate(total_world_energy_cons = total_world_energy_cons - total_us_energy_cons) %>%
  select(c("Year", "total_us_cons", "total_world_cons", 
           "total_us_energy_cons", "total_world_energy_cons")) %>%
  rename_at(vars(old_cols_v3), ~ new_cols_v3) %>%
  gather("place", "consumption", -Year) %>%
  group_by(place) %>%
  mutate(consumption = 100*consumption/consumption[Year == 1990]) %>%
  ungroup() %>%
  mutate(Source = ifelse(grepl('Total', place), 'Total Consumption', 'Natural Gas \r\n Consumption')) %>%
  mutate(Country = ifelse(grepl('US', place), 'US', 'World'))

scaled_cons_plot_v3 <- ggplot(scaled_cons_data_v3) + 
  geom_line(aes(x = Year, y=consumption, group=place, 
                color = Country, linetype = Source)) +
  scale_color_manual(values = line_colors) +
  #guides(color=F) +
  theme_bw() + 
  theme(axis.line.x=element_line(color='black', size=0.5),
        axis.text=element_text(color='black'),
        axis.text.x=element_text(margin=margin(0, 0, 0, 0)),
        axis.text.y=element_text(margin=margin(0, 0, 0, 0)),
        axis.ticks=element_blank(),
        axis.title.y.right=element_text(margin=margin(0, 0, 0, 0)),
        #axis.title.y=element_text(margin=c(0, 0, 0, 0)),
        panel.grid.major.y=element_line(color=grid_color, size=grid_size),
        panel.grid.major.x=element_blank(),
        # geom_segment() will add vertical grid lines in later (minus 1990)
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.caption=element_text(margin=margin(6, 0, 0, 0), size=8),
  ) +
  geom_vline(xintercept=seq(1990, 2017, 5), color=grid_color, size=grid_size) +
  labs(title="Natural Gas and Total Energy Consumption, Scaled to 1990 Levels",
       x=NULL,
       y='Consumption, Percent of 1990 Levels',
       caption=expression(paste('Source: BP, ',
                                italic('Statistical Review of World Energy 2017.'),
                                ' Note that world totals exclude the US.'))) +
  scale_x_continuous(limits=c(1990, 2017),
                     expand = c(0,0),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(100, 200),
                     expand = c(0.0025,0),
                     breaks=seq(100, 200, 20)) 


png('Figures/natgas-scaled-consumption-graph-v3.png', w=800, h=500)
print(scaled_cons_plot_v3)
dev.off()

svg('Figures/natgas-scaled-consumption-graph-v3.svg', w=8, h=5)
print(scaled_cons_plot_v3)
dev.off()


