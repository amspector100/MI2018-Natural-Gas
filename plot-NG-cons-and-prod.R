# C:/Users/amspe/Documents/R/MI2018/Natural Gas or
# C:/Users/aspector/Documents/R Projects/Natural Gas

# ------------------------------------------------------------------
# This file deals purely with US production and consumption of natural gas. 
# There's another file which graphs a various statistics related to NG 
# consumption of the world and of the US. 
# ------------------------------------------------------------------


library(maggritr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)

# Rename columns
old_cols <- c("marketed_production", "total_consumption")
new_cols <- c("Marketed Production", "Total Consumption")

# Read and manipulate
ng_data <- read_csv("us_ng_prod_and_cons.csv") %>%
  rename_at(vars(old_cols), ~ new_cols) %>%
  gather('Variable', 'Value', -Year) %>%
  group_by(Variable) %>%
  mutate(Value = 100*Value/Value[Year == 1990])

# Set global plotting values
grid_color <- 'gray80'
grid_size <- 0.35

# Graph
gg <- ggplot(ng_data) + 
  geom_line(aes(x = Year, y = Value, color = Variable, group = Variable)) + 
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
  labs(x = NULL, y = 'Percent of 1990 Levels',
         title ="US Consumption and Marketed Production of Natural Gas, Scaled to 1990 Levels", 
        caption='Source: EIA Natural Gas Data') +
  scale_x_continuous(limits=c(1990, 2017),
                     breaks=seq(1900, 2015, 5)) +
  scale_y_continuous(limits=c(95, 160),
                     expand = c(0,0),
                     breaks=seq(100, 200, 20))

png('Figures/US-NG-cons-and-prod.png', w = 800, h = 500)
print(gg)
dev.off()

svg('Figures/US-NG-cons-and-prod.svg', w=8, h=5)
print(gg)
dev.off()


