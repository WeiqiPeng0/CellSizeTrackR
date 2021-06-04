#install.packages("beeswarm")
library("beeswarm")
library(dplyr)

filepath <- file.choose()
theData <- read.csv(filepath) 

# TODO: modify parameters below
dot_size = 0.03 # adjust the dot size in the plot
dot_space = 0.9 # adjust dot space
field_num = 1   # TODO: select field number
time_range_l = 1  # TODO: time point range left
time_range_r = 19  # TODO: time point range right
xlabel = "Time Point (h)"
ylabel = "Cell Size"
graph_label = "IVIG"
cellsize_threshold = 300 # set the upper threshold for cell size to display in the graph

# 1. filtering, based on field number&time range
subset <- theData %>% filter(Field.Number==field_num) %>% 
                  filter(Time.Point >= time_range_l) %>% 
                  filter(Time.Point <= time_range_r) %>%
                  filter(Cell.Size <= cellsize_threshold)

# 2. or select a range of row numbers 
# subset <- theData %>% slice(1:1000)

# Plotting
beeswarm(Cell.Size~Time.Point, data=subset,  pch=19, method="swarm",
         cex=dot_size, xlab=xlabel, ylab=ylabel, 
         main=graph_label, spacing=dot_space, xaxt="n") 
axis(1, at=c(time_range_l:time_range_r), labels=NULL, cex.axis=0.8)
