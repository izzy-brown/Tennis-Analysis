library(readxl)
video_analysis <- read_excel("~/Documents/MSc Sport Data Analytics/B1702 - Video Analysis in Sport/video analysis.xlsx")
View(video_analysis) 

library(ggplot2)
library(tidyverse)         

library(tidyr)

video_analysis_long <- video_analysis %>%
  pivot_longer(cols = c(returns_made, points_won), names_to = "metric", values_to = "value")

library(ggplot2)

video_analysis_long %>%
  ggplot(aes(x = Set, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +  # Sets the y-axis range from 0 to 100
  labs(
    title = "Kyrgios’ Return Performance by Set",
    x = "Set",
    y = "Percentage (%)",
    fill = NULL  # Title of the legend
  ) +
  scale_fill_manual(
    values = c("returns_made" = "cadetblue4", "points_won" = "chartreuse2"),
    labels = c("Points Won", "Returns Made")  # New legend labels
  ) +
  theme_minimal()


library(readxl)
video_analysis2 <- read_excel("~/Documents/MSc Sport Data Analytics/B1702 - Video Analysis in Sport/video analysis.xlsx", 
                             sheet = "Sheet2")
View(video_analysis)

library(tidyverse)
library(ggplot2)

ggplot(video_analysis2, aes(x = Set, y = second_serve)) +
  geom_col(fill = "chartreuse2") +
  scale_y_continuous(limits = c(0, 100)) +  # Sets the y-axis range from 0 to 100
  labs(title = "Percentage of second serve returns won", x = "Set", y = "Percentage (%)") +
  theme_minimal()


ggplot(video_analysis2, aes(x = Set, y = second_serve)) +
  geom_point(color = "chartreuse2", size = 3) +  # Adds scatter points
  scale_y_continuous(limits = c(0, 100)) +  # Sets the y-axis range from 0 to 100
  geom_line(color = "chartreuse2", group = 1) +  # Adds a line connecting the points
  labs(
    title = "Percentage of Second Serve Returns Won",
    x = "Set",
    y = "Percentage (%)"
  ) +
  theme_minimal()


# Load necessary library
library(ggplot2)

# Create a data frame with the results
results <- data.frame(
  Location = c("Service Box", "Behind Service Line", "Back Quarter"),
  Percent_Won = c(24, 35, 48)
)

# Create the bar chart
ggplot(data = results, aes(x = Location, y = Percent_Won)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip()+
  labs(title = "Percentage of Returns Won by Court Location",
       x = "Court Location",
       y = "Percentage Won") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


# Create the bar chart with bigger text labels
ggplot(data = results, aes(x = Location, y = Percent_Won)) +
  geom_bar(stat = "identity", width = 0.5) + # Adjust the width for thinner bars
  coord_flip() +
  labs(title = "Percentage of Returns Won by Court Location",
       x = "Court Location",
       y = "Percentage Won") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(
    axis.text = element_text(size = 14),       # Increase axis text size
    axis.title = element_text(size = 16),      # Increase axis title size
    plot.title = element_text(size = 18)  # Increase plot title size
  )


# Load necessary library
library(ggplot2)

# Create a data frame with the results
results <- data.frame(
  Location = c("Service Box", "Behind Service Line", "Back Quarter"),
  Percent_Won = c(24, 35, 48)
)

# Create the bar chart without grid lines and with bigger text labels
ggplot(data = results, aes(x = Location, y = Percent_Won)) +
  geom_bar(stat = "identity", width = 0.65, position = position_dodge(width = 1)) +  # Adjust the width for thinner bars
  labs(x = "Court Location",
       y = "Percentage Won") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(
    axis.text = element_text(size = 14),       # Increase axis text size
    axis.title = element_text(size = 16),      # Increase axis title size
    panel.grid = element_blank()               # Remove grid lines
  )

# Data for the pie chart
sets <- c('1st Set', '2nd Set', '3rd Set', '4th Set')
points_won_percentage <- c(20, 50, 0, 33)

# Creating the pie chart
pie(points_won_percentage, labels = paste(sets, "\n", points_won_percentage, "%"),
    col = c("#FF9999", "#66B3FF", "#99FF99", "#FFCC99"),
    main = "Points Won Against Second Serves in Each Set")

# Data for the pie chart
return_types <- c('Forehand Returns', 'Backhand Returns')
success_percentages <- c(18.6, 33.3)

# Creating the pie chart
pie(success_percentages, labels = paste(return_types, "\n", success_percentages, "%"),
    col = c("cadetblue4", "chartreuse2"),
    main = "Percentage of Successful Forehand and Backhand Returns")

# Data for backhand returns
backhand_labels <- c('Successful Backhand Returns', 'Unsuccessful Backhand Returns')
backhand_percentages <- c(33.3, 66.7)

# Creating the backhand pie chart
pie(backhand_percentages, labels = paste(backhand_labels, "\n", backhand_percentages, "%"),
    col = c("chartreuse2", "cadetblue4"),
    main = "Backhand Return Success")

# Data for forehand returns
forehand_labels <- c('Successful Forehand Returns', 'Unsuccessful Forehand Returns')
forehand_percentages <- c(18.6, 81.4)

# Creating the forehand pie chart
pie(forehand_percentages, labels = paste(forehand_labels, "\n", forehand_percentages, "%"),
    col = c("chartreuse2", "cadetblue4"),
    main = "Forehand Return Success")

#-----------just for second set-----------#

# Data for backhand returns
backhand_labels <- c('Successful Backhand Returns', 'Unsuccessful Backhand Returns')
backhand_percentages <- c(63.6, 36.4)

# Creating the backhand pie chart
pie(backhand_percentages, labels = paste(backhand_labels, "\n", backhand_percentages, "%"),
    col = c("chartreuse2", "cadetblue4"),
    main = "Backhand Return Success")

# Data for forehand returns
forehand_labels <- c('Successful Forehand Returns', 'Unsuccessful Forehand Returns')
forehand_percentages <- c(30, 70)

# Creating the forehand pie chart
pie(forehand_percentages, labels = paste(forehand_labels, "\n", forehand_percentages, "%"),
    col = c("chartreuse2", "cadetblue4"),
    main = "Forehand Return Success")



# Data for the pie chart
errors <- c(33.3, 66.7)
error_types <- c("Forced Errors", "Unforced Errors")

# Create a pie chart
pie(errors, labels = error_types, main = "Errors When Returning a Second Serve",
    col = c("chartreuse2", "cadetblue4"))




  