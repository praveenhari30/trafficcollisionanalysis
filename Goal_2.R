## Goal 2: Visualizing the relationship between time of an accident and fatality rate

### 5 steps are involved in this visualisation
### Step 1: importing Master data
### Step 2: Data cleansing 
### Step 3: Extracting subset of goal specific data from master data 
### Step 4: Manipulation of data to match the plotting needs
### Step 5: Plotting the graph





#################### Step 1: importing Master data ##################################

#importing required libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotrix)
library(RColorBrewer)

# importing master data
master_data <-
  read.csv(
    'C:/Users/VINOD/Desktop/GitWit/R-project-001/Data/Master_Collision_Data.csv',
    header = TRUE
  )




######################### Step 2: Data cleansing ##################################

# Exporing the structure of imported data
str(master_data)

# qualify 'Date' and 'Time' columns as R date and Time
master_data$Date <- as.Date(master_data$Date, '%m/%d/%Y')
master_data$Time <- as.POSIXct(strptime(master_data$Time, 
                                        format = '%H:%M'), tz = "")

#Renaming the column names
names(master_data) <-
  c(
    'Date',
    'Time',
    'Borough',
    'Zip_code',
    'Latitude',
    'Longitude',
    'Location',
    'On_Street_Name',
    'Cross_Street_Name',
    'Off_Street_Name',
    'Injured',
    'Killed',
    'Ped_Injured',
    'Ped_Killed',
    'Cyc_Injured',
    'Cyc_Killed',
    'Mot_Injured',
    'Mot_Killed',
    'Contributing_Factor_Vehicle',
    'Unique_Key',
    'Vehicle_Type_code'
  )





###############Step 3: Extracting subset of goal specific data from master data################

# extracting the subset of data required for the Goal 2
goal2_data <- master_data[c('Date', 'Time', 'Injured', 'Killed', 'Unique_Key')]

# Extracting hour, month and year of accidents from Date and Time Columns
goal2_data$month_group <- strftime(goal2_data$Date, '%B')
goal2_data$month_group <- factor(goal2_data$month_group,
                                 levels  = c('January',
                                              'February',
                                              'March',
                                              'April',
                                              'May',
                                              'June',
                                              'July',
                                              'August',
                                              'September',
                                              'October',
                                              'November',
                                              'December'))
goal2_data$year_group <- cut(goal2_data$Date, 
                             breaks = '1 year', 
                             right = FALSE)
levels(goal2_data$year_group) <- c('2015', '2016', '2017')
goal2_data$hour_group <- cut(goal2_data$Time,
                             breaks = '1 hour',
                             right = FALSE,
                             labels = paste(0:23, 1:24, sep = '-'))

#Categorising whether an accident is Fatal or Non-Fatal based on casualty of an accident
#Assumption: Killed = 0 as Fatal; Killed >0 as Non Fatal
goal2_data$is_fatal <- cut(goal2_data$Killed,
                           breaks = c(-Inf, 0, Inf),
                           labels = c('Non Fatal', 'Fatal'))





#################Step 4: Processing subset of data to match the plotting needs###################

# sorting dataset by date in ascending order
goal2_data <- arrange(goal2_data, Date)

#creating dataset with total number of accidents and number of killed in each hour
#Each row in a dataset represents one accident
count_data <- data.frame(ddply(goal2_data, .(hour_group), function(x) {x %>% summarise(n())}),
                         ddply(goal2_data, .(hour_group), function(x) {sum(x$Killed)})[2])

#renaming the column names
names(count_data) <-c('hour_group', 'accidents_count', 'fatal_count')


# Calculating Fatality proporation per 10000 accidents for each hour
count_data <- 
  count_data %>% 
  mutate(prop_per_10000 = round((fatal_count / accidents_count) *10000, 2))

# Calculating % of accidents and deaths for each hour
count_data <-
  mutate(
    count_data,
    percentage_accidents = count_data$accidents_count * 100 / sum(count_data$accidents_count),
    percentage_fatal = count_data$fatal_count * 100 / sum(count_data$fatal_count))





###########################Step 5: Plotting the graph#############################

# Plot 2.1 Plotting only Fatal accidents
plot2dot1 <-
  ggplot(subset(goal2_data, !(Killed %in% 0)), aes(hour_group)) +
  geom_bar(aes(fill = year_group), position = position_stack()) +
  theme_bw(base_size = 15) +
  geom_smooth(
    data = count_data,
    aes(x = 1:24, y = fatal_count),
    method = 'auto',
    se = TRUE,
    span = 0.3
  ) +
  labs(x = 'Time Interval(1 hour)', y = 'Number of Fatalities', title = 'Fatality rate for each hour by year')

# plot 2.2 = Plotting all accidents

plot2dot2 <- ggplot(goal2_data, aes(hour_group)) +
  geom_bar(aes(fill = year_group), position = position_stack()) +
  theme_bw(base_size = 15) +
  geom_smooth(
    data = count_data,
    aes(x = 1:24, y = accidents_count),
    method = 'auto',
    se = TRUE,
    span = 0.3
  ) +
  labs(x = 'Time Interval(1 hour)', y = 'Number of Accidents', title = 'Number of accidents in each hour by year')

# Comparison of both the plots
grid.arrange(plot2dot2, plot2dot1, nrow = 2)

#Pie plot depicting Fatality proportion
plot2dot3 <- pie3D(count_data$prop_per_10000,
                  theta = 1.3,
                  col = topo.colors(n = 24),
                  labels = paste0(count_data$hour_group, '(', count_data$prop_per_10000, ')'),
                  radius = 1.1,
                  labelcex = 0.7,
                  shade = 0.5,
                  main = "Proportion of Mortality per 10000 accidents grouped by each hour",
                  explode = 0.1)
plot2dot3

# Time series of Accidents % and Fatality %
plot2dot4 <- ggplot(count_data, aes(x = count_data$hour_group, group = 1)) + 
             geom_line(aes(y = count_data$percentage_accidents, col = "Accidents %"),lwd = 1.5) + 
             geom_line(aes(y = count_data$percentage_fatal, col = 'Fatal %'), lwd = 1.5) +
             theme_minimal(15) + 
             labs(title = 'Time Series of Accidents and Fatalities percentage', x = 'Time Interval (1 hour)', y = 'Percentage %') +
             scale_y_continuous(breaks = seq(1, 8, 0.5), labels = paste(seq(1, 8, 0.5), '%'))
plot2dot4
