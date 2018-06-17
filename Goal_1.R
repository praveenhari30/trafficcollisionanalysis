## Goal 1: Hour of the day when the most number of accidents happened

### 5 steps are involved in this visualisation
### Step 1: importing Master data
### Step 2: Data cleansing 
### Step 3: Extracting subset of goal specific data from master data 
### Step 4: Manipulation of data to match the plotting needs
### Step 5: Plotting the graph




#################### Step 1: importing Master data ##################################

# importing required Libraries
library(plyr)
library(dplyr)
library(ggplot2)

# importing master data 
master_data <- read.csv('C:/Users/VINOD/Desktop/GitWit/R-project-001/Data/Master_Collision_Data.csv',
                        header = TRUE)





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

# extracting the subset of data required for the Goal 1
goal1_data <- master_data[c('Date', 'Time', 'Unique_Key', 'Injured', 'Killed')]

# Extracting hour and year of accidents from Date and Time Columns
goal1_data$hour_group <-cut(goal1_data$Time,
                            breaks = '1 hour',
                            right = FALSE,
                            labels = paste(0:23, 1:24, sep = '-'))
goal1_data$year_group <- cut(goal1_data$Date,
                             breaks = '1 year',
                             right = FALSE,
                             labels = sort(as.numeric(unique(strftime(goal1_data$Date, '%Y')))))





####################Step 4: Manipulation of data to match the plotting needs###################

#Capturing maximum number of accidents that happened in an hour for each year
year_max_records <- goal1_data %>% 
                    group_by(year_group, hour_group) %>% 
                    summarise(count_of_records = n()) %>% 
                    summarise(max_hour_count = max(count_of_records))





###########################Step 5: Plotting the graph#############################

#plot 1: Count of accidents in each hour by year
plot1 <- ggplot(goal1_data, aes(hour_group)) +
  geom_bar(aes(fill = hour_group)) +
  theme_bw(base_size = 15) +
  facet_wrap( ~ year_group, nrow = 3) +
  geom_hline(
    data = year_max_records,
    aes(yintercept = max_hour_count),
    size = 1.2,
    col = 'red',
    linetype = 'dotdash'
  ) +
  labs(x = 'Time interval(1 hour)', y = 'count of accidents', title = 'Count of accidents in each hour by year')

plot1
