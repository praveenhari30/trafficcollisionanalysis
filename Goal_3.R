## Goal 3: Summary of factors causing accidents

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
library(googleVis)

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

# extracting the subset of data required for the Goal 3
# Filtering out 'Unspecified Factors

goal3_data <-
  master_data[c(1,2,3, 11, 12, 19)] %>% 
  filter(., !(master_data$Contributing_Factor_Vehicle %in% c('Unspecified', '')))

# Regrouping 'Contributing_Factor_Vehicle' column in to 10 common factors

goal3_data$Contributing_Factor_Vehicle <- as.character(goal3_data$Contributing_Factor_Vehicle)
goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Alcohol Involvement',
                                                                                     'Drugs (Illegal)')] <- 'Alcohol Involvement'
  
goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Other Electronic Device',
                                                                                     'Cell Phone (hands-free)',
                                                                                     'Cell Phone (hand-held)')] <- 'Cellphone Involvement'

goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Illness',
                                                                                      'Fatigued/Drowsy',
                                                                                      'Physical Disability',
                                                                                      'Lost Consciousness',
                                                                                      'Prescription Medication')] <- 'Illness of Driver'
goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Other Vehicular',
                                                                                     'Brakes Defective',
                                                                                     'Glare',
                                                                                     'Steering Failure',
                                                                                     'Oversized Vehicle',
                                                                                     'Accelerator Defective',
                                                                                     'Tow Hitch Defective',
                                                                                     'Tire Failure/Inadequate',
                                                                                     'Headlights Defective',
                                                                                     'Other Lighting Defects',
                                                                                     'Windshield Inadequate')] <- 'Improper Vehicle'

goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Driver Inexperience',
                                                                                     'Driver Inattention/Distraction',
                                                                                     'Outside Car Distraction',
                                                                                     'Fell Asleep',
                                                                                     'Passenger Distraction')] <- 'Inadequate driving skills'

goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('View Obstructed/Limited',
                                                                                     'Passing or Lane Usage Improper',
                                                                                     'Obstruction/Debris',
                                                                                     'Pavement Slippery',
                                                                                     'Pavement Defective',
                                                                                     'Lane Marking Improper/Inadequate',
                                                                                     'Traffic Control Device Improper/Non-Working',
                                                                                     'Shoulders Defective/Improper')] <- 'Infrastructure Failure'
goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Unsafe Speed',
                                                                                     'Aggressive Driving/Road Rage')] <- 'Over Speeding'

goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Unsafe Lane Changing',
                                                                                     'Traffic Control Disregarded',
                                                                                     'Failure to Yield Right-of-Way',
                                                                                     'Backing Unsafely',
                                                                                     'Turning Improperly',
                                                                                     'Following Too Closely',
                                                                                     'Failure to Keep Right')] <- 'Traffic Violation'
goal3_data$Contributing_Factor_Vehicle[goal3_data$Contributing_Factor_Vehicle %in% c('Reaction to Other Uninvolved Vehicle',
                                                                                     'Animals Action',
                                                                                     'Pedestrian/Bicyclist/Other Pedestrian Error/Confusion')] <- 'Unanticipated Collision'

# Qualifying modified 'Contributing_Factor_Vehicle' column as factor
goal3_data$Contributing_Factor_Vehicle <- as.factor(goal3_data$Contributing_Factor_Vehicle)

# Extracting hour of accidents from Time Column and grouping by it
goal3_data <- 
  goal3_data %>%  
  mutate(.,hour_group = cut(goal3_data$Time,
                            breaks = '1 hour',
                            labels = paste(0:23,1:24,sep = '-'),
                            right = FALSE)) %>% 
  group_by(hour_group)

############ Step 4: Manipulation of data to match the plotting needs#######################

#Calculating the % of each factor towards causing an accident
percent_data <- 
  goal3_data %>% 
  group_by(Contributing_Factor_Vehicle) %>% 
  summarise(count = n()) %>% 
  mutate(Factor_Percent = round(count*100/sum(count),2))





###########################Step 5: Plotting the graph#############################

#Plot3:Percentage of factor causing an accident

plot3 <- ggplot(data = percent_data,aes(x = Factor_Percent,y = Contributing_Factor_Vehicle))+
        geom_point(size = 5)+
        geom_segment(aes(x=0,xend = Factor_Percent,yend = Contributing_Factor_Vehicle),lwd =1.2)+
        geom_vline(aes(xintercept = max(Factor_Percent)),linetype = 'dotted',col = 'red',lwd = 1.5)+
        theme_gray(base_size = 15)+
        labs(title="Summary of Factors causing Accidents",x = 'Percentage',y = 'Factors')+
        scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40),labels = paste0(seq(0,40,5),'%'))
plot3
