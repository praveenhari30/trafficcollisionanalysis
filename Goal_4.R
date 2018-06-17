## Goal 4: Motion chart depicting top 5 factors causing accidents and its variation over years
### <<<<<<This is continuation of Goal 3 file. Goal_3.R should be executed before executing this>>>>>>>>>

### 4 steps are involved in this visualisation
### Step 1: Execution of Goal_3.R 
### Step 2: Extracting subset of goal specific data from goal3 data 
### Step 3: Manipulation of data to match the plotting needs
### Step 4: Plotting the graph


################## Step 1: Execution of Goal_3.R ###########################

#Execute Goal3.R





########## Step 2: Extracting subset of goal specific data from goal3 data ##############

# Extracting year from Date column
goal3_data$year_group <- cut(goal3_data$Date, 
                             breaks = '1 year', 
                             right = FALSE)
levels(goal3_data$year_group) <- c('2015', '2016', '2017')

# summarising the count of accidents for each contributing factor yearwise
data_motion_chart <- 
  goal3_data %>% 
  group_by(year_group,Contributing_Factor_Vehicle) %>% 
  summarise('Count of Accidents' = n())





################ Step 3: Manipulation of data to match the plotting needs######################

#ungrouping data and qualifying year_group as integer to suit motion chart 'timevar' parameter
data_motion_chart <- ungroup(data_motion_chart)
data_motion_chart$year_group <- as.character(data_motion_chart$year_group)
data_motion_chart$year_group <- as.integer(data_motion_chart$year_group)





############################Step 4: Plotting the graph###############################

#Plot 4
plot4 <- gvisMotionChart(data = filter(data_motion_chart,Contributing_Factor_Vehicle %in% c('Illness of Driver','Improper Vehicle','Inadequate driving skills','Infrastructure Failure','Traffic Violation')),
                                idvar = 'Contributing_Factor_Vehicle',
                                timevar = 'year_group',
                                yvar = 'Count of Accidents',
                                options = list(height = 420,width = 500,showSidePanel=FALSE))
plot(plot4)
