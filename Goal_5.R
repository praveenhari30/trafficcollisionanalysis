## Goal 5:Geographic representation of Fatal accident zones in each Borough of New York city  
### <<<<<<Load the Master data before executing this.(Complete step 1 and step2 of Goal_1.R) >>>>>>>>>

### 4 steps are involved in this visualisation
### Step 1: Execute step 1 and step2 of Goal_1.R 
### Step 2: Extracting subset of goal specific data from Master data 
### Step 3: Manipulation of data to match the plotting needs
### Step 4: Plotting the graph





################Step 1: Execute step 1 and step2 of Goal_1.R ##########################

# Step 1: Execute step 1 and step2 of Goal_1.R





####### Step 2: Extracting subset of goal specific data from Master data###################
#Extracting required columns
map_data <- data.frame(master_data[c('Borough','Latitude','Longitude','Killed')],stringsAsFactors = FALSE)





########## Step 3: Manipulation of data to match the plotting needs##########################

#Filtering out non-fatal accidents
#Assumption: Killed = 0 as Fatal; Killed >0 as Non Fatal
map_data <- filter(map_data,!(is.na(map_data$Latitude & map_data$Longitude)) & !(Killed ==0))

#Labelling 'Borough' factor
map_data$Borough <- factor(map_data$Borough,
                           levels = c('BRONX','BROOKLYN','MANHATTAN','QUEENS','STATEN ISLAND',''),
                           labels = c('BRONX','BROOKLYN','MANHATTAN','QUEENS','STATEN ISLAND','Unspecified'))





############################## Step 4: Plotting the graph##################################

# Plot 5: Geographical representation of Fatal accident areas
map_plot <- get_googlemap(center = c(lon = -73.93,lat = 40.73),zoom = 11, maptype = 'roadmap')

plot5 <- ggmap(map_plot)+
         geom_point(data = map_data,aes(x = Longitude,y = Latitude),alpha = 0.5,col = 'red')+
         theme_bw(base_size = 15)+
         facet_wrap(~Borough)+
         labs(title = 'Geographical representation of Fatal accident zones in New York city', x = 'Longitude',y = 'Latitude')
plot5
