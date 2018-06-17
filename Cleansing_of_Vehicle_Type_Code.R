# Importing Master data into R
Master_collision_data <- read.csv('C:/Users/VINOD/Downloads/NYPD_Motor_Vehicle_Collisions.csv',header = TRUE)
head(Master_collision_data,n=10)
str(Master_collision_data)

#singling out Vehicle collision data
Vehicle_collision <- Master_collision_data[21:25]
str(Vehicle_collision)

# Cleansing Vehicle Type code Field
for (i in 1:nrow(Vehicle_collision)){
  if(Vehicle_collision[i,1] %in% c('','OTHER','UNKNOWN')){
    if(!(Vehicle_collision[i,2] %in% c('','OTHER','UNKNOWN'))){
      Vehicle_collision[i,1] = Vehicle_collision[i,2] 
    }
    else {
      if(!(Vehicle_collision[i,3] %in% c('','OTHER','UNKNOWN'))){
        Vehicle_collision[i,1] = Vehicle_collision[i,3] 
      }
      else{
        if(!(Vehicle_collision[i,4] %in% c('','OTHER','UNKNOWN'))){
          Vehicle_collision[i,1] = Vehicle_collision[i,4] 
        }
        else{
          if(!(Vehicle_collision[i,5] %in% c('','OTHER','UNKNOWN'))){
            Vehicle_collision[i,1] = Vehicle_collision[i,5] 
          }
        }
      }
    }
  } 
}

Vehicle_collision <- Vehicle_collision[-c(2,3,4,5)]
names(Vehicle_collision) <- 'Vehicle_Type_code'

#Binding cleansed vehicle collision with the Master data
Master_collision_data <- cbind(Master_collision_data,Vehicle_collision)

#dropping unnecessary 'Type Code columns' from the Master data
Master_collision_data <- Master_collision_data[-c(21,22,23,24,25)]

#Renaming the columns of Master data
names(Master_collision_data) <- c('Date','Time','Borough','Zip_code','Latitude','Longitude','Location','On_Street_Name','Cross_Street_Name','Off_Street_Name','#_of_Injured','#_of_Killed','#_of_Ped_Injured','#_of_Ped_Killed','#_of_Cyc_Injured','#_of_Cyc_Killed','#_of_Mot_Injured','#_of_Mot_Killed','Contributing_Factor_Vehicle','Unique_Key','Vehicle_Type_code')

#Exporting the cleansed dataframe to csv file

write.csv(Master_collision_data,'C:/Users/VINOD/Desktop/Master_Collision_Data.csv',row.names = FALSE,col.names = TRUE)
