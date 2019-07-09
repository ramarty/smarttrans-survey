# Import and Clean Video Counting Discrepency Data

# Import and Clean Data --------------------------------------------------------
#### Import Data
counting_sheet <- gs_title("counting_v2")
vehicle_counts <- counting_sheet %>% gs_read(ws = "Vehicle Videos")
pedestrian_counts <- counting_sheet %>% gs_read(ws = "Pedestrian Videos")
footbridge_counts <- counting_sheet %>% gs_read(ws = "Footbridge Videos")

#### Add Variable Names
names(vehicle_counts) <- vehicle_counts[1,] %>% str_replace_all("[[:punct:]]", "") %>% str_replace_all(" ", "_") %>% tolower
vehicle_counts <- vehicle_counts[-1,]

names(pedestrian_counts) <- pedestrian_counts[1,] %>% str_replace_all("[[:punct:]]", "") %>% str_replace_all(" ", "_") %>% tolower
pedestrian_counts <- pedestrian_counts[-1,]

names(footbridge_counts) <- footbridge_counts[1,] %>% str_replace_all("[[:punct:]]", "") %>% str_replace_all(" ", "_") %>% tolower
footbridge_counts <- footbridge_counts[-1,]

#### Add Unique ID
vehicle_counts$id <- paste0(vehicle_counts$video_name, "_", vehicle_counts$road_segment_id)
pedestrian_counts$id <- paste0(pedestrian_counts$video_name, "_", pedestrian_counts$road_segment_id)
footbridge_counts$id <- footbridge_counts$video_name

# Discrepencies ----------------------------------------------------------------
df <- vehicle_counts
id <- vehicle_counts$id[1]

video_constant_vars <- c("video_name", "supervisor_doing_counting", "road_segment_id", "id")

long_to_wide <- function(id, df){
  vehicle_counts_i <- vehicle_counts[vehicle_counts$id %in% id,]
  
  lapply(1:nrow(vehicle_counts_i), function(){
    df_i <- vehicle_counts_i[i,]
    
    df_i_constantvars <- df_i[,video_constant_vars]
    
    df_i_datavars <- df_i[,!(names(df_i) %in% video_constant_vars)]
    df_i_datavars$id <- df_i_constantvars$id
    df_i_datavars <- df_i_datavars %>% gather('question', "value", -id)
    
    df_out <- merge(df_i_datavars, df_i_constantvars, by="id")
    names(df_out)[names(df_out) == "value"] <- paste0("value_", i)
    names(df_out)[names(df_out) == "supervisor_doing_counting"] <- paste0("supervisor_doing_counting_", i)
  
    
  })
  
  
  
}

vehicle_counts$video_name


View(vehicle_counts)



