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
video_constant_vars <- c("video_name", "supervisor_doing_counting", "road_segment_id", "id")
pedestrian_constant_vars <- c("video_name", "supervisor_doing_counting", "road_segment_id", "id")

df = pedestrian_counts
id = pedestrian_counts$id[1]
constant_vars = pedestrian_constant_vars

long_to_wide <- function(id, df, constant_vars){
  df_i <- df[df$id %in% id,]
  
  coders_df <- lapply(1:nrow(df_i), function(i){
    df_i <- df_i[i,]
    
    df_i_constantvars <- df_i[,constant_vars]
    
    df_i_datavars <- df_i[,!(names(df_i) %in% constant_vars)]
    df_i_datavars$id <- df_i_constantvars$id
    df_i_datavars <- df_i_datavars %>% gather('question', "value", -id)
    
    df_out <- merge(df_i_datavars, df_i_constantvars, by="id")
    names(df_out)[names(df_out) == "value"] <- paste0("value_", i)
    names(df_out)[names(df_out) == "supervisor_doing_counting"] <- paste0("supervisor_doing_counting_", i)
    
    return(df_out)
  })
  
  coders_merged <- merge(coders_df[[1]], coders_df[[2]], by=c("id", "video_name", "road_segment_id", "question"))
  
  return(coders_merged)
}

vehicle_counts_df <- lapply(unique(vehicle_counts$id), long_to_wide, vehicle_counts, video_constant_vars) %>% bind_rows
pedestrian_counts_df <- lapply(unique(pedestrian_counts$id), long_to_wide, pedestrian_counts, pedestrian_constant_vars) %>% bind_rows

counts_df <- bind_rows(vehicle_counts_df, pedestrian_counts_df)

# Clean Values -----------------------------------------------------------------
# Video Duration
counts_df$value_1[vehicle_counts_df$question == "video_duration_minutes_and_seconds"] <- 
  counts_df$value_1[vehicle_counts_df$question == "video_duration_minutes_and_seconds"] %>%
    lapply(function(str){
      time_m_s <- str %>% strsplit(":") %>% unlist %>% as.numeric
      time_m <- time_m_s[1] + time_m_s[2]/60
      return(time_m)
    }) %>% unlist

counts_df$value_2[vehicle_counts_df$question == "video_duration_minutes_and_seconds"] <- 
  counts_df$value_2[vehicle_counts_df$question == "video_duration_minutes_and_seconds"] %>%
  lapply(function(str){
    time_m_s <- str %>% strsplit(":") %>% unlist %>% as.numeric
    time_m <- time_m_s[1] + time_m_s[2]/60
    return(time_m)
  }) %>% unlist

# Video Time of Day
counts_df$value_1[vehicle_counts_df$question == "video_time_of_day"] <- 
  counts_df$value_1[vehicle_counts_df$question == "video_time_of_day"] %>%
  lapply(function(str){
    time_m_s <- str %>% strsplit(":") %>% unlist %>% as.numeric
    time_m <- time_m_s[1] + time_m_s[2]/60
    return(time_m)
  }) %>% unlist

counts_df$value_2[vehicle_counts_df$question == "video_time_of_day"] <- 
  counts_df$value_2[vehicle_counts_df$question == "video_time_of_day"] %>%
  lapply(function(str){
    time_m_s <- str %>% strsplit(":") %>% unlist %>% as.numeric
    time_m <- time_m_s[1] + time_m_s[2]/60
    return(time_m)
  }) %>% unlist

# Check Discrepencies ----------------------------------------------------------
counts_df$value_1 <- as.numeric(counts_df$value_1)
counts_df$value_2 <- as.numeric(counts_df$value_2)

counts_df$value_1[is.na(counts_df$value_1)] <- 0
counts_df$value_2[is.na(counts_df$value_2)] <- 0

counts_df <- counts_df[!(counts_df$question %in% c("video_duration_minutes_and_seconds", "video_time_of_day")),]
counts_df$difference <- abs(counts_df$value_1 - counts_df$value_2)



