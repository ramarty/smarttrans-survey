# Check Discrepencies of Coders

# Load Data --------------------------------------------------------------------
survey <- readRDS(file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey.Rds"))
survey$uid <- paste0(survey$road_id,"_",survey$segment_id)

uid_freq <- survey$uid %>% 
  table %>% 
  as.data.frame %>%
  dplyr::rename(uid = ".") 

uid_freq <- uid_freq[uid_freq$Freq %in% 2,]

survey_results_all <- lapply(uid_freq$uid, function(uid){
  survey_i <- survey[survey$uid %in% uid,]
  same_answer <- lapply(names(survey_i), function(var) survey_i[[var]][1] %in% survey_i[[var]][2]) %>% unlist
  
  survey_i_out <- t(survey_i) %>% as.data.frame
  names(survey_i_out) <- c("response_1", "response_2")
  survey_i_out$same_answer <- same_answer
  row.names(survey_i_out) <- 1:nrow(survey_i_out)
  survey_i_out$variable <- names(survey_i)
  survey_i_out$uid <- uid
  
  survey_i_out$supervisor_1 <- survey_i_out$response_1[survey_i_out$variable == "enum_name"]
  survey_i_out$supervisor_2 <- survey_i_out$response_2[survey_i_out$variable == "enum_name"]
  
  return(survey_i_out)
}) %>% bind_rows

# Adjust variables -------------------------------------------------------------
survey_results_all$road_id <- survey_results_all$uid %>% str_replace_all("_[[:digit:]]", "")
survey_results_all$variable_clean <- survey_results_all$variable %>% str_replace_all("_[[:digit:]]", "")

# Add in Full Variable Names ---------------------------------------------------
survey_var_names <- read_excel(file.path(project_file_path, "Hotspot Survey", "Survey Form", "iRAP-surveyform-2019-06-25.xlsx"), 1) %>% dplyr::select(name, label)
survey_var_names <- survey_var_names[!is.na(survey_var_names$label),]

survey_results_all <- merge(survey_results_all, survey_var_names, by.x="variable_clean", by.y="name", all.x=T, all.y=F)

paste(survey_results_all$road_id, survey_results_all$supervisor_1, survey_results_all$supervisor_2) %>% unique

# Subset -----------------------------------------------------------------------
variables_remove <- c("id", "SubmissionDate", "starttime",
                      "endtime", "deviceid", "subscriberid", 
                      "simid", "devicephonenum", "enum_name", 
                      "enum_id", "numroads", "road_id",
                      "test_count", "instanceID", "formdef_version",
                      "KEY", "roadnum", "gps.Latitude", "gps.Longitude",
                      "gps.Altitude", "gps.Accuracy", "landmarks",
                      "operate", "segment_id", "uid",
                      "street_crossings_count")

survey_results_all <- survey_results_all[!(survey_results_all$variable %in% variables_remove),]
survey_results_all <- survey_results_all[!grepl("picture|comment|gps_|how_many_street_crossings|flow", survey_results_all$variable),]

# ------------------------------------------------------------------------------
for(uid in unique(survey_results_all$uid)){
  
  #### Prep Dataframe 
  survey_results_all_i <- survey_results_all[survey_results_all$uid %in% uid,]
  survey_results_all_i <- survey_results_all_i[survey_results_all_i$same_answer %in% FALSE,]
  
  # Only look at results if there are some differences
  if(nrow(survey_results_all_i) > 0){
  
    supervisor_1 <- survey_results_all_i$supervisor_1[1] %>% tolower() %>% str_replace_all(" ","_")
    supervisor_2 <- survey_results_all_i$supervisor_2[1] %>% tolower() %>% str_replace_all(" ","_")
    
    survey_results_all_i$Q <- 1:nrow(survey_results_all_i)
    
    survey_results_all_i <- survey_results_all_i[,c("Q", "label", "response_1", "response_2", "supervisor_1", "supervisor_2")]
    
    names(survey_results_all_i)[names(survey_results_all_i) == "response_1"] <- supervisor_1
    names(survey_results_all_i)[names(survey_results_all_i) == "response_2"] <- supervisor_2
    
    survey_results_all_i <- survey_results_all_i %>% dplyr::select(-c(supervisor_1,supervisor_2))
    
    #### Export CSV
    survey_results_all_i_forcsv <- survey_results_all_i
    survey_results_all_i_forcsv$new_answer <- ""
    survey_results_all_i_forcsv$notes <- ""
    survey_results_all_i_forcsv$Q <- NULL
  
    write.csv(survey_results_all_i_forcsv, file.path(finaldata_file_path, "Hotspot Survey", "Individual Discrepencies", "CSV", paste0(uid, ".csv")), row.names=F)
    
    #### Export Table
    
    ## Prep Table
    t1 <- tableGrob(survey_results_all_i)
    title <- textGrob(uid,gp=gpar(fontsize=50))
    padding <- unit(5,"mm")
    
    table <- gtable_add_rows(
      t1, 
      heights = grobHeight(title) + padding,
      pos = 0)
    table <- gtable_add_grob(
      table, 
      title, 
      1, 1, 1, ncol(table))
    
    ## Export
    pdf(file.path(finaldata_file_path, "Hotspot Survey", "Individual Discrepencies", "PDF", paste0(uid, ".pdf")), 
        height=11, width=16)
    grid.draw(table)
    dev.off()
  }
  
}




