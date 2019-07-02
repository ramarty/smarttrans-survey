# Check Discrepencies of Coders

# Load Data --------------------------------------------------------------------
survey <- readRDS(file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey.Rds"))

#keep July 2 date only
survey <- survey[grepl("Jul 2, 2019", survey$SubmissionDate),]

survey$uid <- paste0(survey$road_id,"_",survey$segment_id)

uid_freq <- survey$uid %>% 
  table %>% 
  as.data.frame %>%
  dplyr::rename(uid = ".") 

uid_freq <- uid_freq[uid_freq$Freq %in% 2,]

survey_results_all <- lapply(uid_freq$uid, function(uid){
  survey_i <- survey[survey$uid %in% uid,]
  out <- lapply(names(survey_i), function(var) survey_i[[var]][1] %in% survey_i[[var]][2]) %>% unlist
  out <- as.data.frame(t(out))
  names(out) <- names(survey_i)
  return(out)
}) %>% bind_rows

vars <- names(survey_results_all)
survey_results_all$id <- 1

collapse_formula <- as.formula(paste(paste(vars, collapse = " + "), " ~ id"))


survey_results <- summaryBy(collapse_formula, data=survey_results_all, FUN=mean, keep.names=T)
survey_results <- survey_results %>% dplyr::select(-c(id, SubmissionDate, starttime,
                                                      endtime, deviceid, subscriberid, 
                                                      simid, devicephonenum, enum_name, 
                                                      enum_id, numroads, road_id,
                                                      test_count, instanceID, formdef_version,
                                                      KEY, roadnum, gps.Latitude, gps.Longitude,
                                                      gps.Altitude, gps.Accuracy, landmarks,
                                                      operate, segment_id, uid,
                                                      street_crossings_count, how_many_street_crossings,
                                                      volume, ped_cross_2, ped_cross_intersect_2, ped_cross_quality_2, ped_fence_2))
survey_results <- survey_results %>% 
  t %>%
  as.data.frame %>%
  dplyr::rename(proportion_same_answer = 1)
survey_results$variable <- row.names(survey_results)
row.names(survey_results) <- 1:nrow(survey_results)

survey_results <- survey_results[!grepl("comment|picture|Latitude|Longitude|Altitude|Accuracy|flow", survey_results$variable),]

survey_results$variable <- survey_results$variable %>% str_replace_all("_[[:digit:]]$","")

# Add in Full Variable Names ---------------------------------------------------
survey_var_names <- read_excel(file.path(project_file_path, "Hotspot Survey", "Survey Form", "iRAP-surveyform-2019-06-25.xlsx"), 1) %>% dplyr::select(name, label)
survey_var_names <- survey_var_names[!is.na(survey_var_names$label),]

survey_results <- merge(survey_results, survey_var_names, by.x="variable", by.y="name", all.x=T, all.y=F)

# Export -----------------------------------------------------------------------
survey_results <- survey_results[,c("variable", "label", "proportion_same_answer")]
write.csv(survey_results, file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey_discrepency_results.csv"), row.names=F)


survey_results$proportion_same_answer %>% mean
