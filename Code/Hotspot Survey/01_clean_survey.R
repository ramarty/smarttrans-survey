# smarTTrans Master

# 1. At end, account for logic. If say no intersection, make everything intersection related false

# Load Data --------------------------------------------------------------------
survey <- read.csv(file.path(rawdata_file_path, "Hotspot Survey", "road_coding_v5_20190711.csv"))

# Deal with Severity Objects ---------------------------------------------------
object_vars <- names(survey)[grepl("object", names(survey))]
object_vars <- object_vars[grepl("_[[:digit:]]_|_[[:digit:]][[:digit:]]_", object_vars)]
survey <- survey[,!(names(survey) %in% object_vars)]

# Stack Segments ---------------------------------------------------------------
common_vars <- which(!grepl("_[[:digit:]]$", names(survey)))
segment_ids <- names(survey)[grepl("roadnum", names(survey))] %>% str_replace_all("[[:alpha:]]|[[:punct:]]", "") %>% as.numeric
repeat_group_vars <- c("gps_pedcrossing", "ped_cross", "ped_cross_quality", "ped_cross_intersect", "ped_fence")
repeat_group_vars_coll <- repeat_group_vars %>% paste(collapse="|")

survey_stacked <- lapply(segment_ids, function(seg_id_i){
  
  if(paste0("roadnum_",seg_id_i+1) %in% names(survey)){
    segment_vars <- which(names(survey) == paste0("roadnum_",seg_id_i)):(which(names(survey) == paste0("roadnum_",seg_id_i+1))-1)
  } else{
    segment_vars <- which(names(survey) == paste0("roadnum_",seg_id_i)):(which(names(survey) == "instanceID")-1)
  }
    
  survey_s <- survey[,c(common_vars, segment_vars)]
  
  # Change names: deal with repeat groups differently. #_roadid_repeatid
  names(survey_s)[!grepl(repeat_group_vars_coll, names(survey_s))] <- names(survey_s)[!grepl(repeat_group_vars_coll, names(survey_s))] %>% str_replace_all("_[[:digit:]]$", "")
  names(survey_s)[grepl(repeat_group_vars_coll, names(survey_s))] <- names(survey_s)[grepl(repeat_group_vars_coll, names(survey_s))] %>% str_replace_all("_[[:digit:]]_", "_")
  
  survey_s$segment_id <- seg_id_i
  
  for(var in names(survey_s)[grepl("object", names(survey_s))]){
    survey_s[[var]] <- survey_s[[var]] %>% as.character
  }
  
  return(survey_s)
}) %>% bind_rows

# Apply Labels -----------------------------------------------------------------
survey_stacked_nolabels <- survey_stacked

survey_choices <- read_excel(file.path(project_file_path, "Hotspot Survey", "Survey Form", "iRAP-surveyform-2019-07-02.xlsx"), 1) %>% dplyr::select(type, name)
survey_form <- read_excel(file.path(project_file_path, "Hotspot Survey", "Survey Form", "iRAP-surveyform-2019-07-02.xlsx"), 2)

survey_choices <- survey_choices[grepl("select_one", survey_choices$type),]
survey_choices$type <- survey_choices$type %>% str_replace_all("select_one ", "")

for(var in names(survey_stacked)){
  type_i <- survey_choices$type[survey_choices$name == var]
  
  if(length(type_i) > 0){
    value_labels_i <- survey_form[survey_form$list_name == type_i,]
    
    survey_stacked[[var]] <- factor(survey_stacked[[var]],
                                    levels = value_labels_i$name,
                                    labels = value_labels_i$label)
  }
  
}

for(var in repeat_group_vars){
  type_i <- survey_choices$type[survey_choices$name == var]
  
  if(length(type_i) > 0){
    value_labels_i <- survey_form[survey_form$list_name == type_i,]
    
    var_i <- names(survey_stacked)[grepl(paste0(var,"_[[:digit:]]"), names(survey_stacked))]
    
    for(var_ij in var_i){
      survey_stacked[[var_ij]] <- factor(survey_stacked[[var_ij]],
                                      levels = value_labels_i$name,
                                      labels = value_labels_i$label)
    }
    

  }
}

# Replace Road Severity Labels with IDs ----------------------------------------
survey_form_objs <- survey_form[survey_form$list_name %in% "roadside_passobj",]
object_vars <- names(survey_stacked)[grepl("object", names(survey_stacked))]

for(var in object_vars){
  
  for(i in 1:nrow(survey_form_objs)){
    survey_form_objs_i <- survey_form_objs[i,]
    
    id <- paste0("\\b", survey_form_objs_i$name, "\\b")
    label <- survey_form_objs_i$label
    
    survey_stacked[[var]] <- survey_stacked[[var]] %>% str_replace_all(id, label)

  }
}

#grepl("object")
#names(survey_stacked)

# Account for Survey Logic -----------------------------------------------------
survey_stacked$inter_channel[survey_stacked$inter_type == "None"] <- NA
survey_stacked$volume[survey_stacked$inter_type == "None"] <- NA
survey_stacked$intersect_quality[survey_stacked$inter_type == "None"] <- NA

# Export -----------------------------------------------------------------------
saveRDS(survey_stacked, file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey.Rds"))
#saveRDS(survey_stacked_nolabels, file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey_nolabels.Rds"))

names(survey_stacked) <- names(survey_stacked) %>% str_replace_all("\\.","_")
write_dta(survey_stacked, file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey.dta"))
saveRDS(survey_stacked, file.path(finaldata_file_path, "Hotspot Survey", "hotspot_survey.Rds"))




survey_stacked_i <- survey_stacked[survey_stacked$road_id == 22 & survey_stacked$segment_id == 5,]
survey_stacked_i <- survey_stacked_i[survey_stacked_i$enum_name == "Salome",]

survey_stacked_i %>% t




