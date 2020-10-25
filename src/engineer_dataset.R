source(file = "src/setup.R")
#load data------------------------------
# final_responses <- read.csv(file = "data/raw/reponses.csv",stringsAsFactors = F)
# key_logs <- read.csv(file = "data/raw/keylogs.csv",stringsAsFactors = F)
# prompts <- read.csv(file = "data/raw/prompts.csv",stringsAsFactors = F)
load("data/raw_data.Rdata")
#morphological features---------------------
source(file = "utils/morphological_features.R")
morph_feat = get_morphological_features(text = final_responses$response, 
                                        id = paste(final_responses$session_id, final_responses$response_box,sep = " - "))


# Get session id and box
morphological_response_summaries = morph_feat %>% 
  mutate(
    response_box = gsub(pattern = ".* - ",x = doc_id,replacement = ""), 
    session_id =  gsub(pattern = " - .*",x = doc_id,replacement = ""), 
) %>% select(-doc_id)


#Keylogs-------------------------------------------------------------------
source(file = "utils/key_log_functions.R")

key_types = get_key_types()

key_logs = key_logs %>% 
  select(-event_timestamp) %>% rename(timestamp = event_handled_at)

x = key_logs %>% group_split(session_id,response_box)

for (i in 1: length(x)){
  temp = x[[i]] %>% ungroup()
  session_id = temp$session_id[1]
  response_box = temp$response_box[1]
  
  temp = temp %>% select (-session_id,-response_box)
  
  temp = temp %>% 
    ks_dedupe %>% 
    ks_preprocess %>%
    ks_identify_strokes %>%
    ks_collapse_strokes %>%
    ks_assign_key_types %>%
    ks_add_lags %>%
    ks_editing_keys %>%
    ks_mark_removed_rows%>%
    ks_dynamics 
  
  metric_dynamics = temp %>% ks_dynamic_aggregates
  metric_deletion = temp %>% ks_deletion_content
  metric_general = temp %>% ks_general_stats
  
  metric = metric_general %>% 
    cbind(metric_dynamics) %>%
    cbind(metric_deletion)%>%
    mutate(
      ratio_total_errors = total_characters_removed/total_input,
      ratio_total_bulk_errors= total_characters_bulk_removed/total_input,
      ratio_total_discrete_errors = total_characters_discretely_removed/total_input
    )
  
  metric$session_id = session_id
  metric$response_box = response_box
  
  if(i==1) {
    response_keystroke_stats <- metric
  } else {
      response_keystroke_stats = rbind(response_keystroke_stats,metric)
      }
}

# Combining All tables---------------------------------------
final_features_raw = final_responses %>% 
  select(session_id,response_box, response,truth,prompt_id,response_start,response_finish,version) %>%  
  left_join(
    y = prompts,
    by = "prompt_id"
    ) %>%
  left_join(
    y = morphological_response_summaries, 
    by = c("session_id", "response_box")
    ) %>% 
  left_join(
    y = response_keystroke_stats,
    by = c("session_id", "response_box")
    )

#Final Features
final_features <- final_features_raw %>% mutate(
  time_before_first_key = ifelse(test = is.na(version),yes = NA ,no = first_key_time - response_start),
  time_after_last_key = ifelse(test = is.na(version),yes = NA ,no = response_finish - last_key_time),
  ratio_duration_char = duration_keys/count_char,
  ratio_duration_word = duration_keys/count_tokens,
)


# Write the table------------------------------------------------
final_features = write.csv(x = final_features,file = "data/final_features.csv",row.names = F)

