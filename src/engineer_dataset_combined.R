source(file = "src/setup.R")
#load data------------------------------
# final_responses <- read.csv(file = "data/raw/reponses.csv",stringsAsFactors = F)
# key_logs <- read.csv(file = "data/raw/keylogs.csv",stringsAsFactors = F)
# prompts <- read.csv(file = "data/raw/prompts.csv",stringsAsFactors = F)
load("data/raw_data.Rdata")
#--------------------------------


concafunction(x)


key_logs <- key_logs %>%
  filter(response_box!='response1')

final_responses <- final_responses %>%
  filter(response_box!='response1') %>%
  group_by(session_id, name) %>%
  mutate(response = paste(response, collapse = ", ")) %>%
  select(session_id, name,response) %>% distinct()
  

#morphological features---------------------
source(file = "utils/morphological_features.R")

morph_feat = get_morphological_features(text = final_responses$response, 
                                        id = paste(final_responses$session_id, sep = " - "))

# Get session id and box
morphological_response_summaries = morph_feat %>% 
  mutate(   session_id =  gsub(pattern = " - .*",x = doc_id,replacement = ""), 
) %>% select(-doc_id)


#Keylogs-------------------------------------------------------------------
source(file = "utils/key_log_functions.R")

key_types = get_key_types()

key_logs = key_logs %>% 
  select(-event_timestamp) %>% 
  rename(timestamp = event_handled_at)

x = key_logs %>% 
  group_split(session_id)

for (i in 1: length(x)){
  temp = x[[i]] %>% ungroup()
  
  session_id = temp$session_id[1]

  temp = temp %>% select (-session_id)
  
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
  
  if(i==1) {
    response_keystroke_stats <- metric
  } else {
      response_keystroke_stats = rbind(response_keystroke_stats,metric)
      }
}



#Final Features
final_features <- response_keystroke_stats %>% mutate(
  ratio_duration_char = duration_keys/count_char,
  ratio_duration_word = duration_keys/count_tokens,
)



# Combining All tables---------------------------------------
final_features_raw = final_responses %>% 
  select(session_id, response) %>%  
  left_join(
    y = morphological_response_summaries, 
    by = c("session_id")
  ) %>% 
  left_join(
    y = response_keystroke_stats,
    by = c("session_id")
  )

#Final Features
final_features <- final_features_raw %>% mutate(
  ratio_duration_char = duration_keys/count_char,
  ratio_duration_word = duration_keys/count_tokens,
)

# Write the table------------------------------------------------
write.csv(x = final_features_raw,file = "data/final_features_combined.csv",row.names = F)






