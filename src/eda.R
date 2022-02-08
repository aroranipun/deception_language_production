library(dplyr)
library(tidyr)
#Starts----------------------
load(file = "data/raw_data.Rdata")

prompts = read.csv(file = "data/v1_prompts.csv")
prompts = prompts %>% select(final_prompts, words, characters, valance, memory_engaged)
prompts$prompt_id = seq(1, nrow(prompts))

#------------------------------------------------------
df_all = read.csv(file = "data/final_features.csv")
df_all = df_all %>%
  filter(response_box != 'response1') 

t = table(df_all$session_id)

df_all = df_all %>%
  filter(!session_id %in% names(which(t != 2)))

df_all = df_all %>%
  left_join(y = prompts, by = c('prompt_id'))

df = df_all %>%
  mutate(efficiency = count_char/total_strokes) %>%
  select(
    session_id,
    truth,
    memory_engaged,
    valance,
    efficiency,
    flight_time_mean,
    flight_time_std_dev,
    ratio_total_errors,
    time_before_first_key,
    time_after_last_key,
    ratio_unique_lemmas
  ) %>% 
  arrange(session_id, truth)
#-----------------------------------


df_test = df %>% 
  pivot_wider(id_cols = c(session_id),
              names_from = c(truth),
              values_from = ratio_unique_lemmas)

t.test(x =df_test$`TRUE` , y=df_test$`FALSE`, paired = T)

write.csv(df_test,file = "data/jasp/df_test.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------


df_flight_time_truth = df %>%
  pivot_wider(id_cols = c(session_id),
              names_from = c(truth),
              values_from = flight_time_mean)

df_flight_time_truth$random = rbinom(n=nrow(df_flight_time_truth), size=1, prob=0.5)
write.csv(df_flight_time_truth,file = "data/jasp/df_flight_time_truth.csv", row.names = FALSE )
#-----------------------

df_flight_timeSTD_truth = df %>%
  pivot_wider(id_cols = c(session_id),
              names_from = c(truth),
              values_from = flight_time_std_dev)

write.csv(df_flight_timeSTD_truth,file = "data/jasp/df_flight_timeSTD_truth.csv", row.names = FALSE )

#-----------------------------------
  
df_flight_time_memory = df %>% 
  group_by(session_id, truth,memory_engaged) %>%
  select(flight_time_mean,flight_time_std_dev)
  
write.csv(df_flight_time_memory,file = "data/jasp/df_flight_time_memory.csv", row.names = FALSE)
#-----------------------------------
df_flight_time_valance = df %>% 
  group_by(session_id, truth,valance) %>%
  select(flight_time_mean,flight_time_std_dev)

write.csv(df_flight_time_valance,file = "data/jasp/df_flight_time_valance", row.names = FALSE)
#-----------------------------------

df_ratio_first_sin_pro = df %>% 
  group_by(session_id, truth) %>%
  select(ratio_first_sin_pro)

write.csv(df_ratio_first_sin_pro,file = "data/jasp/df_ratio_first_sin_pro.csv", row.names = FALSE)


# Logistic Regression-----------------------------------------


df_flight_logistic = df %>%
  group_by(session_id) %>%
  mutate(
    flight_time_overall_mean = sum(flight_time_mean * total_strokes) / sum(total_strokes),
    flight_time_mean_change = sign(flight_time_overall_mean - flight_time_mean),
    flight_time_overall_std_dev = sqrt(sum(flight_time_std_dev ^ 2) / 2),
    flight_time_std_dev_change = sign(flight_time_overall_std_dev - flight_time_std_dev),
    
    valance_flag =  ifelse(test = valance == 'Positive', 1,
                           ifelse(test = valance == 'Neutral', 0, -1)),
    truth_flag= ifelse(truth,1,0)
  ) 
df_flight_logistic = df_flight_logistic %>% 
  select(memory_engaged,flight_time_mean_change,flight_time_std_dev_change,valance_flag,truth_flag)

write.csv(x = df_flight_logistic,file = "data/jasp/df_flight_logistic.csv")

#-----------------------------------------
test(df = df, col_name = 'ratio_first_sin_pro')



test(df = df, col_name = 'flight_time_mean')
test(df = df, col_name = 'di_flight_mean')
test(df = df, col_name = 'ratio_total_discrete_errors')
test(df = df, col_name = 'avg_token_size')
test(df = df, col_name = 'ratio_ADJ')

write.csv(prompts, "data/v1_prompts.csv")
