long_gap_limit = 5 # if someone has not pressed a key for this amount, cap the downtime_diff to this 
flight_time_lower = -2 #negative implies that the second  key was pressed before the first was released
flight_time_upper = 5  # if someone has not pressed a key for this amount, cap the downtime_diff to this 
bulk_remove_threshold = 10 # threshold to mark a et of deletions as bulk
time_since_prev_event_threhold = 1  # threshold to handle skipped key_up events
press_time_limit = 3 # if someone has pressed a key for this amount, cap the downtime_diff to this 

# Fixed Funtions----------------------------------------------------------------
get_key_types <- function(){
  non_content_keys <- c(
    16, #SHIFT KEY
    17, # CONTROL KEY,
    18, #Alt
    19,20,27
    ,33:34
    ,45 #Insert
    ,91:93 #meta key
    ,229 # process
    ,112:123 #F1 TO F12,
    ,144:145 #Numlcok, Scroll Lock
    ,223:250 #misc meta
  )
  navigation_keys <-c(
    35,36 #End, Home Key
    ,37:40 #Arrow keys 
  )
  
  deletion_keys <- c(
    46, #delete
    8# backspace
  )
  
  editing_keys <- c(
    67, 65, 88,86 # c,a,x,v
  )
  meta_editing_keys <-c(
    17, # CTRL or Command key for Opera browser
    91,93, #Command key for Webkit browser,
    224 # Command key for firefox browser
  )
  
  key_codes = list(non_content_keys= non_content_keys,
                   navigation_keys= navigation_keys, 
                   deletion_keys = deletion_keys, 
                   editing_keys  = editing_keys ,
                   meta_editing_keys  = meta_editing_keys)
}
key_types = get_key_types()
na_compare = function(x, y) {
  # takes two elements and returns first if the second is NA
  ifelse(test = is.na(y), x, y)
}
ks_dedupe = function(df) {
  temp <-  df %>%
    select(-keystroke_id) %>%
    distinct() %>%
    arrange(timestamp) %>%
    mutate(keystroke_id = 1:n()) %>%
    select(keystroke_id, everything())
  
  return(temp %>% ungroup())
}
ks_preprocess = function(df) {
  
  temp = df %>% mutate(
    characters_selected_when_pressed = selection_end - selection_start,
    key_value = tolower(key_value)
  )
  
  return(temp %>% ungroup())
}
ks_identify_strokes = function(df){
  
  temp = df %>% 
    group_by(key_code) %>% 
    arrange(timestamp) %>%
    mutate(
      last_event = ifelse(test = is.na(lag(event_type)),
                          yes = "none" ,
                          no =  lag(event_type)),
      next_event = ifelse(test = is.na(lead(event_type)),
                          yes = "none" ,
                          no =  lead(event_type)),
      
      time_since_prev_event = (timestamp -lag(timestamp))/1000,
      
      # Identify first event of the stroke...always keydown and relies on prev event being keyup
      
      first_press_flag = event_type == "keydown" & (last_event %in% c( "keyup", "none")),
      still_pressed_flag = event_type == "keydown" & (last_event %in% c( "keydown")),
      first_press_flag_2 = first_press_flag | (still_pressed_flag & event_type == "keydown" & time_since_prev_event > time_since_prev_event_threhold),
      full_cycle_flag = event_type == "keyup" & last_event=="keydown" & time_since_prev_event < time_since_prev_event_threhold,
      
      status = ifelse(test = first_press_flag_2,
                      yes = "0",
                      no = ifelse(test = still_pressed_flag,
                                  yes = "1",
                                  no =  ifelse(test = full_cycle_flag,
                                               yes = "2",
                                               no = "-1")
                                  )
                      ),
    
      key_press_id = ifelse(test = status=="0",
                            yes = paste(key_value, key_code,keystroke_id,sep = "-"),
                            no = NA),
      key_press_id = Reduce(f = "na_compare",
                            x = key_press_id,
                            accumulate = T)
      
        )
  return(temp %>% ungroup())
}
ks_collapse_strokes <- function (df){
  
  temp = df %>% 
    group_by(key_code,key_press_id) %>% 
    arrange(timestamp) %>%
    summarise(
      #down and up times
      down_time =       min(timestamp)/1000, # convert milliseconds to seconds
      up_time =         max(timestamp)/1000, # convert milliseconds to seconds
      
      #meta information
      number_of_inputs_in_stroke = length(keystroke_id[event_type == "keydown"]),
      characters_selected_when_pressed = max(characters_selected_when_pressed),
      
      full_cycle_flag = length(keystroke_id[event_type == "keyup"]) == 1,
      
      #Simultaneous Key pressed flags
      shift_pressed =   length(keystroke_id[shift_pressed]) >= 1,
      ctrl_pressed =    length(keystroke_id[ctrl_pressed]) >= 1,
      alt_pressed =     length(keystroke_id[alt_pressed]) >= 1,
    )
  return(temp %>% ungroup())
}
ks_assign_key_types <- function(df) {
  #Assign Key types-----------------
  temp <- df %>%
    mutate(
      #Assign various key type flags
      non_content_key_flag = key_code %in% key_types$non_content_keys,
      deletion_flag = key_code %in% key_types$deletion_keys,
      navigation_flag = key_code %in% key_types$navigation_keys,
      content_key_flag = !(navigation_flag |
                             deletion_flag |
                             non_content_key_flag),
    )
}
ks_add_lags <- function(df){
  
  temp <- df %>% arrange(down_time) %>%
    mutate(
      
      last_downtime_1 = lag(down_time, n = 1),
      last_uptime_1 = lag(up_time, n = 1),
     
      last_key_code = lag(key_code, n = 1),
      next_key_code = lead(key_code, n = 1),
    )
  return(temp %>% ungroup())
}
ks_editing_keys = function(df) {
  temp = df %>% arrange(down_time) %>%
    mutate(
      last_key_still_pressed_flag = last_uptime_1 > down_time,
      next_key_continous_press = lead(last_key_still_pressed_flag, 1),
      
      editing_presses_flag_1 = last_key_still_pressed_flag & (key_code %in% key_types$editing_keys & last_key_code %in% key_types$meta_editing_keys),
      editing_presses_flag_2 = next_key_continous_press & (next_key_code %in% key_types$editing_keys & key_code %in% key_types$meta_editing_keys),
      editing_presses_flag_3 = key_code %in% key_types$editing_keys & ctrl_pressed,
      editing_presses_flag = editing_presses_flag_1 | editing_presses_flag_2 |editing_presses_flag_3
    )
}
ks_mark_removed_rows = function(df){
  
  temp <- df %>% mutate(
    removed_flag = ifelse(test = editing_presses_flag | !(content_key_flag|deletion_flag),
                          yes = T ,no = F  ),
    prev_removed_flag = lag(removed_flag,1)
  ) 
    
    return(temp %>% ungroup())
}
ks_dynamics <- function(df){
  
  temp <- df %>%  
    filter(!removed_flag)
  
  temp <- ks_add_lags(temp)
  
  temp <- temp %>% arrange(down_time) %>% 
    mutate(
      
      #Downtime
      down_time_diff = down_time - last_downtime_1,
      long_gap_flag = down_time_diff > long_gap_limit, # this is to idenitfy if somoene left the session for a long time
      
      alt_down_time_diff = ifelse(test = down_time_diff > long_gap_limit,
                                  yes = long_gap_limit,
                                  no = down_time_diff),
      # flight times------------------------
      
      flight_time = down_time - last_uptime_1,
                           
      long_flight_time_flag = flight_time > flight_time_upper,
      short_flight_time_flag = flight_time < flight_time_lower,
      
      alt_flight_time = ifelse(
        test = prev_removed_flag |
          long_flight_time_flag | short_flight_time_flag,
        yes = NA,
        no =  down_time - last_uptime_1
      ),
      
      #alt_flight_time = flight_time,
      # alt_flight_time = ifelse (
      #   test = ,
      #   yes = flight_time_upper,
      #   no = ifelse(
      #     test = flight_time < flight_time_lower ,
      #     yes =  NA,
      #     no =  flight_time
      #   )
      # ),
      # 
      last_flight_time_1 = lag(alt_flight_time,n = 1),
      last_flight_time_2 = lag(alt_flight_time,n = 2),
      
      # press times--------------------------------------
      press_time = up_time - down_time,
      alt_press_time = ifelse (
        test = press_time > press_time_limit | press_time < 0,
        yes =  NA,
        no =  press_time
      ), 
      
      last_press_time_1 = lag(alt_press_time,n = 1),
      last_press_time_2 = lag(alt_press_time,n = 2),
      
      # di_graphs----------------------------
      # di_down = down_time - last_downtime_1,
      # di_up = up_time - last_uptime_1,
      # di_up_and_down = up_time - last_downtime_1,
      
      di_flight = round(alt_flight_time + last_flight_time_1,5),
      di_press = round(press_time +  last_press_time_1,5),
      
      # tri_graphs--------------------------------
      # tri_down = down_time - last_downtime_2,
      # tri_up = up_time - last_uptime_2,
      # tri_up_and_down = up_time - last_downtime_2,
      tri_flight = round(di_flight +last_flight_time_2,5),
      tri_press = round(di_press +  last_press_time_2,5),
    )
  
  temp <- temp %>%select(- flight_time, -press_time ) %>% 
    rename(flight_time = alt_flight_time,
           press_time = alt_press_time)
  
    return(temp %>% ungroup())
}
ks_dynamic_aggregates <- function(df){
  
  temp = df %>% #Get per response box summaries-----------------
    filter(!prev_removed_flag) %>%
    summarise_at(.vars =  
                   vars(
                     flight_time ,
                     press_time,
                     di_flight,
                     tri_flight,
                     di_press,
                     tri_press),
                 .funs = 
                   list(
                     mean = mean,
                     std_dev= sd,
                     # min = min,
                     #  max = max,
                     #  kurtosis = kurtosis ,
                     skewness = skewness
                   ),na.rm = TRUE
    )
  return(temp)
} 
ks_general_stats = function (df){
  
  temp = df %>% #Overall response statistics---------------
    filter(!non_content_key_flag) %>%
    summarise(
      first_key_time = min(down_time),
      last_key_time = max(up_time),
      
      duration_keys = 
        last_key_time - first_key_time - #Gets total observed duration of typing
        sum(down_time_diff[long_gap_flag],na.rm = T) +  #Subtracts duration of temporarily abandonned sessions
        sum(alt_down_time_diff[long_gap_flag],na.rm = T), # Fills in abandoned time with a ceiling threshold
      
      total_strokes = n(),
      total_input = sum(number_of_inputs_in_stroke),
    )
  return(temp %>% ungroup())
}
ks_deletion_content <- function(df){
  
  key_strokes_deletion <- df %>% arrange(down_time) %>%
    filter(!non_content_key_flag) %>%
    filter(!editing_presses_flag) %>%
    filter(!alt_pressed) %>%
    mutate(
      count_total_characters_removed = ifelse(
        test = deletion_flag,
        yes = number_of_inputs_in_stroke + ifelse(characters_selected_when_pressed > 0 , characters_selected_when_pressed-1,0),
        no = characters_selected_when_pressed),
      same_consecutive_key_press = key_code==lag(key_code),
    )
  
  key_strokes_deletion_content <- key_strokes_deletion %>%
    filter(content_key_flag) %>%
    mutate(
      bulk_flag = count_total_characters_removed >= bulk_remove_threshold
    )%>%
    summarise(
      bulk_deletes = sum(count_total_characters_removed[bulk_flag]),
      bulk_delete_occurences =  len_which(count_total_characters_removed >= bulk_remove_threshold),
      discrete_deletes = sum(count_total_characters_removed[!bulk_flag])
    )
  
  
  key_strokes_deletion_key_classify <- key_strokes_deletion %>%
    filter(deletion_flag) %>%
    mutate(
      #continuous deltions will be marked with a keycode of the first deletion action
      continuos_deletion_buckets = ifelse(!same_consecutive_key_press,key_press_id,NA),
      continuous_deletion_id = Reduce(f = "na_compare",
                                      x = continuos_deletion_buckets,
                                      accumulate = T)
    )
  if(nrow(key_strokes_deletion_key_classify) == 0){
    key_strokes_deletion_key_classify <- key_strokes_deletion_key_classify %>% mutate(
      continuous_deletion_id=NA
      )
    }
  
  key_strokes_deletion_key_consecutive_collapsed =  key_strokes_deletion_key_classify %>%
    group_by(continuous_deletion_id) %>%
    summarise(
      count_total_characters_removed = sum(count_total_characters_removed))
  
  key_strokes_deletion_delete_keys = key_strokes_deletion_key_consecutive_collapsed %>%
    mutate(
      bulk_flag = count_total_characters_removed >= bulk_remove_threshold
    ) %>%
    summarise(
      bulk_deletes = sum(count_total_characters_removed[bulk_flag]),
      bulk_delete_occurences =  len_which(bulk_flag),
      discrete_deletes = sum(count_total_characters_removed[!bulk_flag])
    )

  response_deletion_stats = key_strokes_deletion_content  %>% ##Final deletion suammary--------
  union(key_strokes_deletion_delete_keys) %>% 
    summarise(
      total_characters_bulk_removed= sum(bulk_deletes),
      total_occurene_bulk_delete= sum(bulk_delete_occurences),
      total_characters_discretely_removed = sum(discrete_deletes),
      total_characters_removed = total_characters_bulk_removed + total_characters_discretely_removed
    )
  return(response_deletion_stats %>% ungroup())
}


# # Test Code---------------------------------------------------------------------------------------
# df = key_logs %>% ungroup() %>% filter(session_id == 'abb60664-4c73-4bf0-97c0-9af638780bad' & response_box == "response3")%>% 
#   select(-session_id, - response_box,-event_timestamp) %>% rename(timestamp = event_handled_at)
# 
# df = ks_dedupe(df)
# df = ks_preprocess(df)
# df = ks_identify_strokes(df)
# df = ks_collapse_strokes(df)
# df = ks_assign_key_types(df)
# df = ks_add_lags(df)
# df = ks_editing_keys(df)
# df = ks_mark_removed_rows(df)
# df = ks_dynamics(df)
# df_dynamics <- ks_dynamic_aggregates(df)
# df_deletions = ks_deletion_content(df)

 