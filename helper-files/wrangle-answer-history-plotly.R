# WRANGLING CODE FOR Weekly Results Plotly Graphs-------------------------
### Code needed to build weekly_checkin_processed is stored in wrangle-answer-history-table.R

# Join (weekly results and goals separately)
weekly_results <- weekly_checkin_processed %>% 
  mutate(participant_info = case_when(
    participant_info == "drinking_1" ~ "drinking_total",
    participant_info == "druguse_1" ~ "druguse_total",
    participant_info == "craving_1" ~ "craving_total",
    TRUE ~ participant_info
  )) %>% 
  # Filter out recovery score
  filter(participant_info != "recoveryscore_total") %>% 
  filter(str_detect(participant_info, "total"))

goals <- weekly_checkin_processed %>% 
  mutate(participant_info = case_when(
    participant_info == "goals_1" ~ "goal_drinking",
    participant_info == "goals_2" ~ "goal_craving",
    participant_info == "goals_3" ~ "goal_coping",
    participant_info == "goals_4" ~ "goal_outlook",
    participant_info == "goals_5" ~ "goal_mentalhealth",
    TRUE ~ participant_info
  )) %>% 
  # Filter out recovery score
  filter(participant_info != "recoveryscore_total") %>% 
  filter(str_detect(participant_info, "goal"))

combined_df <- weekly_results %>% 
  rbind(goals)





answer_history_plotly <- combined_df %>%  
  # change values to integer (enabled by hack)
  mutate(values = as.integer(values)) %>% 
  #  Join on participant info and values
  left_join(metadata_processed, by = c("participant_info" = "field_name",
                                       "values"           = "numeric_choice")) %>% 
  # Use dummy variable to fill out text choice with additional goals
  mutate(text_choice = case_when(
    is.na(text_choice) ~ dummy_var,
    TRUE ~ text_choice
  )) %>% 
  # Read in only the dates
  mutate(checkin_start_date = as.Date(checkin_start_time, format = "%Y-%m-%d")) %>% 
  # Create month-day-year text for labels on plotly graph (ex: Sep 07, 2020)
  mutate(checkin_start_month = lubridate::month(checkin_start_date,
                                                label = TRUE,
                                                abbr = TRUE),
         checkin_start_day = lubridate::day(checkin_start_date),
         checkin_start_year = lubridate::year(checkin_start_date)) %>% 
  mutate(checkin_date_hover = paste0(checkin_start_month, " ", 
                                     checkin_start_day, ", ",
                                     checkin_start_year))

