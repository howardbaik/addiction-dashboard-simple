# Update: March 3rd, 2021: Got rid of columns first_name, last_name, mrn, and email_address
source("helper-files/redcap-data.R")

# Pivot to longer format
weekly_checkin_processed <- weekly_checkin %>% 
  # Deselect unnecessary columns
  select(-c(weekly_checkin_complete, goals_additional, redcap_survey_identifier,
            participant_information_timestamp, participant_information_complete)) %>% 
  # Pivot
  pivot_longer(cols = !(record_id:checkin_start_time),
               names_to = "participant_info",
               values_to = "values") 


# pivot the "additional_goals" to long format 
# structure the columns identically to weekly_checkin_processed
weekly_checkin_goals_additional <- weekly_checkin %>%
  select(-c(weekly_checkin_complete, participant_information_timestamp, redcap_survey_identifier,
            participant_information_complete)) %>% 
  select_if(is.character) %>%
  pivot_longer(cols=goals_additional, names_to = "participant_info", values_to = "values") 

# merge the two data frames by row and arrange in order
weekly_checkin_processed <- weekly_checkin_processed %>% 
  rbind(weekly_checkin_goals_additional) %>%
  arrange(record_id, checkin_start_time)

weekly_checkin_processed <- weekly_checkin_processed %>% 
  # Create dummy variable
  # dummy_var is a column that contains text input of additional_goals
  mutate(dummy_var = case_when(
    # criteria for additional goals
    str_length(values) > 1 | is.na(values) ~ values,
    TRUE ~ ""
  )) %>% 
  # Change goals_additional values in values column to -1
  # Allows as.integer(values) when joining
  mutate(values = case_when(
    str_length(values) > 1 | is.na(values) ~ "-1",
    TRUE ~ values
  ))

metadata_processed <- metadata %>% 
  # Get rid of html tags in section_header and field_label
  mutate(section_header = str_replace_all(section_header, "<[^>]+>", ""),
         field_label = str_replace_all(field_label, "<[^>]+>", "")) %>% 
  # Filter out the subscales and recovery score
  filter(!(str_detect(field_name, "total"))) %>% 
  group_by(field_name) %>% 
  # Split on vertical bar by creating new row with different response
  separate_rows(select_choices_or_calculations, sep='\\|') %>% 
  # For each response, split on comma and create new column 
  separate(col = select_choices_or_calculations, into = c("numeric_choice", "text_choice"), sep = ",") %>% 
  # Trim unnecessary whitespace from numeric_choice column
  mutate(numeric_choice = str_trim(numeric_choice),
         numeric_choice = as.integer(numeric_choice)) %>% 
  ungroup()


# Join
joined_df <- weekly_checkin_processed %>% 
  # change values to integer (enabled by hack)
  mutate(values = as.integer(values)) %>% 
  #  Join on participant info and values
  left_join(metadata_processed, by = c("participant_info" = "field_name",
                                       "values"           = "numeric_choice")) %>% 
  filter(!(str_detect(participant_info, "total"))) %>% 
  # Move a few columns to the front for easier viewing
  select(record_id, redcap_event_name, weekly_checkin_timestamp, checkin_start_time, 
         participant_info, values, text_choice, dummy_var, section_header,
         everything()) %>% 
  # Use dummy variable to fill out text choice with additional goals
  mutate(text_choice = case_when(
    is.na(text_choice) ~ dummy_var,
    TRUE ~ text_choice
  ))



answer_history_final <- joined_df %>% 
  # Put in missing Section Header (Substance Use and Craving)
  mutate(section_header = case_when(
    participant_info == "drinking_1" ~ "Substance Use and Craving",
    TRUE                             ~ section_header)
  ) %>%
  # Create section_subheader column (fill = "left" makes section_subheader NA for those rows without section_subheader)
  separate(col = field_label, into = c("section_subheader", "field_label"),
           sep = "\\...", fill = "left") %>% 
  # Get rid of dummy variable
  select(-dummy_var) %>% 
  # Add in field label for additional_goals
  mutate(field_label = case_when(
    participant_info == "goals_additional" ~ "(Optional) Please list your top three concerns or goals for the coming week.",
    TRUE                                   ~ field_label
  ))

answer_history_table <- answer_history_final %>% 
  # Pick out columns
  select(record_id, checkin_start_time, participant_info,
         section_header, section_subheader, field_label, values, text_choice) %>% 
  # If text_choice contains NA, replace with empty blank so that the table shows empty blank
  mutate(text_choice = if_else(is.na(text_choice), " ", text_choice))


# Get all the unique record_ids
record_id_total <- answer_history_table %>% 
  pull(record_id) %>% 
  unique()
