get_pop_by_age_sex_year_ctn = function(){
  df = read_excel(paste0(code_root_path,"/data/pop_CH_age_sex_ctn_2010_2021.xlsx"))
  
  df_clean <- df %>%
    select(year = `...1`, ctn = `...3`, pop_type = `...5`, sex = `...10`,  `Âge - total`:`100 ans ou plus`)
  
  # Step 2: Fill missing values in the year and ctn columns
  df_clean <- df_clean %>%
    mutate(year = as.integer(year)) %>%
    fill(year, ctn, pop_type, .direction = "down")
  
  df_clean_long <- df_clean %>%
    pivot_longer(
      cols = `Âge - total`:`100 ans ou plus`, # Range of age columns
      names_to = "age",        # Name for the new "age" column
      values_to = "population" # Name for the new "population" column
    )
  
  # Step 4: Clean and convert `age` column to numeric
  df_clean_long <- df_clean_long %>%
    filter(age != "Âge - total",year<=2022) %>% # Remove total rows
    mutate(
      age = case_when(
        str_detect(age, "ans|an") ~ as.numeric(str_extract(age, "\\d+")), # Extract numeric part of "0 an", "1 an", etc.
        TRUE ~ NA_real_ # Handle unexpected cases
      )
    ) 
  
  # Step 5: Standardize `sex` column and remove "total"
  df_clean_long <- df_clean_long %>%
    filter(!str_detect(sex, "total")) %>% # Remove rows where sex is "total"
    mutate(
      sex = case_when(
        str_detect(sex, "Homme") ~ "M", # Convert "Homme" to "M"
        str_detect(sex, "Femme") ~ "F", # Convert "Femme" to "F"
        TRUE ~ NA_character_ # Handle unexpected cases
      )
    )
  #Step 6: add canton id and nuts-2
  df_clean_long <- df_clean_long %>%
    left_join(canton_df,by="ctn")
  
  return(df_clean_long)
}