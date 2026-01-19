library(dplyr)
library(data.table)
library(tidyr)

setwd("D:/UDISE+/R")

df <- fread("D:/UDISE+/perv1.txt")
View(df)
never_enrolled_df <- df %>%
  mutate(
    Gender = case_when(
      Column19 == 1 ~ "Male",
      Column19 == 2 ~ "Female",
      TRUE ~ "All"
    ),
    Reasons = case_when(
      Column25 %in% c(1,11) ~ "School too far",
      Column25 %in% c(2,12) ~ "Supplement household income",
      Column25 %in% c(3,13) ~ "Education not considered necessary",
      Column25 %in% c(4,14) ~ "Domestic chores",
      Column25 %in% c(5,15) ~ "Others",
      TRUE ~ "All"
    ),
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Age_Group = case_when(
      Column20 >= 14 & Column20 <= 18 ~ "14-18",
      #Column20 >= 11 & Column20 <= 13 ~ "11-13",
      #Column20 >= 14 & Column20 <= 15 ~ "14-15",
      #Column20 >= 16 & Column20 <= 17 ~ "16-17",
      TRUE ~ NA_character_
    ),
    State = Column6,
    Never_Enrolled = if_else(Column25 %in% c(1,2,3,4,5,11,12,13,14,15), "Yes", "No")
  ) %>%
  filter(!is.na(Age_Group), Never_Enrolled == "Yes")

never_enrolled_table_all <- never_enrolled_df %>%
  group_by(State, Age_Group,Gender, Reasons) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = Population,
    values_fill = 0
  ) %>%
  arrange(State)
View(never_enrolled_table_all)

# First, convert State to character if it's numeric (to avoid factor issues later)
never_enrolled_table_s <- never_enrolled_table_all %>%
  mutate(State = as.character(State))

# Add state names using a mapping
never_enrolled_table_state <- never_enrolled_table_s %>%
  mutate(State_Name = case_when(
    #State == "01" ~ "Jammu & Kashmir",
    #State == "02" ~ "Himachal Pradesh",
    #State == "03" ~ "Punjab",
    #State == "04" ~ "Chandigarh",
    State ==  "5" ~ "Uttarakhand",
    #State == "06" ~ "Haryana",
    #State == "07" ~ "Delhi",
    #State == "08" ~ "Rajasthan",
    State ==  "9" ~ "Uttar Pradesh",
    #State == "10" ~ "Bihar",
    #State == "11" ~ "Sikkim",
    #State == "12" ~ "Arunachal Pradesh",
    #State == "13" ~ "Nagaland",
    #State == "14" ~ "Manipur",
    #State == "15" ~ "Mizoram",
    #State == "16" ~ "Tripura",
    #State == "17" ~ "Meghalaya",
    #State == "18" ~ "Assam",
    #State == "19" ~ "West Bengal",
    #State == "20" ~ "Jharkhand",
    #State == "21" ~ "Odisha",
    State == "22" ~ "Chhattisgarh",
    State == "23" ~ "Madhya Pradesh",
    State == "24" ~ "Gujarat",
    State == "25" ~ "Daman & Diu, Dadra & Nagar Haveli",
    State == "27" ~ "Maharashtra",
    #State == "28" ~ "Andhra Pradesh",
    #State == "29" ~ "Karnataka",
    State == "30" ~ "Goa",
    #State == "31" ~ "Lakshadweep",
    #State == "32" ~ "Kerala",
    #State == "33" ~ "Tamil Nadu",
    #State == "34" ~ "Puducherry",
    #State == "35" ~ "Andaman & Nicobar Islands",
    #State == "36" ~ "Telangana",
    #State == "37" ~ "Ladakh",
    TRUE ~ "Unknown"
  ))

# Reorder columns to put State_Name next to State
never_enrolled_table_state <- never_enrolled_table_state %>%
  select(State, State_Name, Gender, Reasons, everything())

View(never_enrolled_table_state)

# specific states
specific_states_table <- never_enrolled_table_state %>%
  filter(State %in% c("5", "9","22", "23", "24", "25", "27", "30"))

View(specific_states_table)




