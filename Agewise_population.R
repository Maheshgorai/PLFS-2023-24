library(dplyr)
library(data.table)
library(tidyr)
library(writexl)

setwd
perv1 <- fread("D:/UDISE+/perv1.txt")

total_pop_df <- perv1 %>%
  mutate(
    Gender = case_when(
      Column19 == 1 ~ "Male",
      Column19 %in% c(2,3) ~ "Female",
      TRUE ~ "Other" 
    ),
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Age_Group = case_when(
      Column20 >= 6 & Column20 <= 17 ~ "Age 6-17",
      Column20 >= 18 & Column20 <= 24 ~ "18-24",
      TRUE ~ NA_character_
    ),
    State = Column6
  ) %>%
  filter(!is.na(Age_Group))
View(total_pop_df)

total_pop_table_all <- total_pop_df %>%
  group_by(State, Age_Group, Gender) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = Population,
    values_fill = 0
  ) %>%
  arrange(State)
View(total_pop_table_all)

total_pop_table_s <- total_pop_table_all %>%
  mutate(State = as.character(State))

total_pop_table_state <- total_pop_table_s %>%
  mutate(State_Name = case_when(
    State == "1" ~ "Jammu & Kashmir",
    State == "2" ~ "Himachal Pradesh",
    State == "3" ~ "Punjab",
    State == "4" ~ "Chandigarh",
    State == "5" ~ "Uttarakhand",
    State == "6" ~ "Haryana",
    State == "7" ~ "Delhi",
    State == "8" ~ "Rajasthan",
    State == "9" ~ "Uttar Pradesh",
    State == "10" ~ "Bihar",
    State == "11" ~ "Sikkim",
    State == "12" ~ "Arunachal Pradesh",
    State == "13" ~ "Nagaland",
    State == "14" ~ "Manipur",
    State == "15" ~ "Mizoram",
    State == "16" ~ "Tripura",
    State == "17" ~ "Meghalaya",
    State == "18" ~ "Assam",
    State == "19" ~ "West Bengal",
    State == "20" ~ "Jharkhand",
    State == "21" ~ "Odisha",
    State == "22" ~ "Chhattisgarh",
    State == "23" ~ "Madhya Pradesh",
    State == "24" ~ "Gujarat",
    State == "25" ~ "Daman & Diu, Dadra & Nagar Haveli",
    State == "27" ~ "Maharashtra",
    State == "28" ~ "Andhra Pradesh",
    State == "29" ~ "Karnataka",
    State == "30" ~ "Goa",
    State == "31" ~ "Lakshadweep",
    State == "32" ~ "Kerala",
    State == "33" ~ "Tamil Nadu",
    State == "34" ~ "Puducherry",
    State == "35" ~ "Andaman & Nicobar Islands",
    State == "36" ~ "Telangana",
    State == "37" ~ "Ladakh",
    TRUE ~ "Unknown"
  ))

total_pop_table_state <- total_pop_table_state %>%
  select(State, State_Name, Gender, everything())









