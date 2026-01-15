library(dplyr)
library(data.table)
library(tidyr)


perv1 <- fread("D:/UDISE+/perv1.txt")

perv1 <- perv1 %>%
  mutate(
    Gender = case_when(
      Column19 == 1 ~ "Male",
      Column19 == 2 ~ "Female",
      TRUE ~ "All"
    ),
    Sector = case_when(
      Column5 == 1 ~ "Rural",
      Column5 == 2 ~ "Urban",
      TRUE ~ "All"
    ),
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Age_Group = case_when(
      Column20 >= 25 & Column20 <= 29 ~ "25-29",
      Column20 >= 30 & Column20 <= 34 ~ "30-34",
      Column20 >= 35 & Column20 <= 39 ~ "35-39",
      Column20 >= 40 & Column20 <= 44 ~ "40-44",
      Column20 >= 45 & Column20 <= 49 ~ "45-49",
      Column20 >= 50 & Column20 <= 54 ~ "50-54",
      Column20 >= 55 & Column20 <= 59 ~ "55-59",
      Column20 >= 60 & Column20 <= 64 ~ "60-64",
      Column20 >= 65 & Column20 <= 69 ~ "65-69",
      Column20 >= 70 & Column20 <= 74 ~ "70-74",
      Column20 >= 75 & Column20 <= 79 ~ "75-79",
      Column20 >= 80 & Column20 <= 84 ~ "80-84",
      Column20 >= 85 ~ "85+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Age_Group))

perv1 <- perv1 %>%
  mutate(
    MultiCategory = case_when(
      Column22 == 1 ~ "No Schooling",   
      Column22 == 5 ~ "Below Primary",  
      Column22 == 6 ~ "Primary",   
      Column22 == 7 ~ "Middle",   
      Column22 == 8 ~ "Secondary",   
      Column22 == 10 ~ "Higher Secondary",   
      Column22 == 11 ~ "PSNT",   
      Column22 == 12 ~ "Graduate",  
      Column22 == 13 ~ "Post Graduate and Above",
      TRUE ~ NA_character_
    )
  )

# For Rural Males
male_rural_multi_table <- perv1 %>%
  filter(Gender == "Male", Sector == "Rural") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(male_rural_multi_table)

# For Urban Males
male_urban_multi_table <- perv1 %>%
  filter(Gender == "Male", Sector == "Urban") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(male_urban_multi_table)

# For Males
male_multi_table <- perv1 %>%
  filter(Gender == "Male") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(male_multi_table)

# For Rural Females
female_rural_multi_table <- perv1 %>%
  filter(Gender == "Female", Sector == "Rural") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(female_rural_multi_table)

# For Urban Females
female_urban_multi_table <- perv1 %>%
  filter(Gender == "Female", Sector == "Urban") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(female_urban_multi_table)

# For Females
female_multi_table <- perv1 %>%
  filter(Gender == "Female",) %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(female_multi_table)

# For Rural Person
person_rural_multi_table <- perv1 %>%
  filter(Sector == "Rural") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(person_rural_multi_table)

# For Urban Person
person_urban_multi_table <- perv1 %>%
  filter(Sector == "Urban") %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(person_urban_multi_table)


# For Person
person_table <- perv1 %>%
  group_by(Age_Group, MultiCategory) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MultiCategory,
    values_from = Population,
    values_fill = 0
  ) %>%
  
  select(Age_Group, `No Schooling`, `Below Primary`, `Primary`, `Middle`, `Secondary`, `Higher Secondary`, `PSNT`, `Graduate`, `Post Graduate and Above`) %>%  # Adjust order if you have only 4
  arrange(factor(Age_Group, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

View(person_table)









































