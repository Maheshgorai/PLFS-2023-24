library(dplyr)
library(data.table)
library(writexl)

df <- fread("D:/UDISE+/perv1.txt")


df <- df %>%
  mutate(State = Column6, State = as.character(State))
df <- df %>%
  mutate(
    Gender = ifelse(Column19 == 1, "Male", 
                    ifelse(Column19 == 2, "Female", NA)),
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Age_Group = case_when(
      Column20 >= 07 & Column20 <= 14 ~ "07-14",
      Column20 >= 15 & Column20 <= 19 ~ "15-19",
      Column20 >= 20 & Column20 <= 24 ~ "20-24",
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
    ),
    Literacy = ifelse(Column22 %in% c(01), "Illiterate", "Literate"),
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
    State_Name = case_when(
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

# State-wise for Persons (All sectors) - AGE 7+ ONLY
state_person_table_7plus <- df %>%
  filter(Column20 >= 7, !is.na(State)) %>%   
  group_by(State_Name, Literacy) %>%         
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(
    Total = Literate + Illiterate,
    Literacy_Rate_7plus = round(100 * Literate / Total, 1)
  ) %>%
  arrange(desc(Literacy_Rate_7plus))          

print("State-wise Literacy Rate (Age 7+, Persons - All India):")
print(state_person_table_7plus)
View(state_person_table_7plus)

# Rural version (Age 7+)
state_rural_table_7plus <- df %>%
  filter(Column20 >= 7, Sector == "Rural", !is.na(State)) %>%
  group_by(State_Name, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(
    Total = Literate + Illiterate,
    Literacy_Rate_rural_7plus = round(100 * Literate / Total, 1)
  ) %>%
  arrange(desc(Literacy_Rate_rural_7plus))
View(state_rural_table_7plus)

# Urban version (Age 7+)
state_urban_table_7plus <- df %>%
  filter(Column20 >= 7, Sector == "Urban", !is.na(State)) %>%
  group_by(State_Name, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(
    Total = Literate + Illiterate,
    Literacy_Rate_urban_7plus = round(100 * Literate / Total, 1)
  ) %>%
  arrange(desc(Literacy_Rate_urban_7plus))
View(state_urban_table_7plus)


