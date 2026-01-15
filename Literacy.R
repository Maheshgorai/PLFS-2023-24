library(dplyr)
library(data.table)


perv1 <- fread("D:/UDISE+/perv1.txt")


perv1 <- perv1 %>%
  mutate(
    Gender = ifelse(Column19 == 1, "Male", 
                    ifelse(Column19 == 2, "Female", NA)),
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    )
  )

total_pop <- sum(perv1$final_weight, na.rm = TRUE)
print(round(total_pop))  

perv1 <- perv1 %>%
  mutate(
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
    )
  ) %>%
  filter(!is.na(Age_Group))

# For Rural Males ==============================================================
male_rural_table <- perv1 %>%
  filter(Gender == "Male") %>%
  filter(Sector == "Rural") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(male_rural_table)

# For Urban Males ==============================================================
male_urban_table <- perv1 %>%
  filter(Gender == "Male") %>%
  filter(Sector == "Urban") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))


print(male_urban_table)

#For Male ======================================================================
male_table <- perv1 %>%
  filter(Gender == "Male") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))
print(male_table)

#For Rural Female ==============================================================
female_rural_table <- perv1 %>%
  filter(Gender == "Female") %>%
  filter(Sector == "Rural") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(female_rural_table)

#For Urban Female ==============================================================
female_urban_table <- perv1 %>%
  filter(Gender == "Female") %>%
  filter(Sector == "Urban") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(female_urban_table)

# For Female ===================================================================
female_table <- perv1 %>%
  filter(Gender == "Female") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(female_table)

# For Rural Person =============================================================
person_rural_table <- perv1 %>%
  filter(Sector == "Rural") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(person_rural_table)

# For Urban Person =============================================================
person_urban_table <- perv1 %>%
  filter(Sector == "Urban") %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(person_urban_table)

# For Person ===================================================================
person_table <- perv1 %>%
  group_by(Age_Group, Literacy) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Literacy, values_from = Population, values_fill = 0) %>%
  mutate(Total = Literate + Illiterate) %>%
  select(Age_Group, Literate, Illiterate, Total) %>%
  arrange(factor(Age_Group, levels = c("7-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")))

print(person_table)

# Literacy Summary ============================================================
literacy_7plus <- bind_rows(
  list(
    "Male_Rural"  = male_rural_table,
    "Male_Urban"  = male_urban_table,
    "Male"        = male_table,
    "Female_Rural"= female_rural_table,
    "Female_Urban"= female_urban_table,
    "Female"      = female_table,
    "Person_Rural"= person_rural_table,
    "Person_Urban"= person_urban_table,
    "All"         = person_table
  ),
  .id = "Category"
) %>%
  group_by(Category) %>%
  summarise(
    Literate   = sum(Literate),
    Illiterate = sum(Illiterate),
    Total      = sum(Total),
    Literacy_Rate_7plus = round(100 * Literate / Total, 1),
    .groups = "drop"
  ) %>%
  arrange(factor(Category, levels = c(
    "Male_Rural","Male_Urban","Male",
    "Female_Rural","Female_Urban","Female",
    "Person_Rural","Person_Urban","All"
  )))

# Rural Urban Literacy ========================================================
rural_urban_summary <- literacy_7plus %>%
  filter(Category %in% c("Person_Rural", "Person_Urban", "All")) %>%
  select(Category, Total, Literate, Illiterate, Literacy_Rate_7plus) %>%
  mutate(
    Category = case_when(
      Category == "Person_Rural" ~ "Rural (Persons)",
      Category == "Person_Urban" ~ "Urban (Persons)",
      Category == "All"          ~ "All-India (Persons)"
    )
  )

# Literacy Gap =================================================================
cat("Male-Female literacy gap (All India): ",
    round(literacy_7plus$Literacy_Rate_7plus[literacy_7plus$Category == "Male"] -
            literacy_7plus$Literacy_Rate_7plus[literacy_7plus$Category == "Female"], 1),
    "%\n")

cat("Rural-Urban literacy gap (Persons): ",
    round(literacy_7plus$Literacy_Rate_7plus[literacy_7plus$Category == "Person_Urban"] -
            literacy_7plus$Literacy_Rate_7plus[literacy_7plus$Category == "Person_Rural"], 1),
    "%\n")

table <- list(
  "Male_Rural" = male_rural_table,
  "Male_Urban" = male_urban_table,
  "Male_AllIndia" = male_table,
  "Female_Rural" = female_rural_table,
  "Female_Urban" = female_urban_table,
  "Feale_AllIndia" = female_table,
  "Person_Rural" = person_rural_table,
  "Person_Urban" = person_urban_table,
  "Person_AllIndia" = person_table,
  "Literacy_7plus" = literacy_7plus,
  "Rural_Urban_literacy" = rural_urban_summary
)





