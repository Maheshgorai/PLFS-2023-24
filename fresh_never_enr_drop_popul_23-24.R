library(dplyr)
library(data.table)
library(writexl)
library(tidyr)

PLFS_23_24 <- fread("D:/Sch_edu/PLFS/perv1.txt")


#============================   Never Enrolled   ===============================

never_enr_1 <- PLFS_23_24 %>%
  mutate(
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Never_enr = if_else(Column25 %in% c(1,2,3,4,5),"Yes","No"),
    Age = as.integer(Column20)
  )%>%
  filter(Never_enr == "Yes")%>%
  group_by(Age)%>%
  summarise(Never_enrol = sum(final_weight, na.rm = TRUE), .groups = "drop")


#=============================  Dropout  =======================================

dropout_1 <- PLFS_23_24 %>%
  mutate(
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Dropout = if_else(Column25 %in% c(11,12,13,14,15),"Yes","No"),
    Age = as.integer(Column20)
  )%>%
  filter(Dropout == "Yes")%>%
  group_by(Age)%>%
  summarise(Dropout = sum(final_weight, na.rm = TRUE), .groups = "drop")
View(dropout_1)
write_xlsx(dropout_1,"Dropout_all_age(23-24).xlsx")


#==========================   Population   =====================================

population_1 <- PLFS_23_24 %>%
  mutate(
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    ),
    Age = as.integer(Column20)
  )%>%
  group_by(Age)%>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop")
View(population_1)












