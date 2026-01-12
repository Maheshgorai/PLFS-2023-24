library(dplyr)
library(data.table)

perv1 <- fread("D:/UDISE+/perv1.txt")

perv1 <- perv1 %>%
  mutate(
    final_weight = case_when(
      Column136 == Column137 ~ Column138 / (Column139 * 100),
      TRUE ~ Column138 / (Column139 * 200)
    )
  )
total_pop <- sum(perv1$final_weight, na.rm = TRUE)
print(round(total_pop))


