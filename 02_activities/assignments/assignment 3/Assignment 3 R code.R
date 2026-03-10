# Open libraries
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

# Open Excel
research_funding_summary_current_2018_2026_en <- read_excel("C:/Users/liane/Dropbox/liane/PhD/Data_science_course/visualization/02_activities/assignments/assignment 3/research_funding_summary_current_2018-2026_en.xlsx", 
                                                      sheet = "July 2018-Jan 31 2026")
research_funding <- research_funding_summary_current_2018_2026_en

#Make fiscal year categorical
research_funding$`Fiscal Year` <- factor(
  research_funding$`Fiscal Year`,
  levels = c(
    "2018-19",
    "2019-20",
    "2020-21",
    "2021-22",
    "2022-23",
    "2023-24",
    "2024-25",
    "2025-26"
  )
)

summary(research_funding$`Ontario Commitment`)
summary(research_funding$`Total Project Costs`)
table(research_funding$'Lead Research Institution')
unique(research_funding$'Lead Research Institution')
table(research_funding$'Institution Type')
unique(research_funding$'Institution Type')
table(research_funding$'City')
unique(research_funding$'City')
table(research_funding$'FOR - Level 1 Division Title')
unique(research_funding$'FOR - Level 1 Division Title')

#Create new dataset for 2024-25 year
research_funding_2024_25 <- research_funding %>%
  filter(`Fiscal Year` == "2024-25")
table(research_funding_2024_25$'City')

#change Scarborough to Toronto
research_funding_2024_25 <- research_funding_2024_25 %>%
  mutate(
    City = ifelse(City == "Scarborough", "Toronto", City)
  )
table(research_funding_2024_25$'City')

#remove brampton
research_funding_2024_25 <- research_funding_2024_25 %>%
  filter(City != "Brampton")

# Add 2024 CMA population from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710014801&utm

research_funding_2024_25 <- research_funding_2024_25 %>%
  mutate(
    population = case_when(
      City == "Barrie" ~ 247098,
      City == "Guelph" ~ 182615,
      City == "Hamilton" ~ 862679,
      City == "Kingston"  ~ 192395,
      City == "London" ~ 626875,
      City == "Oshawa" ~ 482076,
      City == "Ottawa"  ~ 1291272,
      City == "Peterborough" ~ 140630,
      City == "Sarnia" ~ 108798,
      City == "Sault Ste. Marie" ~ 88334,
      City == "St. Catharines"  ~ 493608,
      City == "Sudbury" ~ 192688,
      City == "Toronto" ~ 7109866,
      City == "Waterloo"  ~ 699402,
      City == "Windsor" ~ 484698,
      TRUE              ~ 0  
    )
  )

# Calculate per capita funding
research_funding_2024_25 <- research_funding_2024_25 %>%
  mutate(per_capita_funding = `Ontario Commitment` / population)

# Summarise total per capita funding by city
city_funding <- research_funding_2024_25 %>%
  group_by(City) %>%
  summarise(
    total_per_capita = sum(per_capita_funding, na.rm = TRUE),
    .groups = "drop"  # avoids grouped data issues
  )

# Plot
ggplot(city_funding, aes(x = reorder(City, total_per_capita), y = total_per_capita)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(total_per_capita, 0)), 
            vjust = -0.5, size = 3) +  # adds values on top of bars
  labs(
    title = "Provincial Grant Funding per Capita by Ontario City",
    x = "City",
    y = "Total Grant Funding per Capita ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

# extract excel
library(writexl)
write_xlsx(
  research_funding_2024_25,
  "C:/Users/liane/Dropbox/liane/PhD/Data_science_course/visualization/02_activities/assignments/assignment 3/research_funding_2024_25.xlsx"
)
