library(tidyverse)
library(dplyr)


path <- "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/current/classification_export_current.csv"

df <- read.csv(path)

unique(df$topic)
df %>%
group_by(topic) %>%
summarise(count = n()) %>%
arrange(desc(count))

df %>%
filter(topic == "lifestyle") %>%
View()

write.csv(
  df %>% filter(topic == "lifestyle"),
  file = "/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/research_notes/lifestyle_fix/lifestyle.csv",
  row.names = FALSE
)
