library(tidyverse)
library(dplyr)


path <- "/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/classification/output/title_classifications/classification_export_gpt-5-mini_v6_v6.csv"

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
