# If you don't have these packages installed, run the code:
# 'install.packages("package name")' and then run these library codes listed below.

library(readxl)
library(writexl)
library(tidyverse)
library(data.table)

Womens_Soccer_Drills <- read_excel("DB Women Soccer .xlsx", sheet = "DB WSoccer")

Womens_Soccer_Drills_Cleaned <- Womens_Soccer_Drills %>%
  select(`Drill Date`, `Drill Title`, Accelerations, Decelerations, `HML Distance`, `High Speed Running (Absolute)`, `Total Distance`, Calc_Time)

Womens_Soccer_Drills_Cleaned <- Womens_Soccer_Drills_Cleaned %>%
  mutate( 
  `Drill Title` = case_when(
    `Drill Title` %like% 'Warm Up' ~ 'Warm Up', 
    `Drill Title` %like% 'Warm up' ~ "Warm Up",
    `Drill Title` %like% '2nd ' ~ "Second Half",
    `Drill Title` %like% 'Second Half' ~ 'Second Half',
    `Drill Title` %like% '1st ' ~ "First Half",
    `Drill Title` %like% 'First Half' ~ 'First Half',
    `Drill Title` %like% 'Technical-Escape' ~ 'Technical-Escape',
    `Drill Title` %like% 'Set Plays' ~ 'Set Plays',
    `Drill Title` %like% '2. Technical' ~ 'Technical',
    `Drill Title` %like% 'long ball' ~ 'Long Ball',
    `Drill Title` %like% 'Transition to Attack Finishing pt 2' ~ 'T2A: Finishing',
    `Drill Title` == "3. 3v3+4" ~ "3v3+4",
    `Drill Title` == "3+3v4 to Goal" ~ "3v3+4 to Goal",
    `Drill Title` == '3. Change POA w Cross+Finish' ~ 'Change POA w Cross+Finish',
    `Drill Title` == '4. Mark in Box' ~ 'Mark in Box',
    `Drill Title` == 'Technical 3v1' ~ '3v1 Tech',
    `Drill Title` == "Walkthrough and Train In 3-5-2" ~ 'Train in 3-5-2',
    `Drill Title` == "Transition to Attack Finishing" ~ 'T2A: Finishing',
    `Drill Title` == "Transition" ~ 'Playing in Transition',
    `Drill Title` == "Transition to Attack/Higher Def Positions" ~ 'Attack Transition/Higher Def Positions',
    `Drill Title` == "Technical -Dribble/Escape" ~ 'Tech-Dribble/Escape',
    `Drill Title` == "Technical" ~ 'Skill Development- Technical',
    TRUE ~ as.character(`Drill Title`)
  ) 
)

Womens_Soccer_Drills_Cleaned <- Womens_Soccer_Drills_Cleaned %>%
  filter(`Drill Title` != 'Entire Game') %>%
  filter(`Drill Title` != 'Second Half') %>%
  filter(`Drill Title` != 'First Half') %>%
  filter(`Drill Title` != 'Entire Practice') %>%
  filter(`Drill Title` != 'Entire Session') %>%
  filter(`Drill Title` != 'Warm Up')
  
AverageByDrill = Womens_Soccer_Drills_Cleaned %>%
  filter(`Drill Title` != 'Decisive/Coordinated Movements to Goal') %>%
  group_by(`Drill Title`) %>%
  summarise(
    mean(Accelerations),
    mean(Decelerations),
    mean(`HML Distance`),
    mean(`High Speed Running (Absolute)`),
    mean(`Total Distance`)
    ) 

StDevByDrill = Womens_Soccer_Drills_Cleaned %>%
  filter(`Drill Title` != 'Decisive/Coordinated Movements to Goal') %>%
  group_by(`Drill Title`) %>%
  summarise(
    sd(Accelerations),
    sd(Decelerations),
    sd(`HML Distance`),
    sd(`High Speed Running (Absolute)`),
    sd(`Total Distance`)
  ) 
  
Womens_Soccer_GroupByDate = Womens_Soccer_Drills_Cleaned %>%
  filter(`Drill Title` != 'Decisive/Coordinated Movements to Goal') %>%
  group_by(`Drill Date`) %>%
  summarise(
    mean(Accelerations),
    mean(Decelerations),
    mean(`HML Distance`),
    mean(`High Speed Running (Absolute)`),
    mean(`Total Distance`),
    sd(Accelerations),
    sd(Decelerations),
    sd(`HML Distance`),
    sd(`High Speed Running (Absolute)`),
    sd(`Total Distance`)
  ) 

Womens_Soccer_GroupByDate = Womens_Soccer_GroupByDate %>%
  group_by(`Drill Date`) %>%
  mutate(
    mean_total_load = sum(`mean(Accelerations)`, `mean(Decelerations)`, `mean(\`HML Distance\`)`, 
                          `mean(\`High Speed Running (Absolute)\`)`, `mean(\`Total Distance\`)`),
    sd_total_load = sum(`sd(Accelerations)`, `sd(Decelerations)`, `sd(\`HML Distance\`)`, 
                        `sd(\`High Speed Running (Absolute)\`)`, `sd(\`Total Distance\`)`)
  )
Drill_Titles = AverageByDrill %>%
  select(`Drill Title`) # This is just to check and see if the Name Changes/Cleaning codes worked

Drill_Dates = Womens_Soccer_GroupByDate %>%
  select(`Drill Date`)

Drill_Times = Womens_Soccer_Drills_Cleaned %>%
  filter(`Drill Title` != 'Decisive/Coordinated Movements to Goal') %>%
  select(`Drill Title`, Calc_Time) %>%
  group_by(`Drill Title`) %>%
  summarise(mean(Calc_Time))

Drill_Times$`mean(Calc_Time)` = round(Drill_Times$`mean(Calc_Time)`, 2)
Drill_Times = Drill_Times %>%
  rename(`Mean Time` = `mean(Calc_Time)`)

write_xlsx(Drill_Dates, "Drill_Dates.xlsx")
write_xlsx(AverageByDrill, "WS_AverageByDrill.xlsx")
write_xlsx(StDevByDrill, "WS_StDevByDrill.xlsx")
write_xlsx(Womens_Soccer_GroupByDate, "WS_Histogram_Data.xlsx")
write_xlsx(Drill_Times, "Drill_Times.xlsx")
