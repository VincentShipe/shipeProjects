library(readxl)
library(tidyverse)

TeamData_2022_23 <- read_xlsx('NHL_TeamData_2022_23.xlsx')

TeamData_2022_23 = TeamData_2022_23 %>%
  rename(Team = ...2)

TeamData_2022_23 = TeamData_2022_23 %>%
  filter(Team != 'League Average')

TeamData_2022_23 = TeamData_2022_23 %>%
  mutate(
    'GD/GP' = `GF/G`-`GA/G`,
    'PK Failure Rate' = 100-`PK%`,
    'PP Failure Rate' = 100-`PP%`,
    'Special Teams Effectiveness' = `PP%` + `PK%`,
    'Special Teams Failures' = `PP Failure Rate` + `PK Failure Rate`,
    'PIM Index' = `PIM/G` - `oPIM/G`,
    'PDO' = `S%` + (`SV%`*100),
    'Playoffs' = if_else(SRS > 0.2, "Yes", "No"),
    'ST Breakeven' = `Special Teams Effectiveness` - `Special Teams Failures`,
    'ST Status' = if_else(`ST Breakeven` > 0, 'Effective', 'Not Effective'),
    'Discipline' = if_else(`PIM Index` > 0, 'Undisciplined', 'Disciplined'),
    'PP Breakeven' = PPO - PPOA,
    'PP Status' = if_else(`PP Breakeven` > 0 , 'Surplus', 'Deficit')
  )
TeamData_2022_23$Team <- gsub("\\*", "", TeamData_2022_23$Team)

TeamData_2022_23 = TeamData_2022_23 %>%
  select(-c(Rk, AvAge, SOW, SOL, SOS, SH, SHA, SO))

col_order <- c('Team', 'GP', 'W', 'L', 'OL', 'PTS', 'PTS%', 'GF', 'GA', 'SRS', 'GF/G', 'GA/G', 'GD/GP', 'PP', 'PPO', 'PP%', 'PPA', 'PPOA', 'PK%',
               'PK Failure Rate', 'PP Failure Rate', 'PP Breakeven', 'Special Teams Effectiveness', 'Special Teams Failures', 'ST Breakeven',
               'PIM/G', 'oPIM/G', 'PIM Index', 'S', 'S%', 'SA', 'SV%', 'PDO', 'ST Status', 'PP Status', 'Discipline', 'Playoffs')

TeamData_2022_23 = TeamData_2022_23[, col_order]
