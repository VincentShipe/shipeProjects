library(readxl)
library(tidyverse)

Stenlund <- read_xlsx('Stenlund_Game_Log_2324.xlsx')

Stenlund <- Stenlund %>%
  select(G...3, G...9, S) %>%
  rename(Game = G...3, Goals = G...9, Shots = S)

Stenlund <- Stenlund %>%
  mutate(
    Cumul_goals = cumsum(Goals),
    Cumul_shots = cumsum(Shots),
    C_Shoot_pct = round((Cumul_goals/Cumul_shots)*100, digits = 3)
  )

ggplot(Stenlund, aes(x = Game, y = C_Shoot_pct)) + geom_line() +
  labs(x = 'Game No.', y = 'Shooting Percentage By Game', title = 'Kevin Stenlund S% by Game')


