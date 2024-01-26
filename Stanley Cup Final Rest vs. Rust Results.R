Finals_Matchups <- read_excel('SCFinalist_RestvsRust.xlsx')

Finals_Results = Finals_Matchups %>%
  filter(Year != 2023)

Team2_MoreGP = Finals_Results %>%
  filter(Who_Played_More == 'T2')

Finals_Results %>%
  count(REST_VS_RUST, wt = NULL, sort = TRUE) %>%
  group_by(REST_VS_RUST)

Team2_MoreGP %>%
  count(REST_VS_RUST, wt = NULL, sort = TRUE) %>%
  group_by(REST_VS_RUST)

Team1_MoreGP = Finals_Results %>%
  filter(Who_Played_More == 'T1')

Team1_MoreGP %>%
  count(REST_VS_RUST, wt = NULL, sort = TRUE) %>%
  group_by(REST_VS_RUST)

Finals_Results %>%
  count(Who_Played_More) %>%
  group_by(Who_Played_More)

max(Finals_Results$Days_Off_T1)
max(Finals_Results$Days_Off_T2)

Finals_Results %>%
  select(Year, Team_1, Days_Off_T1) %>%
  arrange(desc(Days_Off_T1))

Finals_Results %>%
  select(Year, Team_2, Days_Off_T2) %>%
  arrange(desc(Days_Off_T2))

(24/35)*100

mean(Finals_Results$Days_Off_T1)
mean(Finals_Results$Days_Off_T2)
