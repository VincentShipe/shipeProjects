TeamData <- read.csv("teams.csv")

library(dplyr)

FiveOnFive = TeamData %>%
  filter(situation == '5on5') %>%
  mutate(xG_per_Game = xGoalsFor/games_played) %>%
  mutate(xGA_per_Game = xGoalsAgainst/games_played)

FoFMutation = FiveOnFive%>%
  select(team, games_played, xG_per_Game, xGA_per_Game, penaltiesFor, penalityMinutesFor, 
         penaltiesAgainst, penalityMinutesAgainst, shotsOnGoalFor, shotsOnGoalAgainst,
         shotAttemptsFor, shotAttemptsAgainst)%>%
  mutate(PenaltyDifferential = penaltiesAgainst - penaltiesFor) %>%
  mutate(PenMinDiff = penalityMinutesAgainst - penalityMinutesFor) %>%
  mutate(TotalGameXG = xG_per_Game + xGA_per_Game) %>%
  mutate(TotalShotAttempts = shotAttemptsAgainst + shotAttemptsFor) %>%
  mutate(TotalSOG = shotsOnGoalFor + shotsOnGoalAgainst) %>%
  mutate(PenaltiesTakenPG = penaltiesFor/games_played) %>%
  mutate(PenaltiesDrawnPG = penaltiesAgainst/games_played)

HighEvent = FiveOnFive %>%
  filter(xG_per_Game > 2) %>%
  filter(xGA_per_Game > 2) %>%
  select(team, games_played, xG_per_Game, xGA_per_Game, penaltiesFor, penalityMinutesFor, 
         penaltiesAgainst, penalityMinutesAgainst, shotsOnGoalFor, shotsOnGoalAgainst,
         shotAttemptsFor, shotAttemptsAgainst) 

HighEvent %>%
  select(team, PenaltyDifferential, PenMinDiff, penalityMinutesFor, penalityMinutesAgainst, 
         penaltiesFor, penaltiesAgainst) %>%
  arrange(PenaltyDifferential)

reg1 <- lm(TotalGameXG ~ PenaltiesTakenPG + PenaltiesDrawnPG, data = HighEvent)
summary(reg1)

reg2 <- lm(TotalGameXG ~ PenaltiesTakenPG + PenaltiesDrawnPG, data = FiveOnFive)
summary(reg2)

library(corrplot)
For = FiveOnFive %>%
  select(penaltiesAgainst, xGoalsFor, shotsOnGoalFor, shotAttemptsFor)
Against = FiveOnFive %>%
  select(penaltiesFor, xGoalsAgainst, shotsOnGoalAgainst, shotAttemptsAgainst)

c1 = cor(For)
corrplot(c1)

c2 = cor(Against)
corrplot(c2)
