library(dplyr)
library(readxl)

Forty_G_Teams = read_excel("Forty_Goal_Scorers.xlsx")

Forty_G_Teams %>%
  group_by(Count40GS) %>%
  count(`Playoffs?`, wt = NULL, sort = TRUE) 

ThreeOrMore = Forty_G_Teams %>%
  filter(Count40GS > 2)

CapEra = Forty_G_Teams %>%
  filter(Season > 2005)

CapEra %>%
  count(`Playoffs?`, wt = NULL, sort = TRUE)
