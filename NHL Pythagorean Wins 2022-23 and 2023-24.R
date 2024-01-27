library(readxl)
library(tidyverse)
library(ggthemes)
library(ggrepel)

### Calculating Exponent for the Pythagorean Wins Formula

NHL_Team_data <- read_csv('Hockey_Archives/Teams.csv')
NHL_Team_data <- NHL_Team_data %>% filter(year >= 1917 & name != 'Finland') %>% select(year, name, G, W, GF, GA)

NHL_Team_data <- NHL_Team_data %>%
  mutate(logWratio = log(W/ (G - W)), 
         logGFratio = log(GF/GA))

pytFit <- lm(logWratio ~ 0 + logGFratio, NHL_Team_data)
pytFit

# The optimal exponent in the Pythagorean wins formula for the NHL is 1.933

# Load in Team Data

NHL_data_2023_24 <- read_xlsx('NHL_Team_Data_2023_24.xlsx') %>% rename(Team = `...2`)
NHL_data_2022_23 <- read_xlsx('NHL_TeamData_2022_23.xlsx') %>% rename(Team = `...2`)

# Clear Asterisks from Team Names
NHL_data_2022_23$Team = gsub('\\*', '', NHL_data_2022_23$Team)

# Calculate Actual and Pythagorean Winning Percentage
NHL_data_2022_23 = NHL_data_2022_23 %>%
  mutate(GD = GF - GA,
          Win_pct = round(W / GP, 3),
         `Pythagorean Win %` = round((GF ^1.933)/ ((GF^1.933) + (GA^1.933)), 3),
         Pythagorean_resid = Win_pct - `Pythagorean Win %`
  )

NHL_data_2023_24 = NHL_data_2023_24 %>%
  mutate(GD = GF - GA,
        Win_pct = round(W / GP, 3),
         `Pythagorean Win %` = round((GF ^1.933)/ ((GF^1.933) + (GA^1.933)), 3),
        Pythagorean_resid = Win_pct - `Pythagorean Win %`
  )

# Scatter Plot of Goal Differential and Win Percentage
ggplot(NHL_data_2022_23, aes(x = GD, y = Win_pct)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color= 'red')
ggplot(NHL_data_2023_24, aes(x = GD, y = Win_pct)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color= 'red') 

# Scatter Plot of Residual Winning Percentage and Goal Differential
ggplot(NHL_data_2022_23, aes(x = GD, y = Pythagorean_resid)) + geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = 'Goal Differential', y = 'Pythagorean Residuals', title = '2022-23 Pythagorean Win % Residuals') +
  geom_text_repel(data = highlight_these22, color = 'black', aes(label = Team))

highlight_these22 <- NHL_data_2022_23 %>%
  arrange(desc(abs(Pythagorean_resid))) %>% head(5)

ggplot(NHL_data_2023_24, aes(x = GD, y = Pythagorean_resid)) + geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = 'Goal Differential', y = 'Pythagorean Residuals', title = '2023-24 Pythagorean Win % Residuals') +
  geom_text_repel(data = highlight_these23, color = 'black', aes(label = Team))

highlight_these23 <- NHL_data_2023_24 %>%
  arrange(desc(abs(Pythagorean_resid))) %>% head(5)
