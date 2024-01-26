People %>%
  head(2)

Batting %>%
  filter(playerID == 'aaronha01')

Pitching %>%
  filter(playerID == 'ruthba01')

Fielding %>%
  filter(playerID == 'ruthba01')

Teams %>%
  filter(teamID == 'NYA') %>%
  filter(yearID == '1927')

hr_leader <- function(data) {
  data %>%
    group_by(playerID) %>%
    summarise(HR = sum(HR)) %>%
    arrange(desc(HR)) %>%
    head(1)
}

Batting %>%
  mutate(decade = 10 * floor(yearID/10)) %>%
  split(pull(., decade)) %>%
  map_df(hr_leader, .id = "decade")

## Exercise 2.13.1.a
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)
Player <- c('Rickey Henderson', 'Lou Brock', 'Ty Cobb', 'Eddie Collins', 'Max Carey', 'Joe Morgan', 'Luis Aparicio', 'Paul Molitor', 'Roberto Alomar')

## Exercise 2.13.1.b
SB.attempt <- SB + CS

## Exercise 2.13.1.c
SuccessRate = SB/SB.attempt

## Exercise 2.13.1.d
SBperGame = SB/G

## Exercise 2.13.1.e
ggplot( , aes(x = SBperGame, y = SuccessRate, label = Player)) + geom_point() + geom_text(hjust = 0, vjust = 0)


## Exercise 2.13.2.a
outcomes <- c('Single', 'Out', 'Out', 'Single', 'Out', 'Double', 'Out', 'Walk', 'Out', 'Single')

## Exercise 2.13.2.b
table(outcomes)

## Exercise 2.13.2.c
f.outcomes  <-  factor(outcomes,
                       levels = c("Out", "Walk", "Single", "Double"))
table(f.outcomes)

## Exercise 2.13.2.d
outcomes == 'Walk'
sum(outcomes == 'Walk')


## Exercise 2.13.3.a
W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316)
Name <- c('Alexander', 'Clemens', 'Galvin', 'Johnson', 'Maddux', 'Mathewson', 'Nichols', 'Spahn', 'Young')

## Exercise 2.13.3.b
Win.PCT = W/(W + L)*100

## Exercise 2.13.3.c
Wins.350 <- data.frame(Name, W, L, Win.PCT)

## Exercise 2.13.3.d
arrange(Wins.350, Win.PCT)

## Exercise 2.13.4.a
SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217)

## Exercise 2.13.4.b
SO.BB.Ratio = SO/BB

## Exercise 2.13.4.c
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)

## Exercise 2.13.4.d
filter(SO.BB, SO.BB.Ratio > 2.8)

## Exercise 2.13.4.e
arrange(SO.BB, desc(BB))

## Exercise 2.13.5.a
library(Lahman)
Pitching = Pitching

## Exercise 2.13.5.b
career.pitching = Pitching %>%
  group_by(playerID) %>%
  summarize(SO = sum(SO, na.rm = TRUE),
            BB = sum(BB, na.rm = TRUE),
            IPouts = sum(IPouts, na.rm = TRUE),
            midYear = median(yearID, na.rm = TRUE))
merge(career.pitching, Pitching, by = 'playerID')

## Exercise 2.13.5.c
career.10000 = career.pitching %>%
  filter(IPouts >= 10000)

## Exercise 2.13.5.d
career.10000 = career.10000 %>%
  mutate(SO.BB.Ratio = SO/BB)
ggplot(career.10000, aes(x = midYear, y = SO.BB.Ratio)) + geom_point() + geom_smooth()

## Home Run Career Graphs

# Function to get proper birth year
get_birthyear <- function(Name){
  Names <- unlist(strsplit(Name, " "))
  People %>%
    filter(nameFirst == Names[1],
           nameLast == Names[2]) %>%
    mutate(birthYear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthYear)
}

# Get Information for Players
PlayerInfo <- bind_rows(
  get_birthyear("Babe Ruth"),
  get_birthyear("Hank Aaron"),
  get_birthyear("Barry Bonds"),
  get_birthyear("Alex Rodriguez")
)

# Create Player Data Frames
Batting %>%
  inner_join(PlayerInfo, by = "playerID") %>%
  mutate(Age = yearID - birthYear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> HRdata

# Creating the Graph
ggplot(HRdata, aes(x = Age, y = CHR, linetype = Player, color = Player)) + geom_line()


## The 1998 Home Run Race

# Load in the Retrosheet data
fields <- read_csv("fields.csv")
data1998 <- read_csv("all1998.csv", col_names = pull(fields, Header))

# Find McGwire, Sosa, (and maybe Griffey) player IDs
sosa_id <- People %>%
  filter(nameFirst == 'Sammy', nameLast == 'Sosa') %>%
  pull(retroID)
mcgwire_id <- People %>%
  filter(nameFirst == 'Mark', nameLast == 'McGwire') %>%
  pull(retroID)
griffey_id <- People %>%
  filter(nameFirst == 'Ken', nameLast == 'Griffey') %>%
  pull(retroID)
  griffey_id = "grifk002"
  
# Extract Plate Appearances
hr_race <- data1998 %>%
  filter(BAT_ID %in% c(sosa_id, mcgwire_id))

# Cumulative HR function
cum_hr <- function(d) {
  d %>%
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) %>%
    arrange(Date) %>%
    mutate(HR = ifelse(EVENT_CD == 23, 1, 0),
           cumulHR = cumsum(HR)) %>%
    select(Date, cumulHR)
}

hr_ytd <- hr_race %>%
  split(pull(., BAT_ID)) %>%
  map_df(cum_hr, .id = "BAT_ID") %>%
  inner_join(People, by = c("BAT_ID" = "retroID"))

# Creating the Graph
ggplot(hr_ytd, aes(x = Date, y = cumulHR, linetype = nameLast)) +
  geom_line() +
  geom_hline(yintercept = 62, color = 'dodgerblue') +
  annotate("text", ymd('1998-04-15'), 65, label = "62", color = 'dodgerblue') +
  ylab("Home Runs in the Season")

## Exercise 3.10.1

hofpitching <- read_csv('hofpitching.csv')
hofpitching <- hofpitching %>%
  mutate(BF.group = cut(BF, c(0, 10000, 15000, 20000, 30000), 
                        labels = c('Less than 10000', "10000-15000", '15000-20000', 'More than 20000')))

# Part A
BFdata = summarise(group_by(hofpitching, BF.group), freq = n())
BFdata

# Part B
ggplot(hofpitching, aes(x = BF.group)) + geom_bar()

# Part C
ggplot(BFdata, aes(x = BF.group, y = freq)) + geom_point() +
  labs(x = 'Batters Faced', y = 'Frequency', title = 'Batters Faced by HOF Pitchers') +
  coord_flip()

## Exercise 3.10.2
hofpitching = rename(hofpitching, Player = `...2`)
hofpitching$Player <- gsub(" HOF", "", hofpitching$Player)

# Part A
ggplot(hofpitching, aes(x = WAR)) + geom_histogram()

# Part B
ggplot(hofpitching, aes(x = WAR)) + geom_histogram() +
  geom_text(data = filter(hofpitching, WAR > 125), aes(x = WAR, label = Player), y = 1.5, vjust = 1)


## Exercise 3.10.3
hofpitching <- hofpitching %>%
  mutate(WAR_season = WAR/Yrs)

# Part A
ggplot(hofpitching, aes(x = WAR_season, y = BF.group)) + geom_point()

# Part B
ggplot(hofpitching, aes(x = WAR_season, y = BF.group)) + geom_boxplot()

# Part C

## It appears that the more batters that a pitcher faces, the higher their WAR per season is, at least in terms of a general trend. 


## Exercise 3.10.4
hofpitching <- hofpitching %>%
  mutate(MidYear = (From+To) / 2)
recent_hofpitching <- hofpitching %>%
  filter(MidYear >= 1960)

# Part A
arrange(recent_hofpitching, WAR_season)

# Part B
ggplot(recent_hofpitching, aes(x = WAR_season, y = 0)) + geom_jitter(height = 0.5) + ylim(-3, 3) +
  geom_text(aes(label = Player), y = 0.8, vjust = -1, size = 3) 

# Part C
ggplot(recent_hofpitching, aes(x = WAR_season, y = 0)) + geom_jitter(height = 0.5) + ylim(-3, 3) +
  geom_text(data = filter(recent_hofpitching, WAR_season > 4.5), aes(x = WAR_season, label = Player), y = 0.8, vjust = 1, size = 3)


## Exercise 3.10.5

# Part A
ggplot(hofpitching, aes(x = MidYear, y = WAR_season)) + geom_point() + geom_smooth()

# Part B

# There does not appear to be much of a pattern in this scatterplot as there appears to be a lot of variation in the points on the graph. There is some general downard movement, but it is a very weak correlation clearly.

# Part C
ggplot(hofpitching, aes(x = MidYear, y = WAR_season)) + geom_point() + geom_smooth() +
  geom_text(data = filter(hofpitching, WAR_season < 2 & MidYear < 1900), aes(x = MidYear, y = WAR_season, label = Player), hjust = -0.1)

## Exercise 3.10.6

# Part A
People <- Lahman::People
Batting <- Lahman::Batting

# Part B and C
PlayerBasics <- bind_rows(
  get_birthyear("Ty Cobb"),
  get_birthyear("Ted Williams"),
  get_birthyear("Pete Rose")
)
PlayerBasics <- filter(PlayerBasics, birthYear < 1945)

Batting %>%
  inner_join(PlayerBasics, by = "playerID") %>%
  mutate(Age = yearID - birthYear) %>%
  select(Player, Age, H) %>%
  group_by(Player) %>%
  mutate(CH = cumsum(H)) -> Hit_data

# Part D
ggplot(data = filter(Hit_data, Player == 'Pete Rose'), aes(x = Age, y = CH)) + geom_line(linewidth = 1) +
  labs(y = 'Total Hits', title = "Pete Rose's Career Hits for Each Season")

# Part E
ggplot(Hit_data, aes(x = Age, y = CH, linetype = Player, color = Player)) + geom_line(linewidth = 1) +
  labs(y = 'Hits', title = 'Hall of Famers Career Hits Throughout their Careers')

# Part F
# Despite Pete Rose starting his career at a later age than both Cobb and Williams, he remained in baseball until age 45 (longer than both of the other players in the graph) which is what allowed him to exceed both Cobb and Williams in career hits. 
# Williams has the lowest number of career hits partly because of his shorter career compared to Cobb and Rose and the break in the middle of his career when Williams went off to serve in World War II.
# Cobb began his career at a much younger age and the slope of his career hits line is very similar to Rose's, but Cobb played only a few fewer seasons than Rose, which is how Cobb got to the 4,191 career hits that was the career record until Rose came along. 
# Had Williams career lasted a few more years and not had gone off to war, then it is likely that he could have gotten to 4,000 hits as there are several points where his career hit line has a similar trend/slope to Rose's and Cobb's career trend/slope in their career hit graphs. 


## Exercise 3.10.7

# Part A
mac_data = data1998 %>% filter(BAT_ID == mcgwire_id)
sosa_data = data1998 %>% filter(BAT_ID == sosa_id)

# Part B
mac_data = filter(mac_data, BAT_EVENT_FL == TRUE)
sosa_data = filter(sosa_data, BAT_EVENT_FL == TRUE)

# Part C
mac_data <- mutate(mac_data, PA = 1:nrow(mac_data), Player = 'Mark McGwire')
sosa_data <- mutate(sosa_data, PA = 1:nrow(sosa_data), Player = 'Sammy Sosa')

# Part D
mac_HR.PA <- mac_data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)
sosa_HR.PA <- sosa_data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)

# Part E
mac_spacings <- diff(c(0, mac_HR.PA))
sosa_spacings <- diff(c(0, sosa_HR.PA))

mac_spacings = data_frame(mac_spacings, Player = 'Mark McGwire')
mac_spacings = rename(mac_spacings, Spacing = mac_spacings)

sosa_spacings = data_frame(sosa_spacings, Player = 'Sammy Sosa')
sosa_spacings = rename(sosa_spacings, Spacing = sosa_spacings)

HR_spacings <- bind_rows(mac_spacings, sosa_spacings)

# Part F
mac.plot = ggplot(data = filter(HR_spacings, Player == 'Mark McGwire'), aes(x = Spacing)) + geom_histogram(color = 'white', fill = 'red') + ggtitle('1998 Home Run Race Spacing')
sosa.plot = ggplot(data = filter(HR_spacings, Player == 'Sammy Sosa'), aes(x = Spacing)) + geom_histogram(color = 'white', fill = 'blue')
ggarrange(mac.plot, sosa.plot, nrow = 2)

## Section 4.3: Linear Regression

my_teams <- Teams %>%
  filter(yearID >= 2001) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

run_diff <- ggplot(my_teams, aes(x = RD, y = Wpct)) + geom_point() +
  labs(x = 'Run Differential', y = 'Winning Percentage')

linfit <- lm(Wpct ~ RD, data = my_teams)
linfit

run_diff + geom_smooth(method = "lm", se = FALSE, color = 'dodgerblue')

my_teams_aug <- augment(linfit, data = my_teams)

# Residual Plot

resid.plot <- ggplot(my_teams_aug, aes(x = RD, y = .resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = 'Run Differential', y = 'Residual')

highlight <- my_teams_aug %>%
  arrange(desc(abs(.resid))) %>%
  head(4)

resid.plot +
  geom_point(data = highlight, color = '#478778') +
  geom_text_repel(data = highlight, color = '#478778', aes(label = paste(teamID, yearID)))

# Mean Square Errors

resid_summary <- my_teams_aug %>%
  summarise(count = n(), avg = mean(.resid),
            RMSE = sqrt(mean(.resid^2)))
resid_summary

rmse <- resid_summary %>% pull(RMSE)

my_teams_aug %>%
  summarise(count = n(),
            within_one = sum(abs(.resid) < rmse),
            within_two = sum(abs(.resid) < 2 * rmse)) %>%
  mutate(within_one_pct = within_one/count,
         within_two_pct = within_two/count)

## 4.4: Pythagorean Winning Percentage

my_teams <- my_teams %>%
  mutate(Wpct_pyt = R^2 / (R^2 + RA^2),
         residuals_pyt = Wpct - Wpct_pyt)
my_teams %>% summarise(rmse = sqrt(mean(residuals_pyt^2)))

my_teams <- my_teams %>%
  mutate(logWratio = log(W/L),
         logRratio = log(R/RA))
pyt_fit <- lm(logWratio ~ 0 + logRratio, data = my_teams)
pyt_fit

# 2011 Season

glheaders <- read_csv('game_log_header.csv')
gl2011 <- read_csv('gl2011.txt',
                   col_names = names(glheaders),
                   na = character())

# Boston Red Sox

BOS2011 <- gl2011 %>%
  filter(HomeTeam == 'BOS' | VisitingTeam == 'BOS') %>%
  select(VisitingTeam, HomeTeam, VisitorRunsScored, HomeRunsScore) %>%
  mutate(ScoreDiff = ifelse(HomeTeam == 'BOS', HomeRunsScore - VisitorRunsScored, VisitorRunsScored - HomeRunsScore),
         W = ScoreDiff > 0)

BOS2011 %>%
  group_by(W) %>%
  skim(ScoreDiff)

# Results: One-run wins

results <- gl2011 %>%
  select(VisitingTeam, HomeTeam, VisitorRunsScored, HomeRunsScore) %>%
  mutate(winner = ifelse(HomeRunsScore > VisitorRunsScored, HomeTeam, VisitingTeam),
         diff = abs(VisitorRunsScored - HomeRunsScore))

one_run_wins <- results %>%
  filter(diff == 1) %>%
  group_by(winner) %>%
  summarise(one_run_w = n())

teams2011 <- my_teams %>%
  filter(yearID == 2011) %>%
  mutate(teamID = ifelse(teamID == "LAA", "ANA", as.character(teamID))) %>%
  inner_join(one_run_wins, by = c("teamID" = "winner"))

ggplot(teams2011, aes(x = one_run_w, y = residuals_pyt)) + geom_point() +
  geom_text_repel(aes(label = teamID)) +
  labs(x = 'One Run Wins', y = 'Pythagorean Residuals')

# Top Closers

top_closers <- Pitching %>%
  filter(GF > 50 & ERA < 2.5) %>%
  select(playerID, yearID, teamID)

my_teams %>%
  inner_join(top_closers) %>%
  pull(residuals_pyt) %>%
  summary()

## Exercise 4.7.1

# Part A

teams60s <- Teams %>%
  filter(yearID >= 1961 & yearID <= 1970) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

teams70s <- Teams %>%
  filter(yearID >= 1971 & yearID <= 1980) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

teams80s <- Teams %>%
  filter(yearID >= 1981 & yearID <= 1990) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

teams90s <- Teams %>%
  filter(yearID >= 1991 & yearID <= 2000) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

sixties <- lm(Wpct ~ RD, data = teams60s)
seventies <- lm(Wpct ~ RD, data = teams70s)
eighties <- lm(Wpct ~ RD, data = teams80s)
nineties <- lm(Wpct ~ RD, data = teams90s)

# Part B

sixties
sixties$coefficients[1] + sixties$coefficients[2] * 10

seventies
seventies$coefficients[1] + seventies$coefficients[2] * 10

eighties
eighties$coefficients[1] + eighties$coefficients[2] * 10

nineties
nineties$coefficients[1] + nineties$coefficients[2] * 10

# In each of the decades, a run differential of 10 leads to a winning percentage estimated to be between .506 and .507 which translates to 82 wins in a 162 game season. The difference in projected winning percentage in each decade for a team with a +10 RD is minimal at best as the decades change.


## Exercise 4.7.2

# Part A

Teams_19thCentury <- Teams %>%
  filter(yearID < 1901)

Teams_19thCentury <- Teams_19thCentury %>%
  mutate(Wpct_pyt = R^2 / (R^2 + RA^2),
         Wpct = W / (W + L),
         resids = Wpct - Wpct_pyt, 
         RD = R - RA)

# Part B

ggplot(Teams_19thCentury, aes(x = RD, y = resids)) + geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = 'Run Differentials', y = 'Residuals', title = 'Run Differential and Pythagorean Wins for Teams in the 19th Century') 

# It appears that the great teams of the 19th Century did slightly better than expected by their Pythagorean Winning Percentage while the worst teams of the 19th Century were slightly worse than expected based on their Pythagorean Winning Percentage. 
# However, it is notable that these differences between Pythagorean winning percentage and actual winning percentage are minimal at the very best. 