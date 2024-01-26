library(readxl)
library(dplyr)

Draft2010 = read_xlsx("2010_NHL_Draft.xlsx")
Draft2011 = read_xlsx("2011_NHL_Draft.xlsx")
Draft2012 = read_xlsx("2012_NHL_Draft.xlsx")
Draft2013 = read_xlsx("2013_NHL_Draft.xlsx")
Draft2014 = read_xlsx("2014_NHL_Draft.xlsx")
Draft2015 = read_xlsx("2015_NHL_Draft.xlsx")
Draft2016 = read_xlsx("2016_NHL_Draft.xlsx")
Draft2017 = read_xlsx("2017_NHL_Draft.xlsx")
Draft2018 = read_xlsx("2018_NHL_Draft.xlsx")
Draft2019 = read_xlsx("2019_NHL_Draft.xlsx")

Draft2010 = Draft2010 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2010)
Draft2011 = Draft2011 %>%
  filter(Pos == 'G') %>%
  rename(Overall = `Overallâ-²`) %>%
  mutate(Draft_Year = 2011)
Draft2012 = Draft2012 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2012)
Draft2013 = Draft2013 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2013)
Draft2014 = Draft2014 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2014)
Draft2015 = Draft2015 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2015)
Draft2016 = Draft2016 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2016)
Draft2017 = Draft2017 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2017)
Draft2018 = Draft2018 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2018)
Draft2019 = Draft2019 %>%
  filter(Pos == 'G') %>%
  mutate(Draft_Year = 2019)

Goalies_Drafted_2010s = bind_rows(Draft2010, Draft2011, Draft2012, Draft2013, Draft2014, 
                                  Draft2015, Draft2016, Draft2017, Draft2018, Draft2019)
Goalies_Drafted_2010s = Goalies_Drafted_2010s %>%
  select(Overall, Player, Team, Draft_Year)

writexl::write_xlsx(Goalies_Drafted_2010s, "Goalies Drafted in the 2010s.xlsx")
