# set up packages ---------------------------------------------------------

library(httr)
library(rjson)

# set up url and params ---------------------------------------------------

key <- "e26d32847e5789a92d8da20882411978"
sportKey <- "basketball_nba"
other_params <- "&region=us&mkt=h2h"
url <- "https://api.the-odds-api.com/v3/odds/"

full_url <- paste(url, 
    "?apiKey=",
    key,
    "&sport=",
    sportKey,
    other_params,
    sep = "")

#import data
data = fromJSON(file=full_url)

#extract betting odds
df <- tibble(sports = data[2])

odds_sheet <- df %>% 
  unnest_longer(sports) %>% 
  unnest_wider(sports) %>% 
  unnest_longer(sites) %>% 
  unnest_wider(sites) %>% 
  filter(site_key == "fanduel")  %>% 
  select(commence_time, teams, home_team, odds) %>% 
  unnest_wider(odds) %>% 
  unnest_wider(h2h) %>% 
  rename(home_odds = "...1", away_odds = "...2") %>% 
  unnest_longer(teams) %>% 
  filter(teams != home_team) %>% 
  rename(away_team = teams) %>% 
  mutate(commence_time = as.POSIXct(commence_time, origin="1970-01-01"))

#pivot to longer
odds_sheet <- odds_sheet %>% 
  transmute(commence_time, 
         team = home_team, 
         odds = home_odds,
         home_ind = TRUE) %>% 
  bind_rows(transmute(odds_sheet,
                      commence_time, 
                      team = away_team, 
                      odds = away_odds,
                      home_ind = FALSE)) %>% 
  arrange(commence_time)
  
#convert from odds to probabilities
odds_sheet <- odds_sheet %>% 
  mutate(probs = 1/odds)
