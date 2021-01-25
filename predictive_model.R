library(readr)

#import betting odds
source("data-sourcing/market-odds.R")

#import complete season data
source("data-sourcing/basketball-reference-scraper.R")
  
#drop unnessecary objects

remove(data, 
       df, 
       full_url, 
       key, 
       other_params, 
       sportKey, 
       team, 
       url, 
       import_team_data)

nba <- complete_data

team_name_crosswalk <- tibble(team_names_short = unique(nba$team),
                              team_names_long = c(
                                "Philadelphia 76ers",
                                "Orlando Magic",
                                "Indiana Pacers",
                                "Atlanta Hawks",
                                "Boston Celtics",
                                "Brooklyn Nets",
                                "Cleveland Cavaliers",
                                "Miami Heat",
                                "Charlotte Hornets",
                                "Chicago Bulls",
                                "New York Knicks",
                                "Milwaukee Bucks",
                                "Toronto Raptors",
                                "Detroit Pistons",
                                "Washington Wizards",
                                "Los Angeles Clippers",
                                "Phoenix Suns",
                                "New Orleans Pelicans",
                                "Los Angeles Lakers",
                                "Sacramento Kings",
                                "Minnesota Timberwolves",
                                "Portland Trail Blazers",
                                "San Antonio Spurs",
                                "Utah Jazz",
                                "Golden State Warriors",
                                "Houston Rockets",
                                "Dallas Mavericks",
                                "Memphis Grizzlies",
                                "Oklahoma City Thunder",
                                "Denver Nuggets"
                              ))


# Build stats sheets ------------------------------------------------------

offensive_stats <- nba %>% 
  group_by(team) %>% 
  summarise(avg_thp = mean(thp),
            avg_twp = mean(twp),
            avg_ft = mean(ft))

deffensive_stats <-  nba %>% 
  left_join(offensive_stats, by = "team") %>% 
  mutate(thp_diff = avg_thp - thp,
         twp_diff = avg_twp - twp,
         ft_diff = avg_ft - ft) %>% 
  group_by(opponent) %>% 
  summarize(deff_thp = mean(thp_diff),
            deff_twp = mean(twp_diff),
            deff_ft = mean(ft_diff))


# build model -------------------------------------------------------------

score_forecast_model <- nba %>% 
  left_join(offensive_stats, by = "team") %>% 
  left_join(deffensive_stats, by = "opponent") %>% 
  lm(points ~ avg_thp + 
              avg_twp + 
              avg_ft +
              deff_thp +
              deff_twp +
              deff_ft +
              home_ind,
     data = .)

nba$score_forecast <- nba %>% 
  left_join(offensive_stats, by = "team") %>% 
  left_join(deffensive_stats, by = "opponent") %>% 
  predict(score_forecast_model, ., type = "response")

final_game_model <- nba %>% 
  inner_join(nba, by = c("team" = "opponent", "game_date" = "game_date")) %>% 
  filter(home_ind.x == TRUE) %>% 
  glm(win_ind.x ~ score_forecast.x + score_forecast.y, data = ., family = "binomial")
  

# execute_predictions -----------------------------------------------------
forecast_data <- odds_sheet %>% 
  inner_join(odds_sheet, by =c("commence_time" = "commence_time", 
                               "team" = "opponent")) %>% 
 # filter(team.x != team.y) %>% 
  select(commence_time, 
         team = team,
         opponent = team.y,
         home_ind = home_ind.x,
         market_probability = market_probs.x) %>% 
  left_join(team_name_crosswalk, by = c("team" = "team_names_long")) %>% 
  left_join(team_name_crosswalk, by = c("opponent" = "team_names_long")) %>% 
  rename(team_short = team_names_short.x, opponent_short = team_names_short.y) %>% 
  left_join(offensive_stats, by = c("team_short" = "team")) %>% 
  left_join(deffensive_stats, by = c("opponent_short" = "opponent"))

forecast_data$score_forecast <- predict(score_forecast_model, forecast_data, type = "response")
  
forecast_data <- forecast_data %>% 
  inner_join(forecast_data, by = c("team" = "opponent", "commence_time" = "commence_time")) %>% 
  filter(home_ind.x == TRUE) %>% 
  select(commence_time,
         home_team = team,
         away_team = opponent,
         home_market_probability = market_probability.x,
         away_market_probability = market_probability.y,
         score_forecast.x,
         score_forecast.y)

forecast_data$prediction <- predict(final_game_model, forecast_data, type = "response")

final_output <- forecast_data %>% 
  select(commence_time, 
         home_team, 
         away_team, 
         home_market_probability, 
         away_market_probability,
         prediction) %>% 
  mutate(true_home_prob = home_market_probability/(home_market_probability + away_market_probability),
         guardrail = (true_home_prob*(1-true_home_prob))*(2/5),
         lower_gr = true_home_prob - guardrail,
         upper_gr = true_home_prob + guardrail) %>%
  filter(prediction >= lower_gr,
         prediction <= upper_gr) %>%
  left_join(odds_sheet, by = c("home_team" = "team", "commence_time" = "commence_time")) %>% 
  left_join(odds_sheet, by = c("away_team" = "team", "commence_time" = "commence_time")) %>% 
  select(commence_time, 
         home_team, 
         away_team, 
         home_win_pred = prediction, 
         home_odds = odds.x, 
         away_odds = odds.y)

write.csv(final_output, "20210124_predictive_model_output.csv", row.names = FALSE)
