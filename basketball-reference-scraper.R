# import libraries --------------------------------------------------------

library(rvest)
library(dplyr)
library(lubridate)

# define functions & vars -------------------------------------------------

import_team_data <- function(team_abrv) {
  
  #scrape table from basketball-reference
  url <- sprintf('https://www.basketball-reference.com/teams/%s/2021/gamelog/', team_abrv)
  
  #import table into dataframe
  table <- url %>% 
    read_html() %>% 
    html_nodes(xpath ='//*[@id="tgl_basic"]') %>% 
    html_table(header = TRUE)
  table <- table[[1]]
  
  #rename cols
  names(table) <- table[1,]
  
  #clean up data
  table <- table[2:nrow(table),]
  table <- table[,3:15]
  names(table)[2] <- "home_ind"
  table$home_ind <- table$home_ind != "@"
  
  #select only relevant columns
  table <- table %>% 
    as_tibble() %>% 
    transmute(game_date = ymd(Date),
           home_ind,
           opponent = Opp,
           team = team_abrv,
           win_ind = `W/L` == "W",
           thp = as.numeric(`3P`),
           twp = as.numeric(FG) - thp,
           ft = as.numeric(FT)
           )
  
  return(table)
}

#define a list of all NBA teams
nba_teams_short <- c('PHI', 'ORL', 'IND', 'ATL', 'BOS', 'BRK', 'CLE', 
                     'MIA', 'CHO', 'CHI', 'NYK', 'MIL', 'TOR', 'DET', 
                     'WAS', 'LAC', 'PHO', 'NOP', 'LAL', 'SAC', 'MIN', 
                     'POR', 'SAS', 'UTA', 'GSW', 'HOU', 'DAL', 'MEM',
                     'OKC', 'DEN')


# loop through teams to assemble data -------------------------------------

#loop through the list and produce a complete dataframe
for (team in nba_teams_short) {
  if (!exists('complete_data')) {
    complete_data <- import_team_data(team)
  } else {
    complete_data <- rbind(complete_data, import_team_data(team))
  }
}

View(complete_data)
