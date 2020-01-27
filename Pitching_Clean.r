

# rm(list = ls())
# library(tidyverse)
# library(retrosheet)
# library(data.table)

setwd("~/Downloads/Project/Baseball/Data/")

gamelogs <- read.csv('games.csv')
atbats <- read.csv('atbats.csv')
player_names <- read.csv('player_names.csv')


# Cleaning Game Logs
games <- gamelogs %>% 
  mutate(date = gsub("-", "/", date),
         date_special = gsub("/", "", date),
         id_v2 = paste0(toupper(home_team), date_special))



# Cleaning Player Names - Pitchers
pnames_pitchers <- player_names %>% 
  mutate(pitcher_first_name = tolower(first_name),
         pitcher_last_name = tolower(last_name))%>% 
  select(id, pitcher_first_name, pitcher_last_name)



# Cleaning Player Names - Hitters
pnames_batters <- player_names %>% 
  mutate(batter_first_name = tolower(first_name),
         batter_last_name = tolower(last_name))%>% 
  select(id, batter_first_name, batter_last_name)



# Cleaning Every at bat per Season
each_ab <- atbats %>% 
  mutate(bottom_inning = ifelse(top == "True", 0, 1)) %>% 
  group_by(g_id, inning, bottom_inning) %>%
  mutate(number_up = row_number()) %>% 
  ungroup() 


each_ab_pitcher_names <- left_join(each_ab, pnames_pitchers, by = c("pitcher_id" = "id"))
each_ab_pitcher_hitter_names <- left_join(each_ab_pitcher_names, pnames_batters, by = c("batter_id" = "id")) 



ab_pitcher_hitter_names_games <- left_join(each_ab_pitcher_hitter_names, games, by = "g_id") %>% 
  mutate(team = ifelse(bottom_inning == 1, toupper(home_team), toupper(away_team)),
         team_v2 = as.character(team),
         team_v2 = sub("SLN", "STL", team_v2),
         team_v2 = sub("CHN", "CHC", team_v2),
         team_v2 = sub("ANA", "LAA", team_v2),
         team_v2 = sub("TBA", "TBR", team_v2),
         team_v2 = sub("CHA", "CHW", team_v2),
         team_v2 = sub("KCA", "KCR", team_v2),
         team_v2 = sub("NYN", "NYM", team_v2),
         team_v2 = sub("SDN", "SDP", team_v2),
         team_v2 = sub("LAN", "LAD", team_v2),
         team_v2 = sub("SFN", "SFG", team_v2),
         team_v2 = sub("NYA", "NYY", team_v2)) %>% 
  mutate(pitcher_first_name = trim(pitcher_first_name),
         pitcher_last_name = trim(pitcher_last_name),
         pitcher_first_name = gsub("\\.| *| ", "", pitcher_first_name),
         pitcher_last_name = gsub("\\.| *| ", "", pitcher_last_name),
         pitcher_last_name = gsub(".+ ", "", pitcher_last_name)) %>% 
  mutate(batter_first_name = trim(batter_first_name),
         batter_last_name = trim(batter_last_name),
         batter_first_name = gsub("\\.| *| ", "", batter_first_name),
         batter_last_name = gsub("\\.| *| ", "", batter_last_name)) %>% 
  select(id_v2, g_id, team, team_v2, inning,
         bottom_inning, number_up, ab_id,batter_id, 
         batter_first_name, batter_last_name,pitcher_id,
         pitcher_first_name, 
         pitcher_last_name, event)

#not_Selected(stand,top,o,p_score,p_throws,attendance,away_final_score,date_special, away_team,date,elapsed_time,home_final_score,
#home_team,start_time,umpire_1B,umpire_2B,umpire_3B,umpire_HP,venue_name,weather,wind,delay


clean_pbyp_data <- left_join(clean, ab_pitcher_hitter_names_games, 
                             by = c("id_v2", "inning", 
                                    "team_v2", "bottom_inning", "batter_last_name", "batter_first_name"))


#rm(games, each_ab, each_ab_pitcher_names, each_ab_pitcher_hitter_names, ab_pitcher_hitter_names_games, clean)


unique(ab_pitcher_hitter_names_games$team)


