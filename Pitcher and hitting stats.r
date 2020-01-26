# Batting and Pitching Stats



# rm(list = ls())
# library(tidyverse)
# library(retrosheet)


setwd("~/Downloads/Project/Baseball/Data/baseballdatabank-2019.2/core/")

# Reads in Season-by-Season Statistics
pitching_stats <- read.csv("Pitching.csv")
batting_stats <- read.csv("Batting.csv")
people <- read.csv("People.csv")


# Cleaning Season-by-season Hitting Statistics
bat_stats <- batting_stats[batting_stats$yearID >= 2015, ] %>% 
  mutate(team = as.character(teamID),
         team = sub("LAA", "ANA", team)) %>% 
  filter(grepl("ANA", team)  | grepl("CLE", team))



# Cleaning Player Names for Season-by-Season Hitting Stats
people_batters <- people %>%
  mutate(batter_first_name = tolower(nameFirst),
         batter_last_name = tolower(nameLast))



# Adding Player Names to Season-by-Season Hitting Stats
bat_stats_people <- left_join(bat_stats, people_batters, by = "playerID") %>% 
  select(playerID,retroID, bbrefID,yearID,teamID,team,batter_first_name,batter_last_name,G,
       AB,R,H,X2B,X3B,HR,RBI,SB,CS)


#not_selected(stint,lgID,BB,SO,IBB,HBP,SH,SF,GIDP,birthYear,birthMonth,birthDay
#birthCountry,birthState, birthCity, deathYear, deathMonth, deathDay, deathCountry,deathState,
#deathCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame)


# Adding Season-by-Season Hitting Stats to our Master Event File
clean_bat_stats_people <- left_join(clean_pbyp_data, bat_stats_people, by = c("yearID", "team", c("play.retroID" = "retroID")))



# Cleaning Season-by-season Pitching Statistics
pitch_stats <- pitching_stats[pitching_stats$yearID >= 2015, ] %>% 
  mutate(team = as.character(teamID),
         team = sub("LAA", "ANA", team)) %>% 
  filter(grepl("ANA", team)  | grepl("CLE", team))

# Cleaning Player Names for Season-by-Season Pithching Stats
people_pitchers <- people %>%
  mutate(pitcher_first_name = tolower(nameFirst),
         pitcher_last_name = tolower(nameLast),
         pitcher_first_name = trim(pitcher_first_name),
         pitcher_last_name = trim(pitcher_last_name),
         pitcher_first_name = gsub("\\.|\\| *| ", "", pitcher_first_name),
         pitcher_last_name = gsub("\\.|\\| *| ", "", pitcher_last_name))


# Adding Player Names to Season-by-Season Pitching Stats
pitch_stats_people <- left_join(pitch_stats, people_pitchers, by = "playerID") %>% 
  select(playerID,retroID, bbrefID, yearID,teamID,team, pitcher_first_name, pitcher_last_name,W,L,G,GS,CG,SHO,SV,ERA,IBB,WP,HBP)


#notselected(lgID, IPouts, H, ER,HR,BB,SO,BAOpp,BK,BFP,GF,R,SH,SF,GIDP,stint,birthYear,
#birthMonth,birthDay,birthCountry,birthState,birthCity,deathYear,deathMonth,deathDay,
#deathCountry,deathState,deathCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame)




clean_bat_pitch_stats_people <- left_join(clean_bat_stats_people, pitch_stats_people, 
                                          by = c("yearID", "pitcher_last_name", "pitcher_first_name"))


#rm(pitch_stats, pitch_stats_people, clean_pbyp_data, clean_pitch_stats_people, bat_stats, bat_stats_people)

