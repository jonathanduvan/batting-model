# Load Baseball Data File


rm(list = ls())
library(tidyverse)
library(retrosheet)
library(data.table)
library(glue)
library(readxl)

# Load 1: Retrosheet Data -------------------------------------------------


logos <- c("ANA", "BAL", "BOS", 'CHA', 'CLE', 'DET', 'HOU', 'KCA', 'MIN', 'NYA', 
           'OAK', 'SEA', 'TBA', 'TEX', 'TOR', 'ARI', 'ATL', 'CHN', 'CIN', 'COL', 
           'LAN', 'MIA', 'MIL', 'NYN', 'PHI', 'PIT', 'SDN', 'SFN', 'SLN', 'WAS')

clean_event_data <- list()
for (year in 2018){
  for (game in 1:2){
    for (team in logos[13]) {
      
      play <- data.frame(getRetrosheet("play", year, team)[[game]]["play"])
      id <- data.frame(getRetrosheet("play", year, team)[[game]]["id"])
      version <- data.frame(getRetrosheet("play", year, team)[[game]]["version"])
      info <- data.frame(getRetrosheet("play", year, team)[[game]]["info"])
      start <- data.frame(getRetrosheet("play", year, team)[[game]]["start"])
      sub <- data.frame(getRetrosheet("play", year, team)[[game]]["sub"])  
      
      sub_v2 <- sub %>% 
        rename(play.retroID = sub.retroID,
               play.name = sub.name,
               play.team = sub.team, 
               play.batPos = sub.batPos,
               play.fieldPos = sub.fieldPos)
      
      start_v2 <- start %>% 
        rename(play.retroID = start.retroID,
               play.name = start.name,
               play.team = start.team, 
               play.batPos = start.batPos,
               play.fieldPos = start.fieldPos)
      
      id_v2 <- id  %>% 
        filter(!is.na(id))
      
      info_v2 <- info  %>% 
        filter(!is.na(info.category)) %>% 
        spread(1, 2)
      
      version_v2 <- version  %>% 
        filter(!is.na(version)) 
      
      
      players <- rbind(start_v2, sub_v2)
      clean_event <- left_join(play, players, by = c("play.retroID", 'play.team'))
      clean_event <- cbind(id_v2, version_v2, info_v2, clean_event) %>% 
        mutate(id = as.character(id))
      
      clean_event_data <- rbind(clean_event_data, clean_event)
      
      
      rm('id', 'version', 'info', 'info_v2', 'start', 'start_v2', 'play','sub', 'sub_v2', 'players')
    }
  }
}



# Load 2: Kaggle Play-by-play Pitching Data --------------------------------------

setwd("~/Downloads/Project/Baseball/Data/")

gamelogs <- read.csv('games.csv')
atbats <- read.csv('atbats.csv')
player_names <- read.csv('player_names.csv')


#ab_pitcher_hitter_name_games
#not_Selected(stand,top,o,p_score,p_throws,attendance,away_final_score,date_special, away_team,date,elapsed_time,home_final_score,
#home_team,start_time,umpire_1B,umpire_2B,umpire_3B,umpire_HP,venue_name,weather,wind,delay


# Load 3: Lehman Season-by-Season Data -----------------------------------

setwd("~/Downloads/Project/Baseball/Data/baseballdatabank-2019.2/core/")

# Reads in Season-by-Season Statistics
pitching_stats <- read.csv("Pitching.csv")
batting_stats <- read.csv("Batting.csv")
people <- read.csv("People.csv")



#Pitch_stats_people
#notselected(lgID, IPouts, H, ER,HR,BB,SO,BAOpp,BK,BFP,GF,R,SH,SF,GIDP,stint,birthYear,
#birthMonth,birthDay,birthCountry,birthState,birthCity,deathYear,deathMonth,deathDay,
#deathCountry,deathState,deathCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame)


#bat_stats_people
#not_selected(stint,lgID,BB,SO,IBB,HBP,SH,SF,GIDP,birthYear,birthMonth,birthDay
#birthCountry,birthState, birthCity, deathYear, deathMonth, deathDay, deathCountry,deathState,
#deathCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame)
