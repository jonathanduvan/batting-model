
# How to Merge Lists Correctly --------------------------------------------
 

rm(list = ls())
library(tidyverse)
library(retrosheet)
library(data.table)
library(glue)

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


clean <- clean_event_data %>% 
  group_by(id, play.inning, play.team) %>%
  mutate(number_up = row_number(),
         id_v2 = substr(as.character(id), 1, nchar(id)-1)) %>% 
  ungroup() %>% 
  mutate(play.inning = as.integer(play.inning),
         inning = play.inning,
         play.team = as.integer(play.team),
         play.team = ifelse(play.team == 1, 0, 1),
         bottom_inning = play.team) %>% 
  mutate(batter_first_name = tolower(gsub(" .+", "", play.name)),
         batter_last_name = tolower(gsub(".+ ", "", play.name)),
         yearID = as.integer(gsub("/.*", "", date)),
         hometeam = as.character(hometeam),
         visteam = as.character(visteam),
         team = ifelse(play.team == 0, visteam, hometeam)) %>% 
  mutate(team_v2 = as.character(team),
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
    team_v2 = sub("NYA", "NYY", team_v2))
 
clean <- clean %>%
  mutate(batter_first_name = trim(batter_first_name),
         batter_last_name = trim(batter_last_name),
         batter_first_name = gsub("\\.| *| ", "", batter_first_name),
         batter_last_name = gsub("\\.| *| ", "", batter_last_name)) %>% 
  select( yearID, id,id_v2, team, team_v2, inning, 
          bottom_inning, number_up, play.retroID, 
          play.name, batter_first_name, batter_last_name, 
          play.batPos, play.fieldPos)



#not_selected(hometeam,visteam, version, attendance, play.count,date, daynight,fieldcond, howscored, number,oscorer, pitches, precip, save,site,sky,starttime,temp,
# timeofgame, ump1b,ump2b, ump3b,umphome, usedh,winddir,windspeed, wp, play.pitches,play.play,)
         
         
