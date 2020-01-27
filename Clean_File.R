
clean_team_names <- function(df, team_v2) {
  mutate(df, team_v2 = as.character(team),
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
}


# Clean1: Retrosheet Data -------------------------------------------------

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




# Clean2: Play-by-play Pitching Data --------------------------------------


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


each_ab_pitcher_names <- left_join(each_ab, 
                                   pnames_pitchers, 
                                   by = c("pitcher_id" = "id"))
each_ab_pitcher_hitter_names <- left_join(each_ab_pitcher_names, 
                                          pnames_batters, 
                                          by = c("batter_id" = "id")) 


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


clean_pbyp_data <- left_join(clean, 
                             ab_pitcher_hitter_names_games, 
                             by = c("id_v2", "inning", 
                                    "team_v2", "bottom_inning", 
                                    "batter_last_name", "batter_first_name"))


# Clean3: Season-by-season Lehman Data ------------------------------------

# Cleaning Season-by-season Hitting Statistics
bat_stats <- batting_stats[batting_stats$yearID >= 2015, ] %>% 
  mutate(team_v2 = as.character(teamID),
         team_v2 = sub("SLN", "STL", team_v2),
         team_v2 = sub("CHN", "CHC", team_v2),
         team_v2 = sub("TBA", "TBR", team_v2),
         team_v2 = sub("CHA", "CHW", team_v2),
         team_v2 = sub("KCA", "KCR", team_v2),
         team_v2 = sub("NYN", "NYM", team_v2),
         team_v2 = sub("SDN", "SDP", team_v2),
         team_v2 = sub("LAN", "LAD", team_v2),
         team_v2 = sub("SFN", "SFG", team_v2),
         team_v2 = sub("NYA", "NYY", team_v2))


# Cleaning Player Names for Season-by-Season Hitting Stats
people_batters <- people %>%
  mutate(batter_first_name = tolower(nameFirst),
         batter_last_name = tolower(nameLast))



# Adding Player Names to Season-by-Season Hitting Stats
bat_stats_people <- left_join(bat_stats, 
                              people_batters, 
                              by = "playerID") %>% 
  select(playerID,retroID, bbrefID,yearID,
         teamID,team_v2,batter_first_name,
         batter_last_name,G,
         AB,R,H,X2B,X3B,HR,RBI,SB,CS)


# Adding Season-by-Season Hitting Stats to our Master Event File
clean_bat_stats_people <- left_join(clean_pbyp_data, 
                                    bat_stats_people, 
                                    by = c("yearID", "team_v2", c("play.retroID" = "retroID")))


# Cleaning Season-by-season Pitching Statistics
pitch_stats <- pitching_stats[pitching_stats$yearID >= 2015, ] %>% 
  mutate(team_v2 = as.character(teamID),
         team_v2 = sub("SLN", "STL", team_v2),
         team_v2 = sub("CHN", "CHC", team_v2),
         team_v2 = sub("TBA", "TBR", team_v2),
         team_v2 = sub("CHA", "CHW", team_v2),
         team_v2 = sub("KCA", "KCR", team_v2),
         team_v2 = sub("NYN", "NYM", team_v2),
         team_v2 = sub("SDN", "SDP", team_v2),
         team_v2 = sub("LAN", "LAD", team_v2),
         team_v2 = sub("SFN", "SFG", team_v2),
         team_v2 = sub("NYA", "NYY", team_v2))


# Cleaning Player Names for Season-by-Season Pithching Stats
people_pitchers <- people %>%
  mutate(pitcher_first_name = tolower(nameFirst),
         pitcher_last_name = tolower(nameLast),
         pitcher_first_name = trim(pitcher_first_name),
         pitcher_last_name = trim(pitcher_last_name),
         pitcher_first_name = gsub("\\.| *| ", "", pitcher_first_name),
         pitcher_last_name = gsub("\\.| *| ", "", pitcher_last_name),
         pitcher_last_name = gsub(".+ ", "", pitcher_last_name))


# Adding Player Names to Season-by-Season Pitching Stats
pitch_stats_people <- left_join(pitch_stats, 
                                people_pitchers, 
                                by = "playerID") %>% 
  select(playerID,retroID, bbrefID, yearID,
         teamID,team_v2, pitcher_first_name, 
         pitcher_last_name,
         W,L,G,GS,CG,SHO,SV,ERA,IBB,WP,HBP)

clean_bat_pitch_stats_people <- left_join(clean_bat_stats_people, 
                                          pitch_stats_people, 
                                          by = c("yearID", "pitcher_last_name", "pitcher_first_name"))


#rm(pitch_stats, pitch_stats_people, clean_pbyp_data, clean_pitch_stats_people, bat_stats, bat_stats_people)




