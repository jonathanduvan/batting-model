

# Clean1: Retrosheet Data -------------------------------------------------


pbyp_retrosheet <- clean_event_data %>% 
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
  mutate(team_v2 = toupper(as.character(team)),
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

pbyp_retrosheet <- pbyp_retrosheet %>%
  mutate(batter_first_name = trim(batter_first_name),
         batter_last_name = trim(batter_last_name),
         batter_first_name = gsub("\\.| *| ", "", batter_first_name),
         batter_last_name = gsub("\\.| *| ", "", batter_last_name)) %>% 
  select( yearID, id,id_v2, team, team_v2, inning, 
          bottom_inning, number_up, play.retroID, 
          play.name, batter_first_name, batter_last_name, 
          play.batPos, play.fieldPos)


rm(pbyp_retrosheet)

# Clean2: Kaggle - Play-by-Play Pitching Data --------------------------------------


# Cleaning Game Logs
games <- gamelogs %>% 
  mutate(date = gsub("-", "/", date),
         date_special = gsub("/", "", date),
         id_v2 = paste0(toupper(home_team), date_special),
         yearID = as.integer(substr(g_id, 1, 4))) %>% 
  mutate(home_team_v2 = toupper(as.character(home_team)),
         home_team_v2 = sub("SLN", "STL", home_team_v2),
         home_team_v2 = sub("CHN", "CHC", home_team_v2),
         home_team_v2 = sub("ANA", "LAA", home_team_v2),
         home_team_v2 = sub("TBA", "TBR", home_team_v2),
         home_team_v2 = sub("CHA", "CHW", home_team_v2),
         home_team_v2 = sub("KCA", "KCR", home_team_v2),
         home_team_v2 = sub("NYN", "NYM", home_team_v2),
         home_team_v2 = sub("SDN", "SDP", home_team_v2),
         home_team_v2 = sub("LAN", "LAD", home_team_v2),
         home_team_v2 = sub("SFN", "SFG", home_team_v2),
         home_team_v2 = sub("NYA", "NYY", home_team_v2)) %>% 
  mutate(away_team_v2 = toupper(as.character(away_team)),
         away_team_v2 = sub("SLN", "STL", away_team_v2),
         away_team_v2 = sub("CHN", "CHC", away_team_v2),
         away_team_v2 = sub("ANA", "LAA", away_team_v2),
         away_team_v2 = sub("TBA", "TBR", away_team_v2),
         away_team_v2 = sub("CHA", "CHW", away_team_v2),
         away_team_v2 = sub("KCA", "KCR", away_team_v2),
         away_team_v2 = sub("NYN", "NYM", away_team_v2),
         away_team_v2 = sub("SDN", "SDP", away_team_v2),
         away_team_v2 = sub("LAN", "LAD", away_team_v2),
         away_team_v2 = sub("SFN", "SFG", away_team_v2),
         away_team_v2 = sub("NYA", "NYY", away_team_v2)) 


# Cleaning Player Names - Pitchers
pnames_pitchers <- player_names %>% 
  mutate(pitcher_first_name = tolower(first_name),
         pitcher_last_name = tolower(last_name),
         pitcher_first_name = trim(pitcher_first_name),
         pitcher_last_name = trim(pitcher_last_name),
         pitcher_first_name = gsub("\\.| *| ", "", pitcher_first_name),
         pitcher_last_name = gsub("\\.| *| ", "", pitcher_last_name)) %>% 
  select(id, pitcher_first_name, pitcher_last_name)



# Cleaning Player Names - Hitters
pnames_batters <- player_names %>% 
  mutate(batter_first_name = tolower(first_name),
         batter_last_name = tolower(last_name),
         batter_first_name = trim(batter_first_name),
         batter_last_name = trim(batter_last_name),
         batter_first_name = gsub("\\.| *| ", "", batter_first_name),
         batter_last_name = gsub("\\.| *| ", "", batter_last_name)) %>% 
  select(id, batter_first_name, batter_last_name)

# Cleaning Every at bat per Season
kaggle_pbyp <- atbats %>% 
  mutate(bottom_inning = ifelse(top == "True", 0, 1)) %>% 
  group_by(g_id, inning, bottom_inning) %>%
  mutate(number_up = row_number()) %>% 
  ungroup() 




# Clean3: Season-by-Season Lehman Data ------------------------------------

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
         batter_last_name = tolower(nameLast),
         batter_first_name = trim(batter_first_name),
         batter_last_name = trim(batter_last_name),
         batter_first_name = gsub("\\.| *| ", "", batter_first_name),
         batter_last_name = gsub("\\.| *| ", "", batter_last_name))


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
         pitcher_last_name = gsub("\\.| *| ", "", pitcher_last_name))






