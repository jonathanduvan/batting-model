
#Merge_File.R


# Merge2: Kaggle - Play-by-Play Pitching Data ------------------------------------------------------


pbyp_pitcher_names <- left_join(kaggle_pbyp, 
                                   pnames_pitchers, 
                                   by = c("pitcher_id" = "id"))


pbyp_p_h_names <- left_join(pbyp_pitcher_names,
                            pnames_batters, 
                            by = c("batter_id" = "id")) 



pbyp_p_h_names_wgames <- left_join(pbyp_p_h_names, 
                                           games, 
                                           by = "g_id")  %>% 
  mutate(team_v2 = ifelse(bottom_inning == 1, toupper(home_team_v2), toupper(away_team_v2)),
         team_v2_away = ifelse(bottom_inning == 1, toupper(away_team_v2), toupper(home_team_v2))) %>% 
  select(id_v2, g_id, yearID, team_v2, team_v2_away, inning,
         bottom_inning, number_up, ab_id,
         batter_id, batter_first_name, 
         batter_last_name,pitcher_id,
         pitcher_first_name, 
         pitcher_last_name, event)

# Version 1
# completed_pbyp <- left_join(pbyp_retrosheet, 
#                              pbyp_p_h_names_wgames, 
#                              by = c("id_v2", "inning", 
#                                     "team_v2", 
#                                     "bottom_inning", 
#                                     "batter_last_name"))




# Merge3: Lehman Season-by-season data ------------------------------------



#Version 1.0
# Adding Player Names to Season-by-Season Hitting Stats
# sbys_hitstats_wnames <- left_join(bat_stats, 
#                               people_batters, 
#                               by = "playerID") %>% 
#   select(playerID,retroID, bbrefID,yearID,
#          teamID,team_v2,batter_first_name,
#          batter_last_name,G,
#          AB,R,H,X2B,X3B,HR,RBI,SB,CS)
# 

# Adding Season-by-Season Hitting Stats to our Master Event File
# completed_pbyp_sbys_hit <- left_join(completed_pbyp, 
#                                     sbys_hitstats_wnames, 
#                                     by = c("yearID", 
#                                            "team_v2", 
#                                            c("play.retroID" = "retroID")))
# 
# 
# 
# 
# # Adding Player Names to Season-by-Season Pitching Stats
# sbys_pitchstats_wnames <- left_join(pitch_stats, 
#                                 people_pitchers, 
#                                 by = "playerID") %>% 
#   select(playerID,retroID, bbrefID, yearID,
#          teamID,team_v2, pitcher_first_name, 
#          pitcher_last_name,
#          W,L,G,GS,CG,SHO,SV,
#          ERA,IBB,WP,HBP)
# 
# 
# 
# completed_pbyp_sbys_both <- left_join(completed_pbyp_sbys_hit, 
#                                           sbys_pitchstats_wnames, 
#                                           by = c("yearID", 
#                                                  "pitcher_last_name", 
#                                                  "pitcher_first_name"))
# 
# 
# rm(pbyp_pitcher_names, pbyp_p_h_names, pbyp_p_h_names_wgames, completed_pbyp, sbys_hitstats_wnames, completed_pbyp_sbys_hit, sbys_pitchstats_wnames)



# Merge3: Lehman Season-by-season data ------------------------------------




# Adding Player Names to Season-by-Season Hitting Stats
sbys_hitstats_wnames <- left_join(bat_stats, 
                                  people_batters, 
                                  by = "playerID") %>% 
  select(playerID,retroID, bbrefID,yearID,
         teamID,team_v2,batter_first_name,
         batter_last_name,G,
         AB,R,H,X2B,X3B,HR,RBI,SB,CS)


# Adding Player Names to Season-by-Season Pitching Stats
sbys_pitchstats_wnames <- left_join(pitch_stats, 
                                    people_pitchers, 
                                    by = "playerID") %>% 
  select(playerID,retroID, bbrefID, yearID,
         teamID,team_v2, pitcher_first_name, 
         pitcher_last_name,
         W,L,G,GS,CG,SHO,SV,
         ERA,IBB,WP,HBP)


#Adding Season-by-Season Hitting Stats to our Master Event File
pbyp_sbys_hit <- left_join(pbyp_p_h_names_wgames, 
                           sbys_hitstats_wnames,
                           by = c("yearID", "team_v2", 
                                  "batter_last_name", "batter_first_name"))




pbyp_sbys_both <- left_join(pbyp_sbys_hit, 
                            sbys_pitchstats_wnames,
                            by = c("yearID", c("team_v2_away" = "team_v2"),
                                   "pitcher_last_name", "pitcher_first_name"))

