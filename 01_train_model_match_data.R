######
# 1. Load packages.
######

library(e1071)
library(caret)
library(pROC)
library(plyr)
library(tidyr)
library(dplyr)
library(Bolstad)
library(reshape2)


######
# 2. Load in aggregated match data and player-level data from previous step. 
######

target_players_match_stats <- read.csv("target_players_match_stats.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
competition_IDs <- read.csv("competition_IDs.csv", colClasses = "character", fileEncoding = "latin1")
target_players_info <- read.csv("target_players_info.csv", colClasses = c(rep("character", 2), "numeric", "Date", rep("numeric", 2), rep("character", 3), "logical", rep("character", 2), "numeric", "Date", "character"), fileEncoding = "UTF-8")
target_players_positions <- read.csv("target_players_positions.csv", colClasses = "character", fileEncoding = "UTF-8")


######
# 3. Clean and reshape.
######

players_clubs_lookup <- target_players_info %>% 
        select(player_id = id, club_id = current_team_id)

target_match_stats_model_input <- target_players_match_stats %>% 
        filter(competitionId %in% competition_IDs$id,
               seasonName == "2018/2019") %>% 
        mutate(player_id = as.character(player_id)) %>% 
        left_join(players_clubs_lookup, "player_id") %>% 
        mutate(target_club_id = ifelse(teamAId == club_id, teamAId,
                                       ifelse(teamBId == club_id, teamBId, NA)),
               teamAScore = gsub("-\\d+$", "", score),
               teamBScore = gsub("^\\d+-", "", score),
               result = ifelse(teamAId == target_club_id & teamAScore > teamBScore, 1,
                               ifelse(teamBId == target_club_id & teamBScore > teamAScore, 1, 0))) %>% 
        filter(!is.na(target_club_id)) %>% 
        select(match_id = id, target_club_id, result,
               acceleration_success, action_in_counterattack_success, aerial_duel_success,
               ball_delivery_to_danger_zone_success, ball_delivery_to_penalty_area_success,
               ball_entry_in_final_third_success, controlled_box_entry_success,
               controlled_penalty_area_entry_success, cross_to_penalty_area_success, 
               cross_to_goalie_box_success, dangerous_opponent_half_recovery_success,
               dangerous_own_half_loss_success, defensive_duel_success, dribble_success,
               forward_pass_success, foul, free_kick_success, interception_success,
               key_pass_success, loose_ball_duel_success, opportunity_success,
               opportunity_creation_success, pass_to_zone_fourteen_success,
               pass_to_final_third_success, pass_to_penalty_area_success,
               progressive_pass_success, progressive_run_success, recovery_success,
               recovery_counterpressing_success, red_cards, smart_pass_success,
               shot_after_corner_success, shot_after_free_kick_success, shot_after_throw_in_success,
               shot_assist_success, shot_buildup_pass_success, shot_from_cross_success,
               shot_from_danger_zone_success, shot_from_outside_area_success, through_pass_success,
               throw_in_success, touch_in_box_success, touch_success, vertical_pass_success,
               yellow_cards) %>% 
        group_by(match_id, target_club_id, result) %>% 
        filter(n() >= 11) %>% 
        summarize_at(vars(acceleration_success:yellow_cards), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        select(acceleration_success:yellow_cards, result) %>% 
        mutate(result = as.factor(result))

# write.csv(target_match_stats_model_input, "target_match_stats_model_input.csv", row.names = FALSE)

# target_match_stats_model_input <- read.csv("target_match_stats_model_input.csv", stringsAsFactors = FALSE, colClasses = c(rep("numeric", 45), "factor"))


######
# 4. Train the model and extract the coefficients.
######

set.seed(239)
lsvm_fit <- svm(result ~ ., data = target_match_stats_model_input, kernel = "linear", cost = 1, scale = FALSE, cross = 10)
lsvm_coefs <- t(lsvm_fit$coefs) %*% lsvm_fit$SV

# write.csv(lsvm_coefs[,-1], "lsvm_coefs.csv", row.names = FALSE)
# lsvm_coefs <- read.csv("lsvm_coefs.csv", colClasses = c("character", "numeric"))

svm.prediction <- data.frame(prediction = predict(lsvm_fit, probability = TRUE, decision.values = TRUE))
roc(as.numeric(target_match_stats_model_input$result), as.numeric(svm.prediction$prediction))$auc
confusionMatrix(svm.prediction$prediction, target_match_stats_model_input$result)

# AUC: 0.71
# Accuracy: 74%


######
# 5. Calculate raw player ratings.
######

target_players_match_stats_ratings_matrix <- target_players_match_stats %>% 
        select(acceleration_success, action_in_counterattack_success, aerial_duel_success,
               ball_delivery_to_danger_zone_success, ball_delivery_to_penalty_area_success,
               ball_entry_in_final_third_success, controlled_box_entry_success,
               controlled_penalty_area_entry_success, cross_to_penalty_area_success, 
               cross_to_goalie_box_success, dangerous_opponent_half_recovery_success,
               dangerous_own_half_loss_success, defensive_duel_success, dribble_success,
               forward_pass_success, foul, free_kick_success, interception_success,
               key_pass_success, loose_ball_duel_success, opportunity_success,
               opportunity_creation_success, pass_to_zone_fourteen_success,
               pass_to_final_third_success, pass_to_penalty_area_success,
               progressive_pass_success, progressive_run_success, recovery_success,
               recovery_counterpressing_success, red_cards, smart_pass_success,
               shot_after_corner_success, shot_after_free_kick_success, shot_after_throw_in_success,
               shot_assist_success, shot_buildup_pass_success, shot_from_cross_success,
               shot_from_danger_zone_success, shot_from_outside_area_success, through_pass_success,
               throw_in_success, touch_in_box_success, touch_success, vertical_pass_success,
               yellow_cards) %>% 
        mutate_all(funs(replace(., is.na(.), 0)))

target_players_match_stats_ratings_matrix <- data.frame(mapply(`*`, target_players_match_stats_ratings_matrix, lsvm_coefs$value, SIMPLIFY = FALSE))

target_players_match_stats_ratings <- target_players_match_stats %>% 
        mutate(raw_player_rating = rowSums(target_players_match_stats_ratings_matrix, na.rm = TRUE))


######
# 6. Calculate relative differences between leagues' play.
######

bayes_leagues <- target_players_match_stats %>% 
        group_by(competitionId, competition) %>% 
        summarize(minutes = sum(minutes_on_field, na.rm = TRUE)) %>% 
        filter(!grepl("^(Africa|Asia|Europe|North America|Oceania|South America|World|N/C America)", competition), 
               !grepl("(Cup|Copa|Play-off|Coupe|Pokal|CUP)", competition), 
               minutes >= 9600) %>% 
        arrange(desc(minutes))

bayes_leagues_players <- target_players_match_stats_ratings %>% 
        filter(competitionId %in% bayes_leagues$competitionId) %>% 
        group_by(player_id, competitionId) %>% 
        summarize(minutes_on_field = sum(minutes_on_field, na.rm = TRUE),
                  raw_player_rating = sum(raw_player_rating, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(minutes_on_field >= 960) %>% 
        mutate(player_rating_per_96 = raw_player_rating * 96 / minutes_on_field)


bayes_leagues_ids <- unique(bayes_leagues_players$competitionId)
bayes_leagues_ids <- bayes_leagues_ids[-which(bayes_leagues_ids == "121")]

bayes_leagues_differences <- data.frame()

for(i in 1:length(bayes_leagues_ids)) {
        tmp_df <- bayes_leagues_players %>% 
                filter(competitionId %in% c("121", bayes_leagues_ids[i])) %>% 
                mutate(competitionId = factor(competitionId, levels = c("121", bayes_leagues_ids[i])))
        
        tmp_diff <- bayes.t.test(player_rating_per_96 ~ competitionId, data = tmp_df)$estimate[[1]]-bayes.t.test(player_rating_per_96 ~ competitionId, data = tmp_df)$estimate[[2]]
        tmp_pval <- bayes.t.test(player_rating_per_96 ~ competitionId, data = tmp_df)$p.value[[1]]
        
        tmp_df <- data.frame(competitionId = bayes_leagues_ids[i],
                             difference = tmp_diff,
                             p_value = tmp_pval,
                             stringsAsFactors = FALSE)
        
        bayes_leagues_differences <- rbind(bayes_leagues_differences, tmp_df)
}

bayes_leagues_differences <- rbind(bayes_leagues_differences,
                                   data.frame(competitionId = 121,
                                              difference = 0,
                                              p_value = 0,
                                              stringsAsFactors = FALSE)) %>% 
        left_join(bayes_leagues %>% select(-minutes)) %>% 
        select(competitionId, competition, difference) %>% 
        mutate(league_multiple = difference + 1) %>% 
        select(-difference)

# write.csv(bayes_leagues_differences, "bayes_leagues_differences.csv", row.names = FALSE)

# bayes_leagues_differences <- read.csv("bayes_leagues_differences.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")


######
# 7. Adjust player ratings by leagues' differences.
######

target_players_match_stats_ratings_adjusted <- target_players_match_stats_ratings %>% 
        left_join(bayes_leagues_differences) %>% 
        filter(!grepl("^(Africa|Asia|Europe|North America|Oceania|South America|World|N/C America)", competition), 
               !grepl("(Cup|Copa|Play-off|Coupe|Pokal|CUP)", competition)) %>% 
        mutate(player_rating_adjusted = raw_player_rating * league_multiple)

# write.csv(target_players_match_stats_ratings_adjusted, "target_players_match_stats_ratings_adjusted.csv", row.names = FALSE, fileEncoding = "UTF-8")

# target_players_match_stats_ratings_adjusted <- read.csv("target_players_match_stats_ratings_adjusted.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")


######
# 8. As a check, determine top n players for each position cluster.
######

position_cluster_lookup <- data.frame(position = c("AMF", "CB", "CF", "DMF", "LAMF", "LB", "LB5",
                                                   "LCB", "LCB3", "LCMF", "LCMF3", "LDMF", "LW",
                                                   "LWB", "LWF", "RAMF", "RB", "RB5", "RCB", "RCB3",
                                                   "RCMF", "RCMF3", "RDMF", "RW", "RWB", "RWF"),
                                      position_cluster = c("Central Midfielder", 
                                                           "Central Defender", 
                                                           "Central Forward",
                                                           "Central Midfielder", 
                                                           "Left-Sided Attacker",
                                                           "Left-Sided Midfielder/Fullback",
                                                           "Left-Sided Midfielder/Fullback",
                                                           "Central Defender",
                                                           "Central Defender",
                                                           "Central Midfielder",
                                                           "Left-Sided Midfielder/Fullback",
                                                           "Central Midfielder",
                                                           "Left-Sided Attacker",
                                                           "Left-Sided Midfielder/Fullback",
                                                           "Left-Sided Attacker",
                                                           "Right-Sided Attacker",
                                                           "Right-Sided Midfielder/Fullback",
                                                           "Right-Sided Midfielder/Fullback",
                                                           "Central Defender",
                                                           "Central Defender",
                                                           "Central Midfielder",
                                                           "Right-Sided Midfielder/Fullback",
                                                           "Central Midfielder",
                                                           "Right-Sided Attacker",
                                                           "Right-Sided Midfielder/Fullback",
                                                           "Right-Sided Attacker"),
                                      stringsAsFactors = FALSE)

target_players_positions_primary <- target_players_positions %>% 
        group_by(id) %>% 
        summarize(position = first(positions)) %>% 
        ungroup() %>% 
        filter(position != "GK") %>% 
        left_join(position_cluster_lookup) %>% 
        filter(!is.na(position_cluster)) %>% 
        arrange(position_cluster)

target_players_match_stats_ratings_adjusted_positional_rankings <- target_players_match_stats_ratings_adjusted %>% 
        mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
        filter(date >= as.Date("2018-07-01")) %>% 
        group_by(player_id) %>% 
        summarize(minutes_on_field = sum(minutes_on_field, na.rm = TRUE),
                  player_rating_adjusted = sum(player_rating_adjusted, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(minutes_on_field >= 960) %>%
        mutate(player_rating_adjusted_per_96 = player_rating_adjusted * 96 / minutes_on_field,
               player_id = as.character(player_id)) %>% 
        inner_join(target_players_positions_primary, c("player_id" = "id")) %>% 
        group_by(position_cluster) %>% 
        top_n(5, player_rating_adjusted_per_96) %>% 
        left_join(target_players_info %>% 
                          select(player_id = id, name, current_team_name, owner_team_name)) %>% 
        select(position_cluster, player_rating_adjusted_per_96, player_id, name, 
               current_team_name, owner_team_name, position) %>% 
        arrange(position_cluster, desc(player_rating_adjusted_per_96))

# write.csv(target_players_match_stats_ratings_adjusted_positional_rankings, "target_players_match_stats_ratings_adjusted_positional_rankings.csv", row.names = FALSE)


######
# 9. Calculate versatility metric for each player.
######

target_players_share_minutes_positions <- target_players_match_stats_ratings_adjusted %>% 
        mutate(positions = gsub('(^c\\("|",.*$)', "", positions)) %>% 
        filter(positions != "GK", !is.na(positions)) %>% 
        left_join(position_cluster_lookup, c("positions" = "position")) %>% 
        filter(!is.na(position_cluster)) %>% 
        group_by(player_id, position_cluster) %>%
        summarize(minutes = sum(minutes_on_field)) %>% 
        ungroup()

# write.csv(target_players_share_minutes_positions, "target_players_share_minutes_positions.csv", row.names = FALSE, fileEncoding = "UTF-8")

target_players_versatility_metric <- target_players_share_minutes_positions %>% 
        group_by(player_id) %>% 
        mutate(share_minutes = minutes / sum(minutes)) %>% 
        summarize(versatility = -1 * (sum(share_minutes * log(share_minutes)) / log(7))) %>% 
        ungroup()

# write.csv(target_players_versatility_metric, "target_players_versatility_metric.csv", row.names = FALSE, fileEncoding = "UTF-8")
