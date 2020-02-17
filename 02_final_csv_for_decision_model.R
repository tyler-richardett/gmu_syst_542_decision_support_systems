######
# 1. Load packages.
######

library(plyr)
library(tidyr)
library(dplyr)
library(DMwR)


######
# 2. Load in necessary data from previous steps. 
######

target_players_match_stats <- read.csv("target_players_match_stats.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
target_players_info <- read.csv("target_players_info.csv", colClasses = c(rep("character", 2), "numeric", "Date", rep("numeric", 2), rep("character", 3), "logical", rep("character", 2), "numeric", "Date", "character"), fileEncoding = "UTF-8")
target_players_match_stats_ratings_adjusted <- read.csv("target_players_match_stats_ratings_adjusted.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
target_players_versatility_metric <- read.csv("target_players_versatility_metric.csv", colClasses = c("character", "numeric"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
target_players_positions <- read.csv("target_players_positions.csv", colClasses = "character", fileEncoding = "UTF-8")


######
# 3. Calculate overall skill rating for each eligible player.
######

target_players_match_stats_ratings_adjusted_sum <- target_players_match_stats_ratings_adjusted %>% 
        mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
        group_by(player_id) %>% 
        summarize(minutes_on_field = sum(minutes_on_field, na.rm = TRUE),
                  player_rating_adjusted = sum(player_rating_adjusted, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(minutes_on_field >= 960) %>%
        mutate(player_id = as.character(player_id)) %>% 
        select(player_id, player_rating_adjusted, minutes_on_field) %>% 
        arrange(desc(player_rating_adjusted))


######
# 4. Recent form rating for each eligible player.
######

target_players_match_stats_ratings_adjusted_recent_form <- target_players_match_stats_ratings_adjusted %>% 
        mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
        filter(date >= as.Date("2018-10-16")) %>% 
        group_by(player_id) %>% 
        summarize(minutes_on_field = sum(minutes_on_field, na.rm = TRUE),
                  player_rating_adjusted = sum(player_rating_adjusted, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(minutes_on_field >= 200) %>%
        mutate(recent_player_rating_adjusted_per_96 = player_rating_adjusted * 96 / minutes_on_field,
               player_id = as.character(player_id)) %>% 
        select(player_id, recent_player_rating_adjusted_per_96)


######
# 5. Impute missing values.
######

# Height, weight, market value
target_players_model_values <- target_players_info %>% 
        inner_join(target_players_match_stats_ratings_adjusted_sum, c("id" = "player_id")) %>% 
        left_join(target_players_match_stats_ratings_adjusted_recent_form, c("id" = "player_id")) %>% 
        distinct()

target_players_imputed_values_knn <- knnImputation(target_players_model_values %>% 
                                                           select(age, height, weight, market_value, player_rating_adjusted, minutes_on_field), k = 10)


# Dominant foot
target_players_model_values <- cbind(target_players_model_values %>% select(-c(age, height, weight, market_value, player_rating_adjusted, minutes_on_field)), target_players_imputed_values_knn) %>% 
        left_join(target_players_versatility_metric, c("id" = "player_id"))

prob_right_foot <- (target_players_model_values %>% 
        filter(!is.na(foot)) %>% 
        summarize(mean(foot == "right")))[[1]]

prob_left_foot <- (target_players_model_values %>% 
                            filter(!is.na(foot)) %>% 
                            summarize(mean(foot == "left")))[[1]]

set.seed(239)
target_players_model_values$foot[which(is.na(target_players_model_values$foot))] <- sample(c("right", "left", "both"), length(which(is.na(target_players_model_values$foot))), replace = TRUE, prob = c(prob_right_foot, prob_left_foot, 1 - prob_right_foot - prob_left_foot))


######
# 6. Bin height and weight values by quantiles. Export data frame.
######

target_players_model_values <- target_players_model_values %>% 
        mutate(binned_height = cut(height, breaks = quantile(target_players_model_values$height, probs = c(0, 0.25, 0.75, 1)), labels = c("Short", "Median", "Tall"), include.lowest = TRUE),
               binned_weight = cut(weight, breaks = quantile(target_players_model_values$weight, probs = c(0, 0.25, 0.75, 1)), labels = c("Light", "Median", "Heavy"), include.lowest = TRUE),
               binned_age = cut(age, breaks = c(0, 22.5, 28.5, 100), labels = c("22 or younger", "23-28", "29 or older"))) %>% 
        select(id, name, age, birth_date, height, weight, foot:current_team_name, market_value, contract_expires, image, everything()) %>% 
        replace_na(list(versatility = 0,
                        recent_player_rating_adjusted_per_96 = 0)) %>% 
        group_by(binned_age) %>% 
        mutate(talent_rank_age_group = rank(player_rating_adjusted) / length(player_rating_adjusted)) %>% 
        ungroup()

# write.csv(target_players_model_values, "target_players_model_values.csv", row.names = FALSE, fileEncoding = "UTF-8")


######
# 7. Create and export data frame containing player IDs and positional clusters (played more than 20% of minutes).
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

target_players_model_positions <- target_players_positions %>%
        filter(positions != "GK") %>% 
        group_by(id) %>% 
        summarize(positions = first(positions)) %>% 
        ungroup() %>% 
        left_join(position_cluster_lookup, c("positions" = "position")) %>% 
        filter(!is.na(position_cluster)) %>% 
        select(-positions)

target_players_share_minutes_positions <- target_players_match_stats_ratings_adjusted %>% 
        mutate(positions = gsub('(^c\\("|",.*$)', "", positions)) %>% 
        filter(positions != "GK", !is.na(positions)) %>% 
        left_join(position_cluster_lookup, c("positions" = "position")) %>% 
        filter(!is.na(position_cluster)) %>% 
        group_by(player_id, position_cluster) %>%
        summarize(minutes = sum(minutes_on_field)) %>% 
        ungroup() %>% 
        group_by(player_id) %>% 
        mutate(share_minutes = minutes / sum(minutes)) %>% 
        ungroup() %>% 
        filter(share_minutes >= 0.2) %>% 
        select(id = player_id, position_cluster)

target_players_model_positions <- rbind(target_players_model_positions, target_players_share_minutes_positions) %>% 
        distinct() %>% 
        arrange(id) %>% 
        filter(id %in% target_players_model_values$id)

# write.csv(target_players_model_positions, "target_players_model_positions.csv", row.names = FALSE, fileEncoding = "UTF-8")


######
# 8. Create and export data frame containing player IDs seasonal statistics.
######

target_players_season_stats <- target_players_match_stats %>% 
        filter(seasonName %in% c("2018/2019", "2017/2018", "2016/2017", "2015/2016"),
               !grepl("^(Africa|Asia|Europe|North America|Oceania|South America|World|N/C America)", competition), 
               !grepl("(Cup|Copa|Play-off|Coupe|Pokal|CUP)", competition)) %>% 
        group_by(player_id, seasonName) %>% 
        summarize(GP = dplyr::n(),
                  Mins = sum(minutes_on_field, na.rm = TRUE),
                  G = sum(goal, na.rm = TRUE),
                  A = sum(assist, na.rm = TRUE),
                  Shots = sum(shot, na.rm = TRUE),
                  SOG = sum(shot_on_goal, na.rm = TRUE),
                  FC = sum(foul, na.rm = TRUE),
                  OFF = sum(offside, na.rm = TRUE),
                  Y = sum(yellow_cards, na.rm = TRUE),
                  R = sum(red_cards, na.rm = TRUE)) %>% 
        ungroup() %>% 
        select(id = player_id, Season = seasonName, everything()) %>% 
        arrange(id, desc(Season))
        
# write.csv(target_players_season_stats, "target_players_season_stats.csv", row.names = FALSE)
        