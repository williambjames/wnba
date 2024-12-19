library(wehoop)
library(tidyr)
library(dplyr)
library(progressr)
library(tictoc)
library(stringr)
schedule <- load_wnba_schedule(most_recent_wnba_season())
box <- load_wnba_player_box()
box <- box %>% 
  filter(game_id %in% schedule$game_id)


pbp <- load_wnba_pbp(season = most_recent_wnba_season())
pbp <- pbp %>% 
  filter(id != 401620426564)

# Create separate home and away starters with team IDs
starters_list <- box %>%
  filter(starter == TRUE) %>%  # Filter for starters
  group_by(game_id, team_id, home_away) %>%  # Group by game, team, and home/away
  summarise(starters = list(athlete_id), .groups = "drop") %>%  # Collect athlete_ids into a list
  mutate(starters = purrr::map(starters, ~ c(.x, rep(NA, 5 - length(.x))))) %>%  # Pad to ensure all lists are of length 5
  unnest_wider(starters, names_sep = "_", names_repair = "unique") %>%  # Expand list into columns
  pivot_wider(
    names_from = home_away, 
    values_from = c(team_id, starters_1, starters_2, starters_3, starters_4, starters_5),
    names_sep = "_"
  ) %>%
  rename(
    home_team_id = team_id_home,
    away_team_id = team_id_away,
    home_player_1 = starters_1_home,
    home_player_2 = starters_2_home,
    home_player_3 = starters_3_home,
    home_player_4 = starters_4_home,
    home_player_5 = starters_5_home,
    away_player_1 = starters_1_away,
    away_player_2 = starters_2_away,
    away_player_3 = starters_3_away,
    away_player_4 = starters_4_away,
    away_player_5 = starters_5_away
  )

pbp <- pbp %>%
  # Add home and away player columns by matching team_id with home_team_id or away_team_id
  left_join(starters_list %>%
              select(game_id, home_team_id, starts_with("home_player_")), 
            by = c("game_id" = "game_id", "team_id" = "home_team_id")) %>%
  left_join(starters_list %>%
              select(game_id, away_team_id, starts_with("away_player_")), 
            by = c("game_id" = "game_id", "team_id" = "away_team_id"))

# Update lineups for substitutions and ensure lineups are consistent
pbp_on_court <- update_lineups(pbp) %>%
  arrange(game_id, game_play_number) %>%
  rename(possession_team = team_id) %>%
  group_by(game_id) %>%
  arrange(game_play_number) %>%
  # Forward-fill player columns for home and away teams
  mutate(
    across(starts_with("home_player_"), ~ zoo::na.locf(.x, na.rm = FALSE)),
    across(starts_with("away_player_"), ~ zoo::na.locf(.x, na.rm = FALSE))
  ) %>%
  # Calculate possession changes and IDs
  mutate(
    possession_change = possession_team != lag(possession_team, default = first(possession_team)),
    possession_id = cumsum(possession_change)
  ) %>%
  ungroup() %>%
  # Handle cases where possession_id == 0 has missing starters
  group_by(game_id) %>%
  mutate(
    across(
      starts_with("home_player_"),
      ~ if_else(possession_id == 0 & is.na(.), first(.x[possession_id == 1], default = NA), .)
    ),
    across(
      starts_with("away_player_"),
      ~ if_else(possession_id == 0 & is.na(.), first(.x[possession_id == 1], default = NA), .)
    )
  ) %>%
  ungroup()

# Generate possession-level data
pbp_poss <- pbp_on_court %>%

  # Ensure proper sorting by game_play_number
  arrange(game_id, game_play_number) %>%
  # Group by game and calculate possession changes
  group_by(game_id) %>%
  mutate(
    possession_change = possession_team != lag(possession_team, default = first(possession_team)),
    possession_id = cumsum(possession_change)
  ) %>%
  ungroup() %>%
  # Ensure sorting is maintained
  arrange(game_id, game_play_number) %>%
  mutate(
    home_poss_id = if_else(possession_team == home_team_id, possession_id, NA_integer_),
    away_poss_id = if_else(possession_team == away_team_id, possession_id, NA_integer_)
  ) %>%
  # Rank the possession IDs for home and away teams to create sequential IDs
  group_by(game_id) %>%
  mutate(
    home_poss_id = dense_rank(home_poss_id),
    away_poss_id = dense_rank(away_poss_id)
  ) %>%
  # Fill in NA values with the last observed possession IDs
  mutate(
    home_poss_id = zoo::na.locf(home_poss_id, na.rm = FALSE),
    away_poss_id = zoo::na.locf(away_poss_id, na.rm = FALSE)
  ) %>%
  # Set initial NA values (losers of jump ball) to 0
  mutate(
    home_poss_id = if_else(is.na(home_poss_id), 0L, home_poss_id),
    away_poss_id = if_else(is.na(away_poss_id), 0L, away_poss_id)
  ) %>%
  ungroup()


pbp_ <- pbp %>% 
  filter(id==40162023428)

pbp_poss_this <- pbp_poss %>%
  filter(game_id == 401620178)

possessions_df <- pbp_poss %>%
  group_by(game_id, possession_id) %>%
  summarize(
    home_players = list(c(home_player_1[1], home_player_2[1], home_player_3[1], home_player_4[1], home_player_5[1])), # Home players
    away_players = list(c(away_player_1[1], away_player_2[1], away_player_3[1], away_player_4[1], away_player_5[1])), # Away players
    home_points = sum(if_else(possession_team == home_team_id, score_value, 0), na.rm = TRUE),  # Points scored by home
    away_points = sum(if_else(possession_team == away_team_id, score_value, 0), na.rm = TRUE),  # Points scored by away
    total_points = home_points - away_points,  # Net points
    .groups = "drop"  # Ungroup after summarization
  ) %>%
  unnest_wider(home_players, names_sep = "_") %>%  # Expand home players into separate columns
  unnest_wider(away_players, names_sep = "_") %>%  # Expand away players into separate columns
  rename_with(~ paste0("home_player_", seq_along(.)), starts_with("home_players_")) %>%  # Rename home columns
  rename_with(~ paste0("away_player_", seq_along(.)), starts_with("away_players_"))  # Rename away columns


pbp_stints <- pbp_poss %>%
  # Define home and away points
  mutate(
    home_points = if_else(possession_team == home_team_id, score_value, 0),  # Assign home points
    away_points = if_else(possession_team == away_team_id, score_value, 0)   # Assign away points
  ) %>%
  # Define lineups
  mutate(
    home_lineup = paste(home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, sep = ","),
    away_lineup = paste(away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, sep = ","),
    lineup_id = paste(home_lineup, away_lineup, sep = " | ")  # Unique ID for the lineup
  ) %>%
  group_by(game_id) %>%
  # Assign stint IDs based on changes in lineup_id
  mutate(
    stint_id = cumsum(lineup_id != lag(lineup_id, default = first(lineup_id)))
  ) %>%
  ungroup() %>%
  # Aggregate by stints
  group_by(game_id, stint_id) %>%
  summarize(
    home_lineup = first(home_lineup),
    away_lineup = first(away_lineup),
    home_points = sum(home_points, na.rm = TRUE),  # Total home points in the stint
    away_points = sum(away_points, na.rm = TRUE),  # Total away points in the stint
    total_points = home_points - away_points,  # Net points
    possession_count = n(),  # Number of possessions in the stint
    .groups = "drop"  # Ungroup after summarization
  )

# Step 2: Assign stint IDs
pbp_stints <- pbp_stints %>%
  group_by(game_id) %>%
  mutate(
    stint_id = cumsum(lineup_id != lag(lineup_id, default = first(lineup_id)))
  ) %>%
  ungroup()

# Step 3: Aggregate possessions into stints
stints_df <- pbp_stints %>%
  group_by(game_id, stint_id) %>%
  summarize(
    home_players = first(home_lineup),  # Record home lineup
    away_players = first(away_lineup),  # Record away lineup
    total_points = sum(total_points, na.rm = TRUE),  # Sum points scored during the stint
    possession_count = n(),  # Count number of possessions in the stint
    .groups = "drop"
  )
stints_df <- stints_df %>%
  separate(home_players, into = paste0("home_player_", 1:5), sep = ",", convert = TRUE) %>%
  separate(away_players, into = paste0("away_player_", 1:5), sep = ",", convert = TRUE)




library(Matrix)

# Get unique players across all columns
home_columns <- colnames(possessions_df %>% select(starts_with("home_player_")))
away_columns <- colnames(possessions_df %>% select(starts_with("away_player_")))

# Get unique players across all columns
all_players <- unique(c(
  possessions_df %>% select(all_of(home_columns)) %>% unlist(),
  possessions_df %>% select(all_of(away_columns)) %>% unlist()
))

# Create sparse matrix
design_matrix <- sparseMatrix(
  i = rep(1:nrow(possessions_df), each = 10),  # Row indices (each possession)
  j = unlist(lapply(1:nrow(possessions_df), function(row) {
    c(match(possessions_df[row, home_columns] %>% unlist(), all_players),  # Home players
      match(possessions_df[row, away_columns] %>% unlist(), all_players))  # Away players
  })),
  x = unlist(lapply(1:nrow(possessions_df), function(row) {
    c(rep(1, length(home_columns)),  # +1 for home players
      rep(-1, length(away_columns))) # -1 for away players
  })),
  dims = c(nrow(possessions_df), length(all_players))  # Possessions x Players
)

# Assign player names to columns
colnames(design_matrix) <- all_players
outcome_vector <- possessions_df$total_points

# library(glmnet)
# 
# # Fit RAPM using ridge regression
# rapm_model <- glmnet(
#   x = design_matrix, 
#   y = outcome_vector, 
#   alpha = 0,            # Ridge regression
#   lambda = 0.1          # Regularization parameter (adjust as needed)
# )
# 
# # Extract player ratings
# player_ratings <- as.data.frame(as.matrix(coef(rapm_model)))
# 
# # Remove the intercept row
# player_ratings <- player_ratings[-1, , drop = FALSE]  # Exclude the first row (intercept)
# 
# player_box_sums <- box %>%
#   group_by(athlete_id, athlete_display_name, team_display_name) %>%
#   summarize(
#     minutes = sum(minutes, na.rm = TRUE),
#     games = n_distinct(game_id),
#     .groups = "drop"
#   )
# # Assign player IDs
# player_ratings$player_id <- all_players
# player_ratings <- player_ratings %>%
#   mutate(RAPM_100 = s0 * 100) %>% 
#   left_join(player_box_sums %>% select(athlete_id, athlete_display_name, team_display_name, minutes), by = c("player_id" = "athlete_id")) %>% 
#   filter(minutes > 200)
# 
# team_ratings <- player_box_sums %>%
#   group_by(team_display_name) %>%
#   summarize(RAPM_100 = sum(RAPM_100), .groups = "drop") %>%
#   arrange(desc(RAPM_100))
