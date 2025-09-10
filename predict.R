seasons <- 2000:2024
stats_list <- lapply(seasons, function(season) {
  calculate_stats(season, "season", "player")
})

stats <- bind_rows(stats_list)

## Get Age
rosters <- fast_scraper_roster(2000:2024) |> 
  select(gsis_id, season, birth_date) |>   
  mutate(
    birth_date = ymd(birth_date),
    sept_1 = ymd(paste0(season, "-09-01")),
    age = floor(interval(birth_date, sept_1) / years(1))
  ) |> 
  select(-birth_date, -sept_1) |> 
  rename(player_id = gsis_id)

## Get years in league

stats <- stats |> 
  arrange(player_id, season) |> 
  group_by(player_id) |> 
  mutate(seasons_in_league = row_number() - 1,
         fp_per_game = as.numeric(round(fantasy_points_ppr / games, 2))) |> 
  ungroup() |> 
  filter(seasons_in_league %in% c(0:23)) |> 
  left_join(rosters, 
            by = c("season", "player_id"))


## Get Correct Lags

player_ids <- stats %>%
  filter(season %in% c(2022, 2023, 2024)) %>%
  group_by(player_id) %>%
  summarise(games_played = sum(!is.na(games), na.rm = TRUE)) %>%
  filter(games_played > 0) %>%
  pull(player_id)

players_last_two_seasons <- stats |> 
  filter(player_id %in% c(player_ids))

latest_info <- players_last_two_seasons %>%
  filter(!is.na(player_name)) %>%
  arrange(player_id, desc(season)) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  rename(age = age.x) |> 
  select(player_id, player_name, player_display_name, position, position_group, age, seasons_in_league)


dummy_2025 <- tibble(
  player_id = player_ids,
  season = 2025
)

# Step 2: Join that info into dummy_2025
dummy_2025 <- dummy_2025 %>%
  left_join(latest_info, by = "player_id")

# Step 1: Get each player's last active season (2023 or 2024)
last_season_played <- players_last_two_seasons %>%
  group_by(player_id) %>%
  summarise(last_season = max(season), .groups = "drop")

# Step 2: Join that info to the full dataset (including 2025 dummy rows)
stats_augmented <- bind_rows(players_last_two_seasons, dummy_2025) %>%
  left_join(last_season_played, by = "player_id") %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    age = if_else(season == 2025,
                  coalesce(age, 0) + if_else(last_season == 2023, 2, 1),
                  age),
    seasons_in_league = if_else(season == 2025,
                                coalesce(seasons_in_league, 0) + if_else(last_season == 2023, 2, 1),
                                seasons_in_league)
  ) %>%
  ungroup() |> 
  select(player_id, player_display_name, position, position_group, season, fp_per_game, 
         games, age, seasons_in_league)

## Get Lags

# lag 1

one_season_ids <- stats_augmented |> 
  group_by(player_id, player_display_name) |> 
  summarise(max_seasons = max(seasons_in_league, na.rm = T)) |> 
  filter(max_seasons == 1) |> 
  pull(player_id)

one_season_predicted <- stats_augmented |> 
  filter(player_id %in% c(one_season_ids)) |> 
  mutate(
    fps_lag1 = lag(fp_per_game, 1),
    games_lag1 = lag(games, 1)) |> 
  filter(season == 2025)

one_season_predicted$preds <- predict(ppr$model_fit, newdata = one_season_predicted)
one_season_predicted$games_pred <- predict(games1$model_fit, newdata = one_season_predicted)

# lag 2

two_season_ids <- stats_augmented |> 
  group_by(player_id, player_display_name) |> 
  summarise(max_seasons = max(seasons_in_league, na.rm = T)) |> 
  filter(max_seasons == 2) |> 
  pull(player_id)

two_season_predicted <- stats_augmented %>%
  filter(player_id %in% c(two_season_ids)) %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    fps_lag1 = coalesce(lag(fp_per_game, 1), 0),
    fps_lag2 = coalesce(lag(fp_per_game, 2), 0),
    games_lag1 = coalesce(lag(games, 1), 0),
    games_lag2 = coalesce(lag(games, 2), 0)
  ) %>%
  ungroup() %>%
  filter(season == 2025)

two_season_predicted$preds <- predict(ppr2$model_fit, newdata = two_season_predicted)
two_season_predicted$games_pred <- predict(games2$model_fit, newdata = two_season_predicted)

# lag 3

three_season_ids <- stats_augmented |> 
  group_by(player_id, player_display_name) |> 
  summarise(max_seasons = max(seasons_in_league, na.rm = T)) |> 
  filter(max_seasons == 3) |> 
  pull(player_id)

three_season_predicted <- stats_augmented %>%
  filter(player_id %in% c(three_season_ids)) %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    fps_lag1 = coalesce(lag(fp_per_game, 1), 0),
    fps_lag2 = coalesce(lag(fp_per_game, 2), 0),
    fps_lag3 = coalesce(lag(fp_per_game, 3), 0),
    games_lag1 = coalesce(lag(games, 1), 0),
    games_lag2 = coalesce(lag(games, 2), 0),
    games_lag3 = coalesce(lag(games, 3), 0)
  ) %>%
  ungroup() %>%
  filter(season == 2025)

three_season_predicted$preds <- predict(ppr3$model_fit, newdata = three_season_predicted)
three_season_predicted$games_pred <- predict(games3$model_fit, newdata = three_season_predicted)

# lag 3

four_season_ids <- stats_augmented |> 
  group_by(player_id, player_display_name) |> 
  summarise(max_seasons = max(seasons_in_league, na.rm = T)) |> 
  filter(max_seasons > 3) |> 
  pull(player_id)

four_season_predicted <- stats_augmented %>%
  filter(player_id %in% c(four_season_ids)) %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    fps_lag1 = coalesce(lag(fp_per_game, 1), 0),
    fps_lag2 = coalesce(lag(fp_per_game, 2), 0),
    fps_lag3 = coalesce(lag(fp_per_game, 3), 0),
    games_lag1 = coalesce(lag(games, 1), 0),
    games_lag2 = coalesce(lag(games, 2), 0),
    games_lag3 = coalesce(lag(games, 3), 0)
  ) %>%
  ungroup() %>%
  filter(season == 2025)

four_season_predicted$preds <- predict(ppr4$model_fit, newdata = four_season_predicted)
four_season_predicted$games_pred <- predict(games4$model_fit, newdata = four_season_predicted)


