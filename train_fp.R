source("functions.R")

library(nflfastR)
library(tidyverse)
library(lubridate)
library(splines)
library(caret)

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

model_formula <- fp_per_game ~  
  ns(age, df = 3):seasons_in_league +   
  fps_lag1 +
  games_lag1


ppr <- train_cv_fp_1_season(control_data = stats,
                             model_formula = model_formula)


model_formula2 <- fp_per_game ~  
  ns(age, df = 3):seasons_in_league + 
  fps_lag1 + fps_lag2 +
  games_lag1 + games_lag2

ppr2 <- train_cv_fp_2_seasons(control_data = stats,
                              model_formula = model_formula2)

model_formula3 <- fp_per_game ~  
  ns(age, df = 3):seasons_in_league + 
  fps_lag1 + fps_lag2 + fps_lag3 +
  games_lag1 + games_lag2 + games_lag3


ppr3 <- train_cv_fp_3_seasons(control_data = stats,
                              model_formula = model_formula3)

model_formula4 <- fp_per_game ~  
  ns(age, df = 3):seasons_in_league + 
  fps_lag1 + fps_lag2 + fps_lag3 +
  games_lag1 + games_lag2 + games_lag3

ppr4 <- train_cv_fp_4_seasons(control_data = stats,
                              model_formula = model_formula4)

