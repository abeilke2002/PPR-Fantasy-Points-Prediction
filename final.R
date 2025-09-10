library(dplyr)
library(openxlsx)

all_preds <- rbind(
  one_season_predicted %>%
    mutate(games_lag2 = 0) %>%
    select(player_display_name, position, preds, games_pred, fps_lag1, games_lag1, games_lag2),
  two_season_predicted %>%
    select(player_display_name, position, preds, games_pred, fps_lag1, games_lag1, games_lag2),
  three_season_predicted %>%
    select(player_display_name, position, preds, games_pred, fps_lag1, games_lag1, games_lag2),
  four_season_predicted %>%
    select(player_display_name, position, preds, games_pred, fps_lag1, games_lag1, games_lag2)
) %>%
  na.omit() %>%
  mutate(
    games_pred = case_when(
      games_lag1 >= 16 & games_lag2 >= 16 ~ 16.5,
      TRUE ~ games_pred
    ),
    points_ppr = preds * games_pred,
    poits_ppr_prev = fps_lag1 * games_lag1
  ) %>%
  filter(player_display_name != "Tom Brady") %>%
  arrange(desc(points_ppr)) %>%
  group_by(position) %>%
  mutate(position_rank = min_rank(desc(points_ppr))) %>%
  ungroup() |> 
  filter(position_rank %in% c(0:25),
         position %in% c("WR", "RB", "QB", "TE"))

write.xlsx(all_preds, "fantasy_big_board.xlsx")




