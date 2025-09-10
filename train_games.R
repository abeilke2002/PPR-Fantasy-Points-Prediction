source("functions.R")

library(nflfastR)
library(tidyverse)
library(lubridate)
library(splines)
library(caret)

game_formula1 <- games ~  
  ns(age, df = 3) + 
  games_lag1

games1 <- train_cv_games_1_season(stats,
                                  model_formula = game_formula1)

game_formula2 <- games ~  
  ns(age, df = 3) +
  games_lag1:games_lag2

games2 <- train_cv_games_2_season(stats,
                                  model_formula = game_formula2)

game_formula3 <- games ~  
  ns(age, df = 3) +
  games_lag1:games_lag2:games_lag3

games3 <- train_cv_games_3_season(stats,
                                  model_formula = game_formula3)

game_formula4 <- games ~  
  ns(age, df = 3) +
  games_lag1:games_lag2:games_lag3

games4 <- train_cv_games_4_season(stats,
                                  model_formula = game_formula4)


check <- games4$df_result


