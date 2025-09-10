setup_cv <- function(control_data,
                     n_folds = 5,
                     validation_year = 2024,
                     seed = 7) {
  # Set seed for reproducibility
  set.seed(seed)
  # Re-sample data to have even distribution of response values across folds
  control_data <- control_data[sample(seq_len(nrow(control_data))), ]
  # Separate validation set from the rest
  validation_set <- control_data %>% filter(season == validation_year)
  cv_set <- control_data %>% filter(season != validation_year)
  # Set number of folds, and fold indices
  fold_ind <- (seq_len(nrow(cv_set)) %% n_folds) + 1
  return(list(cv_set, validation_set, fold_ind))
}

train_cv_fp_1_season <- function(control_data,
                        n_folds = 5,
                        model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 150,
           # games >= 8,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      fps_lag1 = lag(fp_per_game, 1),
      fps_lag2 = lag(fp_per_game, 2),
      fps_lag3 = lag(fp_per_game, 3),
      fps_lag4 = lag(fp_per_game, 4),
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, fps_lag1, fps_lag2, fps_lag3, fps_lag4, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 1,
           !is.na(fps_lag1),
           !is.na(games_lag1))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$fp_per_game
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$fp_per_game
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      season, 
      position,
      seasons_in_league, 
      fps_lag1, 
      games_lag1, 
      games,
      pred,
      fp_per_game
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_fp_2_seasons <- function(control_data,
                                  n_folds = 5,
                                  model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 160,
           # games >= 8,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      fps_lag1 = lag(fp_per_game, 1),
      fps_lag2 = lag(fp_per_game, 2),
      fps_lag3 = lag(fp_per_game, 3),
      fps_lag4 = lag(fp_per_game, 4),
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, games, position,season, seasons_in_league, fps_lag1, fps_lag2, fps_lag3, fps_lag4, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 2,
           !is.na(fps_lag1),
           !is.na(games_lag1),
           !is.na(fps_lag2),
           !is.na(games_lag2))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$fp_per_game
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$fp_per_game
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      position,
      season, 
      seasons_in_league, 
      fps_lag1, 
      fps_lag2, 
      games_lag1, 
      games_lag2,
      games,
      pred,
      fp_per_game
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_fp_3_seasons <- function(control_data,
                                  n_folds = 5,
                                  model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 170,
           # games >= 8,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      fps_lag1 = lag(fp_per_game, 1),
      fps_lag2 = lag(fp_per_game, 2),
      fps_lag3 = lag(fp_per_game, 3),
      fps_lag4 = lag(fp_per_game, 4),
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, fps_lag1, fps_lag2, fps_lag3, fps_lag4, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 3,
           !is.na(fps_lag1),
           !is.na(games_lag1),
           !is.na(fps_lag2),
           !is.na(games_lag2),
           !is.na(fps_lag3),
           !is.na(games_lag3))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$fp_per_game
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$fp_per_game
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      position,
      season, 
      seasons_in_league, 
      fps_lag1, 
      fps_lag2, 
      fps_lag3,
      games_lag1, 
      games_lag2,
      games_lag3,
      games,
      pred,
      fp_per_game
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_fp_4_seasons <- function(control_data,
                                  n_folds = 5,
                                  model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 180,
           # games >= 8,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      fps_lag1 = lag(fp_per_game, 1),
      fps_lag2 = lag(fp_per_game, 2),
      fps_lag3 = lag(fp_per_game, 3),
      fps_lag4 = lag(fp_per_game, 4),
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, fps_lag1, fps_lag2, fps_lag3, fps_lag4, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league > 3,
           !is.na(fps_lag1),
           !is.na(games_lag1),
           !is.na(fps_lag2),
           !is.na(games_lag2),
           !is.na(fps_lag3),
           !is.na(games_lag3),
           !is.na(fps_lag4),
           !is.na(games_lag4))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$fp_per_game
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$fp_per_game
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      position,
      season, 
      seasons_in_league, 
      fps_lag1, 
      fps_lag2, 
      fps_lag3,
      games_lag1, 
      games_lag2,
      games_lag3,
      games,
      pred,
      fp_per_game
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}


## Train Predicted Games Model

train_cv_games_1_season <- function(control_data,
                                 n_folds = 5,
                                 model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 0,
           games >= 8,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4),
      is_qb = ifelse(position == "QB", 1, 0),
      is_wr_rb_te = ifelse(position %in% c("WR", "RB", "TE"), 1, 0)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, is_qb, is_wr_rb_te, games, season, seasons_in_league, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 1,
           !is.na(games_lag1))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$games
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$games
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      season, 
      position,
      is_qb, 
      is_wr_rb_te,
      seasons_in_league, 
      games_lag1, 
      games,
      pred,
      games
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_games_2_season <- function(control_data,
                                    n_folds = 5,
                                    model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 0,
           games >= 10,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4),
      last_two = games_lag1 + games_lag2,
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game, last_two)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 2,
           !is.na(games_lag1),
           !is.na(games_lag2))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$games
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$games
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      season, 
      position,
      seasons_in_league, 
      games_lag1, 
      games_lag2,
      last_two,
      games,
      pred,
      games
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_games_3_season <- function(control_data,
                                    n_folds = 5,
                                    model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 0,
           games >= 12,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league == 3,
           !is.na(games_lag1),
           !is.na(games_lag2),
           !is.na(games_lag3))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$games
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$games
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      season, 
      position,
      seasons_in_league, 
      games_lag1, 
      games_lag2,
      games_lag3,
      games,
      pred,
      games
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}

train_cv_games_4_season <- function(control_data,
                                    n_folds = 5,
                                    model_formula) {
  set.seed(1984)
  control_data <- control_data %>%
    filter(player_id != 0,
           fantasy_points_ppr > 0,
           games >= 14,
           position %in% c("RB", "QB", "K", "WR", "TE")) |> 
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      games_lag1 = lag(games, 1),
      games_lag2 = lag(games, 2),
      games_lag3 = lag(games, 3),
      games_lag4 = lag(games, 4)
    ) %>%
    ungroup() |> 
    select(player_id, player_display_name, age, position, games, season, seasons_in_league, games_lag1, games_lag2, games_lag3, games_lag4, fp_per_game)
  
  control_data <- control_data |> 
    filter(seasons_in_league > 3,
           !is.na(games_lag1),
           !is.na(games_lag2),
           !is.na(games_lag3))
  
  # split data with setup_cv
  train <- setup_cv(control_data)
  cv_set <- train[[1]]
  validation_set <- train[[2]]
  fold_ind <- train[[3]]
  # create a place to store validation results
  validation_results <- data.frame(
    fold = integer(),
    accuracy = numeric(),
    logloss = numeric(),
    stringsAsFactors = FALSE
  )
  # store oof preds
  oof_predictions <- data.frame()
  cat("Training for status Formula: ", deparse(model_formula), "\n")
  for (k in 1:n_folds) {
    cat("Training on Fold:", k, "\n")
    # Split train and validation
    cv_train <- cv_set[which(fold_ind != k), ]
    cv_valid <- cv_set[which(fold_ind == k), ]
    # Train model
    model_definition <- train(
      model_formula,
      data = cv_train,
      method = "lm"
    )
    # model_definition <- scam(
    #  season_added_formula,
    #  data = cv_train,
    #  family = gaussian()
    # )
    for (set_ind in 1:3) {
      if (set_ind == 1) {
        eval_set <- cv_train  # train fold
      }
      else if (set_ind == 2) {
        eval_set <- cv_valid  # validation fold (untouched)
      }
      else {
        eval_set <- validation_set  # external hold-out set
      }
      # Predict p(added status) class
      df_preds <- predict(model_definition, newdata = eval_set)
      actual_values <- eval_set$games
      # performance metrics
      rmse <- sqrt(mean((df_preds - actual_values)^2))
      mae <- mean(abs(df_preds - actual_values))
      r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
      # Log the metrics
      cat("Fold:", k, "Set:", set_ind, "RMSE:", rmse, "MAE:", mae, "R²:", r_squared, "\n")
      # save validation results
      validation_results <- rbind(
        validation_results,
        data.frame(
          fold = k,
          set = set_ind,
          rmse = rmse,
          mae = mae,
          r_squared = r_squared
        )
      )
      # Save OOF predictions for fold 2
      if (set_ind == 2) {
        oof_predictions <- rbind(
          oof_predictions,
          data.frame(
            fold = k,
            actual = actual_values,   # numeric actuals
            predicted_value = df_preds  # numeric predictions
          )
        )
      }
    }
  }
  # Get out of sample metrics
  # Accuracy
  oof_actual <- oof_predictions$actual
  oof_predicted <- oof_predictions$predicted_value
  # RMSE
  oof_rmse <- sqrt(mean((oof_predicted - oof_actual)^2))
  oof_mae <- mean(abs(oof_predicted - oof_actual))
  oof_r2 <- 1 - sum((oof_predicted - oof_actual)^2) / sum((oof_actual - mean(oof_actual))^2)
  # Log the metrics
  cat("Out-of-fold RMSE:", oof_rmse, "\n")
  cat("Out-of-fold MAE:", oof_mae, "\n")
  cat("Out-of-fold R²:", oof_r2, "\n")
  # train final model
  # final_model <- scam(
  #  season_added_formula,                           # interaction term
  #  data = control_data,
  #  family = gaussian()
  # )
  final_model <- train(
    model_formula,
    data = control_data,
    method = "lm"
  )
  df_preds <- predict(final_model, newdata = control_data)
  actual_values <- control_data$games
  # performance metrics
  rmse <- sqrt(mean((df_preds - actual_values)^2))
  mae <- mean(abs(df_preds - actual_values))
  r_squared <- 1 - sum((df_preds - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  df_r <- cbind(
    control_data,
    pred = df_preds
  )
  # print metrics
  cat("training data RMSE:", rmse, "\n")
  cat("training data MAE:", mae, "\n")
  cat("training data r^2:", r_squared, "\n")
  df_r <- df_r %>%
    select(
      player_id, 
      player_display_name, 
      age, 
      season, 
      position,
      seasons_in_league, 
      games_lag1, 
      games_lag2,
      games_lag3,
      games_lag4,
      games,
      pred,
      games
    )
  return(list(
    model_fit = final_model,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    df_result = df_r,
    oof_predictions = oof_predictions
  ))
}




