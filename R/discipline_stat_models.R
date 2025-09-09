library(tidyverse)
library(lightgbm)
library(fastDummies)
library(catboost)
library(progress)
library(shapviz)


##### Swing Model ####

swing_df <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  )) %>% 
  select(
    swing,
    in_zone,
    two_strike,
    three_balls,
    strikes,
    balls,
    pred_plate_x,
    pred_plate_z,
    ball_pos_xyz_0.1_diff,
    plate_xz_diff,
    attack_zone,
    pred_run_exp
  ) %>% 
  dummy_cols(select_columns = 'attack_zone', 
             remove_selected_columns = TRUE)


swing_lgb <- lgb.Dataset(data = as.matrix(
  swing_df %>% select(-swing)
),
label = swing_df$swing)

swing_tuning <- function(sigmoid_swing, subsample_swing, num_leaves_swing){


  set.seed(101);mod_output <- lgb.cv(
    params = list(
      seed = 101,
      sigmoid = sigmoid_swing,
      bagging_fraction = subsample_swing,
      num_leaves = num_leaves_swing,
      eta = 0.1,
      objective = 'binary',
      metric = 'binary_logloss'
    ),
    nrounds = 3000,
    nfold = 10,
    data = swing_lgb,
    early_stopping_rounds = 100,
    verbose = 0,
    eval_freq = 100
  )
  
  mod_logloss <- mod_output$best_score
  mod_iteration <- mod_output$best_iter
  
  df <- tibble(
    sigmoid_swing = sigmoid_swing,
    subsample_swing = subsample_swing,
    num_leaves_swing = num_leaves_swing,
    mod_logloss = mod_logloss,
    mod_iteration = mod_iteration
  )
  
  
  return(df)

}

swing_tuning_df <- expand_grid(
  sigmoid_swing = c(0.25,0.5,1,2),
  num_leaves_swing = c(15, 31),
  subsample_swing = c(0.6, 0.8, 1)
)

swing_tuning_df_updated <- pmap_df(list(
  sigmoid_swing = swing_tuning_df$sigmoid_swing,
  subsample_swing = swing_tuning_df$subsample_swing,
  num_leaves_swing = swing_tuning_df$num_leaves_swing
),
swing_tuning,
.progress = TRUE)


sigmoid_swing <- 1
subsample_swing <- 1
num_leaves_swing <- 31
num_rounds_swing <- 717

set.seed(101);swing_mod <- lightgbm(
  params = list(
    seed = 101,
    sigmoid = sigmoid_swing,
    bagging_fraction = subsample_swing,
    num_leaves = num_leaves_swing,
    eta = 0.1/50,
    objective = 'binary',
    metric = 'binary_logloss'
  ),
  valids = list(
    valid = swing_lgb
  ),
  nrounds = num_rounds_swing*50,
  data = swing_lgb,
  eval_freq = 100
);lgb.save(swing_mod, 'swing_mod.txt')

# load model if needed
swing_mod <- lgb.load('swing_mod.txt')

lgb.configure_fast_predict(swing_mod)

swing_prob_predictions <- function(row_num){

swing_prob <- 
  predict(
    swing_mod,
    as.matrix(
      swing_df[row_num, ] %>% 
        select(-swing)
        ) 
    )

  return(swing_prob)
}


updated_statcast_2024_2025_ordered$swing_prob <- map_dbl(1:nrow(updated_statcast_2024_2025_ordered), swing_prob_predictions, .progress = TRUE)

ggplot(data = updated_statcast_2024_2025_ordered, aes(swing_prob)) + 
  geom_density(adjust = 1)

table('swings' = updated_statcast_2024_2025_ordered$swing, 
      'pred_swings' = ifelse(updated_statcast_2024_2025_ordered$swing_prob >= 0.5,1,0))

whiff_df <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  )) %>% 
  group_by(game_year, pitcher, pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup() %>% 
  group_by(pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% # replacing all remaining NA values
  ungroup() %>% 
  drop_na(whiff) %>%
  select(
    whiff,
    in_zone,
    two_strike,
    three_balls,
    strikes,
    balls,
    pred_plate_x,
    pred_plate_z,
    ball_pos_xyz_0.1_diff,
    plate_xz_diff,
    attack_zone,
    pred_run_exp,
    bat_speed,
    release_speed,
    pfx_x, 
    pfx_z
  ) %>% 
  dummy_cols(select_columns = 'attack_zone', 
             remove_selected_columns = TRUE)


whiff_lgb <- lgb.Dataset(data = as.matrix(
  whiff_df %>% select(-whiff)
),
label = whiff_df$whiff)

whiff_scale_pos_weight <-  nrow(whiff_df[whiff_df$whiff == 0, ])/nrow(whiff_df[whiff_df$whiff == 1, ])

# minimizing the false negative
weighted_logloss <- function(preds, dtrain) {
  labels <- get_field(dtrain, 'label')
  preds <- 1 / (1 + exp(-preds))  # sigmoid
  w_pos <- 2  # penalty for FN
  w_neg <- 1
  grad <- ifelse(labels == 1,
                 w_pos * (preds - 1),
                 w_neg * preds)
  hess <- ifelse(labels == 1,
                 w_pos * preds * (1 - preds),
                 w_neg * preds * (1 - preds))
  list(grad = grad, hess = hess)
}

weighted_logloss_eval <- function(preds, dtrain) {
  labels <- get_field(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  w_pos <- whiff_scale_pos_weight
  w_neg <- 1
  
  # weighted log loss calculation
  loss <- -mean(
    w_pos * labels * log(preds + 1e-15) +
      w_neg * (1 - labels) * log(1 - preds + 1e-15)
  )
  
  return(list(name = "weighted_logloss", value = loss, higher_better = FALSE))
}



whiff_tuning <- function(num_leaves_whiff, lambda_whiff
                        # whiff_scale_pos_weight
                         ){
  
  
  set.seed(101);mod_output <- lgb.cv(
    params = list(
      seed = 101,
      num_leaves = num_leaves_whiff,
      #scale_pos_weight = whiff_scale_pos_weight,
      lambda = lambda_whiff,
      eta = 0.1,
      objective = weighted_logloss#,
      #metric = recall_eval
    ),
    nrounds = 3000,
    nfold = 10,
    data = whiff_lgb,
    eval = weighted_logloss_eval,
    early_stopping_rounds = 100,
    verbose = 0,
    eval_freq = 100
  )
  
  mod_logloss <- mod_output$best_score
  mod_iteration <- mod_output$best_iter
  
  df <- tibble(
    num_leaves_whiff = num_leaves_whiff,
    lambda_whiff = lambda_whiff,
    whiff_scale_pos_weight = whiff_scale_pos_weight,
    mod_logloss = mod_logloss,
    mod_iteration = mod_iteration
  )
  
  
  return(df)
  
}


whiff_tuning_grid <- expand_grid(
  num_leaves_whiff = c(31,62,91, 108),
  lambda_whiff = c(5,10,15,20)
  #whiff_scale_pos_weight = c(whiff_scale_pos_weight, 1/mean(whiff_df$whiff))
)

whiff_tuning_grid <- pmap_df(list(
  num_leaves_whiff = whiff_tuning_grid$num_leaves_whiff,
  lambda_whiff = whiff_tuning_grid$lambda_whiff
  #whiff_scale_pos_weight = whiff_tuning_grid$whiff_scale_pos_weight
),
whiff_tuning,
.progress = TRUE)

num_leaves_whiff <- 31 # 31
lambda_whiff <- 20 # 20
nrounds_whiff <- 350 #350

set.seed(101);whiff_mod <- lightgbm(
  params = list(
    seed = 101,
    num_leaves = num_leaves_whiff,
   # scale_pos_weight = whiff_scale_pos_weight,
    lambda = lambda_whiff,
    eta = 0.1/50,
    objective = weighted_logloss#,
    #metric = 'binary_logloss'
  ),
  nrounds = nrounds_whiff*50,
  data = whiff_lgb,
  valids = list(
    valids = whiff_lgb
  ),
  eval = weighted_logloss_eval,
  verbose = 1,
  eval_freq = 100
);lgb.save(whiff_mod, 'whiff_mod.txt')

# load model if necessary
whiff_mod <- lgb.load('whiff_mod.txt')

lgb.configure_fast_predict(whiff_mod)


whiff_prob_predictions <- function(row_num){
  
  whiff_prob <- 
    predict(
      whiff_mod,
      as.matrix(
        whiff_df_preds[row_num, ]
      ) 
    )
  
  whiff_prob <- sigmoid::sigmoid(whiff_prob, method = 'logistic')
  
  return(whiff_prob)
}

whiff_df_preds <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  )) %>% 
  group_by(game_year, batter) %>% 
  mutate(bat_speed = ifelse(is.na(bat_speed), mean(bat_speed, na.rm = TRUE), bat_speed)) %>% 
  ungroup() %>% 
  group_by(game_year, pitcher, pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup() %>% 
  group_by(pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% # replacing all remaining NA values
  ungroup() %>% 
  select(
    in_zone,
    two_strike,
    three_balls,
    strikes,
    balls,
    pred_plate_x,
    pred_plate_z,
    ball_pos_xyz_0.1_diff,
    plate_xz_diff,
    attack_zone,
    pred_run_exp,
    bat_speed,
    release_speed,
    pfx_x, 
    pfx_z
  ) %>% 
  dummy_cols(select_columns = 'attack_zone', 
             remove_selected_columns = TRUE)


updated_statcast_2024_2025_ordered$whiff_prob <- map_dbl(1:nrow(updated_statcast_2024_2025_ordered), whiff_prob_predictions, .progress = TRUE)

#updated_statcast_2024_2025_ordered$whiff_prob_new <- map_dbl(1:nrow(updated_statcast_2024_2025_ordered), whiff_prob_predictions, .progress = TRUE)

updated_statcast_2024_2025_ordered %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = whiff_prob_new)) +
  stat_summary_hex() +
  scale_x_continuous(limits = c(-0.83*12-40, 0.83*12 + 40)) +
  scale_y_continuous(limits = c(1.6*12-20, 3.6*12+10)) +
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = 0.23,
                                               limits = c(0, 0.6), oob = scales::squish)


summary(updated_statcast_2024_2025_ordered$whiff_prob)
summary(updated_statcast_2024_2025_ordered$whiff_prob_new)

table('whiffs' = updated_statcast_2024_2025_ordered$whiff, 
      'pred_whiffs' = ifelse(
        updated_statcast_2024_2025_ordered$whiff_prob_new > 0.5,
        1,
        0
      ))

table('whiffs' = updated_statcast_2024_2025_ordered$whiff, 
      'pred_whiffs' = ifelse(
        updated_statcast_2024_2025_ordered$whiff_prob > 0.42,
        1,
        0
      ))

whiff_shap_predictions <- function(row_num){
  
  whiff_prob <- 
    predict(
      whiff_mod,
      as.matrix(
        whiff_df[row_num, ] %>% 
          select(-whiff)
      ),
      predcontrib = TRUE
    )
  
  whiff_prob <- as.data.frame(whiff_prob)
  
  return(whiff_prob)
}

whiff_shap_values <- map_df(sample(1:nrow(whiff_df), size = 100000, replace = FALSE), whiff_shap_predictions, .progress = TRUE)

#SHAPforxgboost::shap.plot.summary(predict(whiff_mod, as.matrix(whiff_df %>% select(-whiff)), type = 'contrib'))

colnames(whiff_shap_values) <- c(colnames(
  whiff_df %>% 
    select(-whiff)
), 'BIAS')

sv_importance(
shapviz::shapviz(whiff_mod, X_pred = as.matrix(whiff_df %>% select(-whiff) %>% slice(1:500)), X = as.matrix(whiff_df %>% select(-whiff) %>% slice(1:500))),
kind = 'bee'
)

shap_prep <- SHAPforxgboost::shap.prep(shap_contrib = whiff_shap_values[,-19], X_train = whiff_df %>% select(-whiff) %>% slice_head(n = 100000))

shap_prep$variable <- factor(
  shap_prep$variable,
  levels = colnames(whiff_df %>% select(-whiff))
)

SHAPforxgboost::shap.plot.summary(shap_prep, x_bound = 1)


SHAPforxgboost::shap.plot.summary(shap_prep)

ggplot(data = updated_statcast_2024_2025_ordered, aes(whiff_prob)) + 
  geom_density(adjust = 1)


xwoba_df <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  )) %>% 
  group_by(game_year, pitcher, pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup() %>% 
  group_by(pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% # replacing all remaining NA values
  ungroup() %>% 
  filter(description == 'hit_into_play') %>% 
  drop_na(xwOBA) %>%
  select(
    xwOBA,
    in_zone,
    two_strike,
    three_balls,
    strikes,
    balls,
    pred_plate_x,
    pred_plate_z,
    ball_pos_xyz_0.1_diff,
    plate_xz_diff,
    attack_zone,
    pred_run_exp,
    bat_speed,
    release_speed,
    pfx_x, 
    pfx_z
  ) %>% 
  dummy_cols(select_columns = 'attack_zone', 
             remove_selected_columns = TRUE)



xwoba_lgb <- lgb.Dataset(data = as.matrix(
  xwoba_df %>% select(-xwOBA)
),
label = xwoba_df$xwOBA)


xwoba_tuning <- function(num_leaves_xwoba, lambda_xwoba
){
  
  
  set.seed(101);mod_output <- lgb.cv(
    params = list(
      seed = 101,
      num_leaves = num_leaves_xwoba,
      lambda = lambda_xwoba,
      eta = 0.1,
      objective = 'regression',
      metric = 'rmse'
    ),
    nrounds = 3000,
    nfold = 10,
    data = xwoba_lgb,
    early_stopping_rounds = 100,
    verbose = 1,
    eval_freq = 50,
    eval_train_metric = TRUE
  )
  
  mod_logloss <- mod_output$best_score
  mod_iteration <- mod_output$best_iter
  
  df <- tibble(
    num_leaves_xwoba = num_leaves_xwoba,
    lambda_xwoba = lambda_xwoba,
    mod_logloss = mod_logloss,
    mod_iteration = mod_iteration
  )
  
  
  return(df)
  
}


xwoba_tuning_grid <- expand_grid(
  num_leaves_xwoba = c(62,91, 108, 120),
  lambda_xwoba = c(15,20,25, 30,35)
)

xwoba_tuning_grid <- pmap_df(list(
  num_leaves_xwoba = xwoba_tuning_grid$num_leaves_xwoba,
  lambda_xwoba = xwoba_tuning_grid$lambda_xwoba
),
xwoba_tuning,
.progress = TRUE)

num_leaves_xwoba <- 62 # 62
lambda_xwoba <- 30 # 30
nrounds_xwoba <- 59 #59


set.seed(101);xwoba_mod <- lightgbm(
  params = list(
    seed = 101,
    num_leaves = num_leaves_xwoba,
    lambda = lambda_xwoba,
    eta = 0.1/150,
    objective = 'regression',
    metric = 'rmse'
  ),
  nrounds = nrounds_xwoba*150,
  data = xwoba_lgb,
  valids = list(
    valid = xwoba_lgb
  ),
  verbose = 1,
  eval_freq = 100
);lgb.save(xwoba_mod, 'xwoba_mod.txt')


lgb.configure_fast_predict(xwoba_mod)


xwoba_prob_predictions <- function(row_num){
  
  xwoba_prob <- 
    predict(
      xwoba_mod,
      as.matrix(
        xwoba_df_preds[row_num, ]
      ) 
    )
  
  return(xwoba_prob)
}

xwoba_df_preds <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  )) %>% 
  group_by(game_year, batter) %>% 
  mutate(bat_speed = ifelse(is.na(bat_speed), mean(bat_speed, na.rm = TRUE), bat_speed)) %>% 
  ungroup() %>% 
  group_by(game_year, pitcher, pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup() %>% 
  group_by(pitch_type) %>% 
  mutate(across(c('pfx_x', 'release_speed'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% # replacing all remaining NA values
  ungroup() %>% 
  select(
    in_zone,
    two_strike,
    three_balls,
    strikes,
    balls,
    pred_plate_x,
    pred_plate_z,
    ball_pos_xyz_0.1_diff,
    plate_xz_diff,
    attack_zone,
    pred_run_exp,
    bat_speed,
    release_speed,
    pfx_x, 
    pfx_z
  ) %>% 
  dummy_cols(select_columns = 'attack_zone', 
             remove_selected_columns = TRUE)


updated_statcast_2024_2025_ordered$xwobacon <- map_dbl(1:nrow(updated_statcast_2024_2025_ordered), xwoba_prob_predictions, .progress = TRUE)
