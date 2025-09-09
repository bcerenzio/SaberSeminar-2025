library(RMySQL)
library(tidyverse)
library(lightgbm)
library(parallel)
library(pbapply)
library(progress)
library(data.table)

conn <- dbConnect(MySQL(),
                  username = Sys.getenv('sql_username'),
                  password = Sys.getenv('sql_password'),
                  host = 'localhost',
                  dbname = 'statcast_pbp')


statcast <- dbGetQuery(conn, 'select game_pk, game_date, game_year, game_type, event_index,
                              pitch_number, at_bat_number, home_team, away_team, 
                              batter, bat_side, batter_name, pitcher, p_throws,
                              on_1b, on_2b, on_3b, inning, outs_when_up, balls, strikes,
                              pitch_type, arm_angle, ax, ay, az, vx0, vy0, vz0,
                              release_pos_x, release_pos_y, release_pos_z, release_speed,
                              pfx_x, pfx_z, plate_x, plate_z, release_spin_rate, 
                              spin_axis, sz_top, sz_bot, estimated_woba_using_speedangle as xwOBA,
                              woba_denom, woba_value, delta_run_exp, inning_topbot from statcast
                             where game_year IN (2021,2022,2023) AND
                                   plate_x is not null AND plate_z is not null')


### Calculating ball position after 100 ms

statcast <- statcast %>% 
  mutate(vyR = vyR_fun(vy0, ay, release_pos_y),
         tR = tR_fun(vyR, vy0,ay),
         vxR = vxR_fun(vx0, ax, tR),
         vzR = vzR_fun(vz0, az, tR),
         ball_pos_x_0.1 = ball_pos_x_fun(release_pos_x, vxR, ax),
         ball_pos_y_0.1 = ball_pos_y_fun(release_pos_y, vyR, ay),
         ball_pos_z_0.1 = ball_pos_z_fun(release_pos_z, vzR, az),
         plate_x = plate_x*12,
         plate_z = plate_z*12)

num_cores <- parallel::detectCores() - 2


plate_x_tuning <- function(
  tuning_grid_row
  ) {
  
  
  library(lightgbm)
  library(tidyverse)
  
  leaf_size <- plate_x_tuning_grid[tuning_grid_row, 'leaf_size']
  bag_frac <- plate_x_tuning_grid[tuning_grid_row, 'bag_frac']
  lambda <- plate_x_tuning_grid[tuning_grid_row, 'lambda']
  
model <- lgb.cv(
  params = list(
    seed = 101,
    objective_seed = 101,
    data_seed = 101,
    learning_rate = 0.5,
    min_data_in_leaf = leaf_size,
    subsample = bag_frac,
    lambda_l2 = lambda,
    num_threads = 1,
    feature_pre_filter = FALSE,
    force_col_wise = TRUE
  ),
  data = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                   vx0, ax, vy0, ay)), label = statcast$plate_x),
  nfold = 5,
  obj = 'regression',
  eval = 'rmse',
  nrounds = 1000,
  eval_freq = 100,
  early_stopping_rounds = 100,
  verbose = -1
)

df <- tibble(
  rmse = model$best_score,
  nrounds = model$best_iter,
  lambda = lambda,
  bag_frac = bag_frac,
  leaf_size = leaf_size
)

return(df)

}

plate_x_tuning_grid <- expand.grid(
  lambda = c(0,1,3),
  bag_frac = c(0.5,2/3,0.75,0.9,1),
  leaf_size = c(10,20,30,40)
)

plate_x_tuning_results <- map_df(seq_len(nrow(plate_x_tuning_grid)), plate_x_tuning, .progress = TRUE)

# pboptions(type = 'timer')
# clusters <- makeCluster(num_cores)
# clusterExport(clusters, varlist = c('plate_x_tuning_grid',
#                                     'num_cores',
#                                     'statcast',
#                                     'plate_x_tuning'))
# plate_x_tuning_results <- pblapply(seq_len(nrow(plate_x_tuning_grid)), plate_x_tuning, cl = clusters)
# stopCluster(clusters)
# remove(clusters)
# plate_x_tuning_results <- data.table::rbindlist(plate_x_tuning_results)
# 
plate_x_tuning_best <- plate_x_tuning_results %>% 
  slice_min(rmse, n = 1)


lambda <- plate_x_tuning_best$lambda #3
leaf_size <- plate_x_tuning_best$leaf_size #40
bag_frac <- plate_x_tuning_best$bag_frac # 0.9



set.seed(101);plate_x_mod <- lightgbm(
  params = list(
    seed = 101,
    objective_seed = 101,
    data_seed = 101,
    learning_rate = 0.5/50,
    min_data_in_leaf = leaf_size,
    subsample = bag_frac,
    lambda_l2 = lambda,
    num_threads = 8,
    feature_pre_filter = FALSE,
    force_col_wise = TRUE
  ),
  data = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                   vx0, ax, vy0, ay)), label = statcast$plate_x),
  valids = list(
    valid = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                      vx0, ax, vy0, ay)), label = statcast$plate_x)
  ),
  obj = 'regression',
  eval = 'rmse',
  nrounds = 1000*50,
  eval_freq = 100,
  verbose = 1
);lgb.save(plate_x_mod, filename = 'plate_x_mod.txt')

statcast$pred_plate_x <- predict(plate_x_mod,  as.matrix(
  statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                      vx0, ax, vy0, ay))
)



plate_z_tuning <- function(
    tuning_grid_row
) {
  
  
  library(lightgbm)
  library(tidyverse)
  
  leaf_size_z <- plate_z_tuning_grid[tuning_grid_row, 'leaf_size']
  bag_frac_z <- plate_z_tuning_grid[tuning_grid_row, 'bag_frac']
  lambda_z <- plate_z_tuning_grid[tuning_grid_row, 'lambda']
  
  model <- lgb.cv(
    params = list(
      seed = 101,
      objective_seed = 101,
      data_seed = 101,
      learning_rate = 0.5,
      min_data_in_leaf = leaf_size_z,
      subsample = bag_frac_z,
      lambda_l2 = lambda_z,
      num_threads = 8,
      feature_pre_filter = FALSE,
      force_col_wise = TRUE
    ),
    data = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                     vz0, az, vy0, ay)), label = statcast$plate_z*12),
    nfold = 5,
    obj = 'regression',
    eval = 'rmse',
    nrounds = 1000,
    eval_freq = 100,
    early_stopping_rounds = 100,
    verbose = -1
  )
  
  df <- tibble(
    rmse = model$best_score,
    nrounds = model$best_iter,
    lambda = lambda_z,
    bag_frac = bag_frac_z,
    leaf_size = leaf_size_z
  )
  
  return(df)
  
}

plate_z_tuning_grid <- expand.grid(
  lambda = c(0,1,3),
  bag_frac = c(0.5,2/3,0.75,0.9,1),
  leaf_size = c(10,20,30,40)
)

plate_z_tuning_results <- map_df(seq_len(nrow(plate_z_tuning_grid)), plate_z_tuning, .progress = TRUE)

# pboptions(type = 'timer')
# clusters <- makeCluster(num_cores)
# clusterExport(clusters, varlist = c('plate_x_tuning_grid',
#                                     'num_cores',
#                                     'statcast',
#                                     'plate_x_tuning'))
# plate_x_tuning_results <- pblapply(seq_len(nrow(plate_x_tuning_grid)), plate_x_tuning, cl = clusters)
# stopCluster(clusters)
# remove(clusters)

plate_z_tuning_results <- data.table::rbindlist(plate_z_tuning_results)

plate_z_tuning_best <- plate_z_tuning_results %>% 
  slice_min(rmse, n = 1)


lambda_z <- plate_z_tuning_best$lambda #1
leaf_size_z <- plate_z_tuning_best$leaf_size #40
bag_frac_z <- plate_z_tuning_best$bag_frac # 0.5



set.seed(101);plate_z_mod <- lightgbm(
  params = list(
    seed = 101,
    objective_seed = 101,
    data_seed = 101,
    learning_rate = 0.5/50,
    min_data_in_leaf = leaf_size_z,
    subsample = bag_frac_z,
    lambda_l2 = lambda_z,
    num_threads = 8,
    feature_pre_filter = FALSE,
    force_col_wise = TRUE
  ),
  data = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                   vz0, az, vy0, ay)), label = statcast$plate_z),
  valids = list(
    valid = lgb.Dataset(as.matrix(statcast %>% select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                                                      vz0, az, vy0, ay)), label = statcast$plate_z)
  ),
  obj = 'regression',
  eval = 'rmse',
  nrounds = 1000*50,
  eval_freq = 100,
  verbose = 1
);lgb.save(plate_z_mod, filename = 'plate_z_mod.txt')

