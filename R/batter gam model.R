library(DBI)
library(tidyverse)
library(mgcv)
library(parallel)
library(pbapply)
library(progress)

conn <- DBI::dbConnect(
  RMySQL::MySQL(),
  user =  Sys.getenv('sql_username'),
  password = Sys.getenv('sql_password'),
  host = 'localhost',
  dbname = 'statcast_pbp'
)

statcast_2024_2025 <- dbGetQuery(conn,
                                 'select * from statcast
                                 where game_year >= 2024')


gam_mod <- bam(woba_value ~ te(plate_x, plate_z, batter, bs = c('tp', 'tp','fs'), k = 3), data = statcast_2024_2025 %>% 
        filter(woba_denom == 1) %>% 
        mutate(batter = as.factor(batter)),
      discrete = TRUE
    )


gam_mod_fun <- function(batter_id){
  library(mgcv); library(lightgbm)
  
  df <- statcast_2024_2025 |> 
    dplyr::filter(batter == batter_id) |>
    tidyr::drop_na(plate_x, plate_z) |>
    dplyr::mutate(
      plate_x = plate_x*12,
      plate_z = plate_z*12
    )
  
  tryCatch({
  gam_mod <- mgcv::gam(delta_run_exp ~ te(plate_x, plate_z,bs = 'cs', by = p_throws), data = df
  )
  
  #plate_x_mod <- lightgbm::lgb.load('plate_x_mod.txt')
  #plate_z_mod <- lightgbm::lgb.load('plate_z_mod.txt')
  
  df$pred_plate_x <- predict(plate_x_mod, as.matrix(
    df |>
      dplyr::select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
             vx0, ax, vy0, ay)
    )
  )
  
  df$pred_plate_z <- predict(plate_z_mod, as.matrix(
    df |>
      dplyr::select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                    vz0, az, vy0, ay)
  )
  )
  
  
  # make woba predictions based on expcted pitch location
  df$pred_run_exp <- as.numeric(predict(gam_mod, df |> 
                            dplyr::select(-plate_x, -plate_z) |>
                            dplyr::rename(
                              'plate_x' = 'pred_plate_x',
                              'plate_z' = 'pred_plate_z'
                            )))
  
  #dplyr::glimpse(df)
  return(df)
  
  }, error = function(e){
    print('error')
    
    df$pred_run_exp <- NA
    
    return(df)
  }
  )
}

# getting unique batters
unique_batter_ids <- unique(statcast_2024_2025$batter)

source('pitch_flight_metrics.R')

statcast_2024_2025 <- statcast_2024_2025 %>% 
  mutate(vyR = vyR_fun(vy0, ay, release_pos_y),
         tR = tR_fun(vyR, vy0,ay),
         vxR = vxR_fun(vx0, ax, tR),
         vzR = vzR_fun(vz0, az, tR),
         ball_pos_x_0.1 = ball_pos_x_fun(release_pos_x, vxR, ax),
         ball_pos_y_0.1 = ball_pos_y_fun(release_pos_y, vyR, ay),
         ball_pos_z_0.1 = ball_pos_z_fun(release_pos_z, vzR, az),
         p_throws = as.factor(p_throws))



plate_x_mod <- lightgbm::lgb.load('plate_x_mod.txt')
plate_z_mod <- lightgbm::lgb.load('plate_z_mod.txt')

# Set up parallel processing (6 background processes)
# clusters <- makeCluster(4)
# on.exit(stopCluster(clusters))
# clusterExport(cl = clusters, varlist = c('statcast_2024_2025'))
# updated_statcast_2024_2025 <- pblapply(unique_batter_ids, FUN = gam_mod_fun, cl = clusters)
# updated_statcast_2024_2025 <- data.table::rbindlist(updated_statcast_2024_2025, fill = TRUE)
# stopCluster(clusters)


updated_statcast_2024_2025 <- map_df(unique_batter_ids, gam_mod_fun, .progress = TRUE)


# Jhoensky Noel
updated_statcast_2024_2025 %>% 
  filter(batter == '678877') %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = pred_run_exp)) +
  stat_summary_2d(binwidth = c(2,2)) +
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE,
                                               mid = mean(updated_statcast_2024_2025$pred_run_exp, na.rm = TRUE),
                                               limits = c(-0.05, 0.05),
                                               oob = scales::squish)

# Jason Heyward
updated_statcast_2024_2025_ordered %>% 
  filter(batter == '518792') %>%
  filter(swing == 1) %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = woba_value)) +
  stat_summary_2d(binwidth = c(2,2)) +
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE,
                                               mid = mean(updated_statcast_2024_2025_ordered %>% 
                                                            filter(swing == 1, batter == '518792') %>% 
                                                            pull(woba_value), na.rm = TRUE),
                                               #limits = c(-0.05, 0.05),
                                               oob = scales::squish)

# Aaron Judge
updated_statcast_2024_2025_ordered %>% 
  filter(batter == '592450') %>%
  filter(swing == 1) %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = pred_run_exp)) +
  stat_summary_2d(binwidth = c(2,2)) +
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE,
                                               mid = mean(updated_statcast_2024_2025_ordered %>% 
                                                            filter(swing == 1, batter == '592450') %>% 
                                                            pull(pred_run_exp), na.rm = TRUE),
                                               #limits = c(-0.05, 0.05),
                                               oob = scales::squish)


gam_mod_fun_swings <- function(batter_id){
  library(mgcv); library(lightgbm)
  
  df <- statcast_2024_2025 |> 
    dplyr::filter(batter == batter_id) |>
    tidyr::drop_na(plate_x, plate_z) |>
    dplyr::mutate(
      plate_x = plate_x*12,
      plate_z = plate_z*12
    )
  
  tryCatch({
    gam_mod <- mgcv::gam(delta_run_exp ~ te(plate_x, plate_z,bs = 'cs', by = p_throws), data = df %>% 
                           filter(!is.na(bat_speed))
    )
    
    #plate_x_mod <- lightgbm::lgb.load('plate_x_mod.txt')
    #plate_z_mod <- lightgbm::lgb.load('plate_z_mod.txt')
    
    df$pred_plate_x <- predict(plate_x_mod, as.matrix(
      df |>
        dplyr::select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                      vx0, ax, vy0, ay)
    )
    )
    
    df$pred_plate_z <- predict(plate_z_mod, as.matrix(
      df |>
        dplyr::select(ball_pos_x_0.1, ball_pos_y_0.1, ball_pos_z_0.1,
                      vz0, az, vy0, ay)
    )
    )
    
    
    # make woba predictions based on expcted pitch location
    df$pred_run_exp <- as.numeric(predict(gam_mod, df |> 
                                            dplyr::select(-plate_x, -plate_z) |>
                                            dplyr::rename(
                                              'plate_x' = 'pred_plate_x',
                                              'plate_z' = 'pred_plate_z'
                                            )))
    
    #dplyr::glimpse(df)
    return(df)
    
  }, error = function(e){
    print('error')
    
    df$pred_run_exp <- NA
    
    return(df)
  }
  )
}

judge_df <- gam_mod_fun_swings('592450')

kwan_df <- gam_mod_fun_swings('680757')


judge_df %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = pred_run_exp)) +
  stat_summary_2d(binwidth = c(2,2)) +
  scale_x_continuous(limits = c(-25,25)) + 
  scale_y_continuous(limits = c(0, 60)) + 
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE,
                                               mid = mean(judge_df %>% 
                                                            pull(pred_run_exp), na.rm = TRUE),
                                               #limits = c(-0.05, 0.05),
                                               oob = scales::squish)


kwan_df %>% 
  ggplot(aes(pred_plate_x, pred_plate_z, z = pred_run_exp)) +
  stat_summary_2d(binwidth = c(2,2)) +
  scale_x_continuous(limits = c(-25,25)) + 
  scale_y_continuous(limits = c(0, 60)) + 
  annotate('line', x = -0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = 0.83*12, y = c(1.6*12, 3.6*12)) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 1.6*12) +
  annotate('line', x = c(-0.83*12, 0.83*12), y = 3.6*12) +
  colorspace::scale_fill_continuous_divergingx('RdBu', rev = TRUE,
                                               mid = mean(kwan_df %>% 
                                                            pull(pred_run_exp), na.rm = TRUE),
                                               #limits = c(-0.05, 0.05),
                                               oob = scales::squish)
