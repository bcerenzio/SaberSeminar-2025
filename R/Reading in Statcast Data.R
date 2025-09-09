library(sabRmetrics)
library(parallel)
library(tidyverse)
library(baseballr)
library(DBI)
library(RMySQL)

clusters <- parallel::makeCluster(7)
on.exit(stopCluster(clusters))


#getting weather data (from 2008 - 2022) ####
weather <- load_game_info_sup()
write_csv(weather %>% 
            mutate(across(everything(), ~as.character(.))) %>% 
            mutate(across(everything(), ~replace_na(., 'NULL'))), 'weather.csv')
remove(weather)


#### 2025 ####

conn <- DBI::dbConnect(
  RMySQL::MySQL(),
  user =  Sys.getenv('sql_username'),
  password = Sys.getenv('sql_password'),
  host = 'localhost',
  dbname = 'statcast_pbp',
  client.flag = CLIENT_LOCAL_FILES
)


clusters <- parallel::makeCluster(7)
on.exit(stopCluster(clusters))

for (season in 2025:2008){
  conn <- DBI::dbConnect(
    RMySQL::MySQL(),
    user =  Sys.getenv('sql_username'),
    password = Sys.getenv('sql_password'),
    host = 'localhost',
    dbname = 'statcast_pbp',
    client.flag = CLIENT_LOCAL_FILES
  )
  for (game_type in c('R', 'F', 'D', 'L', 'W', 'S', 'A')){ #removing exhibition for now
    cat("Season:", season, '\nGame Type:', game_type)
    
    if ((season == 2025 & game_type %in% c('F', 'D', 'L', 'W')) | 
        (season == 2011 & game_type %in% c('F', 'R', 'D', 'L', 'W', 'S')) |
        (season < 2011 & game_type == 'F')
        ) {next} # removes downloads for games that haven't happend yet
    
    start_date <- glue::glue('{season}-01-01')
    
    end_date <- glue::glue('{season}-12-31')
    
    baseballsavant <- download_baseballsavant(start_date, end_date, cl = clusters, game_type = game_type)
    
    baseballsavant <- baseballsavant |> 
      select(-spin_dir, -spin_rate_deprecated, -tfs_deprecated, -tfs_zulu_deprecated,
             -break_angle_deprecated, -break_length_deprecated, -umpire, -sv_id) |> 
      rename(
        'game_pk' = 'game_id',
        'game_year' = 'year',
        'batter' = 'batter_id',
        'pitcher' = 'pitcher_id',
        'p_throws' = 'pitch_hand',
        'on_1b' = 'pre_runner_1b_id',
        'on_2b' = 'pre_runner_2b_id',
        'on_3b' = 'pre_runner_3b_id',
        'outs_when_up' = 'outs',
        'release_extension' = 'extension',
        'sz_top' = 'strike_zone_top',
        'sz_bot' = 'strike_zone_bottom',
        'estimated_woba_using_speedangle' = 'expected_woba',
        'estimated_ba_using_speedangle' = 'expected_babip',
        'hc_x' = 'hit_coord_x',
        'hc_y' = 'hit_coord_y'
      ) |>
      mutate(game_type = case_when(
        game_type == 'R' ~ 'Regular Season',
        game_type == FALSE ~ 'Wild Card',
        game_type == 'D' ~ 'Division Series (ALDS/NLDS)', 
        game_type == 'L' ~ 'Conference Series (ALCS/NLCS)',
        game_type == 'W' ~ 'World Series',
        game_type == 'A' ~ 'All-Star',
        game_type == 'E' ~ 'Exhibition (Possibly College)'
      ))
    
    if (season == 2025 & game_type == 'R'){
      dbWriteTable(
        conn,
        'statcast',
        baseballsavant |>
          as.data.frame(),
        append = FALSE,
        row.names = FALSE,
        overwrite = TRUE
      )
    } else{
      dbWriteTable(
        conn,
        'statcast',
        baseballsavant |>
          as.data.frame(),
        append = TRUE,
        row.names = FALSE,
        overwrite = FALSE
      )
    }
    
  }
}


# getting weather data for the 2023-2024 seasons ####
game_pks_2023_24 <- dbGetQuery(conn, 'SELECT
	DISTINCT
	game_pk
FROM
	statcast
WHERE
	game_year >= 2023') %>% pull(game_pk)


weather <- map_df(game_pks_2023_24, function(game_pk){
  df <- mlb_game_info(game_pk); return(df)
}, .progress = TRUE)


dbWriteTable(conn, 
             'weather',
             weather %>% as.data.frame(),
             append = TRUE,
             row.names = FALSE,
             overwrite = FALSE
             )
