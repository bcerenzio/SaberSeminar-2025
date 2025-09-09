library(baseballr)
library(tidyverse)
library(parallel)
library(DBI)
library(RMySQL)
library(progress)

clusters <- parallel::makeCluster(7)


conn <- DBI::dbConnect(RMySQL::MySQL(), 
                       user = 'root', 
                       password = 'Be@m13G!rl',
                       host = 'localhost',
                       dbname = 'statcast_pbp')


standard_stats_table <- dbGetQuery(conn, 'select * from standard_stats_table')
pa_per_season_stats <- dbGetQuery(conn, 'select * from pa_per_season')
ab_per_season_stats <- dbGetQuery(conn, 'select * from ab_per_season')
h_per_season_stats <- dbGetQuery(conn, 'select * from hits_per_season')
bb_per_season_stats <- dbGetQuery(conn, 'select * from walks_per_season')
hbp_per_season_stats <- dbGetQuery(conn, 'select * from hbp_per_season')
k_per_season_stats <- dbGetQuery(conn, 'select * from k_per_season')
batted_ball_season_stats <- dbGetQuery(conn, 'select * from batted_ball_table')

asfs <- pa_per_season_stats %>% 
  left_join(ab_per_season_stats %>% select(-batter_name), by = c('batter', 'game_year')) %>% 
  left_join(h_per_season_stats %>% select(-batter_name), by = c('batter', 'game_year')) %>% 
  left_join(bb_per_season_stats %>% select(-batter_name), by = c('batter', 'game_year')) %>% 
  left_join(k_per_season_stats, by = c('batter', 'game_year')) %>% 
  left_join(hbp_per_season_stats %>% select(-batter_name), by = c('batter', 'game_year')) %>% 
  left_join(standard_stats_table, by = c('batter', 'game_year')) %>% 
  left_join(batted_ball_season_stats, by = c('batter', 'game_year')) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

asfs1 <- asfs %>% filter(BIP == 0)



guts_page <- fg_guts()

dbWriteTable(conn, 'fg_guts_table', guts_page %>% as.data.frame(), row.names = FALSE, overwrite = TRUE)
