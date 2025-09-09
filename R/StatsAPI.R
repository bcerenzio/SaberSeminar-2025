library(baseballr)
library(tidyverse)
library(parallel)
library(DBI)
library(RMySQL)
library(progress)
library(pbapply)
library(furrr)
library(progressr)

readRenviron('.Renviron')

conn <- DBI::dbConnect(
  RMySQL::MySQL(),
  user =  Sys.getenv('sql_username'),
  password = Sys.getenv('sql_password'),
  host = 'localhost',
  dbname = 'statcast_pbp'
)

unique_game_pks <-
  dbGetQuery(conn,
             'SELECT Distinct game_pk, game_year from statcast where game_year = 2023')$game_pk

get_mlb_statsapi_pbp <- function(game_pk) {
  library(tidyverse)
  conn <- DBI::dbConnect(
    RMySQL::MySQL(),
    user = 'root',
    password = 'Be@m13G!rl',
    host = 'localhost',
    dbname = 'statcast_pbp'
  )
  df <- baseballr::get_pbp_mlb(game_pk)
  
  df <- df %>%
    select(
      -details.call.code,
      -details.call.description,
      -atBatIndex,-about.startTime,
      -about.endTime,
      -about.isComplete,
      -about.hasReview,
      -about.isScoringPlay,-matchup.batSide.description,
      -matchup.pitchHand.description,
      -result.isOut,-starts_with('details.violation'),
      -starts_with('reviewDetails')
    ) %>%
    rename(
      'pitcher_id' = 'matchup.pitcher.id',
      'pitcher_name' = 'matchup.pitcher.fullName',
      'at_bat_number' = 'about.atBatIndex',
      'pitch_end_speed' = 'pitchData.endSpeed',
      'pitch_to_plate_time' = 'pitchData.plateTime',
      'pitch_coordinates_50_feet_x' = 'pitchData.coordinates.x0',
      'pitch_coordinates_50_feet_y' = 'pitchData.coordinates.y0',
      'pitch_coordinates_50_feet_z' = 'pitchData.coordinates.z0',
      'pitch_velocity_50_feet_x' = 'pitchData.coordinates.vX0',
      'pitch_velocity_50_feet_y' = 'pitchData.coordinates.vY0',
      'pitch_velocity_50_feet_z' = 'pitchData.coordinates.vZ0',
      'pitch_acceleration_50_feet_x' = 'pitchData.coordinates.aX',
      'pitch_acceleration_50_feet_y' = 'pitchData.coordinates.aY',
      'pitch_acceleration_50_feet_z' = 'pitchData.coordinates.aZ',
      'pitch_break_angle' = 'pitchData.breaks.breakAngle',
      'pitch_spin_rate' = 'pitchData.breaks.spinRate',
      'pitch_spin_direction' = 'pitchData.breaks.spinDirection',
      'batter_id' = 'matchup.batter.id',
      'batter_name' = 'matchup.batter.fullName',
      'description' = 'details.description',
      'event' = 'details.event',
      'away_score' = 'details.awayScore',
      'home_score' = 'details.homeScore',
      'is_scoring_play' = 'details.isScoringPlay',
      'play_reviewed' = 'details.hasReview',
      'play_code' = 'details.code',
      'result_rbis' = 'result.rbi',
      'ballColor' = 'details.ballColor',
      'is_in_play' = 'details.isInPlay',
      'is_ball' = 'details.isBall',
      'is_strike' = 'details.isStrike',
      'pre_balls' = 'count.balls.start',
      'pre_strikes' = 'count.strikes.start',
      'pre_outs' = 'count.outs.start',
      'sz_top' = 'pitchData.strikeZoneTop',
      'sz_bot' = 'pitchData.strikeZoneBottom',
      'pitch_coord_x_hit_pov' = 'pitchData.coordinates.x',
      'pitch_coord_y_hit_pov' = 'pitchData.coordinates.y',
      'bb_trajectory' = 'hitData.trajectory',
      'bb_location' = 'hitData.location',
      'hit_coord_x' = 'hitData.coordinates.coordX',
      'hit_coord_y' = 'hitData.coordinates.coordY',
      'hit_hardness' = 'hitData.hardness',
      'runner_going' = 'details.runnerGoing',
      'result_type' = 'result.type',
      'result_event' = 'result.event',
      'event_type' = 'result.eventType',
      'des' = 'result.description',
      'at_bat_number' = 'about.atBatIndex',
      'top_bot' = 'about.halfInning',
      'inning' = 'about.inning',
      'out_recorded' = 'about.hasOut',
      'post_balls' = 'count.balls.end',
      'post_strikes' = 'count.strikes.end',
      'post_outs' = 'count.outs.end',
      'batter_link' = 'matchup.batter.link',
      'batter_side' = 'matchup.batSide.code',
      'pitcher_link' = 'matchup.pitcher.link',
      'pitcher_handedness' = 'matchup.pitchHand.code',
      'batter_splits' = 'matchup.splits.batter',
      'pitcher_splits' = 'matchup.splits.pitcher',
      'men_on_base' = 'matchup.splits.menOnBase',
      'bb_result' = 'batted.ball.result',
      'last_pitch_of_ab' = 'last.pitch.of.ab',
      'pitch_type_code' = 'details.type.code',
      'pitch_type_description' = 'details.type.description',
      'pitch_velo' = 'pitchData.startSpeed',
      'zone_location' = 'pitchData.zone',
      'pitch_break_length' = 'pitchData.breaks.breakLength',
      'launch_speed' = 'hitData.launchSpeed',
      'launch_angle' = 'hitData.launchAngle',
      'hit_distance' = 'hitData.totalDistance',
      #'vertical_break_w_gravity' = 'pitchData.breaks.breakVertical',
      #'induced_vertical_break' = 'pitchData.breaks.breakVerticalInduced',
      #'horizontal_break' = 'pitchData.breaks.breakHorizontal',
      #'pitcher_disengagment_num' = 'details.disengagementNum',
      #'replaced_player_id' = 'replacedPlayer.id',
      #'replaced_player_link' = 'replacedPlayer.link',
      # 'violation_type' = 'details.violation.type',
      #'violation_description' = 'details.violation.description',
      #'violation_player_id' = 'details.violation.player.id',
      #'violation_player_name' = 'details.violation.player.fullName',
      'is_top_inning' = 'about.isTopInning',
      #'post_on_1B_id' = 'matchup.postOnFirst.id',
      #'post_on_2B_id'= 'matchup.postOnSecond.id',
      #'post_on_3B_id' = 'matchup.postOnThird.id',
      'release_extension' = 'pitchData.extension',
      'pfxX' = 'pitchData.coordinates.pfxX',
      'pfxZ' = 'pitchData.coordinates.pfxZ',
      'plate_x' = 'pitchData.coordinates.pX',
      'plate_z' = 'pitchData.coordinates.pZ'
    ) %>%
    mutate(
      pitcher_last_first = str_c(
        str_split(pitcher_name, ' ', n = 2, simplify = TRUE)[, 2],
        ', ',
        str_split(pitcher_name, ' ', n = 2, simplify = TRUE)[, 1]
      ),
      batter_last_first = str_c(
        str_split(batter_name, ' ', n = 2, simplify = TRUE)[, 2],
        ', ',
        str_split(batter_name, ' ', n = 2, simplify = TRUE)[, 1]
      ),
      at_bat_number = at_bat_number + 1
    )
  
  # if (game_pk == unique_game_pks[1]){
  #
  #   DBI::dbWriteTable(conn, 'mlb_stats_api_pbp',
  #                df %>%
  #                  as.data.frame(),
  #                append = FALSE,
  #                overwrite = TRUE,
  #                row.names = FALSE)
  # } else{
  DBI::dbWriteTable(
    conn,
    'mlb_stats_api_pbp',
    df %>%
      as.data.frame(),
    append = TRUE,
    overwrite = FALSE,
    row.names = FALSE
  )
  #}
  
  DBI::dbDisconnect(conn)
  
  return(df)
}

clusters <-
  makeCluster(6)
clusterExport(clusters, varlist = c('unique_game_pks'))
invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
stopCluster(clusters)
#stopCluster(clusters)

unique_game_pks <-
  dbGetQuery(conn,
             'SELECT Distinct game_pk, game_year from statcast where game_year = 2022')$game_pk
clusters <-
  makeCluster(6)
clusterExport(clusters, varlist = c('unique_game_pks'))
invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
stopCluster(clusters)
#stopCluster(clusters)

unique_game_pks <-
  dbGetQuery(conn,
             'SELECT Distinct game_pk, game_year from statcast where game_year = 2021')$game_pk
clusters <-
  makeCluster(6)
clusterExport(clusters, varlist = c('unique_game_pks'))
invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
stopCluster(clusters)

unique_game_pks <- dbGetQuery(conn,
             'SELECT Distinct game_pk, game_year from statcast where game_year = 2020')$game_pk
clusters <- makeCluster(6)
clusterExport(clusters, varlist = c('unique_game_pks'))
invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
stopCluster(clusters)

unique_game_pks <-
  dbGetQuery(conn,
             'SELECT Distinct game_pk, game_year from statcast where game_year = 2019')$game_pk
clusters <- makeCluster(6)
clusterExport(clusters, varlist = c('unique_game_pks'))
invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
stopCluster(clusters)


for (year in 2018:2015) {
  print(year)
  unique_game_pks <-
    dbGetQuery(
      conn,
      glue::glue(
        'SELECT Distinct game_pk, game_year from statcast where game_year = {year}'
      )
    )$game_pk
  clusters <- makeCluster(6)
  clusterExport(clusters, varlist = c('unique_game_pks'))
  invisible(pblapply(unique_game_pks, FUN = get_mlb_statsapi_pbp, cl = clusters))
  stopCluster(clusters)
  
}


#mlb_pbp_df <- map_df(2015:2024, get_mlb_statsapi_pbp, .progress = TRUE)
# mlb_pbp_df <- mlb_pbp_df %>%
#   select(-details.call.code, -details.call.description, -atBatIndex,
#          -about.startTime, -about.endTime, -about.isComplete, -about.hasReview, -about.isScoringPlay,
#          -matchup.batSide.description, -matchup.pitchHand.description, -result.isOut
#   ) %>%
#   rename(
#     'pitcher_id' = 'matchup.pitcher.id',
#     'pitcher_name' = 'matchup.pitcher.fullName',
#     'at_bat_number' = 'about.atBatIndex',
#     'pitch_end_speed' = 'pitchData.endSpeed',
#     'pitch_to_plate_time' = 'pitchData.plateTime',
#     'pitch_coordinates_50_feet_x' = 'pitchData.coordinates.x0',
#     'pitch_coordinates_50_feet_y' = 'pitchData.coordinates.y0',
#     'pitch_coordinates_50_feet_z' = 'pitchData.coordinates.z0',
#     'pitch_velocity_50_feet_x' = 'pitchData.coordinates.vX0',
#     'pitch_velocity_50_feet_y' = 'pitchData.coordinates.vY0',
#     'pitch_velocity_50_feet_z' = 'pitchData.coordinates.vZ0',
#     'pitch_acceleration_50_feet_x'= 'pitchData.coordinates.aX',
#     'pitch_acceleration_50_feet_y' = 'pitchData.coordinates.aY',
#     'pitch_acceleration_50_feet_z' = 'pitchData.coordinates.aZ',
#     'pitch_break_angle' = 'pitchData.breaks.breakAngle',
#     'pitch_spin_rate' = 'pitchData.breaks.spinRate',
#     'pitch_spin_direction' = 'pitchData.breaks.spinDirection',
#     'batter_id' = 'matchup.batter.id',
#     'batter_name' = 'matchup.batter.fullName',
#     'description' = 'details.description',
#     'event' = 'details.event',
#     'away_score' = 'details.awayScore',
#     'home_score' = 'details.homeScore',
#     'is_scoring_play' = 'details.isScoringPlay',
#     'play_reviewed' = 'details.hasReview',
#     'play_code' = 'details.code',
#     'result_rbis' = 'result.rbi',
#     'ballColor' = 'details.ballColor',
#     'is_in_play' = 'details.isInPlay',
#     'is_ball' = 'details.isBall',
#     'is_strike' = 'details.isStrike',
#     'pre_balls' = 'count.balls.start',
#     'pre_strikes' = 'count.strikes.start',
#     'pre_outs' = 'count.outs.start',
#     'sz_top' = 'pitchData.strikeZoneTop',
#     'sz_bot' = 'pitchData.strikeZoneBottom',
#     'pitch_coord_x_hit_pov' = 'pitchData.coordinates.x',
#     'pitch_coord_y_hit_pov' = 'pitchData.coordinates.y',
#     'bb_trajectory' = 'hitData.trajectory',
#     'bb_location' = 'hitData.location',
#     'hit_coord_x' = 'hitData.coordinates.coordX',
#     'hit_coord_y' = 'hitData.coordinates.coordY',
#     'hit_hardness' = 'hitData.hardness',
#     'runner_going' = 'details.runnerGoing',
#     'result_type' = 'result.type',
#     'result_event' = 'result.event',
#     'event_type' = 'result.eventType',
#     'des' = 'result.description',
#     'at_bat_number' = 'about.atBatIndex',
#     'top_bot' = 'about.halfInning',
#     'inning' = 'about.inning',
#     'out_recorded' = 'about.hasOut',
#     'post_balls' = 'count.balls.end',
#     'post_strikes' = 'count.strikes.end',
#     'post_outs' = 'count.outs.end',
#     'batter_link' = 'matchup.batter.link',
#     'batter_side' = 'matchup.batSide.code',
#     'pitcher_link' = 'matchup.pitcher.link',
#     'pitcher_handedness' = 'matchup.pitchHand.code',
#     'batter_splits' = 'matchup.splits.batter',
#     'pitcher_splits' = 'matchup.splits.pitcher',
#     'men_on_base' = 'matchup.splits.menOnBase',
#     'bb_result' = 'batted.ball.result',
#     'last_pitch_of_ab' = 'last.pitch.of.ab',
#     'pitch_type_code' = 'details.type.code',
#     'pitch_type_description' = 'details.type.description',
#     'pitch_velo' = 'pitchData.startSpeed',
#     'zone_location' = 'pitchData.zone',
#     'pitch_break_length' = 'pitchData.breaks.breakLength',
#     'launch_speed' = 'hitData.launchSpeed',
#     'launch_angle' = 'hitData.launchAngle',
#     'hit_distance' = 'hitData.totalDistance',
#     'vertical_break_w_gravity' = 'pitchData.breaks.breakVertical',
#     'induced_vertical_break' = 'pitchData.breaks.breakVerticalInduced',
#     'horizontal_break' = 'pitchData.breaks.breakHorizontal',
#     'pitcher_disengagment_num' = 'details.disengagementNum',
#     'replaced_player_id' = 'replacedPlayer.id',
#     'replaced_player_link' = 'replacedPlayer.link',
#     'violation_type' = 'details.violation.type',
#     'violation_description' = 'details.violation.description',
#     'violation_player_id' = 'details.violation.player.id',
#     'violation_player_name' = 'details.violation.player.fullName',
#     'is_top_inning' = 'about.isTopInning',
#     'post_on_1B_id' = 'matchup.postOnFirst.id',
#     'post_on_2B_id'= 'matchup.postOnSecond.id',
#     'post_on_3B_id' = 'matchup.postOnThird.id'
#   ) %>%
#   mutate(pitcher_last_first = str_c(str_split(pitcher_name, ' ', n = 2, simplify = TRUE)[,2], ', ', str_split(pitcher_name, ' ', n = 2, simplify = TRUE)[,1]),
#          batter_last_first = str_c(str_split(batter_name, ' ', n = 2, simplify = TRUE)[,2], ', ', str_split(batter_name, ' ', n = 2, simplify = TRUE)[,1]),
#          at_bat_number = at_bat_number + 1)
# tictoc::toc()

# tictoc::tic()
# clusters <- parallel::makeCluster(6);clusterExport(cl = clusters, varlist = 'unique_game_pks');invisible(pblapply(unique_game_pks, get_mlb_statsapi_pbp, cl = clusters))
# tictoc::toc()
#


#writing pitcher lookup table to SQL
dbWriteTable(
  conn,
  'pitcher_lookup_table',
  mlb_pbp_df %>%
    select(pitcher_id, pitcher_name, pitcher_last_first) %>%
    distinct(pitcher_id, .keep_all = TRUE) %>%
    as.data.frame(),
  append = FALSE,
  overwrite = TRUE,
  row.names = FALSE
)


#writing pitch data table to sql
dbWriteTable(
  conn,
  'pitch_characteristics_mlb_pbp',
  mlb_pbp_df %>%
    as.data.frame(),
  append = FALSE,
  row.names = FALSE,
  overwrite = TRUE
)
