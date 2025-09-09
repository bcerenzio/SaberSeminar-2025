#install.packages('arrow')
library(tidyverse)
library(arrow)
library(ggridges)

## Note: Code after making predictions on pitch location & batter gam models

# #### Joining Park Factors df to statcast ####
# updated_statcast_2024_2025 <- updated_statcast_2024_2025 %>% 
#   left_join(park_factors, by = c('home_team' = 'Team', 'game_year' = 'Year'))

glimpse(updated_statcast_2024_2025)

asfs <- updated_statcast_2024_2025_ordered %>% 
  select(release_pos_x, release_pos_y, release_pos_z, 
         game_pk, at_bat_number, pitch_number, plate_x, 
         pred_plate_x, plate_z, pred_plate_z) %>% 
  mutate(diff_x = abs(plate_x - pred_plate_x),
         diff_z = abs(plate_z - pred_plate_z))

asfs %>% 
  ggplot(aes(release_pos_z, diff_z, z = diff_z)) +
  # geom_point() + 
  stat_bin_2d(bins = 30) + 
  scale_x_continuous(limits = c(0,2))


#### Putting dataset in order ####
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025 %>% 
  arrange(game_pk, game_date, at_bat_number, pitch_number)

#remove(updated_statcast_2024_2025) #save space
#### Calculating Run Expecantancy by Count (by PA) ####
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(pitch_count = str_c(balls, strikes, sep = '-')) %>% 
  relocate(pitch_count, .after = 'strikes')

#finding wOBA constants via Fangraphs

woba_2025 <- .314
wobascale_2025 <- 1.237
woba_2024 <- .310
wobascale_2024 <- 1.242
woba_2023 <- .318
wobascale_2023 <- 1.204
woba_2022 <- .310
wobascale_2022 <- 1.259
woba_2021 <- .314
wobascale_2021 <- 1.209

run_exp_fun <- function(game_year, woba_val){
  if(game_year == 2021){
    run_exp <- (woba_val - woba_2021)/wobascale_2021
  } else if(game_year == 2022){
    run_exp <- (woba_val - woba_2022)/wobascale_2022
  } else if (game_year == 2023){
    run_exp <-  (woba_val - woba_2023)/wobascale_2023
  } else if (game_year == 2024){
    run_exp <- (woba_val - woba_2024)/wobascale_2024
  } else if (game_year == 2025){
    run_exp <- (woba_val - woba_2025)/wobascale_2025
  }
  
  return(run_exp)
}


#finding the final pitch in an AB and seeing if any runs scored
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  rowwise() %>% 
  mutate(run_exp = ifelse(pitch_number == max(pitch_number) & woba_denom == 1, run_exp_fun(game_year, woba_value), NA)) %>% 
  ungroup()


### filling in NA values
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  fill(run_exp, .direction = 'up')

# finding run expectancy in each count by year
run_exp_matrix <- updated_statcast_2024_2025_ordered %>% 
  filter(balls <= 3, strikes <= 2) %>% #removing any data errors
  group_by(game_year, pitch_count) %>% 
  reframe(run_exp = mean(run_exp, na.rm = TRUE),
          n = n())

# scaling to a 0-0 count
run_exp_matrix <- run_exp_matrix %>% 
  group_by(game_year) %>% 
  mutate(first_pitch_run_exp = ifelse(pitch_count == '0-0', run_exp, NA)) %>% 
  fill(first_pitch_run_exp, .direction = 'down') %>% 
  ungroup() %>% 
  mutate(count_delta_run_exp = run_exp - first_pitch_run_exp) %>% 
  select(-first_pitch_run_exp)


# joining delta run expectancies to current count
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  left_join(run_exp_matrix %>% select(-run_exp, -n), by = c('game_year','pitch_count'))

# moving count_delta_run_exp next to pitch_count
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  relocate(count_delta_run_exp, .after = 'pitch_count')

# removing 4 ball and 3 strike "counts"
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  filter(strikes <= 2, balls <= 3)

#### Finding Heart, Shadow, Chase, & Waste portions of the zone ####

# converting plate location to inches
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   mutate(plate_x_in = plate_x*12,
#          plate_z_in = plate_z * 12) %>% 
#   relocate(plate_x_in, .after = plate_x) %>% 
#   relocate(plate_z_in, .after = plate_z)

# converting top and bottom of strikezone coordinates to inches
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(sz_top_in = sz_top*12,
         sz_bot_in = sz_bot * 12) %>% 
  relocate(sz_top_in, .after = sz_top) %>% 
  relocate(sz_bot_in, .after = sz_bot)


#### Renaming xwOBACON column ####
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  rename('xwOBA' = estimated_woba_using_speedangle)

#### Removing any ABS where pitches didn't track
original_num_rows <- nrow(updated_statcast_2024_2025_ordered)
# removing NA pitch locations
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  filter(!any(is.na(plate_x))) %>% 
  ungroup()

# ensuring no plate_z NAs are present
any(is.na(updated_statcast_2024_2025_ordered$plate_z)) #FALSE

unique(updated_statcast_2024_2025_ordered$pitch_type)

# removing PAs with NA pitch types, pitchouts, "Other" pitch type, & Eephuses
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  filter(!any(is.na(pitch_type)), !any(pitch_type %in% c('EP','FA','PO'))) %>% 
  ungroup()

# combining slow curves with curvballs
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(pitch_type = ifelse(pitch_type == 'CS', 'CU', pitch_type))

# how much data was lost
cat('% of Data Remaining: ', nrow(updated_statcast_2024_2025_ordered)/original_num_rows)

summary(updated_statcast_2024_2025_ordered) # checking for any more NAs for imputation
# NAs by variable:
# spin axis: 11899
# release_pos_y: 175
# release_extension: 3757
# effective_speed: 4167
# release_spin_rate: 11903
# pfx_x: 10
#pfx_z
# release_speed: 2
# release_pos_x: 175
# release_pos_z: 175
#armangle: 38900

#imputing means for pitchers based on year, and pitch type
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   group_by(pitcher, pitch_type, game_year) %>% 
#   mutate(across(c(spin_axis, release_speed, effective_speed, release_pos_y, release_spin_rate,
#                   pfx_x, pfx_z,release_pos_x, release_pos_z, arm_angle), ~replace_na(., mean(., na.rm = TRUE)))) %>% 
#   ungroup()

# imputing NA release extenstion through release_pos_y
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   mutate(release_extension = ifelse(is.na(release_extension), 60.5-release_pos_y, release_extension))

summary(updated_statcast_2024_2025_ordered) # still some NAs in the previous values (minimal though)


any(is.na(updated_statcast_2024_2025_ordered$release_extension)) #FALSE

#replacing remaining of NA values with mean values across 2021-2024
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   group_by(pitcher, pitch_type) %>% 
#   mutate(across(c(spin_axis, release_speed, effective_speed, release_pos_y, release_spin_rate,
#                   pfx_x, release_pos_x, release_pos_z, arm_angle), ~replace_na(., mean(., na.rm = TRUE)))) %>% 
#   ungroup()

summary(updated_statcast_2024_2025_ordered) # still some NAs in the previous values
# NA Values
# release_spin_rate = 1
# spin_axis = 1
#arm_angle = 45
### All are only relevant to the current pitch, remove After finding lagged/lead values ####

#calculating time to plate
mph_to_ftps <- 1.466666667
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(time_to_plate = -release_pos_y/(vyR)) %>% 
  relocate(time_to_plate, .after = 'release_speed')

summary(updated_statcast_2024_2025_ordered$time_to_plate)

#finding accel_x & accel_z (in/s^2)
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(accel_x = pfx_x/(time_to_plate^2),
         accel_z = pfx_z/(time_to_plate^2)
  ) %>% 
  mutate(accel_x = accel_x*12,
         accel_z = accel_z*12) %>% 
  relocate(accel_x, .after = 'pfx_x') %>% 
  relocate(accel_z, .after = 'pfx_z')

#finding release position in inches
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(release_pos_x_in = release_pos_x*12,
         release_pos_z_in = release_pos_z*12,
         release_pos_y_in = release_pos_y*12,
         release_extension_in = release_extension*12) %>% 
  relocate(release_pos_x_in, .after = 'release_pos_x') %>% 
  relocate(release_pos_z_in, .after = 'release_pos_z') %>% 
  relocate(release_pos_y_in, .after = 'release_pos_y') %>% 
  relocate(release_extension_in, .after = 'release_extension')

# finding information from previous pitch
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  mutate(previous_pitch = ifelse(is.na(lag(pitch_type)), 'first_pitch', lag(pitch_type)),
         pitch_velo_diff = ifelse(is.na(lag(release_speed)), 0, release_speed -lag(release_speed)),
         plate_x_diff = ifelse(is.na(lag(plate_x)), 0, plate_x - lag(plate_x)),
         plate_z_diff = ifelse(is.na(lag(plate_z)), 0, plate_z - lag(plate_z)),
         accel_x_diff = ifelse(is.na(lag(accel_x)), 0, accel_x - lag(accel_x)),
         accel_z_diff = ifelse(is.na(lag(accel_z)), 0, accel_z - lag(accel_z)),
         ball_pos_x_0.1_diff = ifelse(is.na(lag(ball_pos_x_0.1)), 0, ball_pos_x_0.1 - lag(ball_pos_x_0.1)),
         ball_pos_y_0.1_diff = ifelse(is.na(lag(ball_pos_y_0.1)), 0, ball_pos_y_0.1 - lag(ball_pos_y_0.1)),
         ball_pos_z_0.1_diff = ifelse(is.na(lag(ball_pos_z_0.1)), 0, ball_pos_z_0.1 - lag(ball_pos_z_0.1))) %>% 
  ungroup() %>% 
  relocate(previous_pitch, .after = 'pitch_type') %>% 
  relocate(pitch_velo_diff, .after = 'release_speed') %>% 
  relocate(plate_x_diff, .after = 'plate_x') %>% 
  relocate(plate_z_diff, .after = 'plate_z') %>% 
  relocate(accel_x_diff, .after = 'accel_x') %>% 
  relocate(accel_z_diff, .after = 'accel_z') %>% 
  relocate(ball_pos_x_0.1_diff, .after = 'ball_pos_x_0.1') %>% 
  relocate(ball_pos_y_0.1_diff, .after = 'ball_pos_y_0.1') %>% 
  relocate(ball_pos_z_0.1_diff, .after = 'ball_pos_z_0.1')

# ball_pos_0.1_xy euclidean distance
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(
    ball_pos_xyz_0.1_diff = sqrt((ball_pos_x_0.1_diff)^2 + (ball_pos_y_0.1_diff)^2 + (ball_pos_z_0.1_diff)^2),
    plate_xz_diff = sqrt((plate_x_diff)^2 + (plate_z_diff)^2)
  )


## Dummy for 2 and 3 ball counts 
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  mutate(two_strike = ifelse(strikes == 2, 1, 0)) %>%
  mutate(three_balls = ifelse(balls == 3, 1, 0))

## If pitcher and batter handedness is the same
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  mutate(same_hand = ifelse(p_throws == bat_side, 1, 0))

# new_count column (hypothetical count if batter took the pitch)
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  mutate(new_count = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) & between(pred_plate_z, sz_bot_in, sz_top_in),
    paste(balls, strikes + 1, sep = "-"),
    paste(balls + 1, strikes, sep = "-")
  )) %>% 
  relocate(new_count, .after = 'pitch_count')

# post-count
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  group_by(game_pk, at_bat_number) %>% 
  mutate(post_pitch_count = lead(pitch_count)) %>%
  ungroup() %>% 
  relocate(post_pitch_count, .after = 'new_count')

# filling in NAs in post count (ie where the at bat ended)
# finding out what the count would've been if the at-bat continued
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(post_pitch_count = ifelse(is.na(post_pitch_count),
                                   case_when(
                                     events %in% c('walk','hit_by_pitch') ~ str_c(balls + 1, strikes, sep = '-'),
                                     !(events %in% c('walk','hit_by_pitch','truncated_pa')) ~ str_c(balls, strikes+1, sep = '-'),
                                     events == 'truncated_pa' & zone > 9 ~ str_c(balls + 1, strikes, sep = '-'),
                                     events == 'truncated_pa' & zone <= 9 ~ str_c(balls, strikes+1, sep = '-')
                                   ), post_pitch_count))

# wOBA constants for wBB
woba_BB_2025 <- .692
woba_BB_2024 <- .689
woba_BB_2023 <- .696
woba_BB_2022 <- .689
woba_BB_2021 <- .692

#joining delta run expectancies to hypothetical and next pitch count
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  left_join(run_exp_matrix %>% reframe(game_year, pitch_count, new_count_delta_run_exp = count_delta_run_exp), 
            by = c('game_year','new_count' = 'pitch_count')) %>% 
  left_join(run_exp_matrix %>% reframe(game_year, pitch_count, post_count_delta_run_exp = count_delta_run_exp), 
            by = c('game_year','post_pitch_count' = 'pitch_count'))

#relocating them next to each other
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  relocate(new_count_delta_run_exp, .after = 'new_count') %>% 
  relocate(post_count_delta_run_exp, .after = 'post_pitch_count')

# adding run value for walk events
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(new_count_delta_run_exp = ifelse(
    is.na(new_count_delta_run_exp) & new_count %in% c('4-0','4-1','4-2'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,woba_BB_2021),
      game_year == 2022 ~ run_exp_fun(2022,woba_BB_2022),
      game_year == 2023 ~ run_exp_fun(2023,woba_BB_2023),
      game_year == 2024 ~ run_exp_fun(2024,woba_BB_2024),
      game_year == 2025 ~ run_exp_fun(2025,woba_BB_2025),
    ), new_count_delta_run_exp
  ),
  post_count_delta_run_exp = ifelse(
    is.na(post_count_delta_run_exp) & post_pitch_count %in% c('4-0','4-1','4-2'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,woba_BB_2021),
      game_year == 2022 ~ run_exp_fun(2022,woba_BB_2022),
      game_year == 2023 ~ run_exp_fun(2023,woba_BB_2023),
      game_year == 2024 ~ run_exp_fun(2024,woba_BB_2024),
      game_year == 2025 ~ run_exp_fun(2025,woba_BB_2025),
    ), post_count_delta_run_exp
  )  
  )

# adding run value for a strikeout
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(new_count_delta_run_exp = ifelse(
    is.na(new_count_delta_run_exp) & new_count %in% c('0-3','1-3','2-3','3-3'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,0),
      game_year == 2022 ~ run_exp_fun(2022,0),
      game_year == 2023 ~ run_exp_fun(2023,0),
      game_year == 2024 ~ run_exp_fun(2024,0),
      game_year == 2025 ~ run_exp_fun(2025,0),
    ), new_count_delta_run_exp
  ),
  post_count_delta_run_exp = ifelse(
    is.na(post_count_delta_run_exp) & post_pitch_count %in% c('0-3','1-3','2-3','3-3'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,0),
      game_year == 2022 ~ run_exp_fun(2022,0),
      game_year == 2023 ~ run_exp_fun(2023,0),
      game_year == 2024 ~ run_exp_fun(2024,0),
      game_year == 2025 ~ run_exp_fun(2025,0),
    ), post_count_delta_run_exp
  )  
  )

#identifying swings
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  mutate(swing = ifelse(description %in% c('foul','foul_tip','hit_into_play','swinging_strike',
                                           'swinging_strike_blocked','foul_bunt','missed_bunt','bunt_foul_tip'), 1,0)) %>% 
         #zswing = ifelse(zone <= 9, swing/(zone<= 9), NA),
         #chase = ifelse(zone > 9, swing/(zone > 9), NA)) %>%
  relocate(swing, .after = batter) #%>%
  #relocate(zswing, .after = swing) %>%
  #relocate(chase, .after = zswing)

# batter swing%
# batter_swing <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name) %>% 
#   reframe(swing_pct = mean(swing, na.rm = TRUE),
#           zswing_pct = mean(zswing, na.rm = TRUE),
#           chase_pct = mean(chase, na.rm = TRUE),
#           pitches = n()) %>% 
#   mutate(across(c('swing_pct','zswing_pct'), 
#                 ~ifelse(pitches <= 100, quantile(., probs = 0.1, na.rm = TRUE), .))) %>% 
#   mutate(chase_pct = ifelse(pitches <= 100, quantile(chase_pct, probs = 0.9, na.rm = TRUE), chase_pct))

# swing% by pitch 
# batter_swing_pitch <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name, pitch_type) %>% 
#   reframe(pitch_swing_pct = mean(swing, na.rm = TRUE),
#           pitch_zswing_pct = mean(zswing, na.rm = TRUE),
#           pitch_chase_pct = mean(chase, na.rm = TRUE),
#           pitches = n()) %>% 
#   group_by(pitch_type) %>% 
#   mutate(across(c('pitch_swing_pct','pitch_zswing_pct'), 
#                 ~ifelse(pitches <= 40, quantile(., probs = 0.1, na.rm = TRUE), .))) %>% 
#   mutate(pitch_chase_pct = ifelse(pitches <= 40, quantile(pitch_chase_pct, probs = 0.9, na.rm = TRUE), pitch_chase_pct)) %>% 
#   ungroup()

#summary(batter_swing_pitch$pitches)

# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   left_join(batter_swing_pitch %>% select(-'player_name', -'pitches'), by = c('batter','pitch_type')) %>% 
#   left_join(batter_swing %>% select(-'player_name', -'pitches'), by = c('batter'))

#identifying contanct
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>%
  mutate(whiff = case_when(
    swing == 1 & description %in% c('foul', 'foul_tip', 'hit_into_play',
                                    'foul_bunt','bunt_foul_tip') ~ 0,
    swing == 1 & !(description %in% c('foul', 'foul_tip', 'hit_into_play',
                                      'foul_bunt','bunt_foul_tip')) ~ 1,
    .default = NA
  )) %>%
  relocate(whiff, .after = 'swing')

# batter_contact_pct <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name) %>% 
#   reframe(contact_pct = mean(contact, na.rm = TRUE),
#           pitches = n()) %>% 
#   mutate(contact_pct = ifelse(pitches <= 100, quantile(contact_pct, probs = 0.1, na.rm = TRUE), contact_pct))
# 
# batter_contact_pct_pitch <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name, pitch_type) %>% 
#   reframe(pitch_contact_pct = mean(contact, na.rm = TRUE),
#           pitches = n()) %>% 
#   group_by(pitch_type) %>% 
#   mutate(pitch_contact_pct = ifelse(pitches <= 40, quantile(pitch_contact_pct, probs = 0.1, na.rm = TRUE), pitch_contact_pct))
# 
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   left_join(batter_contact_pct %>% select(-pitches, -player_name), by = 'batter') %>% 
#   left_join(batter_contact_pct_pitch %>% select(-pitches, -player_name), by = c('batter','pitch_type'))
# 
# 
# # batter season woba
# batter_woba <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name) %>% 
#   reframe(woba = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
#           pa = sum(woba_denom, na.rm = TRUE))
# 
# summary(batter_woba$pa)
# 
# # batter woba imputation
# batter_woba <- batter_woba %>% 
#   mutate(woba = ifelse(pa <= 50, quantile(woba, probs = 0.1), woba))
# 
# #batter woba by pitch type
# batter_woba_pitchtype <- updated_statcast_2024_2025_ordered %>% 
#   group_by(batter, player_name, pitch_type) %>% 
#   reframe(woba_pitch = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
#           pa = sum(woba_denom, na.rm = TRUE))
# 
# summary(batter_woba_pitchtype$pa)
# 
# batter_woba_pitchtype <- batter_woba_pitchtype %>% 
#   group_by(pitch_type) %>% 
#   mutate(woba_pitch = ifelse(is.infinite(woba_pitch), quantile(woba_pitch, probs = 0.6, na.rm= TRUE), woba_pitch),
#          woba_pitch = ifelse(pa < 20, quantile(woba_pitch, probs = 0.1,  na.rm = TRUE), woba_pitch)) %>% 
#   ungroup()
# 
# updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
#   left_join(batter_woba %>% reframe(batter, season_woba = woba), by = 'batter') %>% 
#   left_join(batter_woba_pitchtype %>% reframe(batter, pitch_type, season_woba_pitchtype = woba_pitch), by = c('batter','pitch_type'))


#### Identifying Attack Zones Code ####
#adding  left and right positions for attack zones
heart_left <- -6.7; heart_right <- 6.7
shadow_farleft <- -13.3; shadow_left <- -6.7
shadow_farright <- 13.3; shadow_right <- 6.7
chase_farleft <- (-13.3-6.6); chase_left <- -13.3
chase_farright <- (13.3+6.6);chase_right <- 13.3
waste_left <- -13.3-6.6; waste_right <- 13.3+6.6


# finding middle of vertical strikezone
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(sz_mid_in = (sz_top_in + sz_bot_in)/2) %>% 
  relocate(sz_mid_in, .after = sz_top_in)

# finding "percentages: in pitch location vs strike zone from attack zones
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(pred_plate_z_pct = ifelse(pred_plate_z <= sz_mid_in,(pred_plate_z - sz_mid_in)/(sz_mid_in-sz_bot_in)*100, (pred_plate_z - sz_mid_in)/(sz_top_in-sz_mid_in)*100)) %>% 
  relocate(pred_plate_z_pct, .after = pred_plate_z) %>% 
  relocate(sz_mid_in, .after = pred_plate_z_pct)

# adding percentage positions for attack zones
heart_pct_bot <- -67; heart_pct_top <- 67
shadow_pct_farbot <- -133; shadow_pct_bot <- -67
shadow_pct_fartop <- 133; shadow_pct_top <- 67
chase_pct_farbot <- -200; chase_pct_bot <- -133
chase_pct_fartop <- 200; chase_pct_top <- 133
waste_bot <- -200; waste_top <- 200

# finding attack zones
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(attack_zone = case_when(
    (pred_plate_x <= heart_right & pred_plate_x >= heart_left) &
      (pred_plate_z_pct <= heart_pct_top & pred_plate_z_pct >= heart_pct_bot) ~ 'Heart',
    (pred_plate_x <= shadow_farright & pred_plate_x > shadow_right & 
       pred_plate_z_pct <= shadow_pct_fartop & pred_plate_z_pct >= shadow_pct_farbot) |
      (pred_plate_x <= shadow_farright & pred_plate_x >= shadow_farleft &
         pred_plate_z_pct <= shadow_pct_fartop & pred_plate_z_pct > shadow_pct_top) |
      (pred_plate_x >= shadow_farleft & pred_plate_x < shadow_left & 
         pred_plate_z_pct <= shadow_pct_fartop & pred_plate_z_pct >= shadow_pct_farbot) |
      (pred_plate_x <= shadow_farright & pred_plate_x >= shadow_farleft &
         pred_plate_z_pct >= shadow_pct_farbot & pred_plate_z_pct < shadow_pct_bot)~ 'Shadow',
    pred_plate_x < waste_left | pred_plate_x > waste_right |
      pred_plate_z_pct > waste_top | pred_plate_z_pct < waste_bot ~ 'Waste',
    .default = 'Chase'
  )) %>% 
  relocate(attack_zone, .after = 'pred_plate_z_pct')

#removing final nas
# NA Values
# release_spin_rate = 1
# spin_axis = 1
#arm_angle = 45
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  drop_na(pred_plate_x, pred_plate_z)

# Percentage of Data Remaining  
cat('% of Data Remaining: ', round((nrow(updated_statcast_2024_2025_ordered)/original_num_rows)*100,1), '%', sep = '')


#### Delta Run Change Difference ####
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(run_exp_change = ifelse(swing == 1, post_count_delta_run_exp - new_count_delta_run_exp, post_count_delta_run_exp - count_delta_run_exp)) %>% 
  relocate(run_exp_change, .after = 'post_count_delta_run_exp')

# Getting player Average xwoba
xwobacon <- updated_statcast_2024_2025_ordered %>% 
  filter(woba_denom == 1, description == 'hit_into_play') %>% 
  group_by(game_year, batter) %>% 
  reframe(player_average_xwobacon = mean(xwOBA, na.rm = TRUE))

# joining it back to statcast dataframe
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  left_join(xwobacon, by = c('game_year', 'batter'))

#### Note: All Code from here on out will have needed to have metrics from models built ####

updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(in_zone = ifelse(
    between(pred_plate_x, -0.83*12, 0.83*12) &
      between(pred_plate_z, sz_bot_in, sz_top_in),
    1,
    0
  ))

# adding manual imputations from formula
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(run_exp_change = case_when(
    #2 Strike Cases
    two_strike == 1 & attack_zone == 'Shadow' & swing == 1 & three_balls == 0 ~ 0,
    two_strike == 1 & attack_zone == 'Shadow' & swing == 0 & three_balls == 0 ~ run_exp_change*1.2,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & three_balls == 0 ~ run_exp_change*1.5,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == "FF" & three_balls == 0 ~ run_exp_change*2,
    # 3 Ball Cases
    three_balls == 1 & attack_zone == 'Shadow' & swing == 1  & two_strike == 0 ~ run_exp_change * 1.2,
    three_balls == 1 & attack_zone == 'Shadow' & swing == 0 & two_strike == 0 ~ run_exp_change * 0.75,
    three_balls == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == 'FF' & two_strike == 0 ~ run_exp_change*1.2,
    .default = run_exp_change
  ))

season_xwobacon <- updated_statcast_2024_2025_ordered %>% 
  group_by(game_year, batter) %>% 
  reframe(
    avg_xwobacon = mean(xwOBA, na.rm = TRUE)
  )

updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  left_join(season_xwobacon, by = c('game_year', 'batter'))


# adding formulas 
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(contact_metric = ifelse(swing == 1,
                                 run_exp_change * (1- swing_prob) * whiff_prob, run_exp_change * swing_prob * (1-whiff_prob)),
         xdamage_metric = ifelse(swing == 1, xwobacon- avg_xwobacon, avg_xwobacon - xwobacon))


updated_statcast_2024_2025_ordered %>% 
  ggplot(aes(contact_metric)) + 
  geom_density(adjust = 5)

summary(updated_statcast_2024_2025_ordered$contact_metric)

summary(scale(updated_statcast_2024_2025_ordered$contact_metric)[,1])

updated_statcast_2024_2025_ordered %>% 
  ggplot(aes(xdamage_metric)) + 
  geom_density()

summary(scale(updated_statcast_2024_2025_ordered$xdamage_metric)[,1])

# adding formulas for xdamage metric
updated_statcast_2024_2025_ordered <- updated_statcast_2024_2025_ordered %>% 
  mutate(
    xdamage_metric_weighted = case_when(
    #2 Strike Cases
    two_strike == 1 & attack_zone == 'Shadow' & swing == 1 & three_balls == 0 ~ 0,
    two_strike == 1 & attack_zone == 'Shadow' & swing == 0  & three_balls == 0~ xdamage_metric*1.2,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & three_balls == 0 ~ xdamage_metric*1.5,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == "FF" & three_balls == 0 ~ xdamage_metric*2,
    # 3 Ball Cases
    three_balls == 1 & attack_zone == 'Shadow' & swing == 1 & two_strike == 0 ~ xdamage_metric * 1.2,
    three_balls == 1 & attack_zone == 'Shadow' & swing == 0  & two_strike == 0~ xdamage_metric * 0.75,
    three_balls == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == 'FF' & two_strike == 0 ~ xdamage_metric*1.2,
    .default = xdamage_metric
  ))


# getting season stats
season_metrics <- updated_statcast_2024_2025_ordered %>% 
  group_by(batter, batter_name, game_year) %>% 
  reframe(contact_metric = mean(contact_metric, na.rm = TRUE),
          xdamage_metric = mean(xdamage_metric_weighted, na.rm = TRUE), 
          pa = sum(woba_denom == 1, na.rm = TRUE)) %>% 
  mutate(
    qualified = ifelse(
      (game_year == 2024 & pa > 150) |
        (game_year == 2025 & pa > 100),
      TRUE,
      FALSE
    )
  ) %>% 
  filter(qualified) %>% 
  select(-qualified)

#scaling metrics
season_metrics <- season_metrics %>% 
  group_by(game_year) %>% 
  mutate(contact_metric_scaled = scale(contact_metric)[,1],
         xdamage_metric_scaled = scale(xdamage_metric)[,1]) %>% 
  ungroup()

# converting to scouting scale
season_metrics <- season_metrics %>% 
  mutate(contact_metric_scouting_scale = contact_metric_scaled*10 + 100,
         xdamage_metric_scouting_scale = xdamage_metric_scaled*10 + 100) %>% 
  arrange(game_year, contact_metric_scouting_scale)

# FINAL METRIC
final_metric <- season_metrics %>% 
  mutate(baez_arraez_scale = (contact_metric_scouting_scale + xdamage_metric_scouting_scale)/2) %>% 
  dplyr::select(batter, batter_name, game_year, pa, contact_metric_scouting_scale, xdamage_metric_scouting_scale, baez_arraez_scale) %>% 
  arrange(desc(baez_arraez_scale))

summary(final_metric$baez_arraez_scale)

summary(final_metric$xdamage_metric_scouting_scale)

summary(final_metric$contact_metric_scouting_scale)

final_metric %>% 
  ggplot(aes(baez_arraez_scale)) +
  geom_density()

final_metric %>% 
  ggplot(aes(xdamage_metric_scouting_scale)) +
  geom_density()

final_metric %>% 
  ggplot(aes(contact_metric_scouting_scale)) +
  geom_density()

pivot_longer(final_metric %>% 
               rename('xDamage+' = 'xdamage_metric_scouting_scale',
                      'Contact+' = 'contact_metric_scouting_scale'),
cols = c('xDamage+', 'Contact+'),
names_to = 'Stat',
values_to = 'value'
) %>% ggplot(aes(x = value, y = Stat, fill = Stat)) +
  geom_density_ridges(alpha = 0.7) +
  scale_x_continuous(limits = c(50, 150), breaks = seq(50,150, by = 10)) + 
  theme_classic() +
  guides(fill = 'none') +
  labs(x = 'Metric Value',
       title = 'Distribution Of Both xDamage+ & Contact+')


write_csv(final_metric, 'baez_arraez_scale.csv')

arraez <- final_metric %>% filter(batter_name == 'Arraez, Luis')

write_csv(arraez, 'baez_arraez_scale_arraez.csv')

baez <- final_metric %>% filter(player_name == 'Báez, Javier')

write_csv(baez, 'baez_arraez_scale_baez.csv')


#testing stickiness
lagged_baez_arraez_scale <- final_metric %>% 
  arrange(batter,game_year)

lagged_baez_arraez_scale <- final_metric %>% 
  group_by(batter) %>% 
  arrange(game_year) %>% 
  mutate(lagged_baez_arraez_scale = lag(baez_arraez_scale),
         lagged_xdamage = lag(xdamage_metric_scouting_scale),
         lagged_contact = lag(contact_metric_scouting_scale))

lm_model <- lm(baez_arraez_scale ~ lagged_baez_arraez_scale, data = lagged_baez_arraez_scale %>% drop_na(lagged_baez_arraez_scale))

summary_lm <- summary(lm_model) # p = 2e-16

lagged_baez_arraez_scale %>% drop_na(lagged_baez_arraez_scale) %>% 
  # mutate(baez_arraez_scale = case_when(
  #   baez_arraez_scale > 80 ~ 80,
  #   baez_arraez_scale < 20 ~ 20,
  #   .default = baez_arraez_scale
  # ),
  # lagged_baez_arraez_scale = case_when(
  #   lagged_baez_arraez_scale > 80 ~ 80,
  #   lagged_baez_arraez_scale < 20 ~ 20,
  #   .default = lagged_baez_arraez_scale
  # )) %>% 
  ggplot(aes(baez_arraez_scale, lagged_baez_arraez_scale, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                      breaks = seq(70,130, by = 10)) +
  # scale_y_continuous(limits = c(20,80),
  #                    breaks = seq(20, 80, by = 10)) +
  annotate('text', x = 71, y = 80, label = paste('r = ', round(corrr::correlate(lagged_baez_arraez_scale$lagged_baez_arraez_scale, lagged_baez_arraez_scale$baez_arraez_scale) %>% 
                                                                 pull(x), 3), sep = '')) +
  #annotate('text', x = 71.5, y = 23, label = 'p = 2e-16***') +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  theme_classic() +
  labs(title = 'Baez-Arraez Score Stickiness (Min. 150 PA)',
       size = 'PA',
       y = 'Previous Season Baez-Arraez Score',
       x = 'Current Season Baez-Arraez Score')

lagged_baez_arraez_scale %>% drop_na(lagged_contact) %>% 
  ggplot(aes(contact_metric_scouting_scale, lagged_contact, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  scale_y_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  annotate('text', x = 128, y = 80, label = paste('r = ', round(corrr::correlate(lagged_baez_arraez_scale$lagged_contact, lagged_baez_arraez_scale$contact_metric_scouting_scale) %>% 
                                                                 pull(x), 3), sep = '')) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  theme_classic() +
  labs(title = 'Contact+ Stickiness',
       size = 'PA',
       y = '2024 Contact+',
       x = '2025 Contact+')

lagged_baez_arraez_scale %>% drop_na(lagged_xdamage) %>% 
  ggplot(aes(xdamage_metric_scouting_scale, lagged_xdamage, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  scale_y_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  annotate('text', x = 128, y = 80, label = paste('r = ', round(corrr::correlate(lagged_baez_arraez_scale$lagged_xdamage, lagged_baez_arraez_scale$xdamage_metric_scouting_scale) %>% 
                                                                  pull(x), 3), sep = '')) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  theme_classic() +
  labs(title = 'xDamage+ Stickiness',
       size = 'PA',
       y = '2024 xDamage+',
       x = '2025 xDamage+')


# Getting Player wOBA
season_wOBA_OBP <- updated_statcast_2024_2025_ordered %>% 
  mutate(correct_swing_rate = ifelse(
    (in_zone == 1 & swing == 1) |
      (in_zone == 0 & swing == 0),
    1,
    0
  )) %>% 
  group_by(batter, game_year) %>% 
  reframe(wOBA = sum(woba_value, na.rm =TRUE)/sum(woba_denom, na.rm = TRUE),
          xwOBA = sum(xwOBA, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
          AVG = sum(events %in% c('single', 'double', 'triple', 'home_run'), na.rm = TRUE)/
            sum(!(events %in% c('sac_bunt', 'truncated_pa', 'hit_by_pitch', 'walk',
                              'sac_fly', 'catcher_interf', 'sac_fly_double_play', NA)), na.rm = TRUE),
          OBP = sum(events %in% c('walk', 'single','double',
                                  'triple','hit_by_pitch', 'home_run'), na.rm = TRUE)/
            sum(woba_denom, na.rm = TRUE),
          walk_rate = sum(events == 'walk', na.rm = TRUE)/sum(woba_denom == 1, na.rm = TRUE),
          k_rate = sum(events %in% c('strikeout', 'strikeout_double_play'), na.rm = TRUE)/sum(woba_denom == 1, na.rm = TRUE),
          chase_rate = sum(in_zone == 0 & swing == 1, na.rm = TRUE)/sum(in_zone == 0),
          wastechasezone_swingrate = sum(swing == 1 & attack_zone %in% c('Waste', 'Chase'), na.rm = TRUE)/sum(attack_zone %in% c('Waste', 'Chase'), na.rm = TRUE),
          heart_swingrate = sum(swing == 1 & attack_zone %in% c('Heart'), na.rm = TRUE)/sum(attack_zone %in% c('Heart'), na.rm = TRUE),
          zone_swing_rate = sum(in_zone == 1 & swing == 1, na.rm = TRUE)/sum(in_zone == 1),
          zone_take_rate = sum(in_zone == 1 & swing == 0, na.rm = TRUE)/sum(in_zone == 1),
          contact_rate = sum(whiff == 0, na.rm = TRUE)/sum(swing == 1, na.rm = TRUE),
          correct_swing_rate = mean(correct_swing_rate, na.rm = TRUE),
          squared_up_rate = sum(ifelse(launch_speed/(bat_speed*1.23 + release_speed*0.23) >= 0.8, 1,0), na.rm = TRUE)/sum(swing == 1, na.rm = TRUE),
          pitches_per_ab = n()/sum(woba_denom, na.rm = TRUE),
          pa = sum(woba_denom, na.rm = TRUE)) %>% 
  mutate(
    qualified = ifelse(
      (game_year == 2024 & pa > 150) |
        (game_year == 2025 & pa > 100),
      TRUE,
      FALSE
    ),
    walk_minus_k = walk_rate - k_rate
  ) %>% 
  filter(qualified) %>% 
  select(-qualified)

#joining on final metrics
metric_woba_df <- final_metric %>% 
  left_join(season_wOBA_OBP, by = c('batter','game_year', 'pa')) #%>% 
  # mutate(
  #   baez_arraez_scale = case_when(
  #     baez_arraez_scale > 80 ~ 80,
  #     baez_arraez_scale < 20 ~ 20,
  #     .default = baez_arraez_scale
  #   )
  #)


woba_lm <- lm(wOBA ~ baez_arraez_scale, data = metric_woba_df)
summary(woba_lm)


metric_woba_df %>% 
  ggplot(aes(baez_arraez_scale, wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Yearly wOBA vs Baez-Arraez Scale',
    x = 'Baez-Arraez Scale',
    size = 'PA') +
  annotate('text', x = 75, y = 0.42, label = paste0('r = ', round(corrr::correlate(metric_woba_df$wOBA, metric_woba_df$baez_arraez_scale)$x,3)))

metric_woba_df %>% 
  ggplot(aes(xdamage_metric_scouting_scale, wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  theme_classic() +
  labs(
    title = 'Yearly wOBA vs xDamage Scale',
    x = 'xDamage',
    size = 'PA') +
  geom_smooth(aes(xdamage_metric_scouting_scale, wOBA), color = 'red', se = FALSE, method = 'lm', inherit.aes = FALSE) +
  annotate('text', x = 75, y = 0.45, label = paste0('r = ', round(corrr::correlate(metric_woba_df$xdamage_metric_scouting_scale, metric_woba_df$wOBA)$x,3)))

metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, OBP, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  # geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Yearly OBP vs Contact Scale',
    x = 'Contact Scale',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, OBP),inherit.aes = FALSE) +
  annotate('text', x = 75, y = 0.45, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$OBP)$x,3)))


metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, walk_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  # geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Yearly BB% vs Contact Scale',
    x = 'Contact Scale',
    y = 'BB%',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, walk_rate),inherit.aes = FALSE)+
  annotate('text', x = 75, y = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$walk_rate)$x,3)))


# Contact vs Chase Rate
metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, chase_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  # geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Yearly Chase% vs Contact Scale',
    x = 'Contact Scale',
    y = 'Chase%',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, chase_rate),inherit.aes = FALSE)+
  annotate('text', x = 75, y = 0.1, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$chase_rate)$x,3)))

metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, zone_swing_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  theme_classic() +
  labs(
    title = 'Yearly Z-Swing% vs Contact Scale',
    x = 'Contact Scale',
    y = 'Z-Swing%',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, zone_swing_rate),inherit.aes = FALSE)+
  annotate('text', x = 75, y = 0.4, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$zone_swing_rate)$x,3)))

# Correct Swing Rate
metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, correct_swing_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  theme_classic() +
  labs(
    title = 'Yearly Correct_Swing% vs Contact Scale',
    x = 'Contact Scale',
    y = 'Correct Swing%',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, correct_swing_rate),inherit.aes = FALSE)+
  annotate('text', x = 75, y = 0.4, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$correct_swing_rate)$x,3)))


# contact scale vs xdamage scale
metric_woba_df %>% 
  ggplot(aes(contact_metric_scouting_scale, xdamage_metric_scouting_scale, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(70,130),
                     breaks = seq(70,130, by = 10)) +
  # geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Contact Scale vs xDamage Scale',
    x = 'Contact Scale',
    y = 'xDamage Scale',
    size = 'PA') + 
  geom_smooth(method = 'lm', aes(contact_metric_scouting_scale, xdamage_metric_scouting_scale),inherit.aes = FALSE) +
  annotate('text', y = 130, x = 75, label = paste0('r = ', round(corrr::correlate(metric_woba_df$contact_metric_scouting_scale, metric_woba_df$xdamage_metric_scouting_scale)$x,3)))


metric_woba_df_lagged <- metric_woba_df %>% 
  arrange(batter, game_year) %>% 
  group_by(batter) %>% 
  mutate(
    lagged_woba = lag(wOBA),
    lagged_xwoba = lag(xwOBA),
    lagged_avg = lag(AVG),
    lagged_obp = lag(OBP),
    lagged_walkrate = lag(walk_rate),
    lagged_krate = lag(k_rate),
    lagged_bb_k = lag(walk_minus_k),
    lagged_chaserate = lag(chase_rate),
    lagged_wastechase_chaserate = lag(wastechasezone_swingrate),
    lagged_heart_swingrate = lag(heart_swingrate),
    lagged_zswing = lag(zone_swing_rate),
    lagged_contactrate = lag(contact_rate),
    lagged_correctswingpercentage = lag(correct_swing_rate),
    lagged_contact = lag(contact_metric_scouting_scale),
    lagged_xdamage = lag(xdamage_metric_scouting_scale),
    lagged_sq_up_rate = lag(squared_up_rate),
    lagged_pitches_ab = lag(pitches_per_ab)
  ) %>% 
  ungroup()

# lagged woba vs woba  
metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_woba, x = wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(lagged_woba, wOBA)) +
  annotate('text', x = 0.25, y = 0.42, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$wOBA, metric_woba_df_lagged$lagged_woba)$x,3))) +
  labs(
    size = 'PA',
    x = '2025 wOBA',
    y = '2024 wOBA',
    title = '2024 wOBA vs 2025 wOBA'
  )

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_xdamage,x = wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_xdamage, x = wOBA)) +
  annotate('text', y = 130, x = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$wOBA, metric_woba_df_lagged$lagged_xdamage)$x,3)))+
  labs(
    size = 'PA',
    x = '2025 wOBA',
    y = '2024 xDamage+',
    title = '2024 xDamage+ vs 2025 wOBA'
  )

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_xwoba, x = wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(lagged_xwoba, wOBA)) +
  annotate('text', x = 0.25, y = 0.42, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$wOBA, metric_woba_df_lagged$lagged_xwoba)$x,3))) +
  labs(
    size = 'PA',
    x = '2025 wOBA',
    y = '2024 xwOBA',
    title = '2024 xwOBA vs 2025 wOBA'
  )

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = wOBA)) +
  annotate('text', y = 80, x = 0.42, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$wOBA, metric_woba_df_lagged$lagged_contact)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_obp, x = OBP, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_obp, x = OBP)) +
  annotate('text', y = 0.4, x = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_obp, metric_woba_df_lagged$OBP)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = OBP, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = OBP)) +
  annotate('text', y = 60, x = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$OBP)$x,3)))


metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_walkrate, x = walk_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_walkrate, x = walk_rate)) +
  annotate('text', y = 0.15, x = 0.05, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_walkrate, metric_woba_df_lagged$walk_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = walk_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = walk_rate)) +
  annotate('text', y = 70, x = 0.15, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$walk_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_chaserate, x = chase_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_chaserate, x = chase_rate)) +
  annotate('text', y = 0.4, x = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_chaserate, metric_woba_df_lagged$chase_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = chase_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = chase_rate)) +
  annotate('text', y = 70, x = 0.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$chase_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_correctswingpercentage, x = correct_swing_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_correctswingpercentage, x = correct_swing_rate)) +
  annotate('text', y = 0.7, x = 0.6, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_correctswingpercentage, metric_woba_df_lagged$correct_swing_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = correct_swing_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = correct_swing_rate)) +
  annotate('text', y = 70, x = 0.6, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$correct_swing_rate)$x,3)))

# metric_woba_df_lagged %>% 
#   ggplot(aes(y = lagged_krate, x = k_rate, size = pa)) +
#   geom_point(alpha = 0.3) +
#   theme_classic() + 
#   geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_krate, x = k_rate)) +
#   annotate('text', y = 0.3, x = 0.1, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_krate, metric_woba_df_lagged$k_rate)$x,3)))
# 
# metric_woba_df_lagged %>% 
#   ggplot(aes(y = lagged_contact, x = k_rate, size = pa)) +
#   geom_point(alpha = 0.3) +
#   theme_classic() + 
#   geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = k_rate)) +
#   annotate('text', y = 70, x = 0.6, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$k_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_bb_k, x = walk_minus_k, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_bb_k, x = walk_minus_k)) +
  annotate('text', y = 0.3, x = 0.1, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_bb_k, metric_woba_df_lagged$walk_minus_k)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = walk_minus_k, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = walk_minus_k)) +
  annotate('text', y = 70, x = 0, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$walk_minus_k)$x,3)))


metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contactrate, x = contact_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contactrate, x = contact_rate)) +
  annotate('text', y = 0.6, x = 0.6, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contactrate, metric_woba_df_lagged$contact_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = contact_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = contact_rate)) +
  annotate('text', y = 70, x = 0, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$contact_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_avg, x = AVG, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_avg, x = AVG)) +
  annotate('text', y = 0.35, x = 0.15, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_avg, metric_woba_df_lagged$AVG)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = AVG, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = AVG)) +
  annotate('text', y = 70, x = 0.35, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$AVG)$x,3)))


metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_wastechase_chaserate, x = wastechasezone_swingrate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_wastechase_chaserate, x = wastechasezone_swingrate)) +
  annotate('text', y = 0.35, x = 0.15, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_wastechase_chaserate, metric_woba_df_lagged$wastechasezone_swingrate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = wastechasezone_swingrate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = wastechasezone_swingrate)) +
  annotate('text', y = 70, x = 0.35, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$wastechasezone_swingrate)$x,3)))


metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_pitches_ab, x = pitches_per_ab, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_pitches_ab, x = pitches_per_ab)) +
  annotate('text', y = 4.2, x = 3.5, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_pitches_ab, metric_woba_df_lagged$pitches_per_ab)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = pitches_per_ab, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = pitches_per_ab)) +
  annotate('text', y = 70, x = 4.2, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$pitches_per_ab)$x,3)))


metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_sq_up_rate, x = squared_up_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_sq_up_rate, x = squared_up_rate)) +
  annotate('text', y = 0.5, x = 0.5, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_sq_up_rate, metric_woba_df_lagged$squared_up_rate)$x,3)))

metric_woba_df_lagged %>% 
  ggplot(aes(y = lagged_contact, x = squared_up_rate, size = pa)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  geom_smooth(method = 'lm', color = 'red', se = FALSE, inherit.aes = FALSE, aes(y = lagged_contact, x = squared_up_rate)) +
  annotate('text', y = 70, x = 0.5, label = paste0('r = ', round(corrr::correlate(metric_woba_df_lagged$lagged_contact, metric_woba_df_lagged$squared_up_rate)$x,3)))



# pitch tunneling density plot
updated_statcast_2024_2025_ordered %>% 
  filter(pitch_number > 1) %>% 
  ggplot(aes(ball_pos_xyz_0.1_diff)) +
  geom_density()

#### Write to Parquet ####
arrow::write_parquet(updated_statcast_2024_2025_ordered, 'final_updated_statcast_2024_2025_ordered.parquet')
