#### Loading Packages ####

require(tidyverse)
require(magrittr)
require(ltm)

#### Loading Data ####

SCera <- updated_statcast_2024_2025_ordered %>% filter(game_year == 2024)

#### Data Wrangling ####

Pitchers_temp <- SCera %>%
  group_by(pitcher) %>%
  summarise(n_thrown = n())

Batters <- SCera %>%
  group_by(batter) %>%
  summarise(n_seen = n()) %>%
  left_join(Pitchers_temp, by = c("batter" = "pitcher")) %>%
  mutate(n_thrown = ifelse(is.na(n_thrown), 0, n_thrown)) %>%
  filter(n_seen >= n_thrown) #eliminate players who threw more pitches than they saw

SCera$rand <- runif(nrow(SCera)) # assigns a random value between 0-1 to the dataset (used later)

PreppedData <- SCera %>%
  dplyr::select(batter, batter_name, game_date, at_bat_number, rand, xdamage_metric_weighted, contact_metric) %>% #selecting necessary columns
  filter(
    batter %in% Batters$batter
  )  %>% 
  group_by(batter) %>% #grouping by batter
  mutate(
    name = first(batter_name), #grabbing player name from first row of player name
    pitches = n() # number of pitches seen in a season
  ) %>%
  ungroup() %>%
  filter(pitches >= 2000) %>% #filtering for players with approx 500 or more PA
  group_by(batter) %>%
  arrange(rand) %>% #Used to randomize sequencing of PA
  mutate(pitch_num = row_number()) %>% #getting pitch number on the season
  filter(pitch_num <= 2000) %>% # filtering for when pitches are less than
  ungroup()

Randomized_Collection_xdamage <- NULL;for(rerun in 1:3){ # run the for loop once
  for(metric in c("xdamage_metric_weighted")){ # for each of these metrics
    for(sampsize in 20:1000){ # 3:2000 -> number of PA
      PA_Matrix <- PreppedData %>%
        pivot_wider(
          id_cols = batter, # make each individual pitch it's own column
          names_from = pitch_num,
          names_prefix = "Pitch",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      PA_Matrix <- PA_Matrix[,sample(1:2000, sampsize)] # sampling for number of pitches
      
      
      
      Randomized_Collection_xdamage <- Randomized_Collection_xdamage %>%
        rbind(
          data.frame(
            metric = metric,
            ss = sampsize,
            s_players = sd(rowSums(PA_Matrix)/ncol(PA_Matrix)), # finding the standard deviation among players at each PA
            alpha = cronbach.alpha(data = PA_Matrix[,1:sampsize])$alpha # finding the cronboch alpha
          )
        )
    }
    print(paste(rerun, metric)) # prints progress
  }
}

Randomized_Collection_contact <- NULL;for(rerun in 1:10){ # run the for loop once
  for(metric in c("contact_metric")){ # for each of these metrics
    for(sampsize in 100:2000){ # 3:2000 -> number of PA
      PA_Matrix <- PreppedData %>%
        pivot_wider(
          id_cols = batter, # make each individual pitch it's own column
          names_from = pitch_num,
          names_prefix = "Pitch",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      PA_Matrix <- PA_Matrix[,sample(1:2000, sampsize)] # sampling for number of pitches
      
      
      
      Randomized_Collection_contact <- Randomized_Collection_contact %>%
        rbind(
          data.frame(
            metric = metric,
            ss = sampsize,
            s_players = sd(rowSums(PA_Matrix)/ncol(PA_Matrix)), # finding the standard deviation among players at each PA
            alpha = cronbach.alpha(data = PA_Matrix[,1:sampsize])$alpha # finding the cronboch alpha
          )
        )
    }
    print(paste(rerun, metric)) # prints progress
  }
}

#Averaging out re-runs
Randomized_Collection_xdamage <- Randomized_Collection_xdamage %>%
  group_by(metric, ss) %>% # grouping by metric and samplesize
  summarise(s_players = mean(s_players), # finding the standard deviation of each player
            alpha = mean(alpha)) #finding the mean alpha for each plate appearance

#Cronbach visual
Randomized_Collection_xdamage %>%
  filter(metric == 'xdamage_metric_weighted') %>% 
  mutate(metric = 'xDamage+') %>% 
  ggplot(aes(x = ss, y = alpha, color = metric)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = sqrt(0.5),
             linetype = "dashed") +
  ggtitle('xDamage+ Cronbach Alpha (2024)') +
  ylab("Cronbach's Alpha") +
  xlab("Sample Size (Pitches)") +
  labs(color = "Metric") +
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.1)) +
  geom_vline(xintercept = 160, linetype = 'dashed') +
  geom_smooth() +
  annotate('text', x = 500, y = 1, label = '160 Pitches\n(Roughly 40 PA)') +
  theme_classic()


#Averaging out re-runs
Randomized_Collection_contact <- Randomized_Collection_contact %>%
  group_by(metric, ss) %>% # grouping by metric and samplesize
  summarise(s_players = mean(s_players), # finding the standard deviation of each player
            alpha = mean(alpha)) #finding the mean alpha for each plate appearance

#Cronbach visual
Randomized_Collection_contact %>%
  filter(metric == 'contact_metric') %>% 
  mutate(metric = 'Contact+') %>% 
  ggplot(aes(x = ss, y = alpha, color = metric)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = sqrt(0.5),
             linetype = "dashed") +
  ggtitle('Contact+ Cronbach Alpha (2024)') +
  ylab("Cronbach's Alpha") +
  xlab("Sample Size (Pitches)") +
  labs(color = "Metric") +
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.1)) +
  geom_vline(xintercept = 1350, linetype = 'dashed') +
  geom_smooth() +
  annotate('text', x = 700, y = 0.9, label = '1350 Pitches\n(Roughly 340 PA)') +
  theme_classic()
