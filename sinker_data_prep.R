library(tidyverse)
library(ggthemes)

tjs <- read.csv('./Data/tjs_clean.csv') %>%
  select(player_id, year) %>%
  mutate(tjs = 1)

files <- list.files('./Data/')
files <- files[grepl('^player.*', files)]

pitches <- vector(mode = 'list', length = length(files))

for (i in 1:length(files)) {
  print(i)
  pitches.i <- read.csv(paste0('./Data/', files[i]))
  # %>%
  #   rowid_to_column('season.n')
  if (nrow(pitches.i) != 0) {
    pitches[[i]] <- pitches.i
  }
}

pitches <- bind_rows(pitches)

total.pitches.hist <- pitches %>%
  group_by(pitcher, game_year) %>%
  summarize(total.pitches = n())

# ggplot(data = total.pitches.hist,
#        aes(x = total.pitches)) +
#   geom_histogram(bins = 14, color = '#002b36') +
#   xlab('Total Pitches') +
#   ylab('Pitcher Seasons') +
#   theme_solarized()

# ggsave('./Viz/Final/total_pitches_hist.png')

# nrow(total.pitches.hist %>% filter(total.pitches < 100))

pitches.season <- pitches %>%
  group_by(pitcher, game_year) %>%
  mutate(total.pitches = n()) %>%
  filter(total.pitches >= 100,
         pitch_type == 'SI')

# colSums(is.na(pitches.season))
# nrow = 264105

pitches.season <- pitches.season %>%
  filter(!is.na(release_speed),
         !is.na(release_pos_x),
         !is.na(release_pos_y),
         !is.na(release_pos_z),
         !is.na(pfx_x),
         !is.na(pfx_z))

# colSums(is.na(pitches.season))
# nrow = 264104

pitches.season <- pitches.season %>%
  summarize(total.pitches = mean(total.pitches),
            si.n = n(),
            mean.release_speed = mean(release_speed),
            q10.release_speed = quantile(release_speed, probs = 0.1),
            q50.release_speed = quantile(release_speed, probs = 0.5),
            q90.release_speed = quantile(release_speed, probs = 0.9),
            sd.release_speed = sd(release_speed),
            absmean.pfx_x = abs(mean(pfx_x)),
            sd.pfx_x = sd(pfx_x),
            mean.pfx_z = mean(pfx_z),
            sd.pfx_z = sd(pfx_z),
            sd.release_pos_x = sd(release_pos_x),
            sd.release_pos_y = sd(release_pos_y),
            sd.release_pos_z = sd(release_pos_z)
            ) %>%
  mutate(si.prop = si.n / total.pitches)
  
# colSums(is.na(pitches.season))

pitches.season <- pitches.season %>% replace(is.na(.), 0)

# colSums(is.na(pitches.season))

pitches.season <- pitches.season %>%
  left_join(tjs, by = c('pitcher' = 'player_id', 'game_year' = 'year')) %>%
  mutate(tjs = replace_na(tjs, 0)) %>%
  mutate(tjs = factor(tjs))

saveRDS(pitches.season, './Data/si_pitcher_seasons.RDS')

# str(pitches.season)

# pitches.season.scaled <- pitches.season %>%
#   mutate(mean.release_speed = c(scale(mean.release_speed)),
#          q10.release_speed = c(scale(q10.release_speed)),
#          q50.release_speed = c(scale(q50.release_speed)),
#          q90.release_speed = c(scale(q90.release_speed)))

train <- pitches.season %>%
  slice_sample(prop = 0.8)
test <- anti_join(pitches.season, train, by = c('pitcher', 'game_year'))

rm(pitches, pitches.i, pitches.season, pitches.season.scaled,
   total.pitches.hist, files, i, tjs)

save.image('./Data/ff_pitcher_seasons.RData')

# colSums(is.na(train))
