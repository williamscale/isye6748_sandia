library(tidyverse)
library(ggthemes)
library(cowplot)

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

pitches %>% group_by(pitch_type) %>% summarize(n.type = n()) %>%
  arrange(desc(n.type)) %>% mutate(prop = n.type / sum(n.type)) %>%
  mutate(cum.prop = cumsum(prop))

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
         pitch_type == 'FF')

# colSums(is.na(pitches.season))
# nrow = 442592

pitches.season <- pitches.season %>%
  filter(!is.na(release_speed),
         !is.na(release_pos_x),
         !is.na(release_pos_y),
         !is.na(release_pos_z),
         !is.na(pfx_x),
         !is.na(pfx_z))

# hist.release_speed <- ggplot(data = pitches.season,
#                              aes(x = release_speed)) +
#   geom_histogram() +
#   xlab('release_speed') +
#   ylab('Count') +
#   theme_solarized()
# 
# hist.pfx_x <- ggplot(data = pitches.season,
#                      aes(x = pfx_x)) +
#   geom_histogram() +
#   xlab('pfx_x') +
#   ylab('Count') +
#   theme_solarized()
# 
# hist.pfx_z <- ggplot(data = pitches.season,
#                      aes(x = pfx_z)) +
#   geom_histogram() +
#   xlab('pfx_z') +
#   ylab('Count') +
#   theme_solarized()
# 
# hist.release_pos_x <- ggplot(data = pitches.season,
#                              aes(x = release_pos_x)) +
#   geom_histogram() +
#   xlab('release_pos_x') +
#   ylab('Count') +
#   theme_solarized()
# 
# hist.release_pos_y <- ggplot(data = pitches.season,
#                              aes(x = release_pos_y)) +
#   geom_histogram() +
#   xlab('release_pos_y') +
#   ylab('Count') +
#   theme_solarized()
# 
# hist.release_pos_z <- ggplot(data = pitches.season,
#                              aes(x = release_pos_z)) +
#   geom_histogram() +
#   xlab('release_pos_z') +
#   ylab('Count') +
#   theme_solarized()
# 
# plot_grid(hist.release_speed, hist.pfx_x, hist.pfx_z, hist.release_pos_x,
#           hist.release_pos_y, hist.release_pos_z, ncol = 2)
# 
# ggsave('./Viz/Final/features_hist.png')

ff.release_speed.plot <- pitches.season %>%
  ungroup() %>%
  filter(pitcher == 543243) %>%
  select(pitcher, player_name, game_year, game_date, inning, at_bat_number, pitch_number,
         release_speed) %>%
  arrange(game_date, inning, at_bat_number, pitch_number) %>%
  mutate(pitch.n = row_number()) %>%
  group_by(game_year) %>%
  mutate(mean.release_speed = mean(release_speed),
         pitch.game.n = mean(pitch.n))

# ggplot(data = ff.release_speed.plot) +
#   geom_point(aes(x = pitch.n,
#                  y = release_speed,
#                  color = 'Pitch Velocity'),
#              alpha = 0.1) +
#   geom_point(aes(x = pitch.game.n,
#                  y = mean.release_speed,
#                  color = 'Mean Pitch Velocity by Season'),
#              size = 2.5) +
#   scale_color_manual(name = NULL,
#                      breaks = c('Pitch Velocity',
#                                 'Mean Pitch Velocity by Season'),
#                      values = c('Pitch Velocity' = '#268bd2',
#                                 'Mean Pitch Velocity by Season' = '#cb4b16')) +
#   ggtitle('Sonny Gray', subtitle = 'Fastballs Thrown 2015-2023') +
#   xlab('Pitch Number') +
#   ylab('Release Speed [mph]') +
#   theme_solarized() +
#   theme(legend.key = element_rect(fill = '#fdf6e3'),
#         legend.position = 'bottom')
# ggsave('./Viz/Final/gray_season_mean.png')


# colSums(is.na(pitches.season))
# nrow = 442591

pitches.season <- pitches.season %>%
  summarize(total.pitches = mean(total.pitches),
            ff.n = n(),
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
  mutate(ff.prop = ff.n / total.pitches)
  
# colSums(is.na(pitches.season))

pitches.season <- pitches.season %>% replace(is.na(.), 0)

# colSums(is.na(pitches.season))

pitches.season <- pitches.season %>%
  left_join(tjs, by = c('pitcher' = 'player_id', 'game_year' = 'year')) %>%
  mutate(tjs = replace_na(tjs, 0)) %>%
  mutate(tjs = factor(tjs))

saveRDS(pitches.season, './Data/ff_pitcher_seasons.RDS')

