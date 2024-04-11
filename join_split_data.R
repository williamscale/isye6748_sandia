library(tidyverse)

ff <- readRDS('./Data/ff_pitcher_seasons.RDS')
si <- readRDS('./Data/si_pitcher_seasons.RDS')
sl <- readRDS('./Data/sl_pitcher_seasons.RDS')

colnames(ff)[seq(5, 16)] <- paste0('ff.', colnames(ff)[seq(5, 16)])
colnames(si)[seq(5, 16)] <- paste0('si.', colnames(si)[seq(5, 16)])
colnames(sl)[seq(5, 16)] <- paste0('sl.', colnames(sl)[seq(5, 16)])

sum(ff$ff.n)
sum(si$si.n)
sum(sl$sl.n)

ff <- ff %>% filter(ff.n >= 5)
si <- si %>% filter(si.n >= 5)
sl <- sl %>% filter(sl.n >= 5)

pitcher.seasons <- inner_join(ff, si, by = c('pitcher', 'game_year', 'tjs'))
pitcher.seasons <- inner_join(pitcher.seasons, sl,
                              by = c('pitcher', 'game_year', 'tjs')) %>%
  select(-c('total.pitches.x', 'total.pitches.y'))
# colSums(is.na(pitcher.seasons))

names(pitcher.seasons)

# scale.cols <- c(
#   'ff.mean.release_speed',
#   'ff.q90.release_speed',
#   'ff.sd.release_speed',
#   'ff.absmean.pfx_x',
#   'ff.sd.pfx_x',
#   'ff.mean.pfx_z',
#   'ff.sd.pfx_z',
#   'ff.sd.release_pos_x',
#   'ff.sd.release_pos_y',
#   'ff.sd.release_pos_z',
#   'si.mean.release_speed',
#   'si.q90.release_speed',
#   'si.sd.release_speed',
#   'si.absmean.pfx_x',
#   'si.sd.pfx_x',
#   'si.mean.pfx_z',
#   'si.sd.pfx_z',
#   'si.sd.release_pos_x',
#   'si.sd.release_pos_y',
#   'si.sd.release_pos_z',
#   'sl.mean.release_speed',
#   'sl.q90.release_speed',
#   'sl.sd.release_speed',
#   'sl.absmean.pfx_x',
#   'sl.sd.pfx_x',
#   'sl.mean.pfx_z',
#   'sl.sd.pfx_z',
#   'sl.sd.release_pos_x',
#   'sl.sd.release_pos_y',
#   'sl.sd.release_pos_z'
#   )
# 
# pitcher.seasons.scaled <- pitcher.seasons %>%
#   ungroup() %>%
#   mutate(across(scale.cols, ~ c(scale(.))))

train <- pitcher.seasons %>%
  ungroup() %>%
  slice_sample(prop = 0.70)
test <- anti_join(pitcher.seasons, train, by = c('pitcher', 'game_year'))

train %>% group_by(tjs) %>% summarize(n = n())
test %>% group_by(tjs) %>% summarize(n = n())

rm(ff, si, sl, pitcher.seasons, pitcher.seasons.scaled, scale.cols)

save.image('./Data/pitcher_seasons.RData')
# save.image('./Data/pitcher_seasons_scaled.RData')
