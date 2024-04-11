library(tidyverse)
library(caret)
library(pROC)
library(glmnet)

load('./Data/pitcher_seasons.RData')

train <- train %>% ungroup()
test <- test %>% ungroup()

train %>% group_by(tjs) %>% summarize(n.tjs = n()) %>%
  mutate(prop = n.tjs / sum(n.tjs))
test %>% group_by(tjs) %>% summarize(n.tjs = n()) %>%
  mutate(prop = n.tjs / sum(n.tjs))

# x <- train %>%
#   select(pitcher, game_year, ff.prop, si.prop, sl.prop, ff.mean.release_speed,
#          si.mean.release_speed, sl.mean.release_speed, tjs) %>%
#   slice_sample(n = 5)

names(train)

# Build model -------------------------------------------------------------

m1 <- glm(tjs ~
            ff.mean.release_speed
          + ff.q90.release_speed
          + ff.sd.release_speed
          + ff.absmean.pfx_x
          + ff.sd.pfx_x
          + ff.mean.pfx_z
          + ff.sd.pfx_z
          + ff.sd.release_pos_x
          + ff.sd.release_pos_y
          + ff.sd.release_pos_z
          + ff.prop
          + si.mean.release_speed
          + si.q90.release_speed
          + si.sd.release_speed
          + si.absmean.pfx_x
          + si.sd.pfx_x
          + si.mean.pfx_z
          + si.sd.pfx_z
          + si.sd.release_pos_x
          + si.sd.release_pos_y
          + si.sd.release_pos_z
          + si.prop
          + sl.mean.release_speed
          + sl.q90.release_speed
          + sl.sd.release_speed
          + sl.absmean.pfx_x
          + sl.sd.pfx_x
          + sl.mean.pfx_z
          + sl.sd.pfx_z
          + sl.sd.release_pos_x
          + sl.sd.release_pos_y
          + sl.sd.release_pos_z
          + sl.prop,
          data = train,
          family = binomial(link = 'logit'))

summary(m1)

# Predict -----------------------------------------------------------------

test$pred1.p <- predict(m1, newdata = test, type = 'response')
# hist(test$pred1.p)

roc1 <- roc(response = test$tjs, predictor = test$pred1.p)
best1 <- roc(data = test,
             response = tjs,
             predictor = pred1.p,
             ret = 'all_coords') %>%
  mutate(f1 = tp / (tp + 0.5 * (fp + fn))) %>%
  slice_max(f1, n = 1) %>%
  select(threshold, f1)

f1.1 <- best1$f1 
thresh1 <- best1$threshold

auc1 <- roc1$auc

ggroc(roc1) +
  ggtitle('ROC Curve: Logistic Regression',
          subtitle = paste0('AUC: ', round(auc1, 3))) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc1_1.png')

test <- test %>%
  mutate(pred1 = case_when(pred1.p >= thresh1 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred1 = factor(pred1, levels = c('0', '1')))

confusionMatrix(test$pred1, test$tjs, positive = '1', mode = 'everything')

# L1 Regularization -------------------------------------------------------

train.predictors <- train %>%
  select(ff.mean.release_speed, ff.q90.release_speed, ff.sd.release_speed,
         ff.absmean.pfx_x, ff.sd.pfx_x, ff.mean.pfx_z, ff.sd.pfx_z,
         ff.sd.release_pos_x, ff.sd.release_pos_y, ff.sd.release_pos_z, ff.prop,
         si.mean.release_speed, si.q90.release_speed, si.sd.release_speed,
         si.absmean.pfx_x, si.sd.pfx_x, si.mean.pfx_z, si.sd.pfx_z,
         si.sd.release_pos_x, si.sd.release_pos_y, si.sd.release_pos_z, si.prop,
         sl.mean.release_speed, sl.q90.release_speed, sl.sd.release_speed,
         sl.absmean.pfx_x, sl.sd.pfx_x, sl.mean.pfx_z, sl.sd.pfx_z,
         sl.sd.release_pos_x, sl.sd.release_pos_y, sl.sd.release_pos_z, sl.prop,
         tjs)

x.mat <- as.matrix(train.predictors %>% select(-tjs))
y.mat <- as.matrix(train.predictors %>% select(tjs))

m.lasso <- cv.glmnet(
  x = x.mat,
  y = y.mat,
  family = 'binomial',
  type.measure = 'auc',
  alpha = 1
  )

# plot(m.lasso)

coef(m.lasso)

# Build model -------------------------------------------------------------

m2 <- glm(tjs ~
            # ff.mean.release_speed
          # + ff.q90.release_speed
          # + ff.sd.release_speed
          # + ff.absmean.pfx_x
          ff.sd.pfx_x,
          # + ff.mean.pfx_z
          # + ff.sd.pfx_z
          # + ff.sd.release_pos_x
          # + ff.sd.release_pos_y
          # + ff.sd.release_pos_z
          # + ff.prop
          # + si.mean.release_speed
          # + si.q90.release_speed
          # + si.sd.release_speed
          # + si.absmean.pfx_x
          # + si.sd.pfx_x
          # + si.mean.pfx_z
          # + si.sd.pfx_z
          # + si.sd.release_pos_x
          # + si.sd.release_pos_y
          # + si.sd.release_pos_z
          # + si.prop
          # + sl.mean.release_speed
          # + sl.q90.release_speed
          # + sl.sd.release_speed
          # + sl.absmean.pfx_x
          # + sl.sd.pfx_x
          # + sl.mean.pfx_z
          # + sl.sd.pfx_z
          # + sl.sd.release_pos_x
          # + sl.sd.release_pos_y
          # + sl.sd.release_pos_z
          # + sl.prop,
        data = train,
        family = binomial(link = 'logit'))

summary(m2)

# Predict -----------------------------------------------------------------

test$pred2.p <- predict(m2, newdata = test, type = 'response')
# hist(test$pred2.p)

roc2 <- roc(response = test$tjs, predictor = test$pred2.p)
best2 <- roc(data = test,
             response = tjs,
             predictor = pred2.p,
             ret = 'all_coords') %>%
  mutate(f1 = tp / (tp + 0.5 * (fp + fn))) %>%
  slice_max(f1, n = 1) %>%
  select(threshold, f1)

f1.2 <- best2$f1 
thresh2 <- best2$threshold

auc2 <- roc2$auc

ggroc(roc2) +
  ggtitle('ROC Curve: Logistic Regression',
          subtitle = paste0('AUC: ', round(auc2, 3), ', L1 Regularization')) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc1_2.png')

test <- test %>%
  mutate(pred2 = case_when(pred2.p >= thresh2 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred2 = factor(pred2, levels = c('0', '1')))

confusionMatrix(test$pred2, test$tjs, positive = '1', mode = 'everything')

# Backward Selection -------------------------------------------------------

step(m1, direction = 'backward')

# Build model -------------------------------------------------------------

m3 <- glm(tjs ~
            # ff.mean.release_speed
          # + ff.q90.release_speed
          # + ff.sd.release_speed
          # + ff.absmean.pfx_x
          ff.sd.pfx_x
          + ff.mean.pfx_z
          # + ff.sd.pfx_z
          # + ff.sd.release_pos_x
          # + ff.sd.release_pos_y
          # + ff.sd.release_pos_z
          # + ff.prop
          # + si.mean.release_speed
          # + si.q90.release_speed
          # + si.sd.release_speed
          + si.absmean.pfx_x
          # + si.sd.pfx_x
          # + si.mean.pfx_z
          # + si.sd.pfx_z
          + si.sd.release_pos_x
          + si.sd.release_pos_y
          # + si.sd.release_pos_z
          # + si.prop
          # + sl.mean.release_speed
          # + sl.q90.release_speed
          # + sl.sd.release_speed
          + sl.absmean.pfx_x
          # + sl.sd.pfx_x
          + sl.mean.pfx_z
          # + sl.sd.pfx_z
          + sl.sd.release_pos_x
          # + sl.sd.release_pos_y
          + sl.sd.release_pos_z,
          # + sl.prop,
          data = train,
          family = binomial(link = 'logit'))

summary(m3)

# Predict -----------------------------------------------------------------

test$pred3.p <- predict(m3, newdata = test, type = 'response')
# hist(test$pred3.p)

roc3 <- roc(response = test$tjs, predictor = test$pred3.p)
best3 <- roc(data = test,
             response = tjs,
             predictor = pred3.p,
             ret = 'all_coords') %>%
  mutate(f1 = tp / (tp + 0.5 * (fp + fn))) %>%
  slice_max(f1, n = 1) %>%
  select(threshold, f1)

f1.3 <- best3$f1 
thresh3 <- best3$threshold

auc3 <- roc3$auc

ggroc(roc3) +
  ggtitle('ROC Curve: Logistic Regression',
          subtitle = paste0('AUC: ', round(auc3, 3), ', Backward Selection')) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc1_3.png')

test <- test %>%
  mutate(pred3 = case_when(pred3.p >= thresh3 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred3 = factor(pred3, levels = c('0', '1')))

confusionMatrix(test$pred3, test$tjs, positive = '1', mode = 'everything')

