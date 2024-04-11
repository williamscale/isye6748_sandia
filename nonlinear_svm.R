library(tidyverse)
library(caret)
library(pROC)
library(e1071)

load('./Data/pitcher_seasons.RData')

# Build model -------------------------------------------------------------

m5 <- svm(tjs ~
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
          probability = TRUE,
          kernel = 'radial',
          class.weights= c('0' = 1, '1' = 20))

# Predict -----------------------------------------------------------------

test$pred5.p <- attr(predict(m5, newdata = test, probability = TRUE),
                     'probabilities')[, 2]
# hist(test$pred5.p)

roc5 <- roc(response = test$tjs, predictor = test$pred5.p)
best5 <- roc(data = test,
             response = tjs,
             predictor = pred5.p,
             ret = 'all_coords') %>%
  mutate(f1 = tp / (tp + 0.5 * (fp + fn))) %>%
  slice_max(f1, n = 1) %>%
  select(threshold, f1)

f1.5 <- best5$f1 
thresh5 <- best5$threshold

auc5 <- roc5$auc

ggroc(roc5) +
  ggtitle('ROC Curve: Non-Linear SVM',
          subtitle = paste0('AUC: ', round(auc5, 3))) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc5.png')

test <- test %>%
  mutate(pred5 = case_when(pred5.p >= thresh5 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred5 = factor(pred5, levels = c('0', '1')))

confusionMatrix(test$pred5, test$tjs, positive = '1', mode = 'everything')


