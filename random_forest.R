library(tidyverse)
library(caret)
library(pROC)
library(randomForest)

load('./Data/pitcher_seasons.RData')

# Build model -------------------------------------------------------------

m3 <- randomForest(tjs ~
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
                   importance = TRUE)

var.imp <- data.frame(m3$importance) %>%
  rownames_to_column(var = 'variable') %>%
  select(variable, MeanDecreaseAccuracy)

ggplot(data = var.imp,
       aes(x = reorder(variable, -MeanDecreaseAccuracy),
           y = MeanDecreaseAccuracy)) +
  geom_col() +
  coord_flip() +
  ggtitle('Variable Importance: Random Forest') +
  xlab('Variable') +
  ylab('Mean Decrease in Accuracy') +
  theme_solarized()

ggsave('./Viz/Final/var_imp_3.png')

# Predict -----------------------------------------------------------------

test$pred3.p <- predict(m3, newdata = test, type = 'prob')[, 2]
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
  ggtitle('ROC Curve: Random Forest',
          subtitle = paste0('AUC: ', round(auc3, 3))) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc3.png')

test <- test %>%
  mutate(pred3 = case_when(pred3.p >= thresh3 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred3 = factor(pred3, levels = c('0', '1')))

confusionMatrix(test$pred3, test$tjs, positive = '1', mode = 'everything')

