library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

load('./Data/pitcher_seasons.RData')

# Build model -------------------------------------------------------------

m2 <- rpart(tjs ~
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
            method = 'class',
            minbucket = 3)

rpart.plot(m2)
# save manually

var.imp <- stack(data.frame(as.list(m2$variable.importance)))

ggplot(data = var.imp,
       aes(x = ind,
           y = values)) +
  geom_col() +
  coord_flip() +
  ggtitle('Variable Importance: Decision Tree') +
  xlab('Variable') +
  ylab('Importance') +
  theme_solarized()

ggsave('./Viz/Final/var_imp_2.png')

# Predict -----------------------------------------------------------------

test$pred2.p <- predict(m2, newdata = test, type = 'prob')[, 2]
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
  ggtitle('ROC Curve: Decision Tree',
          subtitle = paste0('AUC: ', round(auc2, 3))) +
  xlab('Specificity') +
  ylab('Sensitivity') +
  theme_solarized()

ggsave('./Viz/Final/roc2.png')

test <- test %>%
  mutate(pred2 = case_when(pred2.p >= thresh2 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(pred2 = factor(pred2, levels = c('0', '1')))

confusionMatrix(test$pred2, test$tjs, positive = '1', mode = 'everything')


