library(tidyr)
library(dplyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(bayesplot)
library(loo)
library(emmeans)
library(psych)

library(papaja)
library(cowplot)
library(ggbeeswarm)

# global theme for figures
source('analysis/figure_settings.R')

# model helper functions
source('analysis/model_helpers.R')

# output paths
FIG_PATH = 'results_figures/combined'
MOD_PATH = 'results_models/combined'
TAB_PATH = 'results_tables/combined'



# Prepare data ----

## Study 1

df1 = read.csv('data_processed/study1/Study1_by_article.csv')
df1$study = 'study1'

df1 = df1 %>%
  select(study, PID, treatment, article, recall_rmse, recall_signed_err,
         recall_rmse.sc,
         article_recommend, article_surprise, article_interest,
         topic_involvement, 
         attitude_pre, attitude_post,
         att_elic_drug_overdose_opinion_pre, att_elic_drug_overdose_opinion_post,
         att_elic_combat_drug_priority_pre, att_elic_combat_drug_priority_post,
         att_elic_opinion_on_drug_legalization_pre, att_elic_opinion_on_drug_legalization_post)


## Study 2

df2 = read.csv('data_processed/study2/Study2_by_article.csv')
df2$study = 'study2'

df2 = df2 %>%
  select(study, PID, treatment, article, recall_rmse, recall_signed_err,
         recall_rmse.sc,
         article_recommend, article_surprise, article_interest,
         topic_involvement, attitude_pre, attitude_post,
         att_elic_drug_overdose_opinion_pre, att_elic_drug_overdose_opinion_post,
         att_elic_combat_drug_priority_pre, att_elic_combat_drug_priority_post,
         att_elic_opinion_on_drug_legalization_pre, att_elic_opinion_on_drug_legalization_post)


## combine
df = rbind(df1, df2)
df$treatment = factor(df$treatment,
                      levels=c('Control', 'Categorize', 'Draw'))
df$study = factor(df$study,
                  levels=c('study1', 'study2'),
                  labels=c('Study 1', 'Study 2'))
df$article = factor(df$article)
df$PID = factor(df$PID)


# Recall accuracy ----

## Combined figure ----

plt.rmse = ggplot(df, aes(x=treatment, y=recall_rmse.sc, color=study)) +
  stat_summary(position=position_dodge(.5)) +
  #coord_flip() +
  labs(y = 'Recall RMSE',
       title = 'Recall Accuracy') +
  theme(axis.title.x = element_blank(),
        legend.position = 'right',
        legend.title = element_blank())
(plt.rmse)
ggsave(plot=plt.rmse,width=5,height=4,dpi=200,
       filename=sprintf(sprintf("%s/rmse.pdf", FIG_PATH)), useDingbats=FALSE)




## Combined model ----

ggplot(df) +
  geom_histogram(aes(x=recall_rmse.sc, fill=treatment),
                 position='identity', alpha=.7) +
  facet_grid(study ~ article, scales='free')


### Model 0: Just study * treatment
model_id = 'recall_rmse_0'
mod.recall_rmse_0 = brm(bf(recall_rmse.sc ~ study * treatment + article + 
                                      (1|PID)),
                                 family=lognormal(),
                                 data=df,
                                 backend = "cmdstanr", 
                                 cores = parallel::detectCores() - 1, 
                                 iter=5000,
                                 file = model_fit_path(model_id))
summary(mod.recall_rmse_0)
pp_check(mod.recall_rmse_0, ndraws=100)

### Model 1: Add topic involvement and pre attitude
model_id = 'recall_rmse_1'
mod.recall_rmse_1 = brm(bf(recall_rmse.sc ~ study * treatment + article + 
                             scale(topic_involvement) + 
                             scale(attitude_pre) +
                             (1|PID)),
                        family=lognormal(),
                        data=df,
                        backend = "cmdstanr", 
                        cores = parallel::detectCores() - 1, 
                        iter=5000,
                        file = model_fit_path(model_id))
summary(mod.recall_rmse_1)
pp_check(mod.recall_rmse_1, ndraws=100)

### Model 2: Allow sigma to vary between studies
model_id = 'recall_rmse_2'
mod.recall_rmse_2 = brm(bf(recall_rmse.sc ~ study * treatment + article + 
                             scale(topic_involvement) + 
                             scale(attitude_pre) +
                             (1|PID),
                           sigma ~ study),
                        family=lognormal(),
                        data=df,
                        backend = "cmdstanr", 
                        cores = parallel::detectCores() - 1, 
                        iter=5000,
                        file = model_fit_path(model_id))
summary(mod.recall_rmse_2)
pp_check(mod.recall_rmse_2, ndraws=100)

model_id = 'recall_rmse_3'
mod.recall_rmse_3 = brm(bf(recall_rmse.sc ~ study * treatment + article + 
                             (1|PID),
                           sigma ~ study),
                        family=lognormal(),
                        data=df,
                        backend = "cmdstanr", 
                        cores = parallel::detectCores() - 1, 
                        iter=5000,
                        file = model_fit_path(model_id))
summary(mod.recall_rmse_3)
pp_check(mod.recall_rmse_3, ndraws=100)


### Model comparison (original)
loo0 = loo(mod.recall_rmse_0)
loo1 = loo(mod.recall_rmse_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)',
  'Model 0: mu ~ study * treatment + article + (1|PID)'
)
write.csv(round(comparison_results, 2), 
          sprintf("%s/recall_rmse_model_comparison.csv", MOD_PATH))

### Model comparison (with varying scale parameters by study)
loo0 = loo(mod.recall_rmse_0)
loo1 = loo(mod.recall_rmse_1)
loo2 = loo(mod.recall_rmse_2)
loo3 = loo(mod.recall_rmse_3)
comparison_results = as.data.frame(loo_compare(loo0, loo1, loo2, loo3))
rownames(comparison_results) = c(
  'Model 1: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)',
  'Model 3: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)\nsigma ~ study',
  'Model 0: mu ~ study * treatment + article + (1|PID)',
  'Model 2: mu ~ study * treatment + article + (1|PID)\nsigma ~ study'
)
write.csv(round(comparison_results, 2), 
          sprintf("%s/rmse_model_comparison2.csv", TAB_PATH))





### Parameters and contrasts ----
model_id = 'recall_rmse_1'
mod.recall_rmse = mod.recall_rmse_1

# save coefficients
tab = as.data.frame(summary(mod.recall_rmse)$fixed)
rownames(tab) = c('Intercept', 
                  'Study [2]',
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude',
                  'Study [2] x Treatment [Categorize]',
                  'Study [2] x Treatment [Draw]')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


comp_rmse = mod.recall_rmse %>%
  emmeans(~ study + treatment,
          var = "study",
          by = "treatment",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise") %>%
  gather_emmeans_draws() %>%
  median_hdi() %>% 
  as.data.frame()
comp_rmse$study2_study1 = comp_rmse$.value
comp_rmse$outcome = 'recall RMSE'

mod.recall_rmse %>%
  emmeans(~ study + treatment,
          var = "study",
          by = "treatment",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise")

# PD
mod.recall_rmse %>%
  emmeans(~ study + treatment,
          var = "study",
          by = "treatment",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise") %>%
  gather_emmeans_draws() %>%
  group_by(contrast, treatment) %>%
  summarise(med = median(.value),
            pd_neg = sum(.value < 0)/n(),
            pd_pos = sum(.value > 0)/n(),)


# Article engagement measures ----

## Surprise ----

ggplot(df) +
  geom_histogram(aes(x=article_surprise, fill=treatment),
                 position='dodge', alpha=.7) +
  facet_grid(study ~ article, scales='free')


model_id = 'surprise_0'
mod.surprise_0 <- brm(bf(article_surprise ~ study * treatment + article + 
                           (1|PID)),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      file = model_fit_path(model_id))
summary(mod.surprise_0)
pp_check(mod.surprise_0, ndraws=100)


model_id = 'surprise_1'
mod.surprise_1 <- brm(bf(article_surprise ~ study * treatment + article +
                           scale(topic_involvement) + 
                           scale(attitude_pre) +
                           (1|PID)),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      save_pars = save_pars(all = TRUE),
                      file = model_fit_path(model_id))
summary(mod.surprise_1)

model_id = 'surprise_2'
mod.surprise_2 <- brm(bf(article_surprise ~ study * treatment + article +
                           scale(topic_involvement) + 
                           scale(attitude_pre) +
                           (1|PID),
                         disc ~ study),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      control = list(adapt_delta = .99),
                      file = model_fit_path(model_id))
summary(mod.surprise_2)


#### Model comparison
loo0 = loo(mod.surprise_0)
loo1 = loo(mod.surprise_1)
#loo2 = loo(mod.surprise_2)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)',
  'Model 0: mu ~ study * treatment + article + (1|PID)'
)
write.csv(comparison_results, 
          sprintf("%s/surprise_model_comparison.csv", MOD_PATH))


### Parameters and contrasts
model_id = 'surprise_1'
mod.surprise = mod.surprise_1

# save coefficients
tab = as.data.frame(summary(mod.surprise)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]',
                  'Study [2]',
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude',
                  'Study [2] x Treatment [Categorize]',
                  'Study [2] x Treatment [Draw]')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


mns = epred_draws(mod.surprise,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(df$study),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, article, treatment, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = mean(mn))


mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  median_hdi()


# PD
mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  summarise(med = median(study2_study1),
            pd_neg = sum(study2_study1 < 0)/n(),
            pd_pos = sum(study2_study1 > 0)/n(),)


comp_surprise = mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  mean_hdi()
comp_surprise$outcome = 'surprise'


## Recommend ----

ggplot(df) +
  geom_histogram(aes(x=recall_responses_Recommend_article, fill=treatment),
                 position='dodge', alpha=.7) +
  facet_grid(~ variable, scales='free')

model_id = 'recommend_0'
mod.recommend_0 <- brm(bf(article_recommend ~ study * treatment + article + 
                (1|PID)),
           data=df, 
           family=cumulative(link="logit", threshold="flexible"), 
           backend = "cmdstanr", 
           cores = parallel::detectCores() - 1, 
           iter=5000,
           file = model_fit_path(model_id))
summary(mod.recommend_0)
pp_check(mod.recommend_0, ndraws=100)


model_id = 'recommend_1'
mod.recommend_1 <- brm(bf(article_recommend ~ study * treatment + article +
                            scale(topic_involvement) + 
                            scale(attitude_pre) +
                            (1|PID)),
                       data=df, 
                       family=cumulative(link="logit", threshold="flexible"), 
                       backend = "cmdstanr", 
                       cores = parallel::detectCores() - 1, 
                       iter=5000,
                       file = model_fit_path(model_id))
summary(mod.recommend_1)
pp_check(mod.recommend_1, ndraws=100)



model_id = 'recommend_2'
mod.recommend_2 <- brm(bf(article_recommend ~ study * treatment + article +
                           scale(topic_involvement) + 
                           scale(attitude_pre) +
                           (1|PID),
                         disc ~ study),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      control = list(adapt_delta = .99),
                      file = model_fit_path(model_id))
summary(mod.recommend_2)


#### Model comparison
loo0 = loo(mod.recommend_0)
loo1 = loo(mod.recommend_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)',
  'Model 0: mu ~ study * treatment + article + (1|PID)'
)
write.csv(comparison_results, 
          sprintf("%s/recommend_model_comparison.csv", MOD_PATH))


### Parameters
model_id = 'recommend_1'
mod.recommend = mod.recommend_1

# save coefficients
tab = as.data.frame(summary(mod.recommend)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]',
                  'Study [2]',
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude',
                  'Study [2] x Treatment [Categorize]',
                  'Study [2] x Treatment [Draw]')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))




mns = epred_draws(mod.recommend,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(df$study),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, article, treatment, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = mean(mn))

mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  mean_hdi()


# PD
mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  summarise(med = median(study2_study1),
            pd_neg = sum(study2_study1 < 0)/n(),
            pd_pos = sum(study2_study1 > 0)/n(),)

comp_recommend = mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  mean_hdi()
comp_recommend$outcome = 'recommend'


## Interest ----

ggplot(df) +
  geom_histogram(aes(x=recall_responses_view_opinion, fill=treatment),
                 position='dodge', alpha=.7) +
  facet_grid(~ variable, scales='free')


model_id = 'interest_0'
mod.interest_0 <- brm(bf(article_interest ~ study * treatment + 
                           article +
                           (1|PID)),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      file = model_fit_path(model_id))
summary(mod.interest_0)
pp_check(mod.interest_0, ndraws=100)


model_id = 'interest_1'
mod.interest_1 <- brm(bf(article_interest ~ study * treatment + 
                           article +
                            scale(topic_involvement) + 
                            scale(attitude_pre) +
                            (1|PID)),
                       data=df, 
                       family=cumulative(link="logit", threshold="flexible"), 
                       backend = "cmdstanr", 
                       cores = parallel::detectCores() - 1, 
                       iter=5000,
                       file = model_fit_path(model_id))
summary(mod.interest_1)
pp_check(mod.interest_1, ndraws=100)



model_id = 'interest_2'
mod.interest_2 <- brm(bf(article_interest ~ study * treatment + article +
                           scale(topic_involvement) + 
                           scale(attitude_pre) +
                           (1|PID),
                         disc ~ study),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      control = list(adapt_delta = .99),
                      file = model_fit_path(model_id))
summary(mod.interest_2)


#### Model comparison
loo0 = loo(mod.interest_0)
loo1 = loo(mod.interest_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: mu ~ study * treatment + article + topicInvolvement + preAttitude + (1|PID)',
  'Model 0: mu ~ study * treatment + article + (1|PID)'
)
write.csv(comparison_results, 
          sprintf("%s/interest_model_comparison.csv", MOD_PATH))


### Parameters
model_id = 'interest_1'
mod.interest = mod.interest_1

# save coefficients
tab = as.data.frame(summary(mod.interest)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]',
                  'Study [2]',
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude',
                  'Study [2] x Treatment [Categorize]',
                  'Study [2] x Treatment [Draw]')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


mns = epred_draws(mod.interest,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(df$study),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, article, treatment, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = mean(mn))

mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  mean_hdi()


# PD
mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  summarise(med = median(study2_study1),
            pd_neg = sum(study2_study1 < 0)/n(),
            pd_pos = sum(study2_study1 > 0)/n(),)

comp_interest = mns %>%
  pivot_wider(id_cols = c('.draw', 'treatment'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  select(treatment, .draw, study2_study1) %>%
  group_by(treatment) %>%
  mean_hdi()
comp_interest$outcome = 'interest'



# Combined contrast figure ----
comp = rbind(comp_rmse %>%
               select(treatment, study2_study1, .lower, .upper,
                      .width, .point, .interval, outcome)
             , 
             comp_interest,
             comp_recommend,
             comp_surprise)

comp$treatment = factor(comp$treatment,
                        levels=c('Control', 'Categorize', 'Draw'))

comp$outcome = factor(comp$outcome,
                        levels=c('recall RMSE', 'interest', 'recommend', 'surprise'))


plt.studies = ggplot(comp) +
  geom_hline(yintercept=0, linetype='dashed', color='black') +
  geom_linerange(aes(x=treatment, ymin=.lower, ymax=.upper),
                     alpha=.5) +
  geom_point(aes(x=treatment, y=study2_study1),
                 alpha=.5) +
  labs(y='Study 2 - Study 1') +
  #ylim(0, 1) +
  coord_flip() +
  facet_grid(~ outcome, scales='free') +
  theme_apa() +
  theme(axis.title.y = element_blank(),
        )
(plt.studies)
ggsave(plot=plt.studies,width=7,height=4,dpi=200,
       filename=sprintf(sprintf("%s/studies_h3.pdf", FIG_PATH)), useDingbats=FALSE)


# Change in attitude ----


agg = df %>%
  select(c(PID, study, treatment, 
           recall_rmse, recall_rmse.sc,
           article_recommend, 
           article_interest,
           article_surprise,
           topic_involvement, attitude_pre, attitude_post,
           att_elic_drug_overdose_opinion_pre, att_elic_drug_overdose_opinion_post,
           att_elic_combat_drug_priority_pre, att_elic_combat_drug_priority_post,
           att_elic_opinion_on_drug_legalization_pre, att_elic_opinion_on_drug_legalization_post)) %>%
  group_by(study, PID, treatment) %>%
  summarise(across(everything(), mean),
            .groups = 'drop') 
agg$attitude_change = agg$attitude_post - agg$attitude_pre
agg$attitude_change_of = as.ordered(agg$attitude_change)

agg$attitude_change_opinion = agg$att_elic_drug_overdose_opinion_post - agg$att_elic_drug_overdose_opinion_pre
agg$attitude_change_opinion_of = as.ordered(agg$attitude_change_opinion)

agg$attitude_change_combat = agg$att_elic_combat_drug_priority_post - agg$att_elic_combat_drug_priority_pre
agg$attitude_change_combat_of = as.ordered(agg$attitude_change_combat)

agg$attitude_change_legalize = agg$att_elic_opinion_on_drug_legalization_post - agg$att_elic_opinion_on_drug_legalization_pre
agg$attitude_change_legalize_of = as.ordered(agg$attitude_change_legalize)


## By individual question ----

### Opinion ----

model_id = 'attitude_change_opinion_0'
mod.attitude_change_opinion_0 <- brm(bf(attitude_change_opinion_of ~ study * treatment),
                             data=agg, 
                             family=cumulative("logit", threshold="flexible"),
                             backend = "cmdstanr", 
                             cores = parallel::detectCores() - 1, 
                             iter = 4000,
                             file = model_fit_path(model_id))
summary(mod.attitude_change_opinion_0)
pp_check(mod.attitude_change_opinion_0, ndraws=100)



mns = epred_draws(mod.attitude_change_opinion_0,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(agg$study)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = sum(w))

mns %>%
  pivot_wider(id_cols = c('treatment', '.draw'),
              names_from = 'study',
              values_from = 'mn')

comp = mns %>%
  pivot_wider(id_cols = c('treatment', '.draw'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  pivot_longer(study2_study1,
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(treatment, contrast, value) %>%
  group_by(treatment, contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(treatment, contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)




### Combat ----

model_id = 'attitude_change_combat_0'
mod.attitude_change_combat_0 <- brm(bf(attitude_change_combat_of ~ study * treatment),
                                     data=agg, 
                                     family=cumulative("logit", threshold="flexible"),
                                     backend = "cmdstanr", 
                                     cores = parallel::detectCores() - 1, 
                                     iter = 4000,
                                     file = model_fit_path(model_id))
summary(mod.attitude_change_combat_0)
pp_check(mod.attitude_change_combat_0, ndraws=100)

mns = epred_draws(mod.attitude_change_combat_0,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(agg$study)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = sum(w))


comp = mns %>%
  pivot_wider(id_cols = c('treatment', '.draw'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  pivot_longer(study2_study1,
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(treatment, contrast, value) %>%
  group_by(treatment, contrast) %>%
  mean_hdi()

# PD
comp %>%
  group_by(treatment, contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


### Legalize ----

model_id = 'attitude_change_legalize_0'
mod.attitude_change_legalize_0 <- brm(bf(attitude_change_legalize_of ~ study * treatment),
                                    data=agg, 
                                    family=cumulative("logit", threshold="flexible"),
                                    backend = "cmdstanr", 
                                    cores = parallel::detectCores() - 1, 
                                    iter = 4000,
                                    file = model_fit_path(model_id),
                                    control = list(adapt_delta = .95))
summary(mod.attitude_change_legalize_0)
pp_check(mod.attitude_change_legalize_0, ndraws=100)

mns = epred_draws(mod.attitude_change_legalize_0,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      study = levels(agg$study)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(study, treatment, .draw) %>%
  summarise(mn = sum(w))


comp = mns %>%
  pivot_wider(id_cols = c('treatment', '.draw'),
              names_from = 'study',
              values_from = 'mn') %>%
  mutate(study2_study1 = `Study 2` - `Study 1`) %>%
  pivot_longer(study2_study1,
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(treatment, contrast, value) %>%
  group_by(treatment, contrast) %>%
  mean_hdi()

# PD
comp %>%
  group_by(treatment, contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)

