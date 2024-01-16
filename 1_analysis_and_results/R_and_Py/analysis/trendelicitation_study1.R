library(tidyr)
library(dplyr)
library(brms)
library(tidybayes)
library(bayesplot)
library(loo)
library(emmeans)
library(psych)

library(ggplot2)
library(papaja)
library(cowplot)

# global theme for figures
source('analysis/figure_settings.R')

# model helper functions
source('analysis/model_helpers.R')

# paths for outputs
MOD_PATH = 'results_models/study1'
FIG_PATH = 'results_figures/study1'
TAB_PATH = 'results_tables/study1'


# Prepare data ----

df = read.csv('data_processed/study1/study1_tidy.csv')
df = subset(df, exclude == 0)
df$article = factor(df$article)
df$treatment = factor(df$treatment, 
                      levels=c('control', 'txt', 'visual'),
                      labels=c('Control', 'Categorize', 'Draw'))
df$article = factor(df$article,
                    levels=c('drugOverdose', 'population', 'opioids'),
                    labels=c('Drug overdoses', 'Drug-use disorder', 'Synthetic opioids'))

df = df %>% 
  rename(article_recommend = recall_Recommend_article,
         article_surprise = recall_Content_surprise,
         article_interest = recall_view_opinion)


## Recode responses ----

### topic involvement
df$topic_inv_drug_overdose_vlaue_opinion = recode(df$topic_inv_drug_overdose_vlaue_opinion,
       "Not at All" = 1,
       "A little" = 2,
       "Moderately" = 3,
       "A lot" = 4,
       "Extremely" = 5)

df$topic_inv_defend_view_point_drugOverdose = recode(df$topic_inv_defend_view_point_drugOverdose,
       "Not at All" = 1,
       "A little" = 2,
       "Moderately" = 3,
       "A lot" = 4,
       "Extremely" = 5)

df$topic_inv_learning_drug_overdose = recode(df$topic_inv_learning_drug_overdose,
                                                     "Not at All" = 1,
                                                     "A little" = 2,
                                                     "Moderately" = 3,
                                                     "A lot" = 4,
                                                     "Extremely" = 5)

df$topic_inv_motivation_gaining_knowledge = recode(df$topic_inv_motivation_gaining_knowledge,
                                                     "Not at All" = 1,
                                                     "A little" = 2,
                                                     "Moderately" = 3,
                                                     "A lot" = 4,
                                                     "Extremely" = 5)


df$topic_involvement = rowSums(select(df, starts_with('topic_')))


### Attitude ----
df$att_elic_drug_overdose_opinion_pre = recode(df$att_elic_drug_overdose_opinion_pre,
                                               "Not at all a problem" = 1,
                                               "Minor Problem" = 2,
                                               "Moderate problem" = 3,
                                               "Serious problem" = 4,
                                               "Extremely serious problem" = 5)

df$att_elic_combat_drug_priority_pre = recode(df$att_elic_combat_drug_priority_pre,
                                                       "Not a Priority" = 1,
                                                       "Low Priority" = 2,
                                                       "Neutral" = 3,
                                                       "Moderate Priority" = 4,
                                                       "High Priority" = 5)

df$att_elic_opinion_on_drug_legalization_pre = recode(df$att_elic_opinion_on_drug_legalization_pre,
                                                      "Strongly Oppose" = 1,
                                                      "Somewhat Oppose" = 2,
                                                      "Neutral" = 3,
                                                      "Somewhat Favor" = 4,
                                                      "Strongly Favor" = 5)

df$att_elic_drug_overdose_opinion_post = recode(df$att_elic_drug_overdose_opinion_post,
                                                       "Not at all a problem" = 1,
                                                       "Minor Problem" = 2,
                                                       "Moderate problem" = 3,
                                                       "serious problem" = 4,
                                                       "Extremely serious problem" = 5)

df$att_elic_combat_drug_priority_post = recode(df$att_elic_combat_drug_priority_post,
                                                      "Not a Priority" = 1,
                                                      "Low Priority" = 2,
                                                      "Neutral" = 3,
                                                      "Moderate Priority" = 4,
                                                      "High Priority" = 5)

df$att_elic_opinion_on_drug_legalization_post = recode(df$att_elic_opinion_on_drug_legalization_post,
                                                              "Strongly Oppose" = 1,
                                                              "Somewhat Oppose" = 2,
                                                              "Neutral" = 3,
                                                              "Somewhat Favor" = 4,
                                                              "Strongly Favor" = 5)

df$attitude_pre = rowSums(select(df, starts_with('att_elic') & ends_with('_pre')))
df$attitude_post = rowSums(select(df, starts_with('att_elic') & ends_with('_post')))



### Engagement questions ----
df$article_recommend = recode(df$article_recommend,
                                               "Not at All" = 1,
                                               "A little" = 2,
                                               "Moderately" = 3,
                                               "A lot" = 4,
                                               "Extremely" = 5)

df$article_surprise = recode(df$article_surprise,
                                              "Not at All" = 1,
                                              "A little" = 2,
                                              "Moderately" = 3,
                                              "A lot" = 4,
                                              "Extremely" = 5)

df$article_interest = recode(df$article_interest,
                                          "Not at All" = 1,
                                          "A little" = 2,
                                          "Moderately" = 3,
                                          "A lot" = 4,
                                          "Extremely" = 5)



## Overall recall responses ----

# get overall errors and add to df
df_errors = read.csv('data_processed/study1/study1_complete_errors.csv')
df_errors = subset(df_errors, exclude == 0)
df_errors$treatment = factor(df_errors$treatment, 
                      levels=c('control', 'txt', 'visual'),
                      labels=c('Control', 'Categorize', 'Draw'))
df_errors$article = factor(df_errors$article,
                           levels=c('drugOverdose', 'population', 'opioids'),
                           labels=c('Drug overdoses', 'Drug-use disorder', 'Synthetic opioids'))

df = left_join(df,
          df_errors %>%
            select(c('PID', 'article', 
                     'recall_rmse', 'recall_signed_err',
                     'vis_elic_rmse', 'vis_elic_signed_err')),
          by=c('PID', 'article'))

# rescale overall errors
df$recall_rmse.sc = NA
df[df$article %in% c('Drug overdoses', 'Synthetic opioids'), 'recall_rmse.sc'] = df[df$article %in% c('Drug overdoses', 'Synthetic opioids'), 'recall_rmse']/80000
df[df$article %in% c('Drug-use disorder'), 'recall_rmse.sc'] = df[df$article %in% c('Drug-use disorder'), 'recall_rmse']/10

df$vis_elic_rmse.sc = NA
df[df$article %in% c('Drug overdoses', 'Synthetic opioids'), 'vis_elic_rmse.sc'] = df[df$article %in% c('Drug overdoses', 'Synthetic opioids'), 'vis_elic_rmse']/80000
df[df$article %in% c('Drug-use disorder'), 'vis_elic_rmse.sc'] = df[df$article %in% c('Drug-use disorder'), 'vis_elic_rmse']/10

write.csv(df, 'data_processed/study1/study1_by_article.csv')



## Yearly recall responses ----

df_years = read.csv('data_processed/study1/recall_accuracy_yearly_study1.csv')
df_years = subset(df_years, exclude == 0)
df_years$treatment = factor(df_years$treatment, 
                             levels=c('control', 'txt', 'visual'),
                             labels=c('Control', 'Categorize', 'Draw'))

df_years = df_years %>%
  select(PID, treatment, article, year, true_value, recall_value,
         recall_accuracy_abs, recall_accuracy_signed)

df_years$recall_accuracy_abs.sc = NA
df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_accuracy_abs.sc'] = df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_accuracy_abs']/80000
df_years[df_years$article %in% c('population'), 'recall_accuracy_abs.sc'] = df_years[df_years$article %in% c('population'), 'recall_accuracy_abs']/10

df_years$recall_accuracy_signed.sc = NA
df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_accuracy_signed.sc'] = df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_accuracy_signed']/80000
df_years[df_years$article %in% c('population'), 'recall_accuracy_signed.sc'] = df_years[df_years$article %in% c('population'), 'recall_accuracy_signed']/10

df_years$recall_value.sc = NA
df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_value.sc'] = df_years[df_years$article %in% c('drugOverdose', 'opioids'), 'recall_value']/80000
df_years[df_years$article %in% c('population'), 'recall_value.sc'] = df_years[df_years$article %in% c('population'), 'recall_value']/10

df_years$article = factor(df_years$article,
                          levels=c('drugOverdose', 'population', 'opioids'),
                          labels=c('Drug overdoses', 'Drug-use disorder', 'Synthetic opioids'))

df_years_actual = df_years %>%
  select(article, year, true_value) %>%
  group_by(article, year) %>%
  summarise(true_value = mean(true_value))

df_years_actual$true_value.sc = NA
df_years_actual[df_years_actual$article %in% c('Drug overdoses', 'Synthetic opioids'), 'true_value.sc'] = df_years_actual[df_years_actual$article %in% c('Drug overdoses', 'Synthetic opioids'), 'true_value']/80000
df_years_actual[df_years_actual$article %in% c('Drug-use disorder'), 'true_value.sc'] = df_years_actual[df_years_actual$article %in% c('Drug-use disorder'), 'true_value']/10




# Sample characteristics ----

pdata = df %>%
  group_by(PID) %>%
  filter(row_number()==1) %>%
  select(c('PID', 'treatment', 'gender',
            'age', 'race', 'education'))

# Number of people in each condition
table(pdata$treatment)

# age
describe(pdata$age) %>% 
  select(c(mean, sd, min, max))

# demographics
table(pdata$gender)
table(pdata$race)
table(pdata$education)



# Recall accuracy ----

ggplot(df) +
  geom_histogram(aes(x=recall_rmse.sc, 
                     fill=treatment),
                 position='identity', alpha=.7) +
  facet_grid(treatment ~ article, scales='free')

## Models

### Model 0: Treatment + article
model_id = 'recall_rmse_combined_0'
mod.recall_rmse_combined_0 = brm(bf(recall_rmse.sc ~ treatment + article +
                                 (1|PID)),
                            family=lognormal(),
                            data=df,
                            backend = "cmdstanr", 
                            cores = parallel::detectCores() - 1, 
                            iter=5000,
                            file = model_fit_path(model_id))
summary(mod.recall_rmse_combined_0)
pp_check(mod.recall_rmse_combined_0, ndraws=100)


### Model 1: Add topic involvement and attitude
model_id = 'recall_rmse_combined_1'
mod.recall_rmse_combined_1 = brm(bf(recall_rmse.sc ~ treatment + article +
                                      scale(topic_involvement) + 
                                      scale(attitude_pre) +
                                      (1|PID)),
                                 family=lognormal(),
                                 data=df,
                                 backend = "cmdstanr", 
                                 cores = parallel::detectCores() - 1, 
                                 iter=5000,
                                 file = model_fit_path(model_id))
summary(mod.recall_rmse_combined_1)
pp_check(mod.recall_rmse_combined_1, ndraws=100)


### Model comparison
loo0 = loo(mod.recall_rmse_combined_0)
loo1 = loo(mod.recall_rmse_combined_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + article + topicInolvement + preAttitude',
  'Model 0: treatment + article'
)
write.csv(comparison_results, 
          sprintf("%s/recall_rmse_model_comparison.csv", MOD_PATH))



### Parameters and contrasts
model_id = 'recall_rmse_combined_1'
mod.recall_rmse_combined = mod.recall_rmse_combined_1

# save coefficients
tab = as.data.frame(summary(mod.recall_rmse_combined)$fixed)
rownames(tab) = c('Intercept', 
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))

# Contrasts between treatments, by article
mod.recall_rmse_combined %>%
  emmeans(~ treatment + article,
          var = "treatment",
          by = "article",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise") %>%
  gather_emmeans_draws() %>%
  median_hdi() %>% 
  as.data.frame()

# Contrasts between treatments
eff.recall_rmse = mod.recall_rmse_combined %>%
  emmeans(~ treatment,
          var = "treatment",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise") %>%
  gather_emmeans_draws() %>%
  median_hdi() %>% 
  as.data.frame()
eff.recall_rmse$study = 'Study 1'
write.csv(eff.recall_rmse, file=sprintf("%s/recall_contrasts.csv", TAB_PATH))


eff.recall_rmse_draws = mod.recall_rmse_combined %>%
  emmeans(~ treatment,
          var = "treatment",
          epred = TRUE, re_formula = NA) %>%
  contrast(method="revpairwise") %>%
  gather_emmeans_draws()

plt.eff_rmse = ggplot(eff.recall_rmse_draws) +
  geom_hline(yintercept = 0, color='gray', linetype='dashed') +
  stat_pointinterval(aes(x=contrast, y=.value)) +
  coord_flip() +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
(plt.eff_rmse)
ggsave(plot=plt.eff_rmse,width=6,height=2,dpi=200,
       filename=sprintf(sprintf("%s/model_recall_contrasts.pdf", FIG_PATH)), useDingbats=FALSE)



# Recall accuracy by year ----

## Recall responses by year ----

plt.recall_by_year = ggplot() +
  geom_line(data=df_years_actual,
            aes(x=year, y=true_value.sc), 
            color='black',
            alpha=1) +
  stat_summary(data=df_years,
               geom="line",
               aes(x=year, y=recall_value.sc, color=treatment), 
               alpha=1) +
  stat_summary(data=df_years,
               geom="ribbon",
               aes(x=year, y=recall_value.sc, fill=treatment), 
               alpha=.2) +
  facet_grid(~ article) +
  theme_apa() +
  theme(text = element_text(size=10))
(plt.recall_by_year)
ggsave(plot=plt.recall_by_year,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/recall_yearly.pdf", FIG_PATH)), useDingbats=FALSE)


## Recall responses by year with individual lines ----

plt.recall_by_year_ind = ggplot() +
  geom_line(data=df_years,
            aes(x=year, y=recall_value.sc, color=treatment, group=PID),
            alpha=.1) +
  stat_summary(data=df_years,
               geom="line",
               aes(x=year, y=recall_value.sc, color=treatment), 
               alpha=1) +
  stat_summary(data=df_years,
               geom="ribbon",
               aes(x=year, y=recall_value.sc, fill=treatment), 
               alpha=.2) +
  geom_line(data=df_years_actual,
            aes(x=year, y=true_value.sc), 
            color='black',
            alpha=1) +
  facet_grid(treatment ~ article) +
  theme_apa() +
  theme(text = element_text(size=10),
        legend.position = 'bottom',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
(plt.recall_by_year_ind)
ggsave(plot=plt.recall_by_year_ind,width=8,height=8,dpi=200,
       filename=sprintf(sprintf("%s/recall_yearly_with_individuals.pdf", FIG_PATH)), useDingbats=FALSE)
save(plt.recall_by_year_ind, file = sprintf("%s/recall_yearly_with_individuals.rdata", FIG_PATH))



## Signed error by year ----

plt.signederr_by_year = ggplot() +
  stat_summary(data=df_years,
               geom="line",
               aes(x=year, y=recall_accuracy_signed.sc, color=treatment), 
               alpha=1) +
  stat_summary(data=df_years,
               geom="ribbon",
               aes(x=year, y=recall_accuracy_signed.sc, fill=treatment), 
               alpha=.2) +
  facet_grid(~ article) +
  theme_apa() +
  theme(text = element_text(size=10))
(plt.signederr_by_year)
ggsave(plot=plt.signederr_by_year,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/recall_err_signed_yearly.pdf", FIG_PATH)), useDingbats=FALSE)


## Absolute error by year ----

plt.abserr_by_year = ggplot() +
  stat_summary(data=df_years,
               geom="line",
               aes(x=year, y=recall_accuracy_abs.sc, color=treatment), 
               alpha=1) +
  stat_summary(data=df_years,
               geom="ribbon",
               aes(x=year, y=recall_accuracy_abs.sc, fill=treatment), 
               alpha=.2) +
  facet_grid(~ article) +
  theme_apa() +
  theme(text = element_text(size=10))
(plt.abserr_by_year)
ggsave(plot=plt.abserr_by_year,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/recall_err_abs_yearly.pdf", FIG_PATH)), useDingbats=FALSE)


# Article engagement measures ----

## Surprise ----

### Figure ----

props = df %>%
  group_by(treatment, article, article_surprise) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))

props_surprise = props
props_surprise$question = 'surprise'
props_surprise$response = props_surprise$article_surprise

plt.surprise = ggplot(props) +
  geom_bar(aes(x=article_surprise, y=freq, 
               color=treatment, fill=treatment),
           linewidth=0, alpha=1,
           stat="identity", position=position_dodge()) +
  facet_grid(~ article, scales='free') +
  labs(x='surprise', y='relative frequency')
(plt.surprise)
ggsave(plot=plt.surprise,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/article_surprise.pdf", FIG_PATH)), useDingbats=FALSE)

ggplot(props) +
  geom_line(aes(x=article_surprise, y=cumfreq, color=treatment)) +
  geom_point(aes(x=article_surprise, y=cumfreq, color=treatment)) +
  facet_grid(~ article, scales='free')


### Models ----

#### Model 0: Just treatment + article
model_id = 'surprise_0'
mod.surprise_0 <- brm(bf(article_surprise ~ 1 + treatment + article +
                           (1|PID)),
                      data=df, 
                      family=cumulative(link="logit", threshold="flexible"), 
                      backend = "cmdstanr", 
                      cores = parallel::detectCores() - 1, 
                      iter=5000,
                      save_pars = save_pars(all = TRUE),
                      file = model_fit_path(model_id))
summary(mod.surprise_0)

#### Model 1: Add topic involvement and attitude
model_id = 'surprise_1'
mod.surprise_1 <- brm(bf(article_surprise ~ 1 + treatment + article +
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

#### Model comparison
loo0 = loo(mod.surprise_0)
loo1 = loo(mod.surprise_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + article + topicInolvement + preAttitude',
  'Model 0: treatment + article'
)
write.csv(comparison_results, 
          sprintf("%s/surprise_model_comparison.csv", MOD_PATH))


#### Parameters
model_id = 'surprise_1'
mod.surprise = mod.surprise_1

# save coefficients
tab = as.data.frame(summary(mod.surprise)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]', 
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


epred_draws(mod.surprise,
            newdata=expand_grid(treatment = levels(df$treatment),
                                article = levels(df$article),
                                topic_involvement = mean(df$topic_involvement),
                                attitude_pre = mean(df$attitude_pre)),
            re_formula = NA) %>%
  ungroup()


mns = epred_draws(mod.surprise,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, article, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(treatment, .draw) %>%
  summarise(mn = mean(mn))


comp = mns %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


## Recommend ----

### Figure ----

props = df %>%
  group_by(treatment, article, article_recommend) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))

props_recommend = props
props_recommend$question = 'recommend'
props_recommend$response = props_recommend$article_recommend

plt.recommend = ggplot(props) +
  geom_bar(aes(x=article_recommend, y=freq, 
               color=treatment, fill=treatment),
           linewidth=0, alpha=1,
           stat="identity", position=position_dodge()) +
  facet_grid(~ article, scales='free') +
  labs(x='recommend', y='relative frequency') +
  theme_apa() +
  theme(text = element_text(size=11))
(plt.recommend)
ggsave(plot=plt.recommend,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/article_recommend.pdf", FIG_PATH)), useDingbats=FALSE)

# cumulative
ggplot(props) +
  geom_line(aes(x=article_recommend, y=cumfreq, color=treatment)) +
  geom_point(aes(x=article_recommend, y=cumfreq, color=treatment)) +
  facet_grid(~ variable, scales='free')


### Models ----

#### Model 0: Treatment + article
model_id = 'recommend_0'
mod.recommend_0 <- brm(bf(article_recommend ~ treatment + article +
                            (1|PID)),
           data=df, 
           family=cumulative(link="logit", threshold="flexible"), 
           backend = "cmdstanr", 
           cores = parallel::detectCores() - 1, 
           iter=5000,
           file = model_fit_path(model_id))
summary(mod.recommend_0)
pp_check(mod.recommend_0, ndraws=100)

#### Model 1: Add topic involvement and attitude
model_id = 'recommend_1'
mod.recommend_1 <- brm(bf(article_recommend ~ 1 + treatment + article +
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


#### Model comparison
loo0 = loo(mod.recommend_0)
loo1 = loo(mod.recommend_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + article + topicInolvement + preAttitude',
  'Model 0: treatment + article'
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
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))



mns = epred_draws(mod.recommend,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, article, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(treatment, .draw) %>%
  summarise(mn = mean(mn))


comp = mns %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


## Interest ----

### Figure ----

props = df %>%
  group_by(treatment, article, article_interest) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))

props_interest = props
props_interest$question = 'interest'
props_interest$response = props_interest$article_interest


plt.interest = ggplot(props) +
  geom_bar(aes(x=article_interest, y=freq, 
               color=treatment, fill=treatment),
           linewidth=0, alpha=1,
           stat="identity", position=position_dodge()) +
  facet_grid(~ article, scales='free') +
  labs(x='interest', y='relative frequency') +
  theme_apa() +
  theme(text = element_text(size=11))
(plt.interest)
ggsave(plot=plt.interest,width=6,height=3,dpi=200,
       filename=sprintf(sprintf("%s/article_interest.pdf", FIG_PATH)), useDingbats=FALSE)

# cumulative
ggplot(props) +
  geom_line(aes(x=article_interest, y=cumfreq, color=treatment)) +
  geom_point(aes(x=article_interest, y=cumfreq, color=treatment)) +
  facet_grid(~ article, scales='free')


### Models ----

#### Model 0: Treatment + article
model_id = 'interest_0'
mod.interest_0 <- brm(bf(article_interest ~ treatment + article +
                (1|PID)),
           data=df, 
           family=cumulative(link="logit", threshold="flexible"), 
           backend = "cmdstanr", 
           cores = parallel::detectCores() - 1, 
           iter=5000,
           file = model_fit_path(model_id))
summary(mod.interest_0)

#### Model 1: Add topic involvement and attitude
model_id = 'interest_1'
mod.interest_1 <- brm(bf(article_interest ~ 1 + treatment + article +
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
summary(mod.interest_1)

#### Model comparison
loo0 = loo(mod.interest_0)
loo1 = loo(mod.interest_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + article + topicInolvement + preAttitude',
  'Model 0: treatment + article'
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
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Article [Drug use disorder]',
                  'Article [Synthetic opioids]',
                  'Topic involvement',
                  'Pre-Attitude')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


mns = epred_draws(mod.interest,
                  newdata=expand_grid(treatment = levels(df$treatment),
                                      article = levels(df$article),
                                      topic_involvement = mean(df$topic_involvement),
                                      attitude_pre = mean(df$attitude_pre)),
                  re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, article, .draw) %>%
  summarise(mn = sum(w)) %>%
  ungroup() %>%
  group_by(treatment, .draw) %>%
  summarise(mn = mean(mn))


comp = mns %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


## Combined figure ----

props = rbind(props_surprise, props_recommend, props_interest)

plt.engagement = ggplot(props) +
  geom_bar(aes(x=response, y=freq, 
               color=treatment, fill=treatment),
           linewidth=0, alpha=1,
           stat="identity", position=position_dodge()) +
  facet_grid(question ~ article, scales='free') +
  labs(x='rating', y='relative frequency') +
  theme_apa() +
  theme(text = element_text(size=11),
        legend.position = 'bottom')
(plt.engagement)
ggsave(plot=plt.engagement,width=6,height=6,dpi=200,
       filename=sprintf(sprintf("%s/article_responses.pdf", FIG_PATH)), useDingbats=FALSE)



plt.engagement_cumulative = ggplot(props) +
  geom_line(aes(x=response, y=cumfreq, color=treatment)) +
  geom_point(aes(x=response, y=cumfreq, color=treatment)) +
  facet_grid(question ~ article, scales='free') +
  labs(x='rating', y='cumulative frequency') +
  theme_apa() +
  theme(text = element_text(size=11),
        legend.position = 'bottom')
(plt.engagement_cumulative)
ggsave(plot=plt.engagement_cumulative,width=6,height=6,dpi=200,
       filename=sprintf(sprintf("%s/article_responses_cumulative.pdf", FIG_PATH)), useDingbats=FALSE)


## Aggregated figure ----

props_surprise = df %>%
  group_by(treatment, article_surprise) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))
props_surprise$question = 'surprise'
props_surprise$response = props_surprise$article_surprise

props_recommend = df %>%
  group_by(treatment, article_recommend) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))
props_recommend$question = 'recommend'
props_recommend$response = props_recommend$article_recommend

props_interest = df %>%
  group_by(treatment, article_interest) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(cumfreq = cumsum(freq))
props_interest$question = 'interest'
props_interest$response = props_interest$article_interest

props = rbind(props_surprise, props_recommend, props_interest)

plt.engagement_agg = ggplot(props) +
  geom_bar(aes(x=response, y=freq, 
               color=treatment, fill=treatment),
           linewidth=0, alpha=1,
           stat="identity", position=position_dodge()) +
  facet_grid(~ question, scales='free') +
  labs(x='rating', y='relative frequency') +
  theme_apa() +
  theme(text = element_text(size=11),
        legend.position = 'bottom')
(plt.engagement_agg)
ggsave(plot=plt.engagement_agg,width=6,height=4,dpi=200,
       filename=sprintf(sprintf("%s/article_responses_agg.pdf", FIG_PATH)), useDingbats=FALSE)

plt.engagement_agg_cumulative = ggplot(props) +
  geom_line(aes(x=response, y=cumfreq, color=treatment)) +
  geom_point(aes(x=response, y=cumfreq, color=treatment)) +
  facet_grid(~ question, scales='free') +
  labs(x='rating', y='cumulative frequency') +
  theme_apa() +
  theme(text = element_text(size=11),
        legend.position = 'bottom')
(plt.engagement_agg_cumulative)
ggsave(plot=plt.engagement_agg_cumulative,width=6,height=4,dpi=200,
       filename=sprintf(sprintf("%s/article_responses_agg_cumulative.pdf", FIG_PATH)), useDingbats=FALSE)




# Change in attitude ----

## Prepare data ----

agg = df %>%
  select(c(PID, treatment, 
           recall_rmse.sc,
           article_recommend, 
           article_surprise,
           article_interest,
           topic_involvement, 
           attitude_pre, attitude_post,
           att_elic_drug_overdose_opinion_pre, att_elic_drug_overdose_opinion_post,
           att_elic_combat_drug_priority_pre, att_elic_combat_drug_priority_post,
           att_elic_opinion_on_drug_legalization_pre, att_elic_opinion_on_drug_legalization_post)) %>%
  group_by(PID, treatment) %>%
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

agg$study = 'Study 1'
write.csv(agg, 'data_processed/study1/study1_attitude_change.csv')

# for pre/post models
agg2 = agg |>
  select(PID, treatment, 
         topic_involvement, 
         article_interest,
         article_recommend,
         article_surprise,
         starts_with("att_elic")) |>
  pivot_longer(cols = starts_with("att_elic"), 
               names_to = c("question", "time"), 
               names_pattern = "att_elic_(.*)_(p.*)", 
               values_to = "rating") |>
  mutate(time = factor(time, levels = c("pre", "post")))


## By individual question ----

### Opinion ----

model_id = 'attitude_change_opinion_0'
mod.attitude_change_opinion_0 <- brm(bf(attitude_change_opinion_of ~ treatment),
                                     data=agg, 
                                     family=cumulative("logit", threshold="flexible"),
                                     backend = "cmdstanr", 
                                     cores = parallel::detectCores() - 1, 
                                     iter = 4000,
                                     file = model_fit_path(model_id),
                                     control = list(adapt_delta = .95))
summary(mod.attitude_change_opinion_0)
pp_check(mod.attitude_change_opinion_0, ndraws=100)

model_id = 'attitude_change_opinion_1'
mod.attitude_change_opinion_1 <- brm(bf(attitude_change_opinion_of ~ treatment + 
                                          scale(topic_involvement) +
                                          scale(article_interest) + 
                                          scale(article_recommend) +
                                          scale(article_surprise)),
                                     data=agg, 
                                     family=cumulative("logit", threshold="flexible"),
                                     backend = "cmdstanr", 
                                     cores = parallel::detectCores() - 1, 
                                     iter = 4000,
                                     file = model_fit_path(model_id),
                                     control = list(adapt_delta = .95))
summary(mod.attitude_change_opinion_1)
pp_check(mod.attitude_change_opinion_1, ndraws=100)


#### Model comparison
loo0 = loo(mod.attitude_change_opinion_0)
loo1 = loo(mod.attitude_change_opinion_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + topicInvolvement + articleInterest + articleRecommend + articleSurprise',
  'Model 0: treatment'
)
write.csv(comparison_results, 
          sprintf("%s/attitude_change_opinion_model_comparison.csv", MOD_PATH))

### Parameters
model_id = 'attitude_change_opinion_1'
mod.attitude_change_opinion = mod.attitude_change_opinion_1

# save coefficients
tab = as.data.frame(summary(mod.attitude_change_opinion)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]', 
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Topic Involvement',
                  'Article Interest',
                  'Article Recommend',
                  'Article Surprise')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))


ep.attitude_change_opinion = epred_draws(mod.attitude_change_opinion,
                    newdata=expand_grid(treatment = levels(df$treatment),
                                        topic_involvement = mean(agg$topic_involvement),
                                        article_interest = mean(agg$article_interest),
                                        article_recommend = mean(agg$article_recommend),
                                        article_surprise = mean(agg$article_surprise)),
                    re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, .draw) %>%
  summarise(mn = sum(w))

mn.attitude_change_opinion = ep.attitude_change_opinion %>%
  mean_hdi() %>%
  mutate(question = 'opinion') %>%
  as.data.frame()

fe.attitude_change_opinion = as.data.frame(fixef(mod.attitude_change_opinion_1))
fe.attitude_change_opinion$predictor = rownames(fe.attitude_change_opinion)
fe.attitude_change_opinion$question = 'opinion'

#### Treatment contrasts
comp = ep.attitude_change_opinion %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


### Combat ----

model_id = 'attitude_change_combat_0'
mod.attitude_change_combat_0 <- brm(bf(attitude_change_combat_of ~ treatment),
                                    data=agg, 
                                    family=cumulative("logit", threshold="flexible"),
                                    backend = "cmdstanr", 
                                    cores = parallel::detectCores() - 1, 
                                    iter = 4000,
                                    file = model_fit_path(model_id),
                                    control = list(adapt_delta = .95))
summary(mod.attitude_change_combat_0)
pp_check(mod.attitude_change_combat_0, ndraws=100)

model_id = 'attitude_change_combat_1'
mod.attitude_change_combat_1 <- brm(bf(attitude_change_combat_of ~ treatment + 
                                         scale(topic_involvement) +
                                         scale(article_interest) + 
                                         scale(article_recommend) +
                                         scale(article_surprise)),
                                    data=agg, 
                                    family=cumulative("logit", threshold="flexible"),
                                    backend = "cmdstanr", 
                                    cores = parallel::detectCores() - 1, 
                                    iter = 4000,
                                    file = model_fit_path(model_id),
                                    control = list(adapt_delta = .95))
summary(mod.attitude_change_combat_1)
pp_check(mod.attitude_change_combat_1, ndraws=100)

#### Model comparison
loo0 = loo(mod.attitude_change_combat_0)
loo1 = loo(mod.attitude_change_combat_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + topicInvolvement + articleInterest + articleRecommend + articleSurprise',
  'Model 0: treatment'
)
write.csv(comparison_results, 
          sprintf("%s/attitude_change_combat_model_comparison.csv", MOD_PATH))


### Parameters
model_id = 'attitude_change_combat_1'
mod.attitude_change_combat = mod.attitude_change_combat_1

# save coefficients
tab = as.data.frame(summary(mod.attitude_change_combat)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]', 
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Topic Involvement',
                  'Article Interest',
                  'Article Recommend',
                  'Article Surprise')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))

ep.attitude_change_combat = epred_draws(mod.attitude_change_combat,
                                         newdata=expand_grid(treatment = levels(df$treatment),
                                                             topic_involvement = mean(agg$topic_involvement),
                                                             article_interest = mean(agg$article_interest),
                                                             article_recommend = mean(agg$article_recommend),
                                                             article_surprise = mean(agg$article_surprise)),
                                         re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, .draw) %>%
  summarise(mn = sum(w))

mn.attitude_change_combat = ep.attitude_change_combat %>%
  mean_hdi() %>%
  mutate(question = 'combat') %>%
  as.data.frame()


fe.attitude_change_combat = as.data.frame(fixef(mod.attitude_change_combat_1))
fe.attitude_change_combat$predictor = rownames(fe.attitude_change_combat)
fe.attitude_change_combat$question = 'combat'


#### Treatment contrasts
comp = ep.attitude_change_combat %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


### Legalize ----

model_id = 'attitude_change_legalize_0'
mod.attitude_change_legalize_0 <- brm(bf(attitude_change_legalize_of ~ treatment),
                                      data=agg, 
                                      family=cumulative("logit", threshold="flexible"),
                                      backend = "cmdstanr", 
                                      cores = parallel::detectCores() - 1, 
                                      iter = 4000,
                                      file = model_fit_path(model_id),
                                      control = list(adapt_delta = .99))
summary(mod.attitude_change_legalize_0)
pp_check(mod.attitude_change_legalize_0, ndraws=100)

model_id = 'attitude_change_legalize_1'
mod.attitude_change_legalize_1 <- brm(bf(attitude_change_legalize_of ~ treatment + 
                                           scale(topic_involvement) +
                                           scale(article_interest) + 
                                           scale(article_recommend) +
                                           scale(article_surprise)),
                                      data=agg, 
                                      family=cumulative("logit", threshold="flexible"),
                                      backend = "cmdstanr", 
                                      cores = parallel::detectCores() - 1, 
                                      iter = 4000,
                                      file = model_fit_path(model_id),
                                      control = list(adapt_delta = .99))
summary(mod.attitude_change_legalize_1)
pp_check(mod.attitude_change_legalize_1, ndraws=100)

#### Model comparison
loo0 = loo(mod.attitude_change_legalize_0)
loo1 = loo(mod.attitude_change_legalize_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))
rownames(comparison_results) = c(
  'Model 1: treatment + topicInvolvement + articleInterest + articleRecommend + articleSurprise',
  'Model 0: treatment'
)
write.csv(comparison_results, 
          sprintf("%s/attitude_change_legalize_model_comparison.csv", MOD_PATH))

### Parameters
model_id = 'attitude_change_legalize_1'
mod.attitude_change_legalize = mod.attitude_change_legalize_1

# save coefficients
tab = as.data.frame(summary(mod.attitude_change_legalize)$fixed)
rownames(tab) = c('Intercept[1]', 
                  'Intercept[2]', 
                  'Intercept[3]', 
                  'Intercept[4]', 
                  'Intercept[5]',
                  'Treatment [Categorize]',
                  'Treatment [Draw]',
                  'Topic Involvement',
                  'Article Interest',
                  'Article Recommend',
                  'Article Surprise')
write.csv(tab, sprintf("%s/%s_fixef.csv", 
                       model_results_dir(model_id), 
                       model_id))

ep.attitude_change_legalize = epred_draws(mod.attitude_change_legalize,
                                        newdata=expand_grid(treatment = levels(df$treatment),
                                                            topic_involvement = mean(agg$topic_involvement),
                                                            article_interest = mean(agg$article_interest),
                                                            article_recommend = mean(agg$article_recommend),
                                                            article_surprise = mean(agg$article_surprise)),
                                        re_formula = NA) %>%
  ungroup() %>%
  mutate(w = as.numeric(as.character(.category)) * .epred) %>%
  group_by(treatment, .draw) %>%
  summarise(mn = sum(w))

mn.attitude_change_legalize = ep.attitude_change_legalize %>%
  mean_hdi() %>%
  mutate(question = 'legalize') %>%
  as.data.frame()

plt.attitude_change = ggplot(ep.attitude_change_legalize) +
  stat_pointinterval(aes(x=treatment, y=mn, 
                         color=treatment, fill=treatment),
                     alpha=.5) +
  labs(y='Attitude change (post - pre)') +
  #ylim(0, 1) +
  theme_apa() +
  theme(axis.title.x = element_blank())
(plt.attitude_change)

fe.attitude_change_legalize = as.data.frame(fixef(mod.attitude_change_legalize_1))
fe.attitude_change_legalize$predictor = rownames(fe.attitude_change_legalize)
fe.attitude_change_legalize$question = 'legalize'


#### Treatment contrasts
comp = ep.attitude_change_legalize %>%
  pivot_wider(id_cols = '.draw',
              names_from = 'treatment',
              values_from = 'mn') %>%
  mutate(Draw_Control = Draw - Control,
         Draw_Categorize = Draw - Categorize,
         Categorize_Control = Categorize - Control) %>%
  pivot_longer(c(Draw_Control, Draw_Categorize, Categorize_Control),
               names_to = "contrast",
               values_to = "value")

comp %>%
  select(contrast, value) %>%
  group_by(contrast) %>%
  median_hdi()

# PD
comp %>%
  group_by(contrast) %>%
  summarise(med = median(value),
            pd_neg = sum(value < 0)/n(),
            pd_pos = sum(value > 0)/n(),)


# combined means
mn.attitude_change = rbind(mn.attitude_change_opinion,
      mn.attitude_change_combat,
      mn.attitude_change_legalize)
mn.attitude_change$study = 'Study 1'
write.csv(mn.attitude_change, file=sprintf("%s/attitude_change_means.csv", TAB_PATH))


# combined fixed effects
fe.attitude_change = rbind(fe.attitude_change_opinion,
                           fe.attitude_change_combat,
                           fe.attitude_change_legalize)
fe.attitude_change$study = 'Study 2'
rownames(fe.attitude_change) = seq(1, nrow(fe.attitude_change))
write.csv(fe.attitude_change, file=sprintf("%s/attitude_change_fixef.csv", TAB_PATH))


## By individual question (pre/post) ----

### Opinion ----

model_id = 'attitude_change_prepost_opinion_0'
mod.attitude_change_prepost_opinion_0 <- 
  brm(bf(rating ~ time * treatment + 
           (1 | PID)),
      data=subset(agg2, question == "drug_overdose_opinion"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
      #control = list(adapt_delta = .95))
summary(mod.attitude_change_prepost_opinion_0)
pp_check(mod.attitude_change_prepost_opinion_0, ndraws=100)

model_id = 'attitude_change_prepost_opinion_1'
mod.attitude_change_prepost_opinion_1 <- 
  brm(bf(rating ~ time * treatment + 
           time * scale(topic_involvement) +
           time * scale(article_interest) + 
           time * scale(article_recommend) +
           time * scale(article_surprise) +
           (1 | PID)),
      data=subset(agg2, question == "drug_overdose_opinion"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
summary(mod.attitude_change_prepost_opinion_1)
pp_check(mod.attitude_change_prepost_opinion_1, ndraws=100)

loo0 = loo(mod.attitude_change_prepost_opinion_0)
loo1 = loo(mod.attitude_change_prepost_opinion_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))


### Combat ----

model_id = 'attitude_change_prepost_combat_0'
mod.attitude_change_prepost_combat_0 <- 
  brm(bf(rating ~ time * treatment + 
           (1 | PID)),
      data=subset(agg2, question == "combat_drug_priority"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
#control = list(adapt_delta = .95))
summary(mod.attitude_change_prepost_combat_0)
pp_check(mod.attitude_change_prepost_combat_0, ndraws=100)

model_id = 'attitude_change_prepost_combat_1'
mod.attitude_change_prepost_combat_1 <- 
  brm(bf(rating ~ time * treatment + 
           time * scale(topic_involvement) +
           time * scale(article_interest) + 
           time * scale(article_recommend) +
           time * scale(article_surprise) +
           (1 | PID)),
      data=subset(agg2, question == "combat_drug_priority"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
#control = list(adapt_delta = .95))
summary(mod.attitude_change_prepost_combat_1)
pp_check(mod.attitude_change_prepost_combat_1, ndraws=100)

loo0 = loo(mod.attitude_change_prepost_combat_0)
loo1 = loo(mod.attitude_change_prepost_combat_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))


### Legalize ----

model_id = 'attitude_change_prepost_legalize_0'
mod.attitude_change_prepost_legalize_0 <- 
  brm(bf(rating ~ time * treatment + 
           (1 | PID)),
      data=subset(agg2, question == "opinion_on_drug_legalization"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
#control = list(adapt_delta = .95))
summary(mod.attitude_change_prepost_legalize_0)
pp_check(mod.attitude_change_prepost_legalize_0, ndraws=100)

model_id = 'attitude_change_prepost_legalize_1'
mod.attitude_change_prepost_legalize_1 <- 
  brm(bf(rating ~ time * treatment + 
           time * scale(topic_involvement) +
           time * scale(article_interest) + 
           time * scale(article_recommend) +
           time * scale(article_surprise) +
           (1 | PID)),
      data=subset(agg2, question == "opinion_on_drug_legalization"), 
      family=cumulative("logit", threshold="flexible"),
      backend = "cmdstanr", 
      cores = parallel::detectCores() - 1, 
      iter = 5000,
      file = model_fit_path(model_id))
#control = list(adapt_delta = .95))
summary(mod.attitude_change_prepost_legalize_1)
pp_check(mod.attitude_change_prepost_legalize_1, ndraws=100)


loo0 = loo(mod.attitude_change_prepost_legalize_0)
loo1 = loo(mod.attitude_change_prepost_legalize_1)
comparison_results = as.data.frame(loo_compare(loo0, loo1))


