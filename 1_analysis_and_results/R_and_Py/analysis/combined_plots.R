library(tidyr)
library(dplyr)
library(ggplot2)
library(ggdist)

library(papaja)
library(cowplot)

FIG_PATH = 'results_figures/combined'
S1_FIG_PATH = 'results_figures/study1'
S2_FIG_PATH = 'results_figures/study2'


load(file = sprintf("%s/recall_yearly_with_individuals.rdata", S1_FIG_PATH))
plt.recall_by_year_ind_s1 = plt.recall_by_year_ind

load(file = sprintf("%s/recall_yearly_with_individuals.rdata", S2_FIG_PATH))
plt.recall_by_year_ind_s2 = plt.recall_by_year_ind

plt = plot_grid(plt.recall_by_year_ind_s1, plt.recall_by_year_ind_s2,
          ncol = 2, nrow = 1)
(plt)
ggsave(plot=plt,width=15,height=8,dpi=200,
       filename=sprintf(sprintf("%s/recall_yearly_with_individuals.pdf", FIG_PATH)), useDingbats=FALSE)



# Recall RMSE ----



# Recall RMSE contrasts ----

tab1 = read.csv('results_tables/study1/recall_contrasts.csv')
tab2 = read.csv('results_tables/study2/recall_contrasts.csv')

tab = rbind(tab1, tab2)

tab$contrast = factor(tab$contrast,
                      levels=c('txt - control',
                               'visual - control', 
                               'visual - txt'
                               ),
                      labels=c('Categorize - Control',
                               'Draw - Control',
                               'Draw - Categorize'
                               ))

plt.eff_rmse = ggplot(tab) +
  geom_hline(yintercept = 0, color='gray', linetype='dashed') +
  stat_pointinterval(aes(x=contrast, y=.value, color=study),
                     .width=.95, size=1,
                     #orientation='horizontal',
                     position=position_dodge(.5)) +
  coord_flip() +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())
(plt.eff_rmse)
ggsave(plot=plt.eff_rmse,width=6,height=4,dpi=200,
       filename=sprintf(sprintf("%s/model_recall_contrasts.pdf", FIG_PATH)), useDingbats=FALSE)


# Attitude change ----

ac1 = read.csv('data_processed/study1/study1_attitude_change.csv')
ac2 = read.csv('data_processed/study2/study2_attitude_change.csv')

ac = rbind(ac1, ac2)


acl = ac %>%
  select(study, treatment, PID, 
         attitude_change_opinion, attitude_change_combat, attitude_change_legalize) %>%
  pivot_longer(c(attitude_change_opinion, attitude_change_combat, attitude_change_legalize),
               names_to = 'question',
               values_to = 'ac')

ggplot(acl) +
  geom_point(aes(x=treatment, y=ac, ))




# Attitude change means ----

tab1 = read.csv('results_tables/study1/attitude_change_means.csv')
tab2 = read.csv('results_tables/study2/attitude_change_means.csv')

mn.attitude_change = rbind(tab1, tab2)
mn.attitude_change$question = factor(mn.attitude_change$question,
                                     levels=c('opinion', 'combat', 'legalize'))

mn.attitude_change$study = factor(mn.attitude_change$study,
                                     levels=c('Study 1', 'Study 2'))

mn.attitude_change$treatment = factor(mn.attitude_change$treatment,
                                  levels=c('Control', 'Categorize', 'Draw'))

plt.mn_attitude_change = ggplot(mn.attitude_change) + 
  geom_hline(yintercept = 0, color='black', linetype='dashed') +
  geom_linerange(aes(x=treatment, ymin=.lower, ymax=.upper, color=study),
                 position=position_dodge(.5), ) +
  geom_point(aes(x=treatment, y=mn, color=study),
             position=position_dodge(.5)) +
  facet_grid(~ question) +
  coord_flip() +
  labs(y = 'Attitude change', x=NULL) +
  theme_apa() +
  theme(text = element_text(size=10),
        plot.margin = margin(0.5,0.5,0.5,1.8, "cm"))
(plt.mn_attitude_change)
ggsave(plot=plt.mn_attitude_change,width=7,height=3,dpi=200,
       filename=sprintf(sprintf("%s/attitude_change_means.pdf", FIG_PATH)), useDingbats=FALSE)


# Attitude change effects -----

tab1 = read.csv('results_tables/study1/attitude_change_fixef.csv')
tab2 = read.csv('results_tables/study2/attitude_change_fixef.csv')

fe.attitude_change = rbind(tab1, tab2)

fe.attitude_change = fe.attitude_change %>%
  subset(!(predictor %in% c('Intercept[1]', 'Intercept[2]', 
                            'Intercept[2]', 'Intercept[3]',
                            'Intercept[4]', 'Intercept[5]',
                            'Intercept[6]', 'Intercept[7]')))

fe.attitude_change$question = factor(fe.attitude_change$question,
                                     levels=c('opinion', 'combat', 'legalize'))

fe.attitude_change$predictor = factor(fe.attitude_change$predictor,
                                      levels = c('scalearticle_surprise',
                                                 'scalearticle_recommend',
                                                 'scalearticle_interest',
                                                 'scaletopic_involvement',
                                                 'treatmentDraw',
                                                 'treatmentCategorize'),
                                      labels = c('Surprise',
                                                 'Recommend',
                                                 'Interest',
                                                 'Topic involvement',
                                                 'Treatment[Draw]',
                                                 'Treatment[Categorize]'
                                                 ))


plt.attitude_change = ggplot(fe.attitude_change) + 
  geom_hline(yintercept = 0, color='black', linetype='dashed') +
  geom_linerange(aes(x=predictor, ymin=Q2.5, ymax=Q97.5, color=study),
             position=position_dodge(.5), ) +
  geom_point(aes(x=predictor, y=Estimate, color=study),
             position=position_dodge(.5)) +
  facet_grid(~ question) +
  coord_flip() +
  theme_apa() +
  theme(axis.title.y = element_blank(),
        text = element_text(size=10),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

(plt.attitude_change)
ggsave(plot=plt.attitude_change,width=9,height=3,dpi=200,
       filename=sprintf(sprintf("%s/attitude_change_effects.pdf", FIG_PATH)), useDingbats=FALSE)


plt = plot_grid(plt.mn_attitude_change, 
          plt.attitude_change,
          ncol = 1, nrow = 2,
          rel_heights = c(.7, 1))
(plt)
ggsave(plot=plt,width=8,height=8,dpi=200,
       filename=sprintf(sprintf("%s/attitude_change_combined.pdf", FIG_PATH)), useDingbats=FALSE)



