# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on larval mortality


# Setup -------------------------------------------------------------------

library(tidyverse)
larval_df <- read_csv('data/processed_data/density_and_growth.csv')

str(larval_df)

survival_df <- larval_df %>%
  # remove any rows where theres an NA on the eclosion
  # date, that way we only have rows for larvae that
  # successfully eclosed (i.e. survived to adulthood)
  filter(!is.na(period_of_eclosion_day)) %>%
  # make a column per grouping variable stating how
  # many survived
  group_by(date_of_experiment, larval_density,
           replicate_no, species) %>%
  summarise(n_survived = n()) %>%
  # make a column showing how many died
  mutate(n_died = larval_density - n_survived,
         proportion_survival = n_survived / larval_density)


# Plot --------------------------------------------------------------------
            
# for plotting we want larval_density to be a factor, not integer
plotting_df <- survival_df %>%
  mutate(larval_density = forcats::as_factor(larval_density)) %>%
  rename(Species = species)
            
proportion_survival_boxplot <- ggplot(plotting_df, aes(x = larval_density, y = proportion_survival, colour = Species)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab('Proportion surviving') +
  xlab('Larval density (number of larvae per enclosure)')
proportion_survival_boxplot
ggsave('figures/survival/proportion_survival_boxplot.pdf', proportion_survival_boxplot)

number_surviving_boxplot <- ggplot(plotting_df, aes(x = larval_density, y = n_survived, colour = Species)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab('Number surviving') +
  xlab('Larval density (number of larvae per enclosure)')
number_surviving_boxplot
ggsave('figures/survival/number_surviving_boxplot.pdf', number_surviving_boxplot)
