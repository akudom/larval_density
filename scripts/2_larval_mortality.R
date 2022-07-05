# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on larval mortality


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(broom.mixed)
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
         proportion_survival = n_survived / larval_density,
         # we now want larval_density available as a factor,
         # for plotting and analyses
         larval_density_fct = forcats::as_factor(larval_density)) %>%
         rename(Species = species)
         


# Plot --------------------------------------------------------------------
            
# for plotting we want larval_density to be a factor, not integer

proportion_survival_boxplot <- ggplot(survival_df, aes(x = larval_density_fct, y = proportion_survival, colour = Species)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab('Proportion surviving') +
  xlab('Larval density (number of larvae per enclosure)')
proportion_survival_boxplot
ggsave('figures/survival/proportion_survival_boxplot.pdf', proportion_survival_boxplot)

number_surviving_boxplot <- ggplot(survival_df, aes(x = larval_density_fct, y = n_survived, colour = Species)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab('Number surviving') +
  xlab('Larval density (number of larvae per enclosure)')
number_surviving_boxplot
ggsave('figures/survival/number_surviving_boxplot.pdf', number_surviving_boxplot)





# Model -------------------------------------------------------------------

# first we need the data to be in a slightly longer format, where each
# row is a mosquito larva and we have a binary variable of
# if it lived or died. There's probably a more elegant way to do this than
# a for loop, but it'll do.

larval_list <- list()
for(i in 1:nrow(survival_df)){
  temp <- survival_df[i,]
  
  died_rows <- temp %>% 
    ungroup() %>%
    uncount(n_died) %>%
    select(date_of_experiment, larval_density_fct, replicate_no, Species) %>%
    mutate(survival = 'died')
  
  survived_rows <- temp %>% 
    ungroup() %>%
    uncount(n_survived) %>%
    select(date_of_experiment, larval_density_fct, replicate_no, Species) %>%
    mutate(survival = 'survived')
  
  larval_list[[i]] <- bind_rows(died_rows, survived_rows)
}

long_df <- bind_rows(larval_list) %>%
  # make a less human-friendly but more model-friendly
  # binary variable for survival or death
  mutate(binary_survived = ifelse(survival == 'survived', 1, 0),
         grouping_var = paste(date_of_experiment, replicate_no, sep = '_'))



# now do the model
survival_mod = glmer(formula = binary_survived ~ larval_density_fct + Species +
               (1|grouping_var),
             data = long_df,
             family = 'binomial')
summary(survival_mod)


tidy(survival_mod) %>%
  write_csv('results/survival/survival_tidy.csv')

glance(survival_mod) %>%
  write_csv('results/survival/survival_glance.csv')
