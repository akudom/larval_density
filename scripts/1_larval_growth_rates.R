# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on larval growth rates


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(janitor)
library(broom.mixed)

# read in data
input_data <- read_excel('data/raw_data/Data_density dependent.xlsx') %>% 
  # remove spaces etc from column names
  clean_names() %>%
  rename(period_of_eclosion_day = period_of_adult_pupation_day) %>%
  mutate(# remove the character 'L' from larval_density
        larval_density = gsub('L', '', larval_density),
        larval_density = forcats::as_factor(larval_density),
         # replace the string 'N/A' with NA values in period_of_adult_pupation_day
        period_of_eclosion_day = gsub('N/A', NA, period_of_eclosion_day),
        # convert period_of_adult_pupation_day from character to numeric
        period_of_eclosion_day = as.numeric(period_of_eclosion_day),
        # make first character of species capitalised, as it's currently mixed 
        species = str_to_title(species),
    sample_replicate = paste(date_of_experiment, replicate_no, sep = '_'),
    replicate_no = as.character(replicate_no),
    date_of_experiment = as.character(date_of_experiment))
str(input_data)

write_csv(input_data, 'data/processed_data/density_and_growth.csv')

# Summary plot -----------------------------------------------------------

# format the data for ggplot

long_data <- input_data %>% 
  rename(`Days until pupation` = period_of_pupation_day,
         `Days until eclosion` = period_of_eclosion_day,
         Species = species) %>%
  pivot_longer(c(`Days until pupation`, `Days until eclosion`), 
               names_to = 'stat_type', values_to = 'value') %>%
  # reverse the factor order of stat_type, as by default it 
  # plots alphabetically (eclosion before pupation) and 
  # thats a bit confusing
  mutate(stat_type = fct_rev(stat_type))

# make a simple pair of violin plots to show summary stats
violins <- ggplot(long_data, 
                  aes(x = larval_density, y = value, colour = Species))+ 
  geom_violin() + 
  facet_wrap(. ~ stat_type, strip.position = 'left') + 
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab(element_blank()) +
  xlab('Larval density (number of larvae per enclosure)')
ggsave(filename = 'figures/growthrates/violinplot.pdf',
       violins)

box_plot <- ggplot(long_data, 
                  aes(x = factor(larval_density), y = value, colour = Species))+ 
  geom_boxplot() +
  facet_wrap(. ~ stat_type, strip.position = 'left') + 
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 20)) +
  scale_colour_viridis_d() +
  ylab(element_blank()) +
  xlab('Larval density (number of larvae per enclosure)')

box_plot
ggsave(filename = 'figures/growthrates/boxplot.pdf', box_plot)


# Model -------------------------------------------------------------------

# pupation
pupation_model_kisumu <- glmer(formula = period_of_pupation_day ~ larval_density +
                          (1|sample_replicate),
                        data = filter(input_data, species == 'Kisumu'),
                        family = 'poisson')

summary(pupation_model_kisumu)
qqnorm(resid(pupation_model_kisumu)) 
qqline(resid(pupation_model_kisumu), col = "red")

pupation_model_opeibea <- glmer(formula = period_of_pupation_day ~ larval_density +
                                 (1|sample_replicate),
                               data = filter(input_data, species == 'Opeibea'),
                               family = 'poisson')

summary(pupation_model_opeibea)
qqnorm(resid(pupation_model_opeibea)) 
qqline(resid(pupation_model_opeibea), col = "red")

# eclosion

eclosion_model_kisumu <- glmer(formula = period_of_eclosion_day ~ larval_density +
                                 (1|sample_replicate),
                               data = filter(input_data, species == 'Kisumu'),
                               family = 'poisson')

summary(eclosion_model_kisumu)
qqnorm(resid(eclosion_model_kisumu)) 
qqline(resid(eclosion_model_kisumu), col = "red")

eclosion_model_opeibea <- glmer(formula = period_of_eclosion_day ~ larval_density +
                                 (1|sample_replicate),
                               data = filter(input_data, species == 'Opeibea'),
                               family = 'poisson')

summary(eclosion_model_opeibea)
qqnorm(resid(eclosion_model_opeibea)) 
qqline(resid(eclosion_model_opeibea), col = "red")


# Make summary tables of models -------------------------------------------

# this is ugly as hell, improve it sometime Dave
tidy(pupation_model_opeibea) %>%
  write_csv('results/growthrates/opeibea_pupation_tidy.csv')
glance(pupation_model_opeibea) %>%
  write_csv('results/growthrates/opeibea_pupation_glance.csv')

tidy(pupation_model_kisumu) %>%
  write_csv('results/growthrates/kisumu_pupation_tidy.csv')
glance(pupation_model_kisumu) %>%
  write_csv('results/growthrates/kisumu_pupation_glance.csv')


tidy(eclosion_model_opeibea) %>%
  write_csv('results/growthrates/opeibea_eclosion_tidy.csv')
glance(eclosion_model_opeibea) %>%
  write_csv('results/growthrates/opeibea_eclosion_glance.csv')

tidy(eclosion_model_kisumu) %>%
  write_csv('results/growthrates/kisumu_eclosion_tidy.csv')
glance(eclosion_model_kisumu) %>%
  write_csv('results/growthrates/kisumu_eclosion_glance.csv')
