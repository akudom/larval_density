
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(janitor)

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
        # make species entirely lowercase, as it's currently mixed 
        species = tolower(species),
    sample_replicate = paste(date_of_experiment, replicate_no, sep = '_'),
    replicate_no = as.character(replicate_no),
    date_of_experiment = as.character(date_of_experiment))
str(input_data)



# Summary plot -----------------------------------------------------------

# format the data for ggplot

long_data <- input_data %>% 
  rename(`Days until pupation` = period_of_pupation_day,
         `Days until eclosion` = period_of_eclosion_day,
         Species = species) %>%
  pivot_longer(c(`Days until pupation`, `Days until eclosion`), 
               names_to = 'stat_type', values_to = 'value')

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
ggsave(filename = 'figures/violinplot.pdf',
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
ggsave(filename = 'figures/boxplot.pdf', box_plot)


# Model -------------------------------------------------------------------


pupation_model_kisumu <- glmer(formula = period_of_pupation_day ~ larval_density +
                          (1|sample_replicate),
                        data = filter(input_data, species == 'kisumu'),
                        family = 'poisson')

summary(pupation_model_kisumu)
qqnorm(resid(pupation_model_kisumu)) 
qqline(resid(pupation_model_kisumu), col = "red")

eclosion_model_kisumu <- glmer(formula = period_of_eclosion_day ~ larval_density +
                                 (1|sample_replicate),
                               data = filter(input_data, species == 'kisumu'),
                               family = 'poisson')

summary(eclosion_model_kisumu)
qqnorm(resid(eclosion_model_kisumu)) 
qqline(resid(eclosion_model_kisumu), col = "red")
