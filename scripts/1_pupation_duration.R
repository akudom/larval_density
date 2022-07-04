
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
         # convert larval_density to numeric 
         larval_density = as.numeric(larval_density),
         # replace the string 'N/A' with NA values in period_of_adult_pupation_day
         period_of_eclosion_day = gsub('N/A', NA, period_of_eclosion_day),
         # convert period_of_adult_pupation_day from character to numeric
         period_of_eclosion_day = as.numeric(period_of_eclosion_day),
         # make species entirely lowercase, as it's currently mixed 
         species = tolower(species))
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
                  aes(x = factor(larval_density), y = value, colour = Species))+ 
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


pupation_model <- lmer(formula = period_of_pupation_day ~ 1 + (1|species) + 
                         (1|larval_density), data = input_data)

summary(pupation_model)

eclosion_model <- lmer(formula = period_of_eclosion_day ~ 1 + (1|species) + 
                         (1|larval_density), data = input_data)

summary(eclosion_model)
