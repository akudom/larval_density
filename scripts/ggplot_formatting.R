# very basic code to ensure ggplot plots are all formatted uniformly within the project

larval_density_theme <- list(
  theme_bw(),
  theme(legend.position = 'bottom',
        text = element_text(size = 20)))
