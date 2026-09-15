# add crew to tar_option_set:
library(crew)
tar_option_set(controller=crew_controller_local(workers=availableCores()), seed = 123)


#OR just add:

# Load packages required to define the pipeline:
library(targets)
library(crew)
library(parallelly)

# Set target options:
tar_option_set(controller=crew_controller_local(workers=availableCores()-3), seed = 123,
               packages = c("tibble", "dplyr", "stringr", "purrr", "lme4", "lmerTest", "sjPlot", 
                            "RColorBrewer", "showtext", "ggplot2", "emmeans", "parameters", "see",
                            "performance", "patchwork", "ggeffects", "tidyr", "sf", "ggrepel", "scales", 
                            "data.table","janitor","psych", "REdaS", "paran","stringr")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts")