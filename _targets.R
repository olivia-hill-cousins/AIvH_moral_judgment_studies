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
list(
################################################################################
# SETUP
################################################################################
tar_target(twelve_colours, set_colours_object(12)),
tar_target(georgia_theme, set_theme()),
tar_target(MG_key, readRDS("docs/MG_key.rds")),
################################################################################
# CLEANING DATA
################################################################################
###### The following section of code cannot be made publicly available as it contains Public IDs. The targets code and "0_cleaning.R" script are included for transparency.
#===============================================================================
# STUDY 1
#===============================================================================
# NOTE: The initial data cleaning and preprocessing steps are documented in 1_cleaning.R, which is available alongside the analysis pipeline. This script describes the procedures used to transform the raw task exports into the analysis-ready dataset, including data filtering, variable selection, harmonisation, and merging procedures. Due to participant privacy and ethical restrictions, the raw participant-level data required to run these initial preprocessing steps are not publicly available. The publicly shared workflow therefore begins from the deidentified analysis dataset generated after these preprocessing steps have been completed.
tar_target(s1_raw_data_csv, "data_raw/s1_raw_data.csv", format = "file"),
tar_target(s1_raw, read_in_csv(s1_raw_data_csv)),#Here we correct some errors made when coding the factor levels in the df before collecting data 
tar_target(s1_raw_tidied, tidy_s1_vars(s1_raw)),
tar_target(s1_predf, fix_dfs(s1_raw_tidied, MG_key)),
#===============================================================================
# STUDY 2
#===============================================================================
# NOTE: The initial data cleaning and preprocessing steps are documented in 1_cleaning.R, which is available alongside the analysis pipeline. This script describes the procedures used to transform the raw task exports into the analysis-ready dataset, including data filtering, variable selection, harmonisation, and merging procedures. Due to participant privacy and ethical restrictions, the raw participant-level data required to run these initial preprocessing steps are not publicly available. The publicly shared workflow therefore begins from the deidentified analysis dataset generated after these preprocessing steps have been completed.
tar_target(s2_raw_data_csv, "data_raw/s2_raw_data.csv", format = "file"),
tar_target(s2_raw, read_in_csv(s2_raw_data_csv)), #Here we correct some errors made when coding the factor levels in the df before collecting data 
tar_target(s2_predf, fix_dfs(s2_raw, MG_key)),
#===============================================================================
# STUDY 3
#===============================================================================
tar_target(s3_j_key_csv, "data_raw/s3_raw_data_j_yes.csv", format = "file"),
tar_target(s3_f_key_csv, "data_raw/s3_raw_data_f_yes.csv", format = "file"),
tar_target(s3_data_jkey, read_in_csv(s3_j_key_csv)),
tar_target(s3_data_fkey, read_in_csv(s3_f_key_csv)),
################################################################################
# CLEANING DEMOGRAPHIC DATA
################################################################################
#===============================================================================
# STUDY 1
#===============================================================================
# Please see disclaimer above 
#===============================================================================
# STUDY 2
#===============================================================================
# Please see disclaimer above
#===============================================================================
# STUDY 3
#===============================================================================
tar_target(s3_demo_csv, "data_raw/s3_raw_demo_data.csv", format = "file"),
tar_target(s3_raw_demo, read_in_csv(s3_demo_csv)),
tar_target(s3_prolific_demo_csv, "data_raw/s3_private_prolific_demographic.csv", format = "file"),
tar_target(s3_prolific_raw_demo, read_in_csv(s3_prolific_demo_csv)),
tar_target(s3_clean_demo, clean_demo_data(s3_raw_demo, s3_prolific_raw_demo)),
# combine diff. data sources
tar_target(s3_raw_data, combine_keys_to_one_df(s3_data_fkey, s3_data_jkey)),
tar_target(s3_clean_data, s3_clean_raw_data(s3_raw_data, s3_prolific_raw_demo)),
tar_target(s3_combined_df, s3_add_demo_to_df(s3_clean_data, s3_clean_demo)),
tar_target(s3_keyed_df, correct_df_with_mg_key(s3_combined_df, MG_key)),
tar_target(s3_pre_prepdf_silly, s3_tidy_df_cols(s3_keyed_df)), 
tar_target(s3_pre_prepdf, swap_keys_because_im_silly(s3_pre_prepdf_silly)),
################################################################################
# ANALYZING DATA
################################################################################
#===============================================================================
# STUDY 1
#===============================================================================
tar_target(s1_df, prep_for_analysis(s1_predf)),
tar_target(save_s1_df, {write.csv(s1_df, "data_clean/s1_df.csv"); "data_clean/s1_df.csv"}, format = "file"),
tar_target(s1_full_glm, fit_glmer(s1_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s1_full_glm_gof, check_goodness_of_fit_via_deviance(s1_full_glm)),
# fit null model for deviance checks
tar_target(s1_null_glm, fit_glmer(s1_df, "MJ", FE_structure = "1", RE_structure = "(1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
### EDIT: ADD OTHER MODELS IN BETWEEN HERE
tar_target(s1_glm, fit_glmer(s1_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(save_s1_glm_rds, {saveRDS(s1_glm, "outputs/s1_glm.rds"); "outputs/s1_glm.rds"}, format = "file"),
tar_target(s1_glm_gof, check_goodness_of_fit_via_deviance(s1_glm)),
#========================================================================== EMMs
tar_target(s1_glm_emms, get_emmeans(s1_glm, c("Agent", "PBH", "PDE"))),
tar_target(s1_glm_emms_or, convert_emmeans_OR(s1_glm_emms)),
tar_target(s1_glm_emms_contrasts, get_all_contrasts(s1_glm_emms)),
tar_target(s1_glm_emms_or_contrasts, convert_contrasts_OR(s1_glm_emms_contrasts)),
#========================================================================== TOaST
tar_target(s1_toast, run_toast_test(s1_glm, bound_val=0.52)),
tar_target(s1_pwr_toast, run_toast_test(s1_glm, bound_val=0.94)),
#========================================================================== Model Comparison
tar_target(s1_all_glms, fit_all_glmer(df=s1_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s1_model_comp, compare_models(s1_all_glms)),
tar_target(s1_test_models_full_PBHxPDE, test_two_models_performance(model1=s1_all_glms[["PBH × PDE × Agent"]], model2=s1_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s1_all_glms_re, fit_all_glmer_RE(df = s1_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s1_model_comp_re, compare_models(s1_all_glms_re)),
#========================================================================== Model Checks
tar_target(s1_model_checks, conduct_model_diagnostics(model=s1_glm, name="s1_model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s1_linear_pred, check_linear_pred(model=s1_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="s1", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s1_item_level_dilemma_df, create_df_for_item_level_checks(s1_df)),
tar_target(s1_match_item_checks, check_item_difficulty(s1_item_level_dilemma_df, "Match")),
#===============================================================================
# STUDY 2
#===============================================================================
tar_target(s2_df, prep_for_analysis(s2_predf)),
tar_target(save_s2_df, {write.csv(s2_df, "data_clean/s2_df.csv"); "data_clean/s2_df.csv"}, format="file"),
tar_target(s2_full_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_full_glm_gof, check_goodness_of_fit_via_deviance(s2_full_glm)),
tar_target(s2_semi_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_semi_noSlopeDilemma_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
### EDIT: ADD OTHER MODELS IN BETWEEN HERE
tar_target(s2_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(save_s2_glm_rds, {saveRDS(s2_glm, "outputs/s2_glm.rds"); "outputs/s2_glm.rds"}, format = "file"),
tar_target(s2_glm_gof, check_goodness_of_fit_via_deviance(s2_glm)),
#========================================================================== EMMs
tar_target(s2_glm_emms, get_emmeans(s2_glm, c("Agent", "PBH", "PDE"))),
tar_target(s2_glm_emms_or, convert_emmeans_OR(s2_glm_emms)),
tar_target(s2_glm_emms_contrasts, get_all_contrasts(s2_glm_emms)),
tar_target(s2_glm_emms_or_contrasts, convert_contrasts_OR(s2_glm_emms_contrasts)),
#========================================================================== TOaST
tar_target(s2_toast, run_toast_test(s2_glm, bound_val=0.52)),
tar_target(s2_pwr_toast, run_toast_test(s2_glm, bound_val=0.94)),
#========================================================================== Model Comparison
tar_target(s2_all_glms, fit_all_glmer(df=s2_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_model_comp, compare_models(s2_all_glms)),
tar_target(s2_test_models_full_PBHxPDE, test_two_models_performance(model1=s2_all_glms[["PBH × PDE × Agent"]], model2=s2_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s2_all_glms_re, fit_all_glmer_RE(df = s2_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s2_model_comp_re, compare_models(s2_all_glms_re)),
#========================================================================== Model Checks
tar_target(s2_model_checks, conduct_model_diagnostics(model=s2_glm, name="s2_model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s2_linear_pred, check_linear_pred(model=s2_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="s2", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s2_item_level_dilemma_df, create_df_for_item_level_checks(s2_df)),
tar_target(s2_match_item_checks, check_item_difficulty(s2_item_level_dilemma_df, "Match")),
#===============================================================================
# STUDY 3
#===============================================================================
tar_target(s3_full_df, prep_for_analysis(s3_pre_prepdf)),
tar_target(s3_save_full_df, {write.csv(s3_full_df, "data_clean/full_df.csv"); "data_clean/full_df.csv"}, format = "file"),
tar_target(s3_filtered_df, s3_remove_timeout_responses(s3_full_df)),
tar_target(s3_df, prep_for_analysis(s3_filtered_df)),
tar_target(s3_save_df, {write.csv(s3_df, "data_clean/s3_df.csv"); "data_clean/s3_df.csv"}, format = "file"),
tar_target(s3_full_glm, fit_glmer(s3_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_full_glm_gof, check_goodness_of_fit_via_deviance(s3_full_glm)),
tar_target(s3_semi_glm, fit_glmer(s3_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_semi_noSlopeDilemma_glm, fit_glmer(s3_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
### EDIT: ADD OTHER MODELS IN BETWEEN HERE
tar_target(s3_inter_glm, fit_glmer(s3_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_glm, fit_glmer(s3_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_save_glm_rds, {saveRDS(s3_full_glm, "outputs/s3_full_glm.rds"); "outputs/s3_full_glm.rds"}, format = "file"),
tar_target(s3_glm_gof, check_goodness_of_fit_via_deviance(s3_full_glm)),
#========================================================================== EMMs
tar_target(s3_glm_emms, get_emmeans(s3_full_glm, c("Agent", "PBH", "PDE"))),
tar_target(s3_glm_emms_or, convert_emmeans_OR(s3_glm_emms)),
tar_target(s3_glm_emms_contrasts, get_all_contrasts(s3_glm_emms)),
tar_target(s3_glm_emms_or_contrasts, convert_contrasts_OR(s3_glm_emms_contrasts)),
#========================================================================== TOaST
tar_target(s3_toast, run_toast_test(s3_full_glm, bound_val=0.52)),
tar_target(s3_pwr_toast, run_toast_test(s3_full_glm, bound_val=0.94)),
#========================================================================== Model Comparison
tar_target(s3_all_glms, fit_all_glmer(df=s3_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_model_comp, compare_models(s3_all_glms)),
tar_target(s3_test_models_full_PBHxPDE, test_two_models_performance(model1=s3_all_glms[["PBH × PDE × Agent"]], model2=s3_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s3_all_glms_re, fit_all_glmer_RE(df = s3_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s3_model_comp_re, compare_models(s3_all_glms_re)),
#========================================================================== Model Checks
tar_target(s3_model_checks, conduct_model_diagnostics(model=s3_full_glm, name="model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s3_linear_pred, check_linear_pred(model=s3_full_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="full", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s3_item_level_dilemma_df, create_df_for_item_level_checks(s3_df)),
tar_target(s3_match_item_checks, check_item_difficulty(s3_item_level_dilemma_df, "Match")),
#===============================================================================
# COMBINED (for reporting)
#===============================================================================
tar_target(combined_df, join_s1_s2_s3(s1_df, s2_df, s3_df)),
################################################################################
# DATA PRESENTATION
################################################################################
#===============================================================================
# STUDY 1
#===============================================================================
tar_target(s1_glm_pred_prob_plots, plot_predicted_probabilities(model=s1_glm, name="s1_pred_prob_plots", colours=twelve_colours, theme=georgia_theme)),
tar_target(s1_toast_plot, plot_toast(model=s1_glm, name="s1", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s1_pwr_toast_plot, plot_toast(model=s1_glm, name="s1_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
#===============================================================================
# STUDY 2
#===============================================================================
tar_target(s2_glm_pred_prob_plots, plot_predicted_probabilities(model=s2_glm, name="s2_pred_prob_plots", colours=twelve_colours, theme=georgia_theme)),
tar_target(s2_toast_plot, plot_toast(model=s2_glm, name="s2", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s2_pwr_toast_plot, plot_toast(model=s2_glm, name="s2_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
#===============================================================================
# STUDY 3
#===============================================================================
tar_target(s3_glm_pred_prob_plots, plot_predicted_probabilities(model=s3_full_glm, name="s3_pred_prob_plots", colours=twelve_colours, theme=georgia_theme)),
tar_target(s3_toast_plot, plot_toast(model=s3_full_glm, name="s3_low", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s3_pwr_toast_plot, plot_toast(model=s3_full_glm, name="s3_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
################################################################################
# DEMOGRAPHICS
################################################################################
tar_target(s1_one_row_df, one_row_per_df(s1_df)),
tar_target(s2_one_row_df, one_row_per_df(s2_df)),
tar_target(s1_nationality_heatmap, make_nationality_heatmap_for_s1(s1_one_row_df))
)