# Load packages required to define the pipeline:
library(targets)
library(crew)
library(parallelly)

# Set target options:
#controller=crew_controller_local(workers=availableCores()),
tar_option_set( seed = 123, controller=crew_controller_local(workers=availableCores()),
               packages = c("tibble", "dplyr", "stringr", "purrr", "lme4", "lmerTest", "sjPlot", 
                            "RColorBrewer", "showtext", "ggplot2", "emmeans", "parameters", "see",
                            "performance", "patchwork", "ggeffects", "tidyr", "sf", "ggrepel", "scales", 
                            "data.table","janitor","psych", "REdaS", "paran","stringr", "lavaan", "lavaanExtra", "semTools","pheatmap")
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
tar_target(s1_dropAgentInter_glm, fit_glmer(s1_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s1_inter_glm, fit_glmer(s1_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
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
tar_target(s1_true_veff_pwr_toast, run_toast_test(s1_glm, bound_val=s1_veff_intersect)),
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
#==================================================== Reaction Time Modelling
tar_target(s1_logTransform_hist_plots, plot_logTransform_constant_histograms(s1_df, name="s1", theme=georgia_theme)),
tar_target(s1_logTransform_noConstant_hist_plot, plot_logTransform_histogram(s1_df, name="s1_no_constant", theme=georgia_theme)),
tar_target(s1_df_logRT, create_df_with_logRT(df=s1_df, constant=10)),
tar_target(s1_glm_logRT_DV, fit_glmer_with_logRT(s1_df_logRT, "logRT", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)")),
tar_target(s1_glm_logRT_DV_gof, check_goodness_of_fit_via_deviance(s1_glm_logRT_DV)),
tar_target(s1_glm_logRT, fit_glmer(s1_df_logRT, "MJ", FE_structure = "PBH*PDE*Agent*logRT", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s1_glm_logRT_gof, check_goodness_of_fit_via_deviance(s1_glm_logRT)),
tar_target(s1_glm_logRT_pred_prob_plots, plot_predicted_probabilities(model=s1_glm_logRT,DV_name="log_RT", name="s1_logRT_pred_prob_plots", colours=twelve_colours, theme=georgia_theme)),
tar_target(s1_logRT_toast, run_toast_test(s1_glm_logRT, bound_val=0.52)), #TOAST w. SESOI bounds
tar_target(s1_logRT_toast_plot, plot_toast(model=s1_glm_logRT, name="s1_logRT", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(model_comp_s1_logRT, compare_models(c(s1_glm, s1_glm_logRT))), 
tar_target(test_models_s1_logRT, test_two_models_performance(model1=s1_glm, model2=s1_glm_logRT)),
tar_target(s1_glm_logRT_RI, fit_glmer(s1_df_logRT, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma) + (1|logRT)", family_link=binomial(link = "logit"))),
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
tar_target(s2_semi_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_semi_interRE_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE+Agent|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_semi_noSlopeDilemma_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_inter_glm, fit_glmer(s2_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
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
tar_target(s2_true_veff_pwr_toast, run_toast_test(s2_glm, bound_val=s2_veff_intersect)),
#========================================================================== Model Comparison
tar_target(s2_all_glms, fit_all_glmer(df=s2_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_model_comp, compare_models(s2_all_glms)),
tar_target(s2_test_models_full_PBHxPDE, test_two_models_performance(model1=s2_all_glms[["PBH × PDE × Agent"]], model2=s2_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s2_all_glms_re, fit_all_glmer_RE(df = s2_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s2_model_comp_re, compare_models(s2_all_glms_re)),
#==================================================== Reaction Time Modelling
tar_target(s2_logTransform_hist_plots, plot_logTransform_constant_histograms(s2_df, name="s2", theme=georgia_theme)),
tar_target(s2_logTransform_noConstant_hist_plot, plot_logTransform_histogram(s2_df, name="s2_no_constant", theme=georgia_theme)),
tar_target(s2_df_logRT, create_df_with_logRT(df=s2_df, constant=0.1)),
tar_target(s2_glm_logRT_DV, fit_glmer_with_logRT(s2_df_logRT, "logRT", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)")),
tar_target(s2_glm_logRT_DV_gof, check_goodness_of_fit_via_deviance(s2_glm_logRT_DV)),
tar_target(s2_glm_logRT, fit_glmer(s2_df_logRT, "MJ", FE_structure = "PBH*PDE*Agent*logRT", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_glm_logRT_gof, check_goodness_of_fit_via_deviance(s2_glm_logRT)),
tar_target(s2_glm_logRT_pred_prob_plots, plot_predicted_probabilities(model=s2_glm_logRT,DV_name="log_RT", name="s2_logRT_pred_prob_plots", colours=twelve_colours, theme=georgia_theme)),
tar_target(s2_logRT_toast, run_toast_test(s2_glm_logRT, bound_val=0.52)), #TOAST w. SESOI bounds
tar_target(s2_logRT_toast_plot, plot_toast(model=s2_glm_logRT, name="s2_logRT", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(model_comp_s2_logRT, compare_models(c(s2_glm, s2_glm_logRT))), 
tar_target(test_models_s2_logRT, test_two_models_performance(model1=s2_glm, model2=s2_glm_logRT)),
tar_target(s2_glm_logRT_RI, fit_glmer(s2_df_logRT, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma) + (1|logRT)", family_link=binomial(link = "logit"))),
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
tar_target(s3_preprep_timeout_df, create_timeout_df(s3_full_df)),
tar_target(s3_timeout_df, prep_for_timeout_analysis(s3_preprep_timeout_df)),
tar_target(s3_saved_timeout_df, {write.csv(s3_timeout_df, "data_clean/s3_timeout_df.csv"); "data_clean/s3_timeout_df.csv"}, format = "file"),
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
tar_target(s3_glm_gof, check_goodness_of_fit_via_deviance(s3_glm)),
#========================================================================== EMMs
tar_target(s3_glm_emms, get_emmeans(s3_glm, c("Agent", "PBH", "PDE"))),
tar_target(s3_glm_emms_or, convert_emmeans_OR(s3_glm_emms)),
tar_target(s3_glm_emms_contrasts, get_all_contrasts(s3_glm_emms)),
tar_target(s3_glm_emms_or_contrasts, convert_contrasts_OR(s3_glm_emms_contrasts)),
tar_target(s3_glm_emms_twoway, get_emmeans(s3_glm, c("PBH", "Agent"))),
tar_target(s3_glm_emms_twoway_or, convert_emmeans_OR(s3_glm_emms_twoway)),
tar_target(s3_glm_emms_twoway_byAgent_contrasts, get_two_way_contrasts(s3_glm_emms_twoway[["by_PBH"]], by_vars="Agent")),
tar_target(s3_glm_emms_twoway_byAgent_or_contrasts, convert_contrasts_OR(s3_glm_emms_twoway_byAgent_contrasts)),
tar_target(s3_glm_emms_agent_contrasts, get_two_way_contrasts(s3_glm_emms[["by_Agent"]], by_vars=c("PBH", "PDE"))),
tar_target(s3_glm_emms_or_agent_contrasts, convert_contrasts_OR(s3_glm_emms_agent_contrasts)),
#========================================================================== TOaST
tar_target(s3_toast, run_toast_test(s3_glm, bound_val=0.52)),
tar_target(s3_pwr_toast, run_toast_test(s3_glm, bound_val=0.94)),
tar_target(s3_true_veff_pwr_toast, run_toast_test(s3_glm, bound_val=s3_veff_intersect)),
tar_target(s3_95_pwr_toast, run_toast_test(s3_glm, bound_val=s3_95_veff_intersect)),
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
tar_target(s1_glm_pred_prob_plots, plot_predicted_probabilities(model=s1_glm, name="s1_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(s1_toast_plot, plot_toast(model=s1_glm, name="s1", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s1_pwr_toast_plot, plot_toast(model=s1_glm, name="s1_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
tar_target(s1_true_veff_pwr_toast_plot, plot_toast(model=s1_glm, name="s1_true_veff_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = s1_veff_intersect)),
#===============================================================================
# STUDY 2
#===============================================================================
tar_target(s2_glm_pred_prob_plots, plot_predicted_probabilities(model=s2_glm, name="s2_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(s2_toast_plot, plot_toast(model=s2_glm, name="s2", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s2_pwr_toast_plot, plot_toast(model=s2_glm, name="s2_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
tar_target(s2_true_veff_pwr_toast_plot, plot_toast(model=s2_glm, name="s2_true_veff_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = s2_veff_intersect)),
#===============================================================================
# STUDY 3
#===============================================================================
tar_target(s3_glm_pred_prob_plots, plot_predicted_probabilities(model=s3_glm, name="s3_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(s3_toast_plot, plot_toast(model=s3_glm, name="s3_low", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s3_pwr_toast_plot, plot_toast(model=s3_glm, name="s3_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = 0.94)),
tar_target(s3_true_veff_pwr_toast_plot, plot_toast(model=s3_glm, name="s3_true_veff_pwr", colours=twelve_colours, theme=georgia_theme, bound_val = s3_veff_intersect)),
tar_target(s3_95_toast_plot, plot_toast(model=s3_glm, name="s3_95", colours=twelve_colours, theme=georgia_theme, bound_val = s3_95_veff_intersect)),
################################################################################
# DEMOGRAPHICS
################################################################################
tar_target(s1_one_row_df, one_row_per_df(s1_df)),
tar_target(s2_one_row_df, one_row_per_df(s2_df)),
tar_target(s1_nationality_heatmap, make_nationality_heatmap_for_s1(s1_one_row_df)),
################################################################################
# POWER SENSITIVITY ANALYSES
################################################################################
tar_target(s1_power_sens, readRDS("power_sensitivity/s1_power_sens_output.rds")),
tar_target(s1_veff_intersect, find_veff_intersect(0.8,s1_power_sens)),
tar_target(s2_power_sens, readRDS("power_sensitivity/s2_power_sens_output.rds")),
tar_target(s2_veff_intersect, find_veff_intersect(0.8,s2_power_sens)),
tar_target(s3_power_sens, readRDS("power_sensitivity/s3_90_power_sens_output.rds")),
tar_target(s3_veff_intersect, find_veff_intersect(0.9,s3_power_sens)),
tar_target(s3_95_power_sens, readRDS("power_sensitivity/s3_95_power_sens_output.rds")),
tar_target(s3_95_veff_intersect, find_veff_intersect(0.95,s3_95_power_sens)),
################################################################################
# EXPLORATORY ANALYSES
################################################################################
#========================================================================Correct & Incorrect
tar_target(s3_correct_glm, fit_glmer(s3_timeout_df, "MG_resp", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_correct_glm_pred_prob_plots, plot_predicted_probabilities(model=s3_correct_glm, name="s3_correct_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Correctness")),
tar_target(s3_correct_toast, run_toast_test(s3_correct_glm, bound_val=0.52)),
tar_target(s3_correct_toast_plot, plot_toast(model=s3_correct_glm, name="s3_correct", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
tar_target(s3_timeout_glm, fit_glmer(s3_timeout_df, "TO", FE_structure = "PBH*PDE*Agent", RE_structure = "(1|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_timeout_glm_pred_prob_plots, plot_predicted_probabilities(model=s3_timeout_glm, name="s3_timeout_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Timed Out")),
tar_target(s3_timeout_toast, run_toast_test(s3_timeout_glm, bound_val=0.52)),
tar_target(s3_timeout_toast_plot, plot_toast(model=s3_timeout_glm, name="s3_timeout", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
#========================================================================Passed 50%+ AC
tar_target(s1_passed_cr, calculate_number_of_participants_failed_attention_check(s1_df)), 
tar_target(s2_passed_cr, calculate_number_of_participants_failed_attention_check(s2_df)), 
tar_target(s3_passed_cr, calculate_number_of_participants_failed_attention_check(s3_df)), 
tar_target(s1_passed_df, create_df_without_participants_who_failed_all_ac(s1_df,s1_passed_cr$costless_check_all)), 
tar_target(s1_passed_save_df, {write.csv(s1_passed_df, "data_clean/s1_passed_df.csv"); "data_clean/s1_passed_df.csv"}, format = "file"),
tar_target(s2_passed_df, create_df_without_participants_who_failed_all_ac(s2_df,s2_passed_cr$costless_check_all)), 
tar_target(s2_passed_save_df, {write.csv(s2_passed_df, "data_clean/s2_passed_df.csv"); "data_clean/s2_passed_df.csv"}, format = "file"),
tar_target(s3_passed_df, create_df_without_participants_who_failed_all_ac(s3_df,s3_passed_cr$costless_check_all)), 
tar_target(s3_passed_save_df, {write.csv(s3_passed_df, "data_clean/s3_passed_df.csv"); "data_clean/s3_passed_df.csv"}, format = "file"),
## S1
tar_target(s1_passed_glm, fit_glmer(s1_passed_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(save_s1_passed_glm_rds, {saveRDS(s1_passed_glm, "outputs/s1_passed_glm.rds"); "outputs/s1_passed_glm.rds"}, format = "file"),
tar_target(s1_passed_glm_gof, check_goodness_of_fit_via_deviance(s1_passed_glm)),
tar_target(s1_passed_glm_pred_prob_plots, plot_predicted_probabilities(model=s1_passed_glm, name="s1_passed_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
#========================================================================== EMMs
tar_target(s1_passed_glm_emms, get_emmeans(s1_passed_glm, c("Agent", "PBH", "PDE"))),
tar_target(s1_passed_glm_emms_or, convert_emmeans_OR(s1_passed_glm_emms)),
tar_target(s1_passed_glm_emms_contrasts, get_all_contrasts(s1_passed_glm_emms)),
tar_target(s1_passed_glm_emms_or_contrasts, convert_contrasts_OR(s1_passed_glm_emms_contrasts)),
tar_target(s1_passed_glm_emms_twoway, get_emmeans(s1_passed_glm, c("PBH", "Agent"))),
tar_target(s1_passed_glm_emms_twoway_or, convert_emmeans_OR(s1_passed_glm_emms_twoway)),
tar_target(s1_passed_glm_emms_twoway_byAgent_contrasts, get_two_way_contrasts(s1_passed_glm_emms_twoway[["by_PBH"]], by_vars="Agent")),
tar_target(s1_passed_glm_emms_twoway_byAgent_or_contrasts, convert_contrasts_OR(s1_passed_glm_emms_twoway_byAgent_contrasts)),
tar_target(s1_passed_glm_emms_agent_contrasts, get_two_way_contrasts(s1_passed_glm_emms[["by_Agent"]], by_vars=c("PBH", "PDE"))),
tar_target(s1_passed_glm_emms_or_agent_contrasts, convert_contrasts_OR(s1_passed_glm_emms_agent_contrasts)),
#========================================================================== TOaST
tar_target(s1_passed_toast, run_toast_test(s1_passed_glm, bound_val=0.52)),
tar_target(s1_passed_pwr_toast, run_toast_test(s1_passed_glm, bound_val=0.94)),
tar_target(s1_passed_toast_plot, plot_toast(model=s1_passed_glm, name="s1_passed", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
#========================================================================== Model Comparison
tar_target(s1_passed_all_glms, fit_all_glmer(df=s1_passed_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s1_passed_model_comp, compare_models(s1_passed_all_glms)),
tar_target(s1_passed_test_models_full_PBHxPDE, test_two_models_performance(model1=s1_passed_all_glms[["PBH × PDE × Agent"]], model2=s1_passed_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s1_passed_all_glms_re, fit_all_glmer_RE(df = s1_passed_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s1_passed_model_comp_re, compare_models(s1_passed_all_glms_re)),
#========================================================================== Model Checks
tar_target(s1_passed_model_checks, conduct_model_diagnostics(model=s1_passed_glm, name="model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s1_passed_linear_pred, check_linear_pred(model=s1_passed_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="full", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s1_passed_item_level_dilemma_df, create_df_for_item_level_checks(s1_passed_df)),
tar_target(s1_passed_match_item_checks, check_item_difficulty(s1_passed_item_level_dilemma_df, "Match")),
## S2
tar_target(s2_passed_glm, fit_glmer(s2_passed_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH*PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(save_s2_passed_glm_rds, {saveRDS(s2_passed_glm, "outputs/s2_passed_glm.rds"); "outputs/s2_passed_glm.rds"}, format = "file"),
tar_target(s2_passed_glm_gof, check_goodness_of_fit_via_deviance(s2_passed_glm)),
tar_target(s2_passed_glm_pred_prob_plots, plot_predicted_probabilities(model=s2_passed_glm, name="s2_passed_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
#========================================================================== EMMs
tar_target(s2_passed_glm_emms, get_emmeans(s2_passed_glm, c("Agent", "PBH", "PDE"))),
tar_target(s2_passed_glm_emms_or, convert_emmeans_OR(s2_passed_glm_emms)),
tar_target(s2_passed_glm_emms_contrasts, get_all_contrasts(s2_passed_glm_emms)),
tar_target(s2_passed_glm_emms_or_contrasts, convert_contrasts_OR(s2_passed_glm_emms_contrasts)),
tar_target(s2_passed_glm_emms_twoway, get_emmeans(s2_passed_glm, c("PBH", "Agent"))),
tar_target(s2_passed_glm_emms_twoway_or, convert_emmeans_OR(s2_passed_glm_emms_twoway)),
tar_target(s2_passed_glm_emms_twoway_byAgent_contrasts, get_two_way_contrasts(s2_passed_glm_emms_twoway[["by_PBH"]], by_vars="Agent")),
tar_target(s2_passed_glm_emms_twoway_byAgent_or_contrasts, convert_contrasts_OR(s2_passed_glm_emms_twoway_byAgent_contrasts)),
tar_target(s2_passed_glm_emms_agent_contrasts, get_two_way_contrasts(s2_passed_glm_emms[["by_Agent"]], by_vars=c("PBH", "PDE"))),
tar_target(s2_passed_glm_emms_or_agent_contrasts, convert_contrasts_OR(s2_passed_glm_emms_agent_contrasts)),
#========================================================================== TOaST
tar_target(s2_passed_toast, run_toast_test(s2_passed_glm, bound_val=0.52)),
tar_target(s2_passed_pwr_toast, run_toast_test(s2_passed_glm, bound_val=0.94)),
tar_target(s2_passed_toast_plot, plot_toast(model=s2_passed_glm, name="s2_passed", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
#========================================================================== Model Comparison
tar_target(s2_passed_all_glms, fit_all_glmer(df=s2_passed_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s2_passed_model_comp, compare_models(s2_passed_all_glms)),
tar_target(s2_passed_test_models_full_PBHxPDE, test_two_models_performance(model1=s2_passed_all_glms[["PBH × PDE × Agent"]], model2=s2_passed_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s2_passed_all_glms_re, fit_all_glmer_RE(df = s2_passed_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s2_passed_model_comp_re, compare_models(s2_passed_all_glms_re)),
#========================================================================== Model Checks
tar_target(s2_passed_model_checks, conduct_model_diagnostics(model=s2_passed_glm, name="model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s2_passed_linear_pred, check_linear_pred(model=s2_passed_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="full", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s2_passed_item_level_dilemma_df, create_df_for_item_level_checks(s2_passed_df)),
tar_target(s2_passed_match_item_checks, check_item_difficulty(s2_passed_item_level_dilemma_df, "Match")),
### S3
tar_target(s3_passed_glm, fit_glmer(s3_passed_df, "MJ", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(save_s3_passed_glm_rds, {saveRDS(s3_passed_glm, "outputs/s3_passed_glm.rds"); "outputs/s3_passed_glm.rds"}, format = "file"),
tar_target(s3_passed_glm_gof, check_goodness_of_fit_via_deviance(s3_passed_glm)),
tar_target(s3_passed_glm_pred_prob_plots, plot_predicted_probabilities(model=s3_passed_glm, name="s3_passed_pred_prob_plots", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
#========================================================================== EMMs
tar_target(s3_passed_glm_emms, get_emmeans(s3_passed_glm, c("Agent", "PBH", "PDE"))),
tar_target(s3_passed_glm_emms_or, convert_emmeans_OR(s3_passed_glm_emms)),
tar_target(s3_passed_glm_emms_contrasts, get_all_contrasts(s3_passed_glm_emms)),
tar_target(s3_passed_glm_emms_or_contrasts, convert_contrasts_OR(s3_passed_glm_emms_contrasts)),
tar_target(s3_passed_glm_emms_twoway, get_emmeans(s3_passed_glm, c("PBH", "Agent"))),
tar_target(s3_passed_glm_emms_twoway_or, convert_emmeans_OR(s3_passed_glm_emms_twoway)),
tar_target(s3_passed_glm_emms_twoway_byAgent_contrasts, get_two_way_contrasts(s3_passed_glm_emms_twoway[["by_PBH"]], by_vars="Agent")),
tar_target(s3_passed_glm_emms_twoway_byAgent_or_contrasts, convert_contrasts_OR(s3_passed_glm_emms_twoway_byAgent_contrasts)),
tar_target(s3_passed_glm_emms_agent_contrasts, get_two_way_contrasts(s3_passed_glm_emms[["by_Agent"]], by_vars=c("PBH", "PDE"))),
tar_target(s3_passed_glm_emms_or_agent_contrasts, convert_contrasts_OR(s3_passed_glm_emms_agent_contrasts)),
#========================================================================== TOaST
tar_target(s3_passed_toast, run_toast_test(s3_passed_glm, bound_val=0.52)),
tar_target(s3_passed_pwr_toast, run_toast_test(s3_passed_glm, bound_val=0.94)),
tar_target(s3_passed_toast_plot, plot_toast(model=s3_passed_glm, name="s3_passed", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
#========================================================================== Model Comparison
tar_target(s3_passed_all_glms, fit_all_glmer(df=s3_passed_df, "MJ", predictors=c("PBH","PDE","Agent"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(s3_passed_model_comp, compare_models(s3_passed_all_glms)),
tar_target(s3_passed_test_models_full_PBHxPDE, test_two_models_performance(model1=s3_passed_all_glms[["PBH × PDE × Agent"]], model2=s3_passed_all_glms[["PBH × PDE"]])),
# RE structure
tar_target(s3_passed_all_glms_re, fit_all_glmer_RE(df = s3_passed_df,DV = "MJ",FE_structure = "PBH*PDE*Agent")),
tar_target(s3_passed_model_comp_re, compare_models(s3_passed_all_glms_re)),
#========================================================================== Model Checks
tar_target(s3_passed_model_checks, conduct_model_diagnostics(model=s3_passed_glm, name="model_checks", panelling=TRUE, chosen_type="discrete_both", colours=twelve_colours, theme=georgia_theme)),
tar_target(s3_passed_linear_pred, check_linear_pred(model=s3_passed_glm, terms=c("PBH [all]","PDE [all] ","Agent [all]"), name="full", theme=georgia_theme)),
#==================================================== Item-Level Checks
tar_target(s3_passed_item_level_dilemma_df, create_df_for_item_level_checks(s3_passed_df)),
tar_target(s3_passed_match_item_checks, check_item_difficulty(s3_passed_item_level_dilemma_df, "Match")),
#========================================================================PMC 
tar_target(pmc_csv, "data_raw/s2_pmc_data.csv", format = "file"),
tar_target(s2_demo_csv, "data_clean/s2_demo.csv", format = "file"),
tar_target(s2_clean_demo, read_in_csv(s2_demo_csv)),
tar_target(raw_pmc_data, read_in_csv(pmc_csv)),
tar_target(clean_pmc_data, clean_pmc_raw_data(raw_pmc_data, s2_clean_demo)),
tar_target(pmc_df, add_demo_to_pmc_df(clean_pmc_data, s2_clean_demo)),
tar_target(save_pmc_df, {write.csv(pmc_df, "data_clean/pmc_df.csv"); "data_clean/pmc_df.csv"}, format = "file"),
#===== CFA from McMurtie
# make model
tar_target(mcmurtie_pmc_model, create_mcmurtie_pmc_model()),
# run cfa
tar_target(pmc_cfa, conduct_confirmatory_factor_analysis(pmc_df, mcmurtie_pmc_model,"AI_PMC", georgia_theme)),
tar_target(pmc_cfa_AI_baseline, conduct_confirmatory_factor_analysis(pmc_df, mcmurtie_pmc_model,"Human_PMC", georgia_theme)),
#===== EFA
tar_target(pmc_efa, conduct_exploratory_factor_analysis(pmc_df, "AI_PMC")),
tar_target(pmc_efa_AI_baseline, conduct_exploratory_factor_analysis(pmc_df, "Human_PMC")),
tar_target(pmc_scree, make_pretty_scree_plot(pmc_df, "AI_PMC", georgia_theme)),
tar_target(pmc_scree_AI_baseline, make_pretty_scree_plot(pmc_df, "Human_PMC", georgia_theme)),
tar_target(pmc_items_by_factors, create_item_by_factor_summary(pmc_efa$efa)),
tar_target(pmc_pre_prep_efa_df, add_efa_factor_cats_to_pmc_df(pmc_df, pmc_items_by_factors$reduced$All, pmc_items_by_factors$reduced$`>0.4`)),
tar_target(pmc_efa_df, prep_for_pmc_analysis(pmc_pre_prep_efa_df)),
tar_target(save_pmc_efa_df, {write.csv(pmc_efa_df, "data_clean/pmc_efa_df.csv"); "data_clean/pmc_efa_df.csv"}, format = "file"),
#tar_target(mod_df, add_avg_pmc_scores_to_df(df, avg_pmc_id, avg_pmc_dimension_id, avg_mcmurtie_pmc_dimension_id, avg_pmc_item_id, avg_pmc_efa_id, avg_pmc_efa_good_id)),
#============== CFA from EFA
# make model
tar_target(pmc_efa_model, build_cfa_from_fa(pmc_efa)),
tar_target(pmc_efa_cfa, conduct_confirmatory_factor_analysis(pmc_df, pmc_efa_model,"AI_PMC", georgia_theme)),
tar_target(pmc_efa_cfa_AI_baseline, conduct_confirmatory_factor_analysis(pmc_df, pmc_efa_model,"Human_PMC", georgia_theme)),
#-------------- GLMMs
# tar_target(pmc_only_item_glm, fit_lmer(pmc_efa_df, "AI_PMC", FE_structure = "Item", RE_structure = "(1|ID) + (1|ImagePair)")),
# tar_target(pmc_only_glm, fit_lmer(pmc_efa_df, "AI_PMC", FE_structure = "PMC_dimension", RE_structure = "(1|ID) + (1|AI_image) + (1|Human_image)")),
# tar_target(pmc_by_item_plot, plot_pmc_by_items(df=pmc_efa_df, name="pmc_by_item_plot", colours=twelve_colours, theme=georgia_theme, Agent_PMC="AI_PMC")),
# summarise PMC scores for each ID
tar_target(avg_pmc_id, avg_pmc(pmc_efa_df, "AI_PMC")),
tar_target(avg_pmc_dimension_id, avg_pmc_dimension(pmc_efa_df, "AI_PMC")),
tar_target(avg_mcmurtie_pmc_dimension_id, avg_mcmurtie_pmc_dimension(pmc_efa_df, "AI_PMC")),
tar_target(avg_pmc_item_id, avg_pmc_item(pmc_efa_df, "AI_PMC")),
tar_target(avg_pmc_efa_id, avg_pmc_efa(pmc_efa_df, "AI_PMC")),
tar_target(avg_pmc_efa_good_id, avg_pmc_efa_good(pmc_efa_df, "AI_PMC")),
tar_target(pre_mod_df, add_avg_pmc_scores_to_df(s3_df, avg_pmc_id, avg_pmc_dimension_id, avg_mcmurtie_pmc_dimension_id, avg_pmc_item_id, avg_pmc_efa_id, avg_pmc_efa_good_id)),
tar_target(mod_df, prep_for_pmc_mods_analysis(pre_mod_df)),
## MDS
tar_target(save_pmc_mod_df, {write.csv(mod_df, "data_clean/pmc_mod_df.csv"); "data_clean/pmc_mod_df.csv"}, format = "file"),
tar_target(mod_one_row_df, one_row_per_mod_df(mod_df)),
tar_target(pmc_mean_ratings, compute_mean_ratings(mod_one_row_df)),
tar_target(pmc_dissimilarity_matrix, compute_dissimilarity_matrix(pmc_mean_ratings)),
tar_target(pmc_hierarchical_clustering, perform_hierarchical_clustering(pmc_dissimilarity_matrix, pmc_mean_ratings)),
tar_target(pmc_mds, perform_classical_mds(pmc_dissimilarity_matrix)),
tar_target(pmc_mds_plot, plot_mds_results(pmc_mds$mds_df, pmc_mean_ratings)),
## cat
tar_target(pre_prep_pmc_cat_df, convert_pmc_to_category_var(pmc_var="PMC_overall", df=mod_df, new_var="PMC_cat_overall")),
tar_target(pmc_cat_df, prep_for_pmc_cat_mods_analysis(pre_prep_pmc_cat_df)),
tar_target(save_pmc_cat_df, {write.csv(pmc_cat_df, "data_clean/pmc_cat_df.csv"); "data_clean/pmc_cat_df.csv"}, format = "file"),
tar_target(pmc_cat_glm, fit_glmer(pmc_cat_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_cat", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_overall", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_quadratic_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*(PMC_overall + I(PMC_overall^2))", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_agency_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_agency", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
#tar_target(pmc_agency_med_glm, fit_glmer(mod_df, "PMC_agency", FE_structure = "PBH*PDE*Agent", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_experience_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_experience", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
#tar_target(pmc_mcmurtie_affect_dimension_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_affect", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
# tar_target(pmc_mcmurtie_mental_moral_dimension_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_mental_moral", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
# tar_target(pmc_mcmurtie_reality_dimension_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_reality", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_factor_1_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_factor_1", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_factor_2_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_factor_2", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_factor_3_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_factor_3", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
# tar_target(pmc_good_factor_1_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_good_factor_1", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
# tar_target(pmc_good_factor_2_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_good_factor_2", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
# tar_target(pmc_good_factor_3_glm, fit_glmer(mod_df, "MJ", FE_structure = "PBH*PDE*Agent*PMC_good_factor_3", RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
#========= Plot Predicted Probabilities
tar_target(pmc_overall_pred_prob_plot, plot_predicted_probabilities(model=pmc_glm, name="pmc_overall_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(pmc_agency_pred_prob_plot, plot_predicted_probabilities(model=pmc_agency_glm, name="pmc_agency_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(pmc_experience_pred_prob_plot, plot_predicted_probabilities(model=pmc_experience_glm, name="pmc_experience_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(pmc_factor_1_pred_prob_plot, plot_predicted_probabilities(model=pmc_factor_1_glm, name="pmc_factor_1_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(pmc_factor_2_pred_prob_plot, plot_predicted_probabilities(model=pmc_factor_2_glm, name="pmc_factor_2_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
tar_target(pmc_factor_3_pred_prob_plot, plot_predicted_probabilities(model=pmc_factor_3_glm, name="pmc_factor_3_pred_prob_plot", path="figures", colours=twelve_colours, theme=georgia_theme, DV_name="Morally Permissible")),
#========================================================================== EMMs
tar_target(pmc_glm_emms, get_emmeans(pmc_glm, c("Agent", "PBH", "PDE", "PMC_overall"))),
tar_target(pmc_glm_emms_or, convert_emmeans_OR(pmc_glm_emms)),
tar_target(pmc_glm_emms_contrasts, get_all_contrasts(pmc_glm_emms)),
tar_target(pmc_glm_emms_or_contrasts, convert_contrasts_OR(pmc_glm_emms_contrasts)),
tar_target(pmc_glm_emms_twoway, get_emmeans(pmc_glm, c("PBH", "PMC_overall"))),
tar_target(pmc_glm_emms_twoway_or, convert_emmeans_OR(pmc_glm_emms_twoway)),
tar_target(pmc_glm_emms_twoway_byPMC_overall_contrasts, get_two_way_contrasts(pmc_glm_emms_twoway[["by_PBH"]], by_vars="PMC_overall")),
tar_target(pmc_glm_emms_twoway_byPMC_overall_or_contrasts, convert_contrasts_OR(pmc_glm_emms_twoway_byPMC_overall_contrasts)),
tar_target(pmc_glm_emms_twoway_PDE, get_emmeans(pmc_glm, c("PDE", "PMC_overall"))),
tar_target(pmc_glm_emms_twoway_PDE_or, convert_emmeans_OR(pmc_glm_emms_twoway_PDE)),
tar_target(pmc_glm_emms_twoway_PDE_byPMC_overall_contrasts, get_two_way_contrasts(pmc_glm_emms_twoway_PDE[["by_PDE"]], by_vars="PMC_overall")),
tar_target(pmc_glm_emms_twoway_PDE_byPMC_overall_or_contrasts, convert_contrasts_OR(pmc_glm_emms_twoway_PDE_byPMC_overall_contrasts)),
tar_target(pmc_glm_emms_twoway_Agent, get_emmeans(pmc_glm, c("Agent", "PMC_overall"))),
tar_target(pmc_glm_emms_twoway_Agent_or, convert_emmeans_OR(pmc_glm_emms_twoway_Agent)),
tar_target(pmc_glm_emms_twoway_Agent_byPMC_overall_contrasts, get_two_way_contrasts(pmc_glm_emms_twoway_Agent[["by_Agent"]], by_vars="PMC_overall")),
tar_target(pmc_glm_emms_twoway_Agent_byPMC_overall_or_contrasts, convert_contrasts_OR(pmc_glm_emms_twoway_Agent_byPMC_overall_contrasts)),
tar_target(pmc_glm_emms_pmc_overall_contrasts, get_two_way_contrasts(pmc_glm_emms[["by_PMC_overall"]], by_vars=c("PBH", "PDE"))),
tar_target(pmc_glm_emms_or_pmc_overall_contrasts, convert_contrasts_OR(pmc_glm_emms_pmc_overall_contrasts)),
#-------------- TOaST
# overall
tar_target(pmc_toast, run_toast_test(pmc_glm, bound_val=0.52)),
tar_target(pmc_toast_plot, plot_toast(model=pmc_glm, name="pmc_overall", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
# agency
tar_target(pmc_agency_toast, run_toast_test(pmc_agency_glm, bound_val=0.52)),
tar_target(pmc_agency_toast_plot, plot_toast(model=pmc_agency_glm, name="pmc_agency", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
# experience
tar_target(pmc_experience_toast, run_toast_test(pmc_experience_glm, bound_val=0.52)),
tar_target(pmc_experience_toast_plot, plot_toast(model=pmc_experience_glm, name="pmc_experience", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
# factor_1
tar_target(pmc_factor_1_toast, run_toast_test(pmc_factor_1_glm, bound_val=0.52)),
tar_target(pmc_factor_1_toast_plot, plot_toast(model=pmc_factor_1_glm, name="pmc_factor_1", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
# factor_2
tar_target(pmc_factor_2_toast, run_toast_test(pmc_factor_2_glm, bound_val=0.52)),
tar_target(pmc_factor_2_toast_plot, plot_toast(model=pmc_factor_2_glm, name="pmc_factor_2", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
# factor_3
tar_target(pmc_factor_3_toast, run_toast_test(pmc_factor_3_glm, bound_val=0.52)),
tar_target(pmc_factor_3_toast_plot, plot_toast(model=pmc_factor_3_glm, name="pmc_factor_3", colours=twelve_colours, theme=georgia_theme, bound_val = 0.52)),
#-------------- Model Comp
tar_target(all_pmc_glms, fit_all_glmer(df=mod_df, "MJ", predictors=c("PBH","PDE","Agent","PMC_overall"), RE_structure = "(1+PBH+PDE|ID) + (1|Dilemma)", family_link=binomial(link = "logit"))),
tar_target(pmc_model_comp, compare_models(all_pmc_glms)),
tar_target(heatmap_mcmurtie_pmc, create_pmc_heatmap(df=pmc_df, Agent_PMC="AI_PMC", factor_col="PMC_dimension", Factor_Colours=list(Factor = c(agency = "#FB9A99",experience = "#33A02C")),loadings=pmc_cfa$standardized_loadings, name="mcmurtie_pmc_")),
tar_target(heatmap_efa_cfa_pmc, create_pmc_heatmap(df=pmc_efa_df, Agent_PMC="AI_PMC", factor_col="PMC_efa", Factor_Colours=list(Factor = c(`Factor 1` = "#FB9A99",`Factor 2` = "#33A02C", `Factor 3`="#6A3D9A")),loadings=pmc_efa_cfa$standardized_loadings, name="efa_cfa_pmc_"))

)

#tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)