################################################################################
# GLMM
################################################################################
prep_for_analysis <- function(df){
  # make sure everything is factor
  df$Task.Name <- as.factor(df$Task.Name)
  df$Dilemma <- as.factor(df$Dilemma)
  df$Agent <- as.factor(df$Agent)
  df$Context <- as.factor(df$Context)
  df$PDE <- as.factor(df$PDE)
  df$BA <- as.factor(df$BA)
  df$MC <- as.factor(df$MC)
  df$Intent <- as.factor(df$Intent)
  df$PBH <- as.factor(df$PBH)
  df$MG <- as.factor(df$MG)
  df$RT <- abs(as.numeric(df$RT))
  
  
  #set deviation contrasts for ease of interpretation -.5 vs .5
  c<-contr.treatment(2)
  my.coding<-matrix(rep(1/2, 2), ncol=1)
  my.simple<-c-my.coding
  
  contrasts(df$PDE)<-my.simple
  
  contrasts(df$BA)<-my.simple
  
  contrasts(df$MC)<-my.simple
  
  contrasts(df$Intent)<-my.simple
  
  contrasts(df$PBH)<-my.simple
  
  contrasts(df$MG)<-my.simple
  
  contrasts(df$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
  
  df <- df %>%
    mutate(MJ = recode(MJ, "Yes" = "1",
                       "No" = "0"))
  
  df$MJ <- as.factor(df$MJ)
  df
}

fit_glmer <- function(df, DV, FE_structure, RE_structure, family_link){
  df <- df %>%
    mutate(ID = factor(ID),Dilemma = factor(Dilemma))
  # Build formula from strings
  formula_text <- paste0(DV, " ~ ", FE_structure, " + ", RE_structure)
  formula_obj  <- as.formula(formula_text)
  glmer(formula_obj,data = df,family = family_link,control = glmerControl(optCtrl = list(maxfun = 2e5),optimizer = "bobyqa"))
}

################################################################################
# EMMs
################################################################################
get_emmeans <- function(model, predictors, cov.reduce=range) {
  emm_list <- list()
  for(cond_var in predictors) {
    rhs <- paste(setdiff(predictors, cond_var), collapse="*")
    f <- as.formula(paste0("~ ", rhs, "|", cond_var))
    emm_list[[paste0("by_", cond_var)]] <- emmeans::emmeans(model, f, cov.reduce=cov.reduce)
  }
  emm_list
}

## Convert EMMs & CIs to OR
convert_emmeans_OR <- function(emm_list) {
  lapply(emm_list, function(x) {df <- as.data.frame(x); df$emmean <- exp(df$emmean); df$asymp.LCL <- exp(df$asymp.LCL); df$asymp.UCL <- exp(df$asymp.UCL); df})
}

## Contrasts of the EMMs
get_all_contrasts <- function(emm_list) {lapply(names(emm_list), function(nm) emmeans::contrast(emm_list[[nm]], interaction="pairwise", by=sub("by_","",nm))) |> setNames(names(emm_list))}

## Contrasts of the EMMs in OR
convert_contrasts_OR <- function(contrast_list) {lapply(contrast_list, function(x) {df <- as.data.frame(x); df$estimate <- exp(df$estimate); df})}


################################################################################
# TOaST
################################################################################
run_toast_test <- function(model, bound_val){
  toast <- parameters::equivalence_test(model, rule="classic", range=c(-bound_val,bound_val))
  as.data.frame(toast)
}

################################################################################
# Model Comparison
################################################################################
# First, we fit models with all different combinations of our variables of interest
fit_all_glmer <- function(df, DV, predictors, RE_structure, family_link){
  combos <- unlist(lapply(seq_along(predictors), \(n) combn(predictors,n,simplify=FALSE)), recursive=FALSE)
  models <- lapply(combos, \(x) glmer(as.formula(paste(DV,"~",paste(x,collapse="*"),"+",RE_structure)),
                                      data=df, family=family_link,
                                      control=glmerControl(optCtrl=list(maxfun=2e5),optimizer="bobyqa")))
  names(models) <- sapply(combos, function(x) paste(x, collapse=" × "))
  models
}

# next, we compare the models we created above
compare_models <- function(models) {
  out <- performance::compare_performance(models)
  out$Name <- names(models)
  out
}

test_two_models_performance <- function(model1, model2){
  test_performance(model1, model2)
}
############################################################## for RE structure
fit_all_glmer_RE <- function(df, DV, FE_structure = "PBH*PDE*Agent",family_link = binomial(link = "logit")) {
  REs <- c("(1|ID) + (1|Dilemma)","(1+PBH|ID) + (1|Dilemma)","(1+PDE|ID) + (1|Dilemma)","(1+Agent|ID) + (1|Dilemma)","(1+PBH+PDE|ID) + (1|Dilemma)", "(1+PBH+Agent|ID) + (1|Dilemma)","(1+PDE+Agent|ID) + (1|Dilemma)","(1+PBH+PDE+Agent|ID) + (1|Dilemma)","(1+PBH+PDE+Agent|ID) + (1+Agent|Dilemma)")
  models <- lapply(REs, function(re) {
    glmer(as.formula(paste(DV, "~", FE_structure, "+", re)),data = df,family = family_link,control = glmerControl(optCtrl = list(maxfun = 2e5),optimizer = "bobyqa"))})
  names(models) <- REs
  models
}
