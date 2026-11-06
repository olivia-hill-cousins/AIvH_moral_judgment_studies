################################################################################
# REACTION TIME MODELLING
################################################################################
plot_logTransform_histogram <- function(df, name, path="figures",theme = georgia_theme){
  df_temp <- df %>% mutate(logRT = log(RT))
  p <- ggplot(df_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "#B2DF8A", alpha = 0.7, color = "#33A02C") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("No Constant"), x = "log(RT)", y = "Count") +
    theme +
    theme(plot.caption = element_text(size=9, hjust = 0), plot.title = element_text(size=9), axis.title = element_text(size=9)) 
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, "_logTransform_hist_plots.png")),
    plot=p,
    width=7.27,
    height=5.83,
    units="in",
    dpi=300
  )
  p
  
  
}

plot_logTransform_constant_histograms <- function(df, name, path="figures",theme = georgia_theme){
  constants <- c(0.1, 1, 10, 100, 1000)
  plots <- lapply(constants, function(c) {
    df_temp <- df %>% mutate(logRT = log(RT + c))
    ggplot(df_temp, aes(x = logRT)) +
      geom_histogram(bins = 50, fill = "#B2DF8A", alpha = 0.7, color = "#33A02C") +
      #geom_density(color = pink_red, size = 1) +
      labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") +
      theme +
      theme(plot.caption = element_text(size=9, hjust = 0), plot.title = element_text(size=9), axis.title = element_text(size=9)) 
  })
  wrap <- wrap_plots(plots, nrow = 3) 
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, "_logTransform_hist_plots.png")),
    plot=wrap,
    width=7.27,
    height=5.83,
    units="in",
    dpi=300
  )
  wrap
  
  
}
## save the df we create with logRT added to it 
create_df_with_logRT <- function(df, constant=1){
  df$logRT <- log(df$RT + constant)
  df$logRT <- scale(df$logRT, scale = FALSE)
  df
}

fit_glmer_with_logRT <- function(df, DV, FE_structure, RE_structure){
  df <- df %>%
    mutate(ID = factor(ID),Dilemma = factor(Dilemma))
  # Build formula from strings
  formula_text <- paste0(DV, " ~ ", FE_structure, " + ", RE_structure)
  formula_obj  <- as.formula(formula_text)
  glmer(formula_obj,data = df)
}
################################################################################
# ITEM LEVEL ANALYSES
################################################################################
create_df_for_item_level_checks <- function(df){
  df <- df %>%
    mutate(Match = ifelse(MG == "Yes" & MJ == 1 | 
                            MG == "No"  & MJ == 0, "Yes", "No"))
  df$Match <- as.factor(df$Match)
  df
}

check_item_difficulty <- function(df, Match_col){
  df %>%
    count(Dilemma, .data[[Match_col]]) %>%
    group_by(Dilemma) %>%
    mutate(Prop = round(n / sum(n), 3)) %>%
    dplyr::select(Dilemma, .data[[Match_col]], Prop, n) %>%
    pivot_wider(names_from = .data[[Match_col]], values_from = c(Prop, n), 
                names_sep = "_", values_fill = 0)
}

################################################################################
# PMC
################################################################################
# here we dplyr::filter & select the data we need
clean_pmc_raw_data <- function(df, prolific_demo){
  # keep only data rows with moral judgements, remove irrelevant rows (e.g., instructions, etc)
  ids_to_keep <- prolific_demo |> 
    dplyr::pull(ID)
  df <- df %>% 
    filter(Response.Type == "response") |> 
    dplyr::filter(Participant.Private.ID %in% ids_to_keep) |> 
    rename(ID = Participant.Private.ID)
  # select data we need 
  df <- df %>% 
    dplyr::select(ID, `Task.Name`, `Trial.Number`, "Spreadsheet..mcmurtie_dimensions_mind_perception","Spreadsheet..mind_perception_items","Spreadsheet..dimensions_mind_perception",            
                  "Spreadsheet..AI","Spreadsheet..Human","Spreadsheet..left_image", "Spreadsheet..right_image","Spreadsheet..randomise_item", Response,    
                  "Store..pmc_response", `Absolute.Reaction.Time`)
  
  # rename columns to make them easier to work with
  df <- df %>%
    rename(Task_Name = `Task.Name`,
           Trial_Number = `Trial.Number`,
           Item = "Spreadsheet..mind_perception_items",
           PMC_dimension = "Spreadsheet..dimensions_mind_perception",  
           McMurtie_dimension = "Spreadsheet..mcmurtie_dimensions_mind_perception",
           RT = `Absolute.Reaction.Time`,
           AI_image = "Spreadsheet..AI",
           Human_image = "Spreadsheet..Human",
           left_image = "Spreadsheet..left_image",
           right_image = "Spreadsheet..right_image",
           randomise_item = "Spreadsheet..randomise_item", 
           PMC = Response) |> 
    mutate(
      PMC = as.numeric(PMC),
      AI_position = if_else(left_image == "AI", "Left", "Right"),
      AI_PMC = if_else(AI_position == "Left", -PMC, PMC),
      Human_PMC = if_else(AI_position == "Left", PMC, -PMC),
      ImagePair = paste(AI_image, Human_image, sep = "_"),
      AI_PMC_prop = (AI_PMC + 100) / 200,
      Human_PMC_prop = (Human_PMC + 100) / 200
    ) |> 
    mutate(AI_PMC = replace_na(AI_PMC, 0),
           AI_PMC_stand = AI_PMC / sd(AI_PMC, na.rm = TRUE),
           Human_PMC = replace_na(Human_PMC, 0),
           Human_PMC_stand = Human_PMC / sd(Human_PMC, na.rm = TRUE)
           )
  
  return(df)
  
}

## principle component analysis
conduct_principal_component_analysis <- function(df,model, Agent_PMC, georgia_theme) {
  df$Item <- trimws(df$Item, which = "right")
  clean_item <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("['’]", "") |>
      str_replace_all("[/-]", " ") |>
      str_squish()
  }
  
  df <- df |>
    mutate(
      Items = clean_item(Item))
  
  df <- df |> 
    pivot_wider(names_from=Items, values_from=all_of(Agent_PMC), names_prefix="", id_cols="ID") |> 
    clean_names()  
  df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  df <- df |> 
    mutate(across(everything(), as.numeric))
  summary(pca_result)
  pca_result <- princomp(df[2:ncol(df)], cor=T)
  # screeplot
  # visualize importance of each component
  scree <- screeplot(pca_result, type="lines") + georgia_theme
  graph <- plot(pca_result) + georgia_theme
  biplot <- biplot(pca_result)
  t <- list(bart, kmo, parallel, parallel_graph, efa_1, efa_2, efa, efa_diagram)
  names(t) <- c("bart", "kmo", "parallel", "parallel_graph","efa_1", "efa_2", "efa", "efa_diagram")
  return(t)
}

## create mcmurite model
create_mcmurtie_pmc_model <- function(){
  '
    experience =~ can_feel_hungry +
                  can_smell_and_taste +
                  can_feel_pain +
                  can_feel_panic +
                  can_feel_happy +
                  can_feel_pleasure +
                  can_get_angry +
                  can_have_emotions +
                  can_love_specific_people +
                  can_have_intense_urges +
                  can_see_or_hear_the_world_around_them +
                  can_feel_temperature_touch
  
    agency =~ can_plan_for_the_future +
              can_provide_reasons_for_their_actions +
              can_uphold_moral_values +
              can_understand_a_persons_goals +
              can_explain_their_decisions +
              can_set_goals +
              can_praise_moral_actions +
              can_disapprove_of_immoral_actions +
              can_reason_logically +
              can_understand_others_minds
  '
}
## confirmatory factor analysis
conduct_confirmatory_factor_analysis <- function(df,model, Agent_PMC, georgia_theme) {
  df$Item <- trimws(df$Item, which = "right")
  clean_item <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("['’]", "") |>
      str_replace_all("[/-]", " ") |>
      str_squish()
  }
  model1 <- model
  nobs <- length(unique(df$ID))
  df <- df |>
    mutate(
      Items = clean_item(Item))
  
  df <- df |> 
    pivot_wider(names_from=Items, values_from=all_of(Agent_PMC), names_prefix="", id_cols="ID") |> 
    clean_names()  |> 
    dplyr::select(-id) 
  df <- df |> 
    mutate(across(everything(), as.numeric))
  cor_data <- cor(df)
  fit <- cfa(model1, data=df,sample.cov=cor_data, sample.nobs=nobs,estimator="MLR")
  summary(fit,fit.measures=TRUE)
  fit_indices <- nice_fit(fit, stars=TRUE)
  fit_indices_table <- nice_fit(fit, stars=TRUE, nice_table=TRUE)
  cfa_model_plot <- cfa_fit_plot(fit,df)
  standardized_loadings <- standardizedSolution(fit) |>
    filter(op == "=~") |>
    dplyr::select(factor = lhs, item = rhs, est.std, se, z, pvalue)
  # reliability
  factor_mcdonald_omega <- compRelSEM(fit)
  agency_mcdonald_omega <- factor_mcdonald_omega["agency"]$agency[1]
  experience_mcdonald_omega <- factor_mcdonald_omega["experience"]$experience[1]
  # convergent validity 
  ave <- AVE(fit)
  # discriminant validity 
  dV <- htmt(model1, df,htmt2=FALSE)
  
  # cfa_plot
  fig_cfa <- cfa_fit_plot(fit, df) +georgia_theme
  png("figures/cfa_fit_plot.png", width = 1200, height = 900, res = 150)
  print(fig_cfa)
  dev.off()
  t <- list(fit_indices, fit_indices_table, cfa_model_plot, standardized_loadings, factor_mcdonald_omega, agency_mcdonald_omega, experience_mcdonald_omega, ave, dV)
  names(t) <- c("fit_indices", "fit_indices_table", "cfa_model_plot", "standardized_loadings", "factor_mcdonald_omega", "agency_mcdonald_omega", "experience_mcdonald_omega", "ave", "discriminant_validity")
  return(t)
}

# add demo data to df
add_demo_to_pmc_df <- function(df, demo){
  df <- df |> left_join(demo, by="ID") |> 
    dplyr::select(ID, Gender, Age, Ethnicity, Nationality, Country_of_Birth, Country_of_Residence, ID, Task_Name, Trial_Number, McMurtie_dimension, Item, PMC_dimension, AI_image, Human_image, PMC, RT, AI_position, AI_PMC, AI_PMC_stand, Human_PMC, Human_PMC_stand, ImagePair, AI_PMC_prop, Human_PMC_prop)
  df
}


conduct_exploratory_factor_analysis <- function(df, Agent_PMC) {
  nobs <- length(unique(df$ID))
  df <- df |> 
    pivot_wider(names_from=Item, values_from=.data[[Agent_PMC]], names_prefix="", id_cols="ID") |> 
    clean_names()  |> 
    dplyr::select(-id) 
  df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  df <- df |> 
    mutate(across(everything(), as.numeric))
  bart <- REdaS::bart_spher(df)
  kmo <- psych::KMO(df)
  parallel <- paran(df, cfa=TRUE)
  parallel_graph <- paran(df, cfa=TRUE, graph=TRUE, color=TRUE, col=c("#1F78B4","#FB9A99", "#33A02C")) #anything above blue line is a valid factor. Can see that 3 dots are above.
  cor_data <- cor(df)
  efa_1 <- psych::fa(cor_data, nfactors=1, n.obs=nobs,fm="minres", rotate="Promax")
  efa_2 <- psych::fa(cor_data, nfactors=2, n.obs=nobs,fm="minres", rotate="Promax")
  efa <- psych::fa(cor_data, nfactors=3, n.obs=nobs,fm="minres", rotate="Promax")
  efa_diagram <- psych::fa.diagram(efa, main="df", rsize=0.5)
  png("figures/efa_diagram.png", width = 1200, height = 900, res = 150)
  psych::fa.diagram(efa, main="df")
  dev.off()
  png("figures/parallel_analysis.png", width = 1200, height = 900, res = 150)
  paran(
    df,
    cfa = TRUE,
    graph = TRUE,
    color = TRUE,
    col = c("#1F78B4", "#FB9A99", "#33A02C")
  )
  dev.off()
  t <- list(bart, kmo, parallel, parallel_graph, efa_1, efa_2, efa, efa_diagram)
  names(t) <- c("bart", "kmo", "parallel", "parallel_graph","efa_1", "efa_2", "efa", "efa_diagram")
  return(t)
}

make_pretty_scree_plot <- function(df, Agent_PMC,georgia_theme) {
  nobs <- length(unique(df$ID))
  df <- df |> 
    pivot_wider(names_from=Item, values_from=.data[[Agent_PMC]], names_prefix="", id_cols="ID") |> 
    clean_names()  |> 
    dplyr::select(-id) 
  df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  df <- df |> 
    mutate(across(everything(), as.numeric))
  cor_data <- cor(df)
  # ---  Making a Pretty Scree Plot with Parallel Analysis Using ggplot2 --- # 
  parallel = fa.parallel(cor_data,      # specify our data frame;
                         fa = 'fa',     # indicate that we only want the CF eigenvalues 
                         n.iter = 50,   # indicate that 50 times simulation for our parallel analysis;
                         n.obs = nobs,  # specify the number of observations in our data frame;
                         quant = .95)   # specify that we would like the 95th quantile
  # STEP 1: Create data frame from observed eigenvalue data
  obs <- data.frame(parallel$fa.values)
  obs$type <- c('Observed Data')
  obs$num <- c(row.names(obs))
  obs$num <- as.numeric(obs$num)
  colnames(obs) <- c('eigenvalue', 'type', 'num')
  
  # STEP 2: Calculate quantiles for eigenvalues, 
  # but only store those from simulated CF model in percentile1
  percentile <- apply(parallel$values, 2, function(x) quantile(x, .95))
  min <- as.numeric(nrow(obs))
  min <- (4 * min) - (min - 1)
  max <- as.numeric(nrow(obs))
  max <- 4 * max
  percentile1 <- percentile[min:max]
  
  # Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
  sim <- data.frame(percentile1)
  sim$type <- c("Simulated Data (95th Percentile)")
  sim$num <- c(row.names(obs))
  sim$num <- as.numeric(sim$num)
  colnames(sim) <- c("eigenvalue", "type", "num")
  
  # Merge the two data frames (obs and sim) together into data frame called eigendat
  eigendat <- rbind(obs, sim)
  
  # Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
  p <- ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
    # Add lines connecting data points
    geom_line() +
    # Add the data points.
    geom_point(size = 4) +
    # Label the y-axis 'Eigenvalue'
    scale_y_continuous(name = "Eigenvalue") +
    # Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
    scale_x_continuous(name = "Factor Number", breaks = min(eigendat$num):max(eigendat$num)) +
    # Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
    scale_shape_manual(values = c(16, 1)) +
    # Add vertical line indicating parallel analysis suggested max # of factors to retain
    geom_vline(xintercept = parallel$nfact, linetype = "dashed") +
    # Apply theme
    georgia_theme  +
    theme(legend.title=element_blank(),
          legend.position=c(.7,.8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()) 
    
    
  file_name <- paste0("figures/",Agent_PMC, "_scree_plot_with_parallel_analysis.png")
  png(file_name, width = 8, height = 8,units = "in",res=300)
  print(p)
  
  dev.off()
  p
}



## Now we create a df that summarises the items suggested by EFA to belong to each factor
create_item_by_factor_summary <- function(fa){
  loadings <- as.data.frame(unclass(fa$loadings))
  
  item_membership <- loadings |>
    mutate(item = rownames(loadings)) |>
    relocate(item) |>
    mutate(
      primary_factor = colnames(loadings)[apply(abs(loadings), 1, which.max)],
      primary_loading = apply(loadings, 1, function(x) x[which.max(abs(x))])
    )
  factor_columns <- item_membership |>
    arrange(primary_factor, desc(abs(primary_loading))) |>
    group_by(primary_factor) |>
    mutate(row = row_number()) |>
    ungroup() |>
    dplyr::select(primary_factor, row, item) |>
    pivot_wider(
      names_from = primary_factor,
      values_from = item
    ) |> 
    dplyr::select(-row) |> 
    rename(`Factor 1`="MR1",
           `Factor 2`="MR2",
           `Factor 3` = "MR3") |> 
    mutate(
      `Factor 1` = str_replace(`Factor 1`, "^[^_]+_", ""), 
      `Factor 2` = str_replace(`Factor 2`, "^[^_]+_", ""), 
      `Factor 3` = str_replace(`Factor 3`, "^[^_]+_", ""), 
      `Factor 1` = str_replace_all(`Factor 1`, "_", " "),
      `Factor 2` = str_replace_all(`Factor 2`, "_", " "),
      `Factor 3` = str_replace_all(`Factor 3`, "_", " "),
      `Factor 1` = str_to_sentence(`Factor 1`),
      `Factor 2` = str_to_sentence(`Factor 2`),
      `Factor 3` = str_to_sentence(`Factor 3`)
    )
  item_membership_filtered <- item_membership |>
    filter(abs(primary_loading) >= 0.40)
  factor_columns_filtered <- item_membership_filtered |>
    arrange(primary_factor, desc(abs(primary_loading))) |>
    group_by(primary_factor) |>
    mutate(row = row_number()) |>
    ungroup() |>
    dplyr::select(primary_factor, row, item) |>
    pivot_wider(
      names_from = primary_factor,
      values_from = item
    ) |> 
    dplyr::select(-row) |> 
    rename(`Factor 1`="MR1",
           `Factor 2`="MR2",
           `Factor 3` = "MR3") |> 
    mutate(
      `Factor 1` = str_replace(`Factor 1`, "^[^_]+_", ""), 
      `Factor 2` = str_replace(`Factor 2`, "^[^_]+_", ""), 
      `Factor 3` = str_replace(`Factor 3`, "^[^_]+_", ""), 
      `Factor 1` = str_replace_all(`Factor 1`, "_", " "),
      `Factor 2` = str_replace_all(`Factor 2`, "_", " "),
      `Factor 3` = str_replace_all(`Factor 3`, "_", " "),
      `Factor 1` = str_to_sentence(`Factor 1`),
      `Factor 2` = str_to_sentence(`Factor 2`),
      `Factor 3` = str_to_sentence(`Factor 3`)
    )
  
  fa_table <- loadings |> 
    mutate(
      primary_factor = colnames(loadings)[apply(abs(loadings), 1, which.max)],
      primary_loading = apply(loadings, 1, function(x) x[which.max(abs(x))])
    ) |>
    arrange(primary_factor, desc(abs(primary_loading))) |>
    mutate(
      MR1 = ifelse(primary_factor == "MR1", MR1, ""),
      MR2 = ifelse(primary_factor == "MR2", MR2, ""),
      MR3 = ifelse(primary_factor == "MR3", MR3, "")
    ) 
  fa_table <- fa_table |> 
    mutate(Item = rownames(fa_table)) |>
    dplyr::select(Item, MR1, MR2, MR3) |> 
    rename(`Factor 1`="MR1",
           `Factor 2`="MR2",
           `Factor 3` = "MR3") |> 
    mutate(
      Item = str_replace(Item, "^[^_]+_", ""), 
      Item = str_replace_all(Item, "_", " "),
      Item = str_to_sentence(Item)
    ) 
  
  t <- list(factor_columns,factor_columns_filtered, fa_table)
  names(t) <- c("All", ">0.4","stats_table")
  
  ## table 2
  loadings <- as.data.frame(unclass(fa$loadings))
  loadings <- loadings |> 
    mutate(
      MR2 = ifelse(abs(as.numeric(MR2)) < 0.3, NA_real_, as.numeric(MR2)),
      MR1 = ifelse(abs(as.numeric(MR1)) < 0.3, NA_real_, as.numeric(MR1)),
      MR3 = ifelse(abs(as.numeric(MR3)) < 0.3, NA_real_, as.numeric(MR3))
      
    ) |> 
    arrange(desc(abs(MR1)),desc(abs(MR2))) |> 
    rename(`Factor 1`="MR1",
           `Factor 2`="MR2",
           `Factor 3` = "MR3") |> 
    mutate(
      `Factor 1` = str_replace(`Factor 1`, "^[^_]+_", ""), 
      `Factor 2` = str_replace(`Factor 2`, "^[^_]+_", ""), 
      `Factor 3` = str_replace(`Factor 3`, "^[^_]+_", ""), 
      `Factor 1` = str_replace_all(`Factor 1`, "_", " "),
      `Factor 2` = str_replace_all(`Factor 2`, "_", " "),
      `Factor 3` = str_replace_all(`Factor 3`, "_", " "),
      `Factor 1` = str_to_sentence(`Factor 1`),
      `Factor 2` = str_to_sentence(`Factor 2`),
      `Factor 3` = str_to_sentence(`Factor 3`)
    )  |>
    mutate(Item = rownames(loadings)) |>  
    mutate(
      Item = str_replace(Item, "^[^_]+_", ""), 
      Item = str_replace_all(Item, "_", " "),
      Item = str_to_sentence(Item)
    ) |> 
    dplyr::select(Item, `Factor 1`, `Factor 2`, `Factor 3`)
  list <- list(t, loadings)
  names(list) <- c("reduced", "all")
  return(list)
  
}
add_efa_factor_cats_to_pmc_df <- function(df, items_by_factors, good_items_by_factors) {
  clean_item <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("['’]", "") |>
      str_replace_all("[/-]", " ") |>
      str_remove("^can\\s+") |>
      str_squish()
  }
  
  items_by_factors <- items_by_factors |>
    clean_names() |>
    mutate(
      across(
        c(factor_1, factor_2, factor_3),
        clean_item
      ))
  good_items_by_factors <- good_items_by_factors |>
    clean_names() |>
    mutate(
      across(
        c(factor_1, factor_2, factor_3),
        clean_item
      ))
  
  df <- df |>
    mutate(
      Items = clean_item(Item),
      PMC_efa = case_when(
        Items %in% items_by_factors$factor_1 ~ "Factor 1",
        Items %in% items_by_factors$factor_2 ~ "Factor 2",
        Items %in% items_by_factors$factor_3 ~ "Factor 3",
        TRUE ~ NA_character_
      )) |>
    mutate(
      Good_items = clean_item(Item),
      Good_item = Item,
      PMC_efa_good = case_when(
        Good_items %in% good_items_by_factors$factor_1 ~ "Factor 1",
        Good_items %in% good_items_by_factors$factor_2 ~ "Factor 2",
        Good_items %in% good_items_by_factors$factor_3 ~ "Factor 3",
        TRUE ~ NA_character_
      )) |> 
    dplyr::select(-Items, -Good_items)
  df
}

### CONDUCT CFA FROM EFA
build_cfa_from_fa <- function(fa_object, loading_cutoff = 0.40) {
  fa_object <- fa_object$efa
  # extract loadings as a clean data frame
  load <- as.data.frame(unclass(fa_object$loadings))
  load$item <- rownames(load)
  
  nf <- fa_object$factors
  factor_names <- colnames(load)[1:nf]
  
  # identify each item's primary factor
  load$primary_factor <- factor_names[apply(abs(load[, factor_names]), 1, which.max)]
  
  # extract the primary loading value
  load$primary_loading <- apply(load[, factor_names], 1, function(x) x[which.max(abs(x))])
  
  # filter weak items
  load_filtered <- load |> dplyr::filter(abs(primary_loading) >= loading_cutoff)
  
  # split items by factor
  items_by_factor <- split(load_filtered$item, load_filtered$primary_factor)
  
  # build lavaan syntax
  syntax <- purrr::imap_chr(items_by_factor, function(items, factor_name) {
    paste0(factor_name, " =~ ", paste(items, collapse = " + "))
  })
  
  # return full model string
  paste(syntax, collapse = "\n")
}
## PREPARE FOR PMC ANALYSIS
prep_for_pmc_analysis <- function(df){
  # make sure everything is factor
  df$Task_Name <- as.factor(df$Task_Name)
  df$ID <- as.factor(df$ID)
  df$McMurtie_dimension <- as.factor(df$McMurtie_dimension)
  df$PMC_dimension <- as.factor(df$PMC_dimension)
  df$AI_position <- as.factor(df$AI_position)
  df$AI_PMC <- as.numeric(df$AI_PMC)
  df$Human_PMC <- as.numeric(df$Human_PMC)
  df$AI_image <- as.factor(df$AI_image)
  df$Human_image <- as.factor(df$Human_image)
  df$ImagePair <- as.factor(df$ImagePair)
  df$AI_PMC_prop <- as.numeric(df$AI_PMC_prop)
  df$Human_PMC_prop <- as.numeric(df$Human_PMC_prop)
  df$Item <- as.factor(df$Item)
  df$RT <- abs(as.numeric(df$RT))
  df$PMC_efa <- as.factor(df$PMC_efa)
  
  df
}

prep_for_pmc_mods_analysis <- function(df){
  df <- df  |>
    mutate(
      across(
        c(PMC_overall, PMC_factor_1, PMC_factor_2, PMC_factor_3,
          PMC_agency, PMC_experience),
        ~ .x / sd(.x, na.rm = TRUE)
      )
    )
  # make sure everything is factor
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
  # scale all PMC mod vars
  # df <- df |>
  #   mutate(
  #     across(
  #       c(PMC_overall, PMC_factor_1, PMC_factor_2, PMC_factor_3, PMC_agency, PMC_experience),
  #       ~ .x / sd(.x, na.rm = TRUE)
  #     )
  #   )
  
  
  df
}

fit_lmer <- function(df, DV, FE_structure, RE_structure){
  # Build formula from strings
  formula_text <- paste0(DV, " ~ ", FE_structure, " + ", RE_structure)
  formula_obj  <- as.formula(formula_text)
  lmer(formula_obj,data = df,control = lmerControl(optCtrl = list(maxfun = 2e5),optimizer = "bobyqa"))
}

#################################################### summarise PMC 
# overall 
avg_pmc <- function(df, Agent_PMC){
  df |>
    group_by(ID) |>
    summarise(
      PMC_overall = mean(.data[[Agent_PMC]], na.rm = TRUE)
    )
}

avg_pmc_dimension <- function(df, Agent_PMC){
  df |>
    group_by(ID, PMC_dimension) |>
    summarise(
      PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = PMC_dimension,
      values_from = PMC,
      names_prefix = "PMC_"
    )
}

avg_mcmurtie_pmc_dimension <- function(df, Agent_PMC){
  df |>
    group_by(ID, McMurtie_dimension) |>
    summarise(
      PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = McMurtie_dimension,
      values_from = PMC,
      names_prefix = "PMC_"
    )
}

avg_pmc_item <- function(df, Agent_PMC){
  df |>
    group_by(ID, Item) |>
    summarise(
      PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = Item,
      values_from = PMC,
      names_prefix = "PMC_"
    )
}

avg_pmc_efa <- function(df, Agent_PMC){
  df |>
    mutate(PMC_efa = gsub(" ", "_", PMC_efa) |> tolower()) |>
    group_by(ID, PMC_efa) |>
    summarise(
      PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = PMC_efa,
      values_from = PMC,
      names_prefix = "PMC_"
    )
}


avg_pmc_efa_good <- function(df, Agent_PMC){
  df |>
    filter(!is.na(PMC_efa_good)) |> 
    group_by(ID, PMC_efa_good) |>
    summarise(
      PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = PMC_efa_good,
      values_from = PMC,
      names_prefix = "PMC_good_"
    )
}

add_avg_pmc_scores_to_df <- function(df, avg_pmc, avg_pmc_dimension, avg_mcmurtie_pmc_dimension, avg_pmc_item, avg_pmc_efa, avg_pmc_efa_good){
  df$ID <- as.factor(df$ID)
  df |> left_join(avg_pmc, by = "ID") |> 
    left_join(avg_pmc_dimension, by="ID") |> 
    left_join(avg_mcmurtie_pmc_dimension, by="ID") |> 
    left_join(avg_pmc_item, by="ID") |> 
    left_join(avg_pmc_efa, by="ID") |> 
    left_join(avg_pmc_efa_good, by="ID") 
}


#### plot PMC with -100<-Human 0 equal and +100->AI
plot_pmc_by_items <- function(df, name, path="figures", colours, theme, Agent_PMC){
  
  pmc_plot_df <- df %>%
    group_by(PMC_efa, Item) %>%
    summarise(
      mean_PMC = mean(.data[[Agent_PMC]], na.rm = TRUE),
      SE = sd(.data[[Agent_PMC]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[Agent_PMC]]))),
      .groups = "drop"
    ) %>%
    mutate(
      Item = str_replace(Item, "^can ", ""),
      Item = str_to_sentence(Item),
      PMC_efa = str_replace(PMC_efa, "_", " "),
      PMC_efa = str_to_title(PMC_efa)
    )
  
  factor_colours <- c(
    "Factor 1" = "#1F78B4",
    "Factor 2" = "#33A02C",
    "Factor 3" = "#FB9A99"
  )
  
  factor_backgrounds <- pmc_plot_df %>%
    distinct(PMC_efa) %>%
    mutate(
      xmin = -100,
      xmax = 100
    )
  
  p <- ggplot(
    pmc_plot_df,
    aes(
      x = mean_PMC,
      y = reorder(Item, mean_PMC)
    )
  ) +
    geom_rect(
      data = factor_backgrounds,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = PMC_efa
      ),
      alpha = 0.12,
      colour = NA,
      inherit.aes = FALSE
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "black"
    ) +
    geom_errorbar(
      aes(
        xmin = mean_PMC - 1.96 * SE,
        xmax = mean_PMC + 1.96 * SE
      ),
      orientation = "y",
      height = 0,
      colour = "black",
      linewidth = 0.6
    ) +
    geom_point(
      size = 1
    ) +
    scale_x_continuous(
      limits = c(-100, 100),
      breaks = seq(-100, 100, 25)
    ) +
    scale_fill_manual(
      values = factor_colours,
      name = NULL,
      guide = guide_legend(
        override.aes = list(
          shape = 22,
          size = 1,
          colour = "black",
          alpha = 0.32
        )
      )
    ) +
    facet_grid(
      PMC_efa ~ .,
      scales = "free_y",
      space = "free_y"
    ) +
    labs(
      x = "Estimated PMC attribution\nHuman ← Equal → AI",
      y = NULL
    ) +
    theme_minimal(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia"),
      axis.text = element_text(family = "Georgia"),
      axis.title = element_text(family = "Georgia"),
      legend.text = element_text(family = "Georgia"),
      legend.title = element_text(family = "Georgia"),
      legend.position = "right",
      strip.text.y = element_blank(),
      strip.background = element_blank(),
      axis.line = element_line(
        colour = "black",
        linewidth = 0.1
      )
    )
  
  dir.create(
    path,
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  ggplot2::ggsave(
    filename = file.path(path, paste0(name, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  p
}




#### pmc heatmap
create_pmc_heatmap <- function(df, Agent_PMC, factor_col, Factor_Colours,loadings, name){
  df$Item <- trimws(df$Item, which = "right")
  clean_Items <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("['’]", "") |>
      str_replace_all("[/-]", " ") |>
      str_squish()
  }
  
  df <- df |>
    mutate(
      Items = clean_Items(Item)) 
  item_factors <- df %>%
    dplyr::distinct(
      Item,
      Factor = .data[[factor_col]]
    )
  #df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  # df <- df |> 
  #   dplyr::select(ID, Items, AI_PMC) |> 
  #   mutate(Human = 0) |> 
  #   rename(
  #     AI = AI_PMC
  #   )
  
  df <- df |> 
    pivot_wider(names_from=Item, values_from=all_of(Agent_PMC), names_prefix="", id_cols="ID") 
  
  #df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  df <- df |> 
    mutate(across(everything(), as.numeric))
  item_order <- item_factors %>%
    arrange(Factor, Item) |> 
    pull(Item)
  
  df <- df[, item_order]
  item_factors <- item_factors %>%
    tibble::column_to_rownames("Item")
  item_factors <- item_factors[colnames(df), , drop = FALSE]
  direction_labels <- if (Agent_PMC == "AI_PMC") {
    c("-100 (Human higher)","-50", "0 (Equal)","+50", "100 (AI higher)")
  } else {
    c("-100 (AI higher)","-50", "0 (Equal)","+50", "100 (Human higher)")
  }
  three_colours <- brewer.pal(3, "Paired")
  scale_colours <- c(colorRampPalette(c(three_colours[[1]], three_colours[[2]]))(50),
                     "white",colorRampPalette(c(three_colours[[2]], three_colours[[3]]))(50)
  )
  number_of_factors <- length(unique(item_factors$Factor))
  item_factors <- item_factors[colnames(df), , drop = FALSE]
  # Cluster items yourself
  item_hclust <- loadings %>%
    dplyr::select(item, est.std,factor) %>%
    pivot_wider(
      names_from = factor,
      values_from = est.std
    ) %>%
    tibble::column_to_rownames("item") %>%
    replace(is.na(.), 0) %>%
    dist() %>%
    hclust(method = "complete")
  factor_order <- item_factors %>%
    arrange(Factor)
  
  df <- df[, rownames(factor_order)]
  
  factor_sizes <- table(factor_order$Factor)
  font_add("Georgia",
           regular = "Georgia.ttf", bold = "Georgia Bold.ttf",
           italic = "Georgia Italic.ttf", bolditalic = "Georgia Bold Italic.ttf"
  )
  
  showtext_auto() # Enable for all plots
  p <- pheatmap(
    df,
    color = scale_colours,
    breaks = seq(-100, 100, length.out = 101),
    legend_breaks = c(-100, -50, 0, 50, 100),
    legend_labels = direction_labels,
    cluster_cols = FALSE,
    gaps_col = cumsum(factor_sizes)[-length(factor_sizes)],
    annotation_col = factor_order,
    annotation_colors = Factor_Colours,
    annotation_names_col = FALSE,
    show_rownames = FALSE,
    fontfamily = "Georgia"
  )
  ggplot2::ggsave(
    filename=file.path("figures/", paste0(name, "heatmap.png")),
    plot=p,
    width=11,
    height=8,
    units="in",
    dpi=300
  )
  p
}



################################################################################
# 
################################################################################
plot_predicted_probability <- function(model, name, path="figures", colours, theme) {
  # Create plots and suppress plot_model output
  p <- sjPlot::plot_model(model, type="int", colors=colours) + theme + ggplot2::labs(title=NULL)
  
  # Save each plot with name + number
  dir.create(path, showWarnings=FALSE, recursive=TRUE)
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, ".png")),
    plot=p,
    width=7,
    height=5,
    dpi=300
  )
  
  p
}
## plot pred prob
plot_pmc_predicted_probabilities <- function(df, name, path="figures", colours, theme, FF_structure, RE_structure) {
  model <- glmer(
    formula = as.formula(
      paste0("MJ ~ ", FF_structure, " + ", RE_structure)
    ),
    data = df,
    family = binomial,
    control = glmerControl(
      optCtrl = list(maxfun = 2e5),
      optimizer = "bobyqa"
    )
  )
  # Create plots and suppress plot_model output
  p_int_list <- 
    sjPlot::plot_model(model, type="int", colors=colours)
  
  # Apply formatting
  p_int_list <- lapply(p_int_list, function(p) p + theme + ggplot2::labs(title=NULL))
  
  # Save each plot with name + number
  dir.create(path, showWarnings=FALSE, recursive=TRUE)
  for (i in seq_along(p_int_list)) {
    ggplot2::ggsave(
      filename=file.path(path, paste0(name, "_", i, ".png")),
      plot=p_int_list[[i]],
      width=7,
      height=5,
      dpi=300
    )
  }
  
  p_int_list
}
################################################################################
# REACTION TIME MODELLING
################################################################################

plot_logTransform_constant_histograms <- function(df, name, path="figures",theme = georgia_theme){
  constants <- c(0.1, 1, 10, 100, 1000)
  plots <- lapply(constants, function(c) {
    df_temp <- df %>% mutate(logRT = log(RT + c))
    ggplot(df_temp, aes(x = logRT)) +
      geom_histogram(bins = 50, fill = "#B2DF8A", alpha = 0.7, color = "#33A02C") +
      #geom_density(color = pink_red, size = 1) +
      labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") +
      theme +
      theme(plot.caption = element_text(size=9, hjust = 0), plot.title = element_text(size=9), axis.title = element_text(size=9)) 
  })
  wrap <- wrap_plots(plots, nrow = 3) 
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, "_logTransform_hist_plots.png")),
    plot=wrap,
    width=7.27,
    height=5.83,
    units="in",
    dpi=300
  )
  wrap
  
  
}
## save the df we create with logRT added to it 
create_df_with_logRT <- function(df, constant=1){
  df$logRT <- log(df$RT + constant)
  df$logRT <- scale(df$logRT, scale = FALSE)
  df
}

fit_glmer_with_logRT <- function(df, DV, FE_structure, RE_structure){
  df <- df %>%
    mutate(ID = factor(ID),Dilemma = factor(Dilemma))
  # Build formula from strings
  formula_text <- paste0(DV, " ~ ", FE_structure, " + ", RE_structure)
  formula_obj  <- as.formula(formula_text)
  glmer(formula_obj,data = df)
}

################################################################################
# INTENT
################################################################################
plot_predicted_probability <- function(model, name, path="figures", colours, theme) {
  # Create plots and suppress plot_model output
  p <- sjPlot::plot_model(model, type="int", colors=colours) + theme + ggplot2::labs(title=NULL)
  
  # Save each plot with name + number
  dir.create(path, showWarnings=FALSE, recursive=TRUE)
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, ".png")),
    plot=p,
    width=7,
    height=5,
    dpi=300
  )
  
  p
}
## plot pred prob
plot_pmc_predicted_probabilities <- function(df, name, path="figures", colours, theme) {
  df <- df %>% 
    mutate(
      PMC_overall_c = PMC_overall - mean(PMC_overall, na.rm = TRUE)
    )
  model <- glmer(
    MJ ~ PBH * PDE * Agent * PMC_overall_c +
      (1 + PBH + PDE | ID) +
      (1 | Dilemma),
    data = df,
    family = binomial,
    control = glmerControl(
      optCtrl = list(maxfun = 2e5),
      optimizer = "bobyqa"
    )
  )
  # Create plots and suppress plot_model output
  p_int_list <- 
    sjPlot::plot_model(model, type="int", colors=colours)
  
  # Apply formatting
  p_int_list <- lapply(p_int_list, function(p) p + theme + ggplot2::labs(title=NULL))
  
  # Save each plot with name + number
  dir.create(path, showWarnings=FALSE, recursive=TRUE)
  for (i in seq_along(p_int_list)) {
    ggplot2::ggsave(
      filename=file.path(path, paste0(name, "_", i, ".png")),
      plot=p_int_list[[i]],
      width=7,
      height=5,
      dpi=300
    )
  }
  
  p_int_list
}


##################
# 3 Category Mod
##################
convert_pmc_to_category_var <- function(pmc_var, df, new_var) {
  
  x <- df[[pmc_var]]
  df[[new_var]] <- factor(
    ifelse(x < 0, "Human", ifelse(x > 0, "AI", "Equal")),
    levels = c("Human", "Equal", "AI")
  )
  df[[new_var]] <- as.factor(df[[new_var]])
  df
}

prep_for_pmc_cat_mods_analysis <- function(df){
  df$ID <- as.factor(df$ID)
  # make sure everything is factor
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
  df$PMC_cat <- df$PMC_cat
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
  # for PMC_cat
  k <- 3
  c <- contr.treatment(k)                                  # k x (k-1) matrix of 0/1
  my.coding <- matrix(rep(1/k, k * (k - 1)), ncol = k - 1)  # same shape, filled with 1/k
  my.simple <- c - my.coding
  contrasts(df$PMC_cat) <- my.simple
  contrasts(df$PMC_cat)
  df <- df %>%
    mutate(MJ = recode(MJ, "Yes" = "1",
                       "No" = "0"))
  
  df$MJ <- as.factor(df$MJ)
  # scale all PMC mod vars
  # df <- df |>
  #   mutate(
  #     across(
  #       c(PMC_overall, PMC_factor_1, PMC_factor_2, PMC_factor_3, PMC_agency, PMC_experience),
  #       ~ .x / sd(.x, na.rm = TRUE)
  #     )
  #   )
  
  
  df
}



######### MDS
one_row_per_mod_df <- function(df){
  unique_df <- df %>%
    distinct(as.character(ID), .keep_all = TRUE)
  selected_df <- unique_df %>%
    dplyr::select(ID, starts_with("PMC_")) |> 
    dplyr::select(-PMC_Stimuli_List, -PMC_overall, -PMC_agency, -PMC_experience, -PMC_affect, -PMC_mental_moral, -PMC_reality, -PMC_factor_1, -PMC_factor_2,-PMC_factor_3, -`PMC_good_Factor 1`, -`PMC_good_Factor 2`, -`PMC_good_Factor 3`
                  )
  library(stringr)
  
  # Clean column names
  clean_names <- function(col_names) {
    col_names %>%
      str_replace_all("[[:space:]]|/|-|\\.", "_") %>%  # Replace spaces, /, -, . with _
      str_replace_all("_+", "_") %>%                  # Replace multiple underscores with a single one
      str_trim() %>%                                   # Remove leading/trailing whitespace
      str_replace("_$", "")                            # Remove trailing underscore
  }
  
  # Apply to column names
  colnames(selected_df) <- clean_names(colnames(selected_df))
  # Remove "PMC_" from the beginning of column names
  colnames(selected_df) <- str_replace(colnames(selected_df), "^PMC_", "")
  selected_df
}

# MDS
compute_mean_ratings <- function(df){
  mean_ratings <- colMeans(df[, -1], na.rm = TRUE)
  mean_ratings
}

compute_dissimilarity_matrix <- function(mean_ratings){
  # Use absolute differences between mean ratings
  dissimilarity_matrix <- as.matrix(dist(mean_ratings, method = "manhattan"))
  rownames(dissimilarity_matrix) <- names(mean_ratings)
  colnames(dissimilarity_matrix) <- names(mean_ratings)
  dissimilarity_matrix
}

perform_classical_mds <- function(dissimilarity_matrix){
  mds_result <- cmdscale(dissimilarity_matrix, k = 2, eig = TRUE)  # 2D MDS
  # Extract coordinates for plotting
  mds_coords <- mds_result$points
  mds_df <- data.frame(
    Capacity = rownames(mds_coords),
    Dim1 = mds_coords[, 1],
    Dim2 = mds_coords[, 2]
  )
  t <- list(mds_result=mds_result, mds_coords=mds_coords, mds_df=mds_df)
  return(t)
}

perform_hierarchical_clustering <- function(dissimilarity_matrix, mean_ratings){
  # Perform hierarchical clustering
  hc <- hclust(as.dist(dissimilarity_matrix), method = "complete")
  
  # Plot the dendrogram
  plot(hc, main = "Hierarchical Clustering of Capacities", xlab = "", sub = "")
  ggsave("figures/hierarchical_clustering_PMC_Plot_AI_Human_Associations.png", width = 10, height = 7, dpi = 300)
  # Cut the tree into 2 clusters
  clusters <- cutree(hc, k = 2)
  
  # Add cluster labels to the mean ratings data frame
  mean_ratings_df <- data.frame(
    Capacity = names(mean_ratings),
    Mean_Rating = mean_ratings,
    Cluster = clusters
  )
  
  t <- list(hc=hc, clusters=clusters, mean_ratings_df = mean_ratings_df)
  return(t)
}


plot_mds_results <- function(mds_df, mean_ratings){
  # Create a color scale based on mean ratings (AI vs. Human association)
  color_scale <- ifelse(
    mds_df$Capacity %in% names(mean_ratings[mean_ratings > 0]),
    "AI-associated",
    ifelse(
      mds_df$Capacity %in% names(mean_ratings[mean_ratings < 0]),
      "Human-associated",
      "Shared"
    )
  )
  
  # Plot
  ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Capacity, color = color_scale)) +
    geom_point(size = 5) +
    geom_text_repel(size = 4, color = "black", segment.color = "grey") +
    scale_color_manual(
      values = c("AI-associated" = "red", "Human-associated" = "blue", "Shared" = "green"),
      name = "Association"
    ) +
    labs(
      title = "MDS Plot: AI vs. Human Associations by Capacity",
      subtitle = "Red = AI-associated, Blue = Human-associated, Green = Shared",
      x = "Dimension 1",
      y = "Dimension 2"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Save the plot
  ggsave("figures/MDS_Plot_AI_Human_Associations.png", width = 10, height = 7, dpi = 300)
  
}

################################################################################
# ATTENTION CHECK
################################################################################
calculate_number_of_participants_failed_attention_check <- function(df){
  length(unique(df$ID))
  
  # create a variable that calculates matches between MG and MJ
  df <- df %>%
    mutate(MG_match = ifelse(MG == "Yes" & MJ == 1 | 
                               MG == "No"  & MJ == 0, 1, 0))
  
  # count matches per participant for human-only costless rescue scenarios
  costless_check_all <- df %>% 
    filter(Agent %in% c("Human")) %>% 
    filter(str_detect(Dilemma, "Costless_Rescue")) |> 
    group_by(ID) %>%
    summarise(matches = sum(MG_match),
              n_costless_seen = n(), 
              n_costless_correct = sum(MG_match == 1),
              passed_costless_all = (n_costless_seen == n_costless_correct),
              percent_costless = ((n_costless_correct / n_costless_seen) *100),
              .groups = "drop")
  
  costless_check_all
  
  # print rows where passed_costless_all is FALSE
  costless_check <- costless_check_all %>%
    filter(passed_costless_all == FALSE)
  
  t <- list(costless_check_all=costless_check_all, costless_check_notAllPassed=costless_check)
  return(t)
}

# create df without those who failed all
create_df_without_participants_who_failed_all_ac <- function(df, costless_check_all){
  
  df_match <- df %>%
    filter(ID %in% costless_check_all$ID[costless_check_all$percent_costless != 0]) # keep if 50-100% correct on costless rescue dilemmas
  # check how many Ps were removed
  length(unique(df$ID)) - length(unique(df_match$ID))
  
  
  ### Prep variables for analysis
  # make sure everything is factor
  df_match$Task.Name <- as.factor(df_match$Task.Name)
  df_match$Agent <- as.factor(df_match$Agent)
  df_match$Context <- as.factor(df_match$Context)
  df_match$PDE <- as.factor(df_match$PDE)
  df_match$BA <- as.factor(df_match$BA)
  df_match$MC <- as.factor(df_match$MC)
  df_match$Intent <- as.factor(df_match$Intent)
  df_match$PBH <- as.factor(df_match$PBH)
  df_match$MG <- as.factor(df_match$MG)
  df_match$RT <- abs(as.numeric(df_match$RT))
  
  ## Contrasts Coding
  #set deviation contrasts for ease of interpretation -.5 vs .5
  c<-contr.treatment(2)
  my.coding<-matrix(rep(1/2, 2), ncol=1)
  my.simple<-c-my.coding
  my.simple
  ### Set the Contrast Coding Per Variable
  contrasts(df_match$PDE)<- my.simple
  contrasts(df_match$PDE)
  contrasts(df_match$BA)<-my.simple
  contrasts(df_match$BA)
  contrasts(df_match$MC)<-my.simple
  contrasts(df_match$MC)
  contrasts(df_match$Intent)<-my.simple
  contrasts(df_match$Intent)
  contrasts(df_match$PBH)<-my.simple
  contrasts(df_match$PBH)
  contrasts(df_match$MG)<-my.simple
  contrasts(df_match$MG)
  contrasts(df_match$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
  contrasts(df_match$Agent)
  
  
  df_match$MJ <- as.numeric(as.character(df_match$MJ))
  df_match
}