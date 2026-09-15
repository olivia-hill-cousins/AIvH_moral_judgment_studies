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
    )
  
  
  return(df)
  
}

# add demo data to df
add_demo_to_pmc_df <- function(df, demo){
  df <- df |> left_join(demo, by="ID") |> 
    dplyr::select(ID, Gender, Age, Ethnicity, Nationality, Country_of_Birth, Country_of_Residence, ID, Task_Name, Trial_Number, McMurtie_dimension, Item, PMC_dimension, AI_image, Human_image, PMC, RT, AI_position, AI_PMC, Human_PMC, ImagePair, AI_PMC_prop, Human_PMC_prop)
  df
}
conduct_exploratory_factor_analysis <- function(df, Agent_PMC) {
  df <- df |> 
    pivot_wider(names_from=Item, values_from=.data[[Agent_PMC]], names_prefix="Item ", id_cols="ID") |> 
    clean_names()  |> 
    dplyr::select(-id) 
  df[] <- lapply(df, function(x) ifelse(is.na(x), 0, x))
  df <- df |> 
    mutate(across(everything(), as.numeric))
  bart <- bart_spher(df)
  kmo <- KMO(df)
  parallel <- paran(df, cfa=TRUE)
  parallel_graph <- paran(df, cfa=TRUE, graph=TRUE, color=TRUE, col=c("#1F78B4","#FB9A99", "#33A02C")) #anything above blue line is a valid factor. Can see that 3 dots are above.
  efa_1 <- fa(df, nfactors=1, rotate = "oblimin")
  efa_2 <- fa(df, nfactors=2, rotate = "oblimin")
  efa <- fa(df, nfactors=3, rotate = "oblimin")
  efa_diagram <- fa.diagram(efa, main="df")
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
    ) |> 
    mutate(Item = rownames(loadings)) |>
    dplyr::select(Item, MR1, MR2, MR3) |> 
    relocate(Item) |>
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
  t
}
add_efa_factor_cats_to_pmc_df <- function(df, items_by_factors, good_items_by_factors) {
  clean_item <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("['’]", "") |>
      str_replace_all("[/-]", " ") |>
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
        Items %in% items_by_factors$factor_1 ~ "factor_1",
        Items %in% items_by_factors$factor_2 ~ "factor_2",
        Items %in% items_by_factors$factor_3 ~ "factor_3",
        TRUE ~ NA_character_
      )) |>
    mutate(
      Good_items = clean_item(Item),
      PMC_efa_good = case_when(
        Good_items %in% good_items_by_factors$factor_1 ~ "factor_1",
        Good_items %in% good_items_by_factors$factor_2 ~ "factor_2",
        Good_items %in% good_items_by_factors$factor_3 ~ "factor_3",
        TRUE ~ NA_character_
      ))
  df
}
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
