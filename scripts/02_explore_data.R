################################################################################
# PIVOT DF TO WIDE FORMAT FOR DEMOGRAPHICS
################################################################################
# for most of the demographic summaries, it is easier to have only one row per Ps
one_row_per_df <- function(df){
  df %>%
    arrange(ID, Trial.Number) %>%
    pivot_wider(
      id_cols = c(ID, Gender,Gender.quant, Age, Nationality, "cultural.arab",       "cultural.asian",      "cultural.black", 
                  "cultural.multiple",   "cultural.white",     "cultural.prefNo",     "cultural.another",    "primary_cultural",    "multiple_cultural"),
      names_from = Trial.Number,
      values_from = c(MJ, Agent, Dilemma)
    )
}

################################################################################
# Model Checks
################################################################################
# first create a helper function as the environment causes a lot of issues with the check_model() function inside plot
conduct_model_diagnostics <- function(model,name,panelling=FALSE,chosen_type="discrete_both",colours_override=NULL,theme_override=NULL){
  plots <- plot(check_model(model, panel = FALSE),type="discrete_both")
  titles <- vapply(plots,function(p)if(is.null(p$labels$title))""else p$labels$title,FUN.VALUE=character(1))
  tags <- paste0(letters[seq_along(plots)],")")
  plots <- Map(function(p,tag){
    p+labs(title=NULL,tag=tag)+theme(
      text=element_text(family="Georgia",size=9),
      axis.title=element_text(size=9),axis.text=element_text(size=7),
      legend.title=element_text(size=7),legend.text=element_text(size=7),
      strip.text=element_text(size=7),plot.title=element_text(size=7),
      plot.caption=element_text(size=7),
      plot.tag=element_text(size=7),
      plot.tag.position=c(-0.02,0.99))
  },plots,tags)
  idx <- list(1:4,5:7)
  designs <- c("AABB\nCCDD","EEFF\nGGFF")
  files <- paste0("figures/",name,"_page",seq_along(idx),".png")
  for(i in seq_along(idx)){
    p <- patchwork::wrap_plots(plots[idx[[i]]],design=designs[i])
    if(i==2){
      lab <- paste0(letters[1:7],") ",titles[1:7])
      foot <- paste(paste(lab[1:4],collapse="   "),paste(lab[5:7],collapse="   "),sep="\n")
      p <- p+patchwork::plot_annotation(caption=foot,theme=theme(
        text=element_text(family="Georgia",size=9),
        plot.caption=element_text(family="Georgia",size=9,hjust=0),
        plot.caption.position="plot"))
    }
    ggsave(filename=files[i],plot=p,width=10.69,height=7.27,dpi=300)
  }
  files
}

## check linearity of residuals on response 
check_linear_pred <- function(model, terms=c("PBH [all]","PDE [all]","Agent [all]"), name, theme=georgia_theme, path="figures"){
  pr <- predict_response(model, terms = terms, bias_correction = TRUE)
  p <- plot(pr, show_residuals = TRUE, show_residuals_line = TRUE, grid = TRUE) + scale_colour_manual(values = c("#FB9A99", "#A6CEE3")) + scale_fill_manual(values = c("#FB9A99", "#A6CEE3")) +
    theme +theme(title = element_blank(), text = element_text(family="Georgia", size=9))
  ggsave(filename=file.path(path, paste0(name, "_linear_pred_resp_plot.png")),
         plot=p, width=7.27, height=5.83, units="in", dpi=300)
}

# check goodness of fit via deviance, manually
check_goodness_of_fit_via_deviance <- function(model){
  deviance <- deviance(model)
  chi_critical_value <- qchisq(0.95,df=6) #df is # of data points - 1
  diff <- deviance(model) - qchisq(0.95,df=length(model@resp$y))
  df <- data.frame(
    deviance = deviance,
    chi_critical_value = chi_critical_value,
    diff = diff
  )
  df
}