################################################################################
# PREDICTED PROBABILITIES PLOTS
################################################################################
plot_predicted_probabilities <- function(model, name, path="figures", colours, theme) {
  # Create plots and suppress plot_model output
  p_int_list <- 
    sjPlot::plot_model(model, type="int", colors=colours)
  
  # Apply formatting
  p_int_list <- lapply(p_int_list, function(p) p + theme +
                         ggplot2::labs(title = NULL,y = "Morally Permissible"))
  
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
# TOaST PLOTS
################################################################################

plot_toast <- function(model, name, path="figures", colours, theme, bound_val){
  toast <- equivalence_test(model, rule="classic", range=c(-bound_val,bound_val))
  p <-plot(toast)
  
  
  
  # Custom colours & fonts
  p_custom <- p +
    scale_colour_manual(values = c("#FB9A99", "#33A02C", "#FDBF6F")) + scale_fill_manual(values = c("#FB9A99", "#33A02C", "#FDBF6F")) +
    theme +
    theme(plot.caption = element_text(size=11, hjust = 0)) 
  
  ggplot2::ggsave(
    filename=file.path(path, paste0(name, "_toast_plot.png")),
    plot=p_custom,
    width=7.27,
    height=5.83,
    units="in",
    dpi=300
  )
  p_custom
}

################################################################################
# NATIONALITY HEATMAP (for S1)
################################################################################
make_nationality_heatmap_for_s1 <- function(demo){
  # load in data of world
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # mark which countries of <- are represented in sample 
  world_highlighted <- world %>%
    mutate(has_participants = name_en %in% demo$Nationality)
  
  
  
  # map w. varying colour for number of participants
  country_counts <- demo %>%
    count(Nationality, name = "participant_count")
  
  world_map <- left_join(world, country_counts,
                         by = c("name_en" = "Nationality"))
  
  world_centroids <- st_centroid(world_map) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    bind_cols(world_map %>% st_drop_geometry() %>% dplyr::select(name_en, participant_count))
  
  
  world_centroids <- world_centroids |>
    mutate(
      participant_count = ifelse(participant_count == 0, NA, participant_count)
    )
  
  p <- ggplot(world_map) +
    geom_sf(aes(fill = participant_count), colour = "white", size = 0.2) +
    geom_label_repel(
      data = world_centroids %>% filter(!is.na(participant_count)),
      aes(X, Y, label = name_en, family = "Georgia"),
      size = 3,
      label.size = 0.2,                  # border width (0 removes border)
      label.padding = unit(3, "pt"),     # small padding
      box.padding = 1,
      label.r = unit(0.15, "lines"),     # corner radius
      fill = scales::alpha("grey90", 0.7), # semi-transparent grey box
      color = "black",
      max.overlaps = Inf,
      min.segment.length = 0             # always draw lines if needed
    ) +
    scale_fill_distiller(
      palette = "Paired", 
      trans = pseudo_log_trans(sigma = 2),
      na.value = "grey90",
      name = expression(Participant~italic(N))
    ) +
    coord_sf(expand = TRUE) +
    theme_bw() +
    theme(
      plot.margin = margin(0,10, 0, 10, "pt"),
      plot.title = element_text(
        family = "Georgia", # set font
        size = 11,          # title size
        face = "bold.italic",      
        lineheight = 2,    # double spacing
      ),
      plot.subtitle = element_text(
        family = "Georgia", # set font
        size = 11,          # title size
        face = "plain",      
        lineheight = 2    # double spacing
      ),
      axis.title = element_blank(),
      axis.text = element_text(
        family = "Georgia", 
        size = 11
      ),
      annotate.text = element_text(
        family = "Georgia",
        size = 9
      ),
      #panel.grid = element_blank(),
      legend.title = element_text(
        family = "Georgia", 
        face = "bold",
        size = 9
      ),
      legend.text = element_text(
        family = "Georgia", 
        size = 8
      )
    )
  
  png("figures/s1_nationality_heatmap.png", width = 3508, height = 2480, res = 300)
  print(p)
  dev.off()
  p
}