set_colours_object <- function(number_of_colours){
  brewer.pal(number_of_colours, "Paired")
}
#uncomment to see colours
#"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
#  "#FB9A99" "#33A02C"
set_theme <- function(){
  font_add("Georgia",
           regular = "Georgia.ttf", bold = "Georgia Bold.ttf",
           italic = "Georgia Italic.ttf", bolditalic = "Georgia Bold Italic.ttf"
  )
  
  showtext_auto() # Enable for all plots
  
  theme_minimal(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia"),
      axis.text = element_text(family = "Georgia"),
      axis.title = element_text(family = "Georgia"),
      legend.text = element_text(family = "Georgia"),
      legend.title = element_text(family = "Georgia"),
      # Add axis lines back in
      axis.line = element_line(colour = "black", linewidth = 0.1)
    )
}
