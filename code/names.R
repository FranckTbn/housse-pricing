#=====================================
# Couleurs LinkPact ----
#=====================================

lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)             # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255)            # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255)         # Magenta LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255)          # Gris LinkPact
lkp_comp_blue <- grDevices::rgb(0, 113, 148, maxColorValue = 255)       # Gris LinkPact
lkp_light_blue  <- grDevices::rgb(35, 95, 221, maxColorValue = 255)     # Bleu LinkPact
lkp_light_green <- grDevices::rgb(0, 227, 166, maxColorValue = 255)     # Vert LinkPact
lkp_colors <- c(lkp_comp_blue, lkp_magenta, lkp_green, lkp_light_blue, lkp_light_green)




custom_theme <- theme(
  axis.title.x = element_text(size = 15, color = "darkblue", face = "bold" ),
  axis.title.y = element_text( size = 13, color = "darkred", face = "italic"),
  
  axis.text.x = element_text(size = 9, color = "darkblue", face = "italic", angle = 45, hjust = 1),
  axis.text.y = element_text(size = 9, color = "darkred", face = "bold" ),
  
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  
  panel.background = element_rect(fill = "#F7F9F2"),
  panel.grid.major = element_line(color = "white"),
  panel.grid.minor = element_line(color = "#F7F9F2"),
  
  legend.position = "bottom",
  legend.background = element_rect(fill = "lightblue"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10)
)

