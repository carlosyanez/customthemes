#' Custom theme for ggplot plots
#' @return ggplot theme object
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  title_colour Colour for title and legend textx
#' @param  general_colour Colour for all other texts in plot
#' @param  background_colour Background colour
#' @param  legend_pos legend's position (default right)
#' @param  legend_dir legend's direction (defaul vertical)
#' @export custom_plot_theme
custom_plot_theme <- function(google_font="Roboto",
                              title_colour="#272928",
                              general_colour="azure4",
                              background_colour="#fcf8f7",
                              legend_pos="right",
                              legend_dir="vertical"){
                        sysfonts::font_add_google(google_font, google_font)
                        showtext::showtext_auto()

                        my_theme <- ggplot2::theme_minimal() +
                                    ggplot2::theme(
                         panel.background = element_rect(fill = background_colour, colour = NA),
                         plot.background = element_rect(fill = background_colour, colour = NA),
                         plot.title = element_text(size=16,face="bold",colour = title_colour,family=google_font),
                         plot.subtitle =element_text(size=10,colour = general_colour,family=google_font),
                         plot.caption =  element_text(size=10,colour = general_colour,family=google_font),
                         axis.text = element_text(size=10,colour = general_colour,family=google_font),
                         axis.title = element_text(size=10,colour = general_colour,family=google_font),
                         legend.text = element_text(size=10,colour = title_colour,family=google_font),
                         strip.text = element_text(face = "bold", color = general_colour,
                                                   hjust = 0, size = 8,family=google_font),
                         strip.background = ggplot2::element_rect(fill = background_colour,linetype = "blank"),
                         legend.position=legend_pos,
                         legend.direction=legend_dir)

                        my_theme
}







