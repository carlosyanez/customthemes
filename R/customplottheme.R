#' Custom theme for ggplot plots
#' @return ggplot theme object
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  title_colour Colour for title and legend textx
#' @param  general_colour Colour for all other texts in plot
#' @param  bg_colour Background colour
#' @export custom_plot_theme
custom_plot_theme <- function(google_font="Roboto",
                              title_colour="#272928",
                              general_colour="azure4",
                              bg_colour="#fcf8f7"){
                        sysfonts::font_add_google(google_font, google_font)
                        showtext::showtext_auto()
                        my_theme <- ggplot2::theme_set(ggplot2::theme_minimal())

                        my_theme <- ggplot2::theme_update(legend.position="bottom",
                         plot.title = element_text(size=16,face="bold",colour = title_colour,family=google_font),
                         plot.subtitle =element_text(size=10,colour = general_colour,family=google_font),
                         plot.caption =  element_text(size=10,colour = general_colour,family=google_font),
                         axis.text = element_text(size=10,colour = general_colour,family=google_font),
                         axis.title = element_text(size=10,colour = general_colour,family=google_font),
                         legend.text = element_text(size=10,colour = title_colour,family=google_font),
                         strip.text = element_text(face = "bold", color = general_colour,
                                                   hjust = 0, size = 8,family=google_font),
                         strip.background = ggplot2::element_rect(fill = bg_colour,linetype = "blank"))

                        my_theme
}







