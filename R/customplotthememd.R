#' Custom theme for ggplot plots with markdown texts
#' @return ggplot theme object
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  title_colour Colour for title and legend textx
#' @param  general_colour Colour for all other texts in plot
#' @param  background_colour Background colour
#' @param  title_size Title's size
#' @param  subtitle_size Subtitle's size
#' @param  caption_size Caption's size
#' @param  plot_margin margins c(top,right,bottom,left) - default c(3,0,1,0)
#' @param  legend_pos legend's position (default right)
#' @param  legend_dir legend's direction (defaul vertical)
#' @param  axis_size  text size for axis items
#' @param legend_size text size for legend
#' @export custom_plot_theme_md
custom_plot_theme_md <- function(google_font="Roboto",
                              title_colour="#272928",
                              general_colour="azure4",
                              background_colour="white",
                              legend_pos="right",
                              legend_dir="vertical",
                              plot_margin=c(3,0,1,0),
                              title_size=14,
                              subtitle_size=12,
                              caption_size=9,
                              axis_size=10,
                              legend_size=8){
                        sysfonts::font_add_google(google_font, google_font)
                        showtext::showtext_auto()

                        my_theme <-ggplot2::theme_minimal() %+replace%
                                   ggplot2::theme(
                                   panel.background = element_rect(fill = background_colour, colour = NA),
                                   plot.background = element_rect(fill = background_colour, colour = NA),
                                   plot.title.position = "plot",
                                   plot.margin=grid::unit(plot_margin, "mm"),   ##top, right, bottom, left
                                   plot.title = element_markdown(size=title_size,colour = title_colour,family=google_font),
                         plot.subtitle = element_markdown(size=subtitle_size,colour = general_colour,family=google_font),
                         plot.caption = element_markdown(size=caption_size,colour = general_colour,family=google_font),
                         axis.text = element_markdown(size=axis_size,colour = general_colour,family=google_font),
                         axis.title = element_markdown(size=axis_size,colour = general_colour,family=google_font),
                         legend.text = element_text(size=legend_size,colour = title_colour,family=google_font),
                         legend.position=legend_pos,
                         legend.direction=legend_dir
                         )

                        my_theme
}







