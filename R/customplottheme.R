#'  Set colour for standard chart fillings and lines
#'  @return no output
#'  @import ggplot2
#'  @export set_plot_colours
set_plot_colours <- function(){

  ggplot2::update_geom_defaults("point",   list(colour = "darkblue"))
  ggplot2::update_geom_defaults("violin",   list(colour = "darkblue",fill = "deepskyblue"))
  ggplot2::update_geom_defaults("line",   list(colour = "darkblue"))
  ggplot2::update_geom_defaults("bar",   list(colour = "darkblue",fill = "deepskyblue"))
  ggplot2::update_geom_defaults("col",   list(colour = "darkblue",fill = "deepskyblue"))
  ggplot2::update_geom_defaults("boxplot",   list(colour = "darkblue",fill = "white"))
  ggplot2::update_geom_defaults("density",   list(colour = "darkblue",fill = "deepskyblue",alpha=0.3))

}


#' Custom theme for ggplot plots
#' @return ggplot theme object
#' @import ggplot2
#' @import showtext
#' @import sysfonts
#' @export custom_plot_theme
custom_plot_theme <- function(){
                        sysfonts::font_add_google("Roboto", "Roboto")
                        showtext::showtext_auto()
                        my_theme <- ggplot2::theme_set(ggplot2::theme_minimal())

                        my_theme <- ggplot2::theme_update(legend.position="bottom",
                         plot.title = ggplot2::element_text(size=16,face="bold",colour = "#272928",family="Roboto"),
                         plot.subtitle =ggplot2::element_text(size=10,colour = "azure4",family="Roboto"),
                         plot.caption =  ggplot2::element_text(size=10,colour = "azure4",family="Roboto"),
                         axis.text = ggplot2::element_text(size=10,colour = "azure4",family="Roboto"),
                         axis.title = ggplot2::element_text(size=10,colour = "azure4",family="Roboto"),
                         legend.text = ggplot2::element_text(size=10,colour = "#272928",family="Roboto"),
                         strip.text = ggplot2::element_text(face = "bold", color = "azure4",
                                                   hjust = 0, size = 8,family="Roboto"),
                         strip.background = ggplot2::element_rect(fill = "#fcf8f7",linetype = "blank"))

                        my_theme
}



