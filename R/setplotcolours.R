#' Set colour for standard chart fillings and lines
#' @return no output
#' @import ggplot2
#' @param line_colour general colour for lines
#' @param fill_colour fill colour for plots like density plots
#' @param alpha_setting  alpha value when fill_colour is used
#' @param alt_fill_colour fill colour for plots like boxplots
#' @export set_plot_colours
set_plot_colours <- function(line_colour="darkblue",fill_colour="deepskyblue",
                             alt_fill_colour="white",alpha_setting=0.3){

  ggplot2::update_geom_defaults("point",   list(colour = line_colour))
  ggplot2::update_geom_defaults("violin",   list(colour = line_colour,fill = fill_colour))
  ggplot2::update_geom_defaults("line",   list(colour = line_colour))
  ggplot2::update_geom_defaults("bar",   list(colour = line_colour,fill = fill_colour))
  ggplot2::update_geom_defaults("col",   list(colour = line_colour,fill = fill_colour))
  ggplot2::update_geom_defaults("boxplot",   list(colour = line_colour,fill = alt_fill_colour))
  ggplot2::update_geom_defaults("density",   list(colour = line_colour,fill = fill_colour,alpha=alpha_setting))
  ggplot2::update_geom_defaults("rect",   list(colour = line_colour,fill = fill_colour,alpha=alpha_setting))

}
