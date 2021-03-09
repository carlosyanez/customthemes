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

}


#' Custom theme for ggplot plots
#' @return ggplot theme object
#' @import ggplot2
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
                         plot.title = ggplot2::element_text(size=16,face="bold",colour = title_colour,family=google_font),
                         plot.subtitle =ggplot2::element_text(size=10,colour = general_colour,family=google_font),
                         plot.caption =  ggplot2::element_text(size=10,colour = general_colour,family=google_font),
                         axis.text = ggplot2::element_text(size=10,colour = general_colour,family=google_font),
                         axis.title = ggplot2::element_text(size=10,colour = general_colour,family=google_font),
                         legend.text = ggplot2::element_text(size=10,colour = title_colour,family=google_font),
                         strip.text = ggplot2::element_text(face = "bold", color = general_colour,
                                                   hjust = 0, size = 8,family=google_font),
                         strip.background = ggplot2::element_rect(fill = bg_colour,linetype = "blank"))

                        my_theme
}

#' Custom theme for ggplot maps
#' @return ggplot theme object
#' @import ggplot2
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  map_title_size Title's size
#' @export custom_map_theme
custom_map_theme <- function(google_font="Roboto",map_title_size=14){

                   sysfonts::font_add_google(google_font, google_font)
                   showtext::showtext_auto()

                   theme_map <-   ggplot2::theme_minimal()+
                     ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                           axis.text.x=ggplot2::element_blank(),
                           axis.line.y = ggplot2::element_blank(),
                           axis.line.x = ggplot2::element_blank(),
                           axis.title = ggplot2::element_blank(),
                           panel.grid.major = ggplot2::element_blank(),
                           legend.title = ggplot2::element_text(vjust=0.75,hjust=1,size=10,family=google_font),
                           legend.text=ggplot2::element_text(size=8,family=google_font),
                           panel.grid.minor = ggplot2::element_blank(),
                           plot.title = ggplot2::element_text(size = map_title_size, face = "bold",family=google_font),
                           plot.subtitle = ggplot2::element_text(size = 12,family=google_font),
                           legend.position="bottom",
                           legend.direction="horizontal"
                     )

                   theme_map

}


#' Custom theme for temperatue strips
#' based on https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/, modified
#' @return ggplot theme object
#' @import ggplot2
#' @import RColorBrewer
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  rbrewer_pal RColorBrewer palette used
#' @export custom_strip_theme
custom_strip_theme <- function(google_font="Roboto",rbrewer_pal="RdBu"){

  sysfonts::font_add_google(google_font, google_font)
  showtext::showtext_auto()
  ## colour palette
  col_strip <- RColorBrewer::brewer.pal(11, rbrewer_pal)

  # plot theming

  theme_strip <-  ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          legend.title = ggplot2::element_text(vjust=0.75,hjust=1,size=10,family=google_font),
          legend.text=ggplot2::element_text(size=8,family=google_font),
          axis.text.x = ggplot2::element_text(vjust = 1,size=10,family=google_font),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 14, face = "bold",family=google_font),
          plot.subtitle = ggplot2::element_text(size = 12,family=google_font),
          legend.position="bottom",
          legend.direction="horizontal"

    )

  theme_strip
}


