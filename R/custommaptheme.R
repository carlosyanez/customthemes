#' Custom theme for ggplot maps
#' @return ggplot theme object
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  map_title_size Title's size
#' @param  legend_pos legend's position (default bottom)
#' @param  legend_dir legend's direction (defaul horizontal)
#' @export custom_map_theme

custom_map_theme <- function(google_font="Roboto",map_title_size=14,
                             legend_pos="bottom",legend_dir="horizontal"){

  sysfonts::font_add_google(google_font, google_font)
  showtext::showtext_auto()

  theme_map <-   ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   legend.title = element_text(vjust=0.75,hjust=1,size=10,family=google_font),
                   legend.text=element_text(size=8,family=google_font),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = element_text(size = map_title_size, face = "bold",family=google_font),
                   plot.subtitle = element_text(size = 12,family=google_font),
                   legend.position=legend_pos,
                   legend.direction=legend_dir
    )

  theme_map

}
