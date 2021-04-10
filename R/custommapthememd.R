#' Custom theme for ggplot maps, using ggtext for markdown text
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  map_title_size Title's size
#' @param  map_subtitle_size Subtitle's size
#' @param  map_caption_size Caption's size
#' @param  legend_pos legend's position (default none)
#' @param  legend_dir legend's direction (defaul horizontal)
#' @param  title_pos  title's position (default plot)
#' @param  background_colour background's overall colour (default white)
#' @param  plot_margin margins c(top,right,bottom,left) - default c(3,0,1,0)
#' @export custom_map_theme_md
custom_map_theme_md <- function(google_font="Roboto",map_title_size=14,
                                map_subtitle_size=12, map_caption_size=9,
                             legend_pos="none",legend_dir="horizontal",
                             title_pos="plot", background_colour="white",
                             plot_margin=c(3,0,1,0)
                             ){

  sysfonts::font_add_google(google_font, google_font)
  showtext::showtext_auto()

  theme_map <-   ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   legend.title = element_markdown(vjust=0.75,hjust=1,size=10,family=google_font),
                   legend.text=element_text(size=8,family=google_font),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = element_rect(fill = background_colour,
                                                   colour = NA),
                   plot.background = element_rect(fill = background_colour,
                                                  colour = NA),
                   plot.title.position = "plot",

                   plot.title = element_markdown(size = map_title_size,
                                                 face = "bold",family=google_font,
                                                 hjust=0),
                   plot.subtitle = element_markdown(size = map_subtitle_size,family=google_font,hjust=0),
                   plot.caption = element_markdown(size = map_caption_size,hjust=0,vjust=0,family=google_font),
                   legend.position=legend_pos,
                   legend.direction=legend_dir
    )



  theme_map

}



