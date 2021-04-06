#' Custom theme for temperatue strips
#' based on https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/, modified
#' @return ggplot theme object
#' @import ggplot2
#' @import RColorBrewer
#' @import ggtext
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
                   legend.title = element_text(vjust=0.75,hjust=1,size=10,family=google_font),
                   legend.text=element_text(size=8,family=google_font),
                   axis.text.x = element_text(vjust = 1,size=10,family=google_font),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = element_text(size = 14, face = "bold",family=google_font),
                   plot.subtitle = element_text(size = 12,family=google_font),
                   legend.position="bottom",
                   legend.direction="horizontal"

    )

  theme_strip
}
