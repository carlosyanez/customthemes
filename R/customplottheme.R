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
                         plot.title = ggtext::element_markdown(size=16,face="bold",colour = title_colour,family=google_font),
                         plot.subtitle =ggtext::element_markdown(size=10,colour = general_colour,family=google_font),
                         plot.caption =  ggtext::element_markdown(size=10,colour = general_colour,family=google_font),
                         axis.text = ggtext::element_markdown(size=10,colour = general_colour,family=google_font),
                         axis.title = ggtext::element_markdown(size=10,colour = general_colour,family=google_font),
                         legend.text = ggtext::element_markdown(size=10,colour = title_colour,family=google_font),
                         strip.text = ggtext::element_markdown(face = "bold", color = general_colour,
                                                   hjust = 0, size = 8,family=google_font),
                         strip.background = ggplot2::element_rect(fill = bg_colour,linetype = "blank"))

                        my_theme
}

#' Custom theme for ggplot maps
#' @return ggplot theme object
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param  google_font font, from Google Fonts
#' @param  map_title_size Title's size
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
                           legend.title = ggtext::element_markdown(vjust=0.75,hjust=1,size=10,family=google_font),
                           legend.text=ggtext::element_markdown(size=8,family=google_font),
                           panel.grid.minor = ggplot2::element_blank(),
                           plot.title = ggtext::element_markdown(size = map_title_size, face = "bold",family=google_font),
                           plot.subtitle = ggtext::element_markdown(size = 12,family=google_font),
                           legend.position=legend_pos,
                           legend.direction=legend_dir
                     )

                   theme_map

}


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
          legend.title = ggtext::element_markdown(vjust=0.75,hjust=1,size=10,family=google_font),
          legend.text=ggtext::element_markdown(size=8,family=google_font),
          axis.text.x = ggtext::element_markdown(vjust = 1,size=10,family=google_font),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggtext::element_markdown(size = 14, face = "bold",family=google_font),
          plot.subtitle = ggtext::element_markdown(size = 12,family=google_font),
          legend.position="bottom",
          legend.direction="horizontal"

    )

  theme_strip
}


#' Add logo to plot, option to write into file
#' @return raster object
#' @import magick
#' @import ggplot2
#' @importFrom stringr str_c
#' @import magrittr
#' @import ggtext
#' @import showtext
#' @import sysfonts
#' @param orig_plot existing chart
#' @param logo_file logo to be added (image file)
#' @param annotation_text text to be added next to the text (e.g. twitter handle)
#' @param plot_file  file where to save the plot
#' @param plot_dpi   plot file resolution
#' @param plot_width plot file width
#' @param plot_height plot file height
#' @param logo_resize size of the logo when added (magick parameter)
#' @param image_extent extent addition (magick parameter)
#' @param image_gravity image gravity alignment (magick parameter)
#' @param annotation_colour colour of the annotation text
#' @param google_font  font of the text, from Google fonts
#' @param annotation_location relative location of text (from logo, magick parameter)
#' @param annotation_size  relative size of annotation (from logo, magick parameter)
#' @param x_pos_round relative position rounding (magick parameter)
#' @param rewrite_file whether to rewrite the file with finished plot
#' @export add_logo
add_logo <- function(orig_plot,logo_file,annotation_text,
                     plot_file="temp.png",plot_dpi=320,
                     plot_width=NA,plot_height=NA,
                     logo_resize="60x60",
                     image_extent="700x60",image_gravity="west",
                     annotation_colour="black",google_font="Roboto",
                     annotation_location="+61+0",annotation_size=35,
                     x_pos_round=0.01,rewrite_file=TRUE
                     ){

  sysfonts::font_add_google(google_font, google_font)
  showtext::showtext_auto()

  ggsave(plot_file,orig_plot,dpi=plot_dpi,width = plot_width,height = plot_height)

  plot <- image_read(plot_file)
  logo_raw <- image_read(logo_file)
  logo <- logo_raw %>% image_resize(logo_resize) %>%
    image_extent(image_extent,gravity=image_gravity) %>%
    image_annotate(annotation_text, color = annotation_colour,
                   font=google_font,
                   location=annotation_location,
                   size = annotation_size, gravity = image_gravity)



  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width

  # get dims of the logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height

  y_pos <-round(plot_height - logo_height ,0)
  x_pos <-round(plot_width * x_pos_round,0)

  offset_text <- str_c("+",x_pos,"+",y_pos)


  final_plot <-image_composite(plot,logo, offset = offset_text)

  if(rewrite_file) {image_write(final_plot,plot_file)
  }else{file.remove(plot_file)}
  return(final_plot)


}



