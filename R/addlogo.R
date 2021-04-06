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
