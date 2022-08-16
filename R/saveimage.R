
# from https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/

#' save ggplot into PNG file
#' @return saved file in disk
#' @param plot_name ggplot object
#' @param file_name file's name (including .png)
#' @param  dpi_value  resolution (default 600)
#' @param  height_value  height for ggsave
#' @param  width_value  width for ggsave
#' @param  unit_value  unit for ggsave
#' @import pdftools
#' @importFrom ggplot2 ggsave
#' @importFrom png writePNG
#' @importFrom knitr plot_crop
#' @importFrom ragg agg_png
#' @return ggplot theme object
#' @export save_image
save_image <- function(plot_name,file_name,dpi_value=600,width_value = NA,height_value = NA, unit_value=NA){

  if(is.na(unit_value)){
    ggsave(file_name,plot_name,
           device=ragg::agg_png,
           dpi=dpi_value,
           width = width_value,
           height = height_value)
  }else{
    ggsave(file_name,plot_name,
           device=ragg::agg_png,
           dpi=dpi_value,
           width = width_value,
           height = height_value,
           units=unit_value)

  }

  #knitr::plot_crop("temp.pdf")
  #bitmap <- pdftools::pdf_render_page("temp.pdf", dpi = dpi_value)
  #png::writePNG(bitmap, file_name)
  #file.remove("temp.pdf")

  message(str_c("File ",file_name," saved"))
  invisible(TRUE)
}

