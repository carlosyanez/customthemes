# from https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/

#' save ggplot into PNG file
#' @return saved file in disk
#' @param plot_name ggplot object
#' @param file_name file's name (including .png)
#' @param  dpi_value  resolution (default 600)
#' @import pdftools
#' @importFrom ggplot2 ggsave
#' @importFrom grDevices cairo_pdf
#' @importFrom png writePNG
#' @importFrom knitr plot_crop
#' @return ggplot theme object
#' @export save_image
save_image <- function(plot_name,file_name,dpi_value=600){

  ggsave("temp.pdf",  device = cairo_pdf,plot_name,dpi=dpi_value)
  knitr::plot_crop("temp.pdf")
  bitmap <- pdftools::pdf_render_page("temp.pdf", dpi = dpi_value)
  png::writePNG(bitmap, file_name)
  file.remove("temp.pdf")

  message(str_c("File ",file_name," saved"))
  invisible(TRUE)
}
