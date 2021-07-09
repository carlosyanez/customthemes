#' Small wrapper of formatC for number formatting
#' @return formatted number
#' @param x number to format
#' @param sig_digits decimal precission
#' @nbr_format format (default "f")
#' @export digits_formatter
digits_formatter <- function(x,sig_digits=3,
                             nbr_format="f"){
  formatC(x,big.mark = " ",digits = sig_digits,format=nbr_format)
  }
