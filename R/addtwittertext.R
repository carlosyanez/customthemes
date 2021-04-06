#' Create HTML/Markdown text with Twitter's logo and handle/text
#' @return  text
#' @param   twitter_handle like @abcd
#' @param   logo_size (default = 9)
#' @param   logo_url custom image (i.e dark mode). If none provide, will download logo from wikimedia
#' @importFrom stringr str_c
#' @export add_twitter_text
add_twitter_text <- function(twitter_handle, logo_size=9,logo_url=NULL){

  if(is.null(logo_url)) logo_url <-"https://upload.wikimedia.org/wikipedia/de/thumb/9/9f/Twitter_bird_logo_2012.svg/200px-Twitter_bird_logo_2012.svg.png"


  text <- str_c("<img src='https://upload.wikimedia.org/wikipedia/de/thumb/9/9f/Twitter_bird_logo_2012.svg/200px-Twitter_bird_logo_2012.svg.png' height='",
                logo_size,"' />",twitter_handle)


  return(text)

}
