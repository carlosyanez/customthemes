% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addlogo.R
\name{add_logo}
\alias{add_logo}
\title{Add logo to plot, option to write into file}
\usage{
add_logo(
  orig_plot,
  logo_file,
  annotation_text,
  plot_file = "temp.png",
  plot_dpi = 320,
  plot_width = NA,
  plot_height = NA,
  logo_resize = "60x60",
  image_extent = "700x60",
  image_gravity = "west",
  annotation_colour = "black",
  google_font = "Roboto",
  annotation_location = "+61+0",
  annotation_size = 35,
  x_pos_round = 0.01,
  rewrite_file = TRUE
)
}
\arguments{
\item{orig_plot}{existing chart}

\item{logo_file}{logo to be added (image file)}

\item{annotation_text}{text to be added next to the text (e.g. twitter handle)}

\item{plot_file}{file where to save the plot}

\item{plot_dpi}{plot file resolution}

\item{plot_width}{plot file width}

\item{plot_height}{plot file height}

\item{logo_resize}{size of the logo when added (magick parameter)}

\item{image_extent}{extent addition (magick parameter)}

\item{image_gravity}{image gravity alignment (magick parameter)}

\item{annotation_colour}{colour of the annotation text}

\item{google_font}{font of the text, from Google fonts}

\item{annotation_location}{relative location of text (from logo, magick parameter)}

\item{annotation_size}{relative size of annotation (from logo, magick parameter)}

\item{x_pos_round}{relative position rounding (magick parameter)}

\item{rewrite_file}{whether to rewrite the file with finished plot}
}
\value{
raster object
}
\description{
Add logo to plot, option to write into file
}
