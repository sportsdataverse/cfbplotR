% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_cfb.R
\name{scale_color_cfb}
\alias{scale_color_cfb}
\alias{scale_colour_cfb}
\alias{scale_fill_cfb}
\title{Scale for college football team colors}
\usage{
scale_color_cfb(
  alt_colors = NULL,
  values = NULL,
  ...,
  aesthetics = "colour",
  breaks = ggplot2::waiver(),
  na.value = "grey50",
  guide = NULL,
  alpha = NA
)

scale_colour_cfb(
  alt_colors = NULL,
  values = NULL,
  ...,
  aesthetics = "colour",
  breaks = ggplot2::waiver(),
  na.value = "grey50",
  guide = NULL,
  alpha = NA
)

scale_fill_cfb(
  alt_colors = NULL,
  values = NULL,
  ...,
  aesthetics = "fill",
  breaks = ggplot2::waiver(),
  na.value = "grey50",
  guide = NULL,
  alpha = NA
)
}
\arguments{
\item{alt_colors}{Vector of team names to use the alternate color of.}

\item{values}{If \code{NULL} (the default) use the internal team color vectors. Otherwise
a set of aesthetic values to map data values to. The values
will be matched in order (usually alphabetical) with the limits of the
scale, or with \code{breaks} if provided. If this is a named vector, then the
values will be matched based on the names instead. Data values that don't
match will be given \code{na.value}.}

\item{...}{Arguments passed on to scale_color_manual}

\item{aesthetics}{Character string or vector of character strings listing the
name(s) of the aesthetic(s) that this scale works with. This can be useful, for
example, to apply colour settings to the \code{colour} and \code{fill} aesthetics at the
same time, via \code{aesthetics = c("colour", "fill")}.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks (the scale limits)
\item A character vector of breaks
\item A function that takes the limits as input and returns breaks
as output
}}

\item{na.value}{The aesthetic value to use for missing (\code{NA}) values}

\item{guide}{A function used to create a guide or its name. If \code{NULL} (the default)
no guide will be plotted for this scale. See \code{\link[ggplot2:guides]{ggplot2::guides()}} for more information.}

\item{alpha}{Factor to modify color transparency via a call to \code{\link[scales:alpha]{scales::alpha()}}.
If \code{NA} (the default) no transparency will be applied. Can also be a vector of
alphas. All alpha levels must be in range \verb{[0,1]}.}
}
\description{
These functions allows you to map college football team names as levels to the color and fill aesthetics
}
\examples{
library(cfbplotR)
library(ggplot2)

df <- data.frame(
  y = 6:9,
  teams = c("Alabama","Florida State","Oregon","Utah")
 )
 ggplot(df, aes(x = teams, y = y)) +
   geom_col(aes(color = teams, fill = teams), size = 2) +
   scale_color_cfb(alt_colors = df$teams) +
   scale_fill_cfb() +
   theme_minimal()
}
