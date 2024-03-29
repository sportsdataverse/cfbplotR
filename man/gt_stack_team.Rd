% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_stack_team.R
\name{gt_stack_team}
\alias{gt_stack_team}
\alias{gt_merge_stack_team_color}
\title{Merge and stack text from two columns in \code{gt} and color one with school colors}
\usage{
gt_merge_stack_team_color(
  gt_object,
  col1,
  col2,
  team_col,
  font_size_top = 14,
  font_size_bottom = 12,
  color = "black"
)
}
\arguments{
\item{gt_object}{An existing gt table object of class \code{gt_tbl}}

\item{col1}{The column to stack on top. Will be converted to all caps, with black and bold text.}

\item{col2}{The column to merge and place below. Will be smaller and the school color that corresponds to \code{team_col}.}

\item{team_col}{The column of team names that match \code{valid_team_names()} for the color of the bottom.}

\item{font_size_top}{the font size for the top text.}

\item{font_size_bottom}{the font size for the bottom text.}

\item{color}{The color for the top text.}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
The \code{gt_merge_stack_team_color()} function takes an existing \code{gt} table and merges
column 1 and column 2, stacking column 1's text on top of column 2's.
Top text is in all caps with black bold text, while the lower text is smaller
and colored by the team name in another column.
This is a slightly modified version of \href{https://jthomasmock.github.io/gtExtras/reference/gt_merge_stack.html}{\code{gtExtras::gt_merge_stack()}}  written by Tom Mock.
}
\section{Figures}{

\if{html}{\figure{merge-stack.png}{options: width=50\%}}
}

\examples{
library(gt)
teams <-
"https://github.com/sportsdataverse/cfbfastR-data/raw/main/team_info/rds/cfb_team_info_2020.rds"
team_df <- readRDS(url(teams))

stacked_tab <- team_df \%>\%
  dplyr::transmute(logo = school,school,mascot,conference,city,state) \%>\%
  head(8) \%>\%
  gt::gt() \%>\%
  gt_merge_stack_team_color(school,mascot,school) \%>\%
  gt_fmt_cfb_logo(columns = c("logo","conference"))

}
