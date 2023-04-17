#' **Data in the package for reference**
#' @name data
NULL
#' @rdname data
#' @title **NCAA Team logos & colors**
#' @description
#' A dataset containing the school names, colors & logos
#' for all college teams (NCAA DI/DII/DIII and FBS/FCS).
#' @keywords data
#' @format A data frame with 830+ rows and 6 variables:
#' \describe{
#'   \item{school}{School name}
#'   \item{type}{Conference-tier (i.e. P5, G5, etc.)}
#'   \item{logo}{primary school logo from ESPN.com}
#'   \item{color}{current primary school color}
#'   \item{alt_color}{current secondary school color}
#'   \item{wordmark}{Wordmark for school if available}
#' }
"logo_ref"

#' @rdname data
#' @title **Team name mapping**
#' @description
#' A dataset mapping from abbreviation or alternate name forms to most commonly used.
#' @keywords data
#' @format A vector with 1100+ name variations
#' \describe{
#'   \item{Short Name}{to Full Naame}
#' }
"team_name_mapping"
