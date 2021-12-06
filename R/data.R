#' Energy consumption in the UK in 2000
#'
#' A dataset containing approximations to
#' some of the energy flows in the UK in the year 2000.
#' These data first appeared as the example in
#' Figures 3 and 4 of
#' M.K. Heun, A. Owen, and P.E. Brockway.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Sustainability Research Institute Paper 111,
#' University of Leeds, School of Earth and Environment,
#' Sustainability Research Institute,
#' Leeds, England,
#' 13 November 2017.
#'
#' @format A data frame with 36 rows and 7 variables:
#' \describe{
#'   \item{Country}{country, GB (Great Britain, only one country)}
#'   \item{Year}{year, 2000 (only one year)}
#'   \item{Ledger.side}{Supply or Consumption}
#'   \item{Flow.aggregation.point}{tells where each row should be aggregated}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{E.ktoe}{magnitude of the energy flow in ktoe}
#' }
#' @source \url{http://www.see.leeds.ac.uk/fileadmin/Documents/research/sri/workingpapers/sri-wp111.pdf}
"UKEnergy2000"

#' Iris data, in a long table
#' 
#' The Edgar Anderson's iris dataset, which is included in R as "iris", but as a long table.
#' 
#' @format A tibble with 600 rows and four variables:
#' \describe{
#'   \item{flower}{An identifier for the flower being measured}
#'   \item{species}{The species of the flower}
#'   \item{dimension}{The part and dimension of the flower being measured (for example, sepal width or sepal length)}
#'   \item{length}{The measurement, in cm}
#' }
"long_iris"
