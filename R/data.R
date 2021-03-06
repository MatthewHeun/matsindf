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
