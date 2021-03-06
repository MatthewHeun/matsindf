% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{verify_cols_missing}
\alias{verify_cols_missing}
\title{Verify that column names in a data frame are not already present}
\usage{
verify_cols_missing(.DF, newcols)
}
\arguments{
\item{.DF}{the data frame to which \code{newcols} are to be added}

\item{newcols}{a single string, a single name,
a vector of strings representing the names of new columns to be added to \code{.DF}, or
a vector of names of new columns to be added to \code{.DF}}
}
\value{
\code{NULL}. This function should be called for its side effect of checking the validity
of the names of \code{newcols} to be added to \code{.DF}.
}
\description{
In the \code{Recca} package, many functions add columns to an existing data frame.
If the incoming data frame already contains columns with the names of new columns to be added,
a name collision could occur, deleting the existing column of data.
This function provides a way to quickly check whether \code{newcols} are already present in
\code{.DF}.
}
\details{
This function terminates execution if a column of \code{.DF} will be overwritten
by one of the \code{newcols}.
}
\examples{
df <- data.frame(a = c(1,2), b = c(3,4))
verify_cols_missing(df, "d") # Silent. There will be no problem adding column "d".
newcols <- c("c", "d", "a", "b")
\dontrun{verify_cols_missing(df, newcols)} # Error: a and b are already in df.
}
