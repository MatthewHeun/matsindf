\name{matsindf-package}
\alias{matsindf-package}
\alias{matsindf}
\docType{package}
\title{
\packageTitle{matsindf}
}
\description{
\packageDescription{matsindf}
}
\details{
    This package provides two helpful functions:

    \enumerate{
      \item \code{\link[matsindf]{collapse_to_matrices}}:
            collapses a tidy data frame into matrices in a data frame and

      \item \code{\link[matsindf]{expand_to_tidy}}:
            expands a data frame of matrices into a tidy data frame.
    }
}
\author{
\packageAuthor{matsindf}

\strong{Maintainer}: \packageMaintainer{matsindf}
}
\keyword{
array
}
\seealso{
The
\code{\link[matsindf:matsindf-package]{matsindf}}
package works well with the
\code{\link[byname:byname-package]{byname}}
package.
}
\examples{
  library(byname)
  library(magrittr)
  library(dplyr)
  tidy <- data.frame(matrix = c("V1", "V1", "V1", "V2", "V2"),
                     row = c("i1", "i1", "i2", "i1", "i2"),
                     col = c("p1", "p2", "p2", "p1", "p2"),
                     vals = c(1, 2, 3, 4, 5)) \%>\%
    mutate(
      rowtype = "Industries",
      coltype  = "Products"
    ) \%>\%
    group_by(matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", values = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtype", coltypes = "coltype")
  mats
  tidy2 <- mats \%>\%
    expand_to_tidy(matnames = "matrix", matvals = "vals",
                   rownames = "row", colnames = "col",
                   rowtypes = "rowtype", coltypes = "coltype", drop = 0)
  tidy2
}
