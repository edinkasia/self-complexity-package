\name{complexity_data}
\alias{complexity_data}
\docType{data}
\title{
Dataset containing the Showers card sort task results from Banas & Smyth (2021)
}
\description{
This dataset contains data from 640 participants who completed the Showers card sort task (listed their self-aspects and corresponding attributes). The attributes used were a set of 40 (20 positive, 20 negative). Data are structured in a long format (each participant has multiple rows, each self-aspect is a unique row).
}
\usage{data("complexity_data")}
\format{
  A data frame with 4441 observations on the following 5 variables.
  \describe{
    \item{\code{ResponseId}}{a character vector}
    \item{\code{n_subtypes}}{a numeric vector}
    \item{\code{Subtype}}{a character vector}
    \item{\code{Name}}{a character vector}
    \item{\code{Attributes}}{a character vector}
  }
}
\source{
Banas & Smyth (2021) "Structure, Content and Inter-relationships between Self-aspects: Integrating Findings from the Social Identity and Self Complexity Traditions".
European Journal of Social Psychology
https://doi.org/10.1002/ejsp.2760
}
\keyword{datasets}
