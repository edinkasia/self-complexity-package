\name{complexity_data}
\alias{complexity_data}
\docType{data}
\title{
Data from the Banas & Smyth (2021) study
}
\description{
Data from a study by Banas & Smyth (2021), who that used the Showers card sort task
and then calculated self-complexity indices. The data are presented in a long format,
so that each participant has multiple rows (one for each self-aspect).
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
Banas and Smyth (2021). Structure, Content and Inter‐relationships
between Self‐aspects: Integrating Findings from the Social Identity and
Self Complexity Traditions. European Journal of Social Psychology.
(\href{https://doi.org/10.1002/ejsp.2760}{Online article})
}
\references{
Showers, C. J. (1992). Compartmentalization of positive and negative
self-knowledge: Keeping bad apples out of the bunch. Journal of
Personality and Social Psychology, 62, 1036–1049.
(\href{https://doi.org/10.1037/0022-3514.62.6.1036}{Online article})
}
\examples{
data(complexity_data)
head(complexity_data)
}
\keyword{datasets}
