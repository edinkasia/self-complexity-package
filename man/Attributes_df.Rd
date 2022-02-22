\name{attributes_df}
\alias{attributes_df}
\docType{data}
\title{
Dataframe of attributes for the Showers' card-sorting task (40 items)
}
\description{
This is a set of adjectives available to participants completing the Showers' card-sorting task. The task was developed by Linville (1985, 1987) and then adapted by Showers (1992). This set is commonly used in the literature, and it contains 20 positive and 20 negative adjectives. Columns Positive and Negative indicate the valence of each adjective.
}
\usage{data("attributes_df")}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{Attribute}}{a character vector}
    \item{\code{Positive}}{a numeric vector}
    \item{\code{Negative}}{a numeric vector}
  }
}
\source{
Showers, C. (1992a). Compartmentalization of positive and negative
self-knowledge: Keeping bad apples out of the bunch. Journal of
Personality and Social Psychology, 62, 1036-1049. https://doi.org/10.1037/0022-3514.62.6.1036
}
\examples{
data(attributes_df)
str(attributes_df)
}
\keyword{datasets}
