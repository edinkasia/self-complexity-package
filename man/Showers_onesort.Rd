\name{showers_onesort}
\alias{showers_onesort}
\docType{data}
\title{
Results of the Showers card sort task for a single participant.
}
\description{
This dataset contains the results of the Showers card sort task for a single participant. The example comes from Showers, Abramson and Hogan (1998). The original paper contains some self-complexity indices for this example (Phi, proportion positive, proportion negative), and additional ones (e.g. H) are reported in Showers & Zeiger-Hill (2003).
}
\usage{data("showers_onesort")}
\format{
  A data frame with 11 observations on the following 3 variables.
  \describe{
    \item{\code{ResponseId}}{a character vector}
    \item{\code{Subtype_name}}{a character vector}
    \item{\code{Attributes}}{a character vector}
  }
}
\details{
All rows have the same ResponseId value, as the data come from a single participant.
Each row has a unique Subtype_name.
Attributes are different in each row, with some overlap.
}
\source{
Showers, C. J., Abramson, L. Y., & Hogan, M. E. (1998). The dynamic self: How the content and structure of the self-concept change with mood. Journal of Personality and Social Psychology, 75(2), 478-493. https://doi.org/10.1037/0022-3514.75.2.478
}
\references{
Showers, C. J., & Zeigler-Hill, V. (2003). Organization of self-knowledge: Features, functions, and flexibility. In M. R. Leary & J. P. Tangney (Eds.), Handbook of self and identity (pp. 47–67). The Guilford Press.
}
\keyword{datasets}
