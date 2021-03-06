% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{theme_zi_titels}
\alias{theme_zi_titels}
\title{Zi Theme with axis and labels based on theme_zi() optimal suited to print maps.}
\usage{
theme_zi_titels(fontsize = 14, font = "Calibri")
}
\arguments{
\item{Define}{a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)}
}
\description{
Zi Theme with axis and labels based on theme_zi() optimal suited to print maps.
}
\examples{
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()

}
\keyword{theme}
