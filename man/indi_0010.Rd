% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indi_0010.R
\name{indi_0010}
\alias{indi_0010}
\title{Indicator: Taxa de Mortalidade Infantil}
\usage{
indi_0010(
  agg,
  agg_time = "year",
  ano,
  multi = 1000,
  decimals = 2,
  pop_source = "datasus",
  pcdas_token = NULL,
  adjust_rates = FALSE
)
}
\arguments{
\item{agg}{character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.}

\item{agg_time}{character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.}

\item{ano}{numeric. Year of death.}

\item{multi}{integer. Multiplicator for indicator.}

\item{decimals}{integer. Number of decimals for indicator.}

\item{pop_source}{character. Population source, from {brpop} package.}

\item{pcdas_token}{character. PCDaS API token. If not provided, the function will look for it on renvirom.}

\item{adjust_rates}{logical. Adjust rates by age. Argument ignored for this indicator.}
}
\description{
Indicator: Taxa de Mortalidade Infantil
}
\examples{
# Some examples
indi_0010(agg = "mun_res", ano = 2013)

}
