% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete_with_zeros.R
\name{complete_with_zeros}
\alias{complete_with_zeros}
\title{Complete indicator result with zeros}
\usage{
complete_with_zeros(res, agg, agg_time, ano, pop_source)
}
\arguments{
\item{res}{tibble. Indicator results table.}

\item{agg}{character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.}

\item{agg_time}{character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.}

\item{ano}{numeric. Year of death.}

\item{pop_source}{character. Population source, from {brpop} package.}
}
\description{
Complete indicator result with zeros considering combinations of spatial and temporal aggregation without results.
}
