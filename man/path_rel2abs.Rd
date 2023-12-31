% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path_rel2abs.R
\name{path_rel2abs}
\alias{path_rel2abs}
\title{Converts a relative path to an absolute path. It tries really hard to create
canonical paths.}
\usage{
path_rel2abs(path, cwd = getwd())
}
\arguments{
\item{path}{(\code{character(1)}) Character string representing a relative path.}

\item{cwd}{(\code{character(1)}) Current working directory.}
}
\value{
(\code{character(1)}) Character string representing the absolute path
}
\description{
Converts a relative path to an absolute path. It tries really hard to create
canonical paths.
}
\examples{
path_rel2abs("..")
}
