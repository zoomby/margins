\name{marginplot}
\alias{marginplot}
\title{Marginal Plot}
\usage{
  marginplot(margins, xlab = "Marginal Effect", ylab = "",
    expected)
}
\arguments{
  \item{margins}{margin estimates as created by the margins
  function}

  \item{xlab}{(optional) label of the outcome variable
  (default is 'Marginal Effect')}

  \item{ylab}{(optional) label to describe predictors
  (default is nothing)}

  \item{expected}{(optional) the expected outcome in the
  population, to be drawn as a red dashed line.}
}
\value{
  produces a plot and returns the ggplot2 object invisibly
}
\description{
  Takes the output of the margins function, and creates a
  lattice-like dot-plot called a 'cake plot' or 'margin
  plot'
}

