\name{margins}
\alias{margins}
\title{Marginal Effects}
\usage{
  margins(model, data, vars, boots = 1000,
    predictor = quote(predict(eval_model, data)),
    mc.cores = getOption("cores"),
    weights = rep(1, nrow(data)))
}
\arguments{
  \item{model}{a fully specified model to be run, nested
  within the quote() function}

  \item{data}{data.frame or data.table}

  \item{vars}{names of variables to compute, in a character
  vector}

  \item{boots}{number of iterations to use to compute
  quantile confidence intervals. Set to 0 to just produce
  effect}

  \item{predictor}{quoted function that will produce
  predictions from a model}

  \item{mc.cores}{number of cores used to run bootstraps}

  \item{weights}{if missing than all elements are given the
  same weight, otherwise the weights are coerced to a
  numeric and normalized to sum to one (which may not be
  possible if there are missing wights)}
}
\value{
  data.table with the columns 'variable', 'value' for the
  predictors, and 'y' for the outcome. If boots is set to a
  number greater than 0, then confidence intervals will
  also be produced
}
\description{
  Computes the average marginal effect.
}

