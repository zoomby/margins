#' Marginal Plot
#'
#' Takes the output of the margins function, and creates a lattice-like dot-plot called a 'cake plot' or 'margin plot'
#'
#'@param margins margin estimates as created by the margins function
#'@param xlab (optional) label of the outcome variable (default is 'Marginal Effect')
#'@param ylab (optional) label to describe predictors (default is nothing)
#'@param expected (optional) the expected outcome in the population, to be drawn as a red dashed line. 
#'@return produces a plot and returns the ggplot2 object invisibly
#'@import ggplot2
#'@export

marginplot <- function(margins, xlab="Marginal Effect", ylab="", expected){
  require('ggplot2')
  data = copy(as.data.table(margins))
  p<- ggplot(data=data, aes(x=y,y=value, group=variable)) + geom_point() + labs(x=xlab, y=ylab) 
  if(!is.null(data[['y05']])){
    p<- p+ geom_segment( aes(y = value, yend = value, x=y10, xend=y90) , alpha=0.10 ) + geom_segment( aes(y = value, yend = value, x=y05, xend=y95) , alpha=0.30 ) 
  }
  if(!missing(expected)){
    p<- p +  geom_vline(xintercept=expected, linetype = "longdash", color="red")
  }
  p<- p + facet_grid( variable~., scales="free_y")
  plot(p)
  invisible(p)
}
