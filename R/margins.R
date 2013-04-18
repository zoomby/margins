#' Marginal Effects
#'
#' Computes the average marginal effect. 
#'
#'@param model a fully specified model to be run, nested within the quote() function
#'@param data data.frame or data.table
#'@param vars names of variables to compute, in a character vector
#'@param boots number of iterations to use to compute quantile confidence intervals. Set to 0 to just produce effect
#'@param predictor quoted function that will produce predictions from a model
#'@param mc.cores number of cores used to run bootstraps
#'@param weights if missing than all elements are given the same weight, otherwise the weights are coerced to a numeric and normalized to sum to one (which may not be possible if there are missing wights)
#'@return data.table with the columns 'variable', 'value' for the predictors, and 'y' for the outcome. If boots is set to a number greater than 0, then confidence intervals will also be produced
#'@import multicore
#'@import data.table
#'@export
#'
margins <- function(model,data, vars, boots=1000, predictor = quote(predict(eval_model, data)), mc.cores=getOption("cores"), weights= rep(1, nrow(data))){
  #eventually needs to work with factors and dummies and logical, etc.
  require('multicore');require('data.table')
  data_copy = copy(as.data.table(data))
  N <- nrow(data_copy)
  grid=data.table()
  
  for(i in seq_along(vars)){
    
    values =unique(quantile(na.omit(data_copy[[vars[i]]]),probs = seq(from=0,to=1,by=0.01), type=3))
    grid= rbind(grid, data.table(
      variable=vars[i], 
      value =     values))      
  }
  grid[, y := as.numeric(NA)]
  data = copy(data_copy)
  eval_model= eval(model)
  
  
  
  for (i in 1:nrow(grid)) {
    data = copy(data_copy)
    cl = class(data_copy[[grid[i]$variable]])
    set(data,j=which(names(data)==grid[i]$variable), value = grid[i]$value)
    this = eval(predictor)
    grid[i, y:=weighted.mean(this, weights, na.rm=TRUE)] 
    
  }
  
  
  run_model <- function(iteration, data_copy, grid, predictor ,model) {
    #browser()
    #cat(iteration)
    # cat("|")
    random_sample = sample(1:nrow(data_copy),nrow(data_copy), replace=TRUE)
    data = copy(data_copy[random_sample])
    eval_model= eval(model)
    random_grid = copy(grid)
    random_grid[,y:=as.numeric(NA)]
    
    for (i in 1:nrow(grid)) {
      data = copy(data_copy)
      set(data,j=which(names(data)==random_grid[i]$variable), value = random_grid[i]$value) 
      this = eval(predictor)
      random_grid[i, y:=weighted.mean(this, weights, na.rm=TRUE)] 
    }
    return(random_grid)
    
  }
  if(is.finite(boots) & boots > 0){
    boot_output = mclapply(1:boots, run_model,data_copy=data_copy,grid=grid, predictor=predictor, model=model,mc.set.seed = TRUE)
    
    boot_output = rbindlist(boot_output)
    confidence = boot_output[, list(
      y001= quantile(y, probs=0.001),
      y01= quantile(y, probs=0.01),
      y05= quantile(y, probs=0.05),
      y10= quantile(y, probs=0.10),
      y50= quantile(y, probs=0.50),
      y90= quantile(y, probs=0.90),
      y95= quantile(y, probs=0.95),
      y99= quantile(y, probs=0.99),
      y999= quantile(y, probs=0.999)
    ),
                             by=list(variable, value)]
    
    grid = merge(grid, confidence, by=c("variable","value"), all.x=T)
  }
  
  return(grid)
}
