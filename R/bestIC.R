## function to identify the best-matching component
#'
#' @title Function to identify the best-matching component
#' @description This function aims to identify the best-matching component with the reference component
#' @param true the reference data
#' @param estimated the estimated data, from which the best-matching components will be identified
#'
#' @return The resulting dataset gives the best-matching component for each reference component
#' @export
#'
#' @examples
#' # generate data
#' set.seed(1234)
#' dat1 = mvtnorm::rmvnorm(n = 100, mean = rep(0, 2), sigma = matrix(c(1,0.8,0.8,1), nrow = 2, ncol = 2)) # sigma: covariance matrix
#' dat2 = mvtnorm::rmvnorm(n = 100, mean = rep(0, 2), sigma = matrix(c(1,0.5,0.5,1), nrow = 2, ncol = 2))
#' dat3 = matrix(rnorm(300, 0, 1), nrow = 100, ncol = 3)
#' true = cbind(dat1[,1],dat2[,1]); estimated = cbind(dat1[,2],dat2[,2],dat3)
#' colnames(true) = paste0("comp", 1:2); colnames(estimated) = paste0("comp", 1:5)
#' # find the best-matching component
#' cor(true, estimated)
#' bestIC(true, estimated)
bestIC = function(true, estimated){

  true_ncomp = ncol(true)
  est_ncomp = ncol(estimated)

  colnames(estimated) = paste0("comp", 1:est_ncomp)

  identified_comp = NULL
  for (c in 1:true_ncomp) {
    cor = suppressWarnings(cor(true[,c], estimated)) # ~~~ NEWLY ADDED, suppress warning messages: the standard deviation is zero
    max.abs.cor = max(abs(cor), na.rm = T) # max abs cor val, in case cor = NA
    comp = colnames(cor)[which(abs(cor) == max.abs.cor)] # identified component name
    cor_val = (cor)[which(abs(cor) == max.abs.cor)] # identified component's correlation value
    identified_comp = rbind(identified_comp, cbind(ref_comp = colnames(true)[c], best_match = comp, cor_val))
    estimated = estimated[,-which(colnames(estimated) == comp)]
  }

  return(identified_comp)

}


