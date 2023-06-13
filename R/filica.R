## function to run FI-LICA
#'
#' @title Function to run FI-LICA
#' @description This function runs FI-LICA. Note: current function only supports the analysis on 2 modalities.
#' @param n number of FI-LICA updates
#' @param ncomp number of components to be used in LICA
#' @param flica_niter number of iterations to be used in FI-LICA's Step 1 (completer case analysis)
#' @param flica_niter2 number of iterations to be used in FI-LICA's Step 2
#' @param rescale rescale H and XW (default is TRUE)
#'
#' @return The resulting dataset includes the results estimated from FI-LICA, convergence measure for H and for XW, and the dF history in LICA
#' @export
#'
#' @examples
#' # FI-LICA
#' #re_filica = filica(ncomp = 5, flica_niter = 1500, n = 20, flica_niter2 = 1000, rescale = TRUE)
filica = function(ncomp = 5, flica_niter = 1500, n = 20, flica_niter2 = 1000, rescale = TRUE){
  message("< -- Running FI-LICA -- >")
  # Step 1
  message("< -- FI-LICA Step 1: LICA on completers ( ", flica_niter, " iterations ) -- >")
  re_completer = flica_completer(ncomp = ncomp, flica_niter = flica_niter)
  re_step1 = filica_step1(re = re_completer, rescale = rescale)
  ## last dF
  last_dF = re_completer$F.history[,length(re_completer$F.history)]-re_completer$F.history[,length(re_completer$F.history)-1]
  message("Last dF = ", last_dF)
  # step 2
  message("< -- FI-LICA Step 2: ( ", n, " updates + ", flica_niter2, " niter ) -- >")
  re_step2 = filica_step2(n = n, ncomp = ncomp, flica_niter2 = flica_niter2, H_prev = re_step1$H, XW_prev = re_step1$XW, rescale = rescale)
  re_filica = re_step2

  return(re_filica = re_filica)
}

