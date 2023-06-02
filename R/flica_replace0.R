## function to run LICA on data after replacing missing with 0
#'
#' @title Function to run LICA on data after replacing missing with 0
#' @description This function calls the matlab function to run LICA on data after replacing missing with 0
#' @param ncomp number of components to be used in LICA
#' @param flica_niter number of iterations to be used in LICA
#'
#' @return The resulting dataset is the results estimated from LICA
#' @export
#'
#' @examples
#' # Save data at "./MATLAB_code/flica/data_replace0.mat"
#' #writeMat("./MATLAB_code/flica/data_replace0.mat",
#' #         mod1_miss = mod1_replace0, mod2_miss = mod2_replace0,      # data with missing as 0
#' #         mod1_std = mod1_replace0_std, mod2_std = mod2_replace0_std # data with missing as 0 after standardization; code in matlab will use this
#' #         )
#' # Run current practice: replace NA with 0 + LICA
#' #re_replace0 = try(flica_replace0(ncomp = 5, flica_niter = 1500))
flica_replace0 = function(ncomp, flica_niter){

  ncomp = ncomp; flica_niter = flica_niter

  # save ncomp, flica_niter in flica folder s.t. matlab can read them
  writeMat("./MATLAB_code/flica/flica_replace0_info.mat",
           ncomp = ncomp, flica_niter = flica_niter
  )

  # flica
  a = run_matlab_script("./MATLAB_code/flica/code_replace0.m", verbose = FALSE)

  # to indicate whether flica can be run without error
  if (a == 0) {
    message("flica worked. ")
  } else {
    stop("flica failed. Stop. ")
  }

  # load results
  results = readMat("./MATLAB_code/flica/results_replace0.mat")
  re = results$Morig20
  names(re) = dimnames(re)[[1]]
  return(re)
}
