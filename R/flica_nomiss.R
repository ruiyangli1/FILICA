## function to run LICA on no-missing data
#'
#' @title Function to run LICA on no-missing data
#' @description This function calls the matlab function to run LICA on no-missing data
#' @param ncomp number of components to be used in LICA
#' @param flica_niter number of iterations to be used in LICA
#'
#' @return The resulting dataset is the results estimated from LICA
#' @export
#'
#' @examples
#' # Save data at "./MATLAB_code/flica/data_nomiss.mat"
#' #writeMat("./MATLAB_code/flica/data_nomiss.mat",
#' #         mod1_true = mod1_true, mod2_true = mod2_true  # simulated data w/ no missing (before standardization, code in matlab will do the standardization)
#' #         )
#' # Run LICA on no-miss data
#' #re_nomiss = flica_nomiss(ncomp = 5, flica_niter = 1500)
flica_nomiss = function(ncomp, flica_niter){

  ncomp = ncomp; flica_niter = flica_niter

  # save ncomp, flica_niter in flica folder s.t. matlab can read them
  writeMat("./MATLAB_code/flica/flica_nomiss_info.mat",
           ncomp = ncomp, flica_niter = flica_niter
  )

  # flica
  a = run_matlab_script("./MATLAB_code/flica/code_nomiss.m", verbose = FALSE)

  # to indicate whether flica can be run without error
  if (a == 0) {
    message("flica worked. ")
  } else {
    stop("flica failed. Stop.\n")
  }

  # load results
  results = readMat("./MATLAB_code/flica/results_nomiss.mat")
  re = results$Morig20
  names(re) = dimnames(re)[[1]]
  return(re)
}


