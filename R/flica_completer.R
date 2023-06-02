## function to run LICA on complete cases
#'
#' @title Function to run LICA on complete cases
#' @description This function calls the matlab function to run LICA on complete cases
#' @param ncomp number of components to be used in LICA
#' @param flica_niter number of iterations to be used in LICA
#'
#' @return The resulting dataset is the results estimated from LICA
#' @export
#'
#' @examples
#' # Save data at "./MATLAB_code/flica/data_std.mat"
#' #writeMat("./MATLAB_code/flica/data_std.mat",
#' #         mod1_std_cmplt = mod1_std_cmplt, mod2_std_cmplt = mod2_std_cmplt # complete cases from standardized missing data
#' #         )
#' # Run LICA on completers
#' #re_completer = flica_completer(ncomp = 5, flica_niter = 1500)
flica_completer = function(ncomp, flica_niter){

  ncomp = ncomp; flica_niter = flica_niter

  # save ncomp, flica_niter in flica folder s.t. matlab can read them
  writeMat("./MATLAB_code/flica/flica_completer_info.mat",
           ncomp = ncomp, flica_niter = flica_niter
  )

  # flica
  a = run_matlab_script("./MATLAB_code/flica/code_completer.m", verbose = FALSE)

  # to indicate whether flica can be run without error
  if (a == 0) {
    message("flica worked. ")
  } else {
    stop("flica failed. Stop.\n")
  }

  # load results
  results = readMat("./MATLAB_code/flica/results_completer.mat")
  re = results$Morig20
  names(re) = dimnames(re)[[1]]
  return(re)
}

