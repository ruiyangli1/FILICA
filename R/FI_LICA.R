## function to run FI-LICA
#'
#' @title Function to run FI-LICA
#' @description This function runs FI-LICA.
#' @param data data from different modalities in lists
#' @param n number of FI-LICA updates
#' @param ncomp number of components to be used in LICA
#' @param flica_niter number of LICA iterations to be used in FI-LICA's Step 1 (initialization process; completer case analysis)
#' @param flica_niter2 number of LICA iterations to be used in FI-LICA's Step 2 (optimization process)
#' @param rescale rescale H and XW (default is TRUE)
#' @param re_completer results from completer case analysis. Default is NULL and completer case analysis will be run and saved during the process; otherwise, the given results will be used.
#'
#' @return The resulting list includes the estimated results, the convergence measures for H and for XW, and the dF history from FI-LICA
#' @export
#'
#' @examples
#' # generate data
#' data = data_generate_MAR(nsubj = 100, seed = 7452, n_miss = 5)
#' # data with missing in lists
#' data = list(data$Y1_miss, data$Y2_miss)
#' # run FI-LICA
#' #re_filica = FI_LICA(data = data, ncomp = 5, flica_niter = 1500, n = 20, flica_niter2 = 1000, rescale = TRUE, re_completer = NULL)
#' # or
#' #re_completer = LICA(data = data, ncomp = 5, niter = 1500, method = "completer")
#' #re_filica = FI_LICA(data = data, ncomp = 5, flica_niter = 1500, n = 20, flica_niter2 = 1000, rescale = TRUE, re_completer = re_completer)
FI_LICA = function(data, ncomp = 5, flica_niter = 1500, n = 20, flica_niter2 = 1000, rescale = TRUE, re_completer = NULL){

  # modality number
  mod_n = length(data)

  # Step 0
  message("< -- FI-LICA Step 0: Data Preparation -- >")

  # rename subjects (X1,X2,...)
  for (k in 1:mod_n) {
    colnames(data[[k]]) = paste0("X", 1:ncol(data[[k]]))
  }

  # standardized missing data
  mod_std = NULL
  for (k in 1:mod_n) {
    mod_std[[k]] = data_standardization(data[[k]])
  }
  assign("mod_std", mod_std, .GlobalEnv) ## s.t. filica_step1() can use it

  #complete data from the standardized missing data
  mod_std_complete = data_complete(mod_std)
  assign("mod_std_complete", mod_std_complete, .GlobalEnv) ## s.t. filica_step1() can use it

  # subjects with missing
  subj_miss = lapply(1:mod_n, function(k) {
    as.numeric(which(colSums(is.na(data[[k]])) != 0))
  })
  assign("subj_miss", subj_miss, .GlobalEnv) ## s.t. filica_step1() can use it

  message("Done.")



  # Step 1
  message("< -- FI-LICA Step 1: Initialization -- >")

  if (is.null(re_completer)) { # if already has the results from completer case analysis, use; otherwise, run it here
    re_completer = LICA(data = data, ncomp = ncomp, niter = flica_niter, method = "completer")
    assign("completer_results_saved_from_FILICA", re_completer, .GlobalEnv) ## to store the interim completer analysis
  }
  ## last dF
  last_dF = re_completer$F.history[,length(re_completer$F.history)]-re_completer$F.history[,length(re_completer$F.history)-1]
  message("Last dF = ", last_dF)

  re_step1 = filica_step1(re = re_completer, rescale = rescale)



  # Step 2
  message("< -- FI-LICA Step 2: Optimization ( ", n, " updates ) -- >")

  re_step2 = filica_step2(n = n, ncomp = ncomp, flica_niter2 = flica_niter2, H_prev = re_step1$H, XW_prev = re_step1$XW, rescale = rescale)
  re_filica = re_step2

  return(re_filica = re_filica)
}

