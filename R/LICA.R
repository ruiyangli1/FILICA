## function to run LICA
#'
#' @title Function to run LICA
#' @description This function runs LICA for no-missing data and two current practices for missing data (complete case analysis and replaceing missing with 0s)
#' @param data data from different modalities in lists
#' @param ncomp number of components to be used in LICA
#' @param niter number of iterations to be used in LICA
#' @param method LICA method. nomiss: running LICA on no-missing data. completer: running LICA on complete cases. replace0: running LICA after replacing missing with 0s.
#'
#' @return The resulting list includes the results estimated from LICA
#' @export
#'
#' @examples
#' # e.g., LICA on no-missing data
#' #LICA(data = data, ncomp = 5, niter = 1500, method = "nomiss")
LICA = function(data, ncomp = 5, niter = 1000, method = c('nomiss','completer','replace0')){

  mod_n = length(data)

  if (method == 'nomiss') {
    # save data (standardization will be performed in MATLAB)
    filename = "'./MATLAB_code/flica/data_nomiss.mat'"
    eval(parse(text = paste('writeMat(',filename,',',
                            paste0(paste('mod', 1:mod_n, '_true = data[[',1:mod_n,']]', sep=''),
                                   collapse=','),
                            ', mod_n = mod_n)',
                            sep='') ))
    # run LICA
    message("< Running LICA on no-miss data >")
    re_nomiss = flica_nomiss(ncomp = ncomp, flica_niter = niter)

    return(re_nomiss)
  }

  if (method == 'completer') {
    # data is supposed to have missings
    # standardize missing data
    mod_std = NULL
    for (k in 1:mod_n) {
      mod_std[[k]] = data_standardization(data[[k]])
    }
    # keep complete data from the standardized missing data
    mod_std_complete = data_complete(mod_std)
    # save data
    filename = "'./MATLAB_code/flica/data_std.mat'"
    eval(parse(text = paste('writeMat(',filename,',',
                            paste0(paste('mod', 1:mod_n, '_std_cmplt = mod_std_complete[[',1:mod_n,']]', sep=''),
                                   collapse=','),
                            ', mod_n = mod_n)',
                            sep='') ))

    # run LICA
    message("< Running LICA on completers >")
    re_completer = flica_completer(ncomp = ncomp, flica_niter = niter)

    return(re_completer)
  }

  if (method == 'replace0') {
    # data is supposed to have missings
    # replace NA w/ 0
    subj_miss = NULL
    mod_replace0 = data
    for (k in 1:mod_n) {
      # subjects with NA
      subj_na = as.numeric(which(is.na(colSums(data[[k]]))))
      subj_miss[[k]] = subj_na
      # replace NA w/ 0
      mod_replace0[[k]][,subj_na] = 0
    }
    # standardize replace0 data
    mod_replace0_std = NULL
    for (k in 1:mod_n) {
      mod_replace0_std[[k]] = data_standardization(mod_replace0[[k]])
    }
    # save data
    filename = "'./MATLAB_code/flica/data_replace0.mat'"
    eval(parse(text = paste('writeMat(',filename,',',
                            paste0(paste('mod', 1:mod_n, '_std = mod_replace0_std[[',1:mod_n,']]', sep=''),
                                   collapse=','),
                            ', mod_n = mod_n)',
                            sep='') ))

    # run LICA
    message("< Running current practice: replace NA with 0 + LICA >")
    re_replace0 = try(flica_replace0(ncomp = ncomp, flica_niter = niter))

    return(re_replace0)
  }

}

