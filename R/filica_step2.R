## function to run Step 2 in FI-LICA
#'
#' @title Function to run Step 2 in FI-LICA
#' @description This function runs Step 2 in FI-LICA.
#' @param n number of FI-LICA updates
#' @param ncomp number of components to be used in LICA
#' @param flica_niter2 number of iterations to be used in LICA
#' @param H_prev the estimated H from Step 1
#' @param XW_prev the estimated XW from Step 1
#' @param rescale rescale H and XW (default is TRUE)
#'
#' @return The resulting list includes the estimated results, convergence measure for H and for XW, and the dF history from Step 2 in FI-LICA
#' @export
#'
#' @examples
#' # results from Step 1 in FI-LICA
#' #re_step1 = filica_step1(re = re_completer, rescale = TRUE) ## need subj_miss, mod_std, mod_std_complete
#' # run Step 2 in FI-LICA
#' #re_step2 = filica_step2(n = 20, ncomp = 5, flica_niter2 = 1000, H_prev = re_step1$H, XW_prev = re_step1$XW, rescale = TRUE)
filica_step2 = function(n, ncomp, flica_niter2, H_prev, XW_prev, rescale = TRUE){

  ncomp = ncomp; flica_niter = flica_niter2
  # save ncomp, flica_niter in flica folder s.t. matlab can read them
  writeMat("./MATLAB_code/flica/filica_step2_info.mat",
           ncomp = ncomp, flica_niter = flica_niter )

  XW_convergence = rep(NA, n)
  H_convergence = rep(NA, n)
  F_hist = matrix(NA, nrow = flica_niter, ncol = n)

  message("FI-LICA step 2 started.")

  for (j in 1:n) {
    message("Update ", j, " started. ")

    a = run_matlab_script("./MATLAB_code/flica/code_filica_update.m", verbose = FALSE)

    # to indicate whether flica can be run without error
    if (j == 1 & !(a == 0)) { # if first update failed, stop
      message("flica failed. Process stopped.")
      re = NULL
      break
    }

    if (a == 0) {
      message("flica worked. ")
    } else {
      message("flica failed. Process stopped.")
      re = NULL
      break
    }

    # load results
    results = readMat("./MATLAB_code/flica/results_filica_update.mat")
    re = results$Morig20
    names(re) = dimnames(re)[[1]]

    # save F
    F_hist[,j] = re$F.history

    # number of modality
    mod_n = length(re[["X"]])

    # estimated H: comp x completer
    H = re$H

    # compute XW for each mod k: voxel x comp
    XW_k = lapply(1:mod_n, function(k){
      re[["X"]][[k]][[1]] %*% diag( as.vector(re[["W"]][[k]][[1]]) )
    })

    if (rescale == TRUE) {
      # scale H (for each comp) ----
      H_scaled = t(scale(t(H), center = FALSE, scale = apply(t(H), 2, sd, na.rm = TRUE))) #?scale(): To scale by the standard deviations without centering
      # same as: D = diag(1/apply(H,1,sd)); t(t(H) %*% D); i.e. D %*% H
      # if any comp (rowwise) has NaN, make it 0
      H_scaled[which(rowSums(is.na(H_scaled))>0),] <- 0

      # scale XW
      D_inv = diag(apply(H,1,sd))
      XW_k_scaled = lapply(1:mod_n, function(k){XW_k[[k]] %*% D_inv})

      # rename
      H = H_scaled
      XW_k = XW_k_scaled
    }

    set.seed(1)
    Y_pred = lapply(1:mod_n, function(k){
      # each mod k {w/ missing subject}:

      if (length(subj_miss[[k]])>0) {

        # noise: voxel x missing_subj
        ## randomly take missing_subj_# of lambdas from its completers
        lambda_sample = sample(re$lambda[[k]][[1]], length(subj_miss[[k]]))
        ## generate E ~ N(0, 1/lambda) for each missing_subj, with length voxel
        E = do.call(cbind, lapply(1:length(lambda_sample), function(i){
          rnorm(nrow(XW_k[[k]]), mean = 0, sd = 1/lambda_sample[i]) }))

        # estimate Y for missing subj in mod k
        H_est_miss = H[,subj_miss[[k]]]
        Y_pred_miss = XW_k[[k]] %*% H_est_miss
        Y_pred_miss_addE = XW_k[[k]] %*% H_est_miss + E

        return(list(lambda_sample = lambda_sample, E = E,
                    H_est_miss = H_est_miss,
                    Y_pred_miss = Y_pred_miss, Y_pred_miss_addE = Y_pred_miss_addE))
      }
    })

    # replace missing (standardized scale)
    mod_std_update = lapply(1:mod_n, function(k){
      mod_std[[k]][,subj_miss[[k]]] <- Y_pred[[k]]$Y_pred_miss_addE
      return(mod_std[[k]])
    })

    # save data
    filename = "'./MATLAB_code/flica/data_std_update.mat'"
    eval(parse(text = paste('writeMat(',filename,',',
                            paste0(paste('mod', 1:mod_n, '_std = mod_std_update[[',1:mod_n,']]', sep=''),
                                   collapse=','),
                            ', H = H, mod_n = mod_n)',
                            sep='') ))


    # compute Frobenius-norm on change in H, and in weighted X
    H_convergence[j] = sqrt(sum((H-H_prev)^2))

    XW = do.call(rbind, lapply(1:mod_n, function(k){XW_k[[k]]}))
    XW_convergence[j] = sqrt(sum((XW-XW_prev)^2))

    message("XW convergence = ", XW_convergence[j])
    message("H convergence = ", H_convergence[j])

    # save current H, XW as previous for next iteration
    H_prev = H
    XW_prev = XW

    message("Done.")
  }

  message("FI-LICA step 2 done.")

  # save convergence results
  return(
    list(
      results = re, # last iteration results
      H_convergence = H_convergence,
      XW_convergence = XW_convergence,
      F_hist = F_hist
    )
  )
}

