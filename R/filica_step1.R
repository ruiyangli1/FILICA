## function to run Step 1 in FI-LICA
#'
#' @title Function to run Step 1 in FI-LICA
#' @description This function runs Step 1 in FI-LICA.
#' @param re the saved results from completer case analysis using LICA
#' @param rescale rescale H and XW (default is TRUE)
#'
#' @return The resulting list includes the estimated H, XW, and the last dF from Step 1 in FI-LICA
#' @export
#'
#' @examples
#' # run Step 1 in FI-LICA
#' #re_step1 = filica_step1(re = re_completer, rescale = TRUE) ## need subj_miss, mod_std, mod_std_complete
filica_step1 = function(re, rescale = TRUE){

  message("FI-LICA step 1 started. ")

  # number of modality
  mod_n = length(re[["X"]])

  # check convergence (want the difference in F < 0.1):
  flica_niter = length(re$F.history)
  last_dF = re$F.history[flica_niter] - re$F.history[flica_niter-1]

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

      XW_k_copy = XW_k
      XW_this = XW_k_copy[[k]]
      XW_k_copy[[k]] = NULL
      XW_other = do.call(rbind,lapply(1:length(XW_k_copy), function(x){XW_k_copy[[x]]}))

      mod_std_copy = mod_std
      mod_std_copy[[k]] = NULL
      Y_other = do.call(rbind,lapply(1:length(mod_std_copy), function(y){mod_std_copy[[y]]}))

      # estimate H for missing subj in mod k using mod -k (std) data
      H_est = MASS::ginv(t(XW_other) %*% XW_other) %*% t(XW_other) %*% as.matrix(Y_other)
      H_est_miss = H_est[,subj_miss[[k]]]
      Y_pred_miss = XW_this %*% H_est_miss
      Y_pred_miss_addE = XW_this %*% H_est_miss + E

      return(list(lambda_sample = lambda_sample, E = E,
                  H_est = H_est, H_est_miss = H_est_miss,
                  Y_pred_miss = Y_pred_miss, Y_pred_miss_addE = Y_pred_miss_addE,
                  XW_other = XW_other, XW_this = XW_this))
    }
  })

  # replace missing (standardized scale)
  mod_std_update = lapply(1:mod_n, function(k){
    mod_std[[k]][,subj_miss[[k]]] <- Y_pred[[k]]$Y_pred_miss_addE
    return(mod_std[[k]])
  })

  # save weighted X (stacked)
  XW_prev = do.call(rbind, lapply(1:mod_n, function(k){XW_k[[k]]}))

  # save H (combined)
  # estimated crude H for missing subj in each mod {w/ missing subject}
  H_k_crude = do.call(cbind, lapply(1:mod_n, function(k){
    if (length(subj_miss[[k]])>0) {
      H_est_miss = as.matrix(Y_pred[[k]]$H_est_miss)
      colnames(H_est_miss) = paste0("X", subj_miss[[k]])
      return(H_est_miss)
    }
  }))
  name = colnames(mod_std_update[[1]]) ## name of all subjects
  colnames(H) = colnames(mod_std_complete[[1]]) ## completers
  H_prev = cbind(H, H_k_crude) ## combine subjects
  H_prev = H_prev[,name] # reorder subjects

  # save data
  filename = "'./MATLAB_code/flica/data_std_update.mat'"
  eval(parse(text = paste('writeMat(',filename,',',
                          paste0(paste('mod', 1:mod_n, '_std = mod_std_update[[',1:mod_n,']]', sep=''),
                                 collapse=','),
                          ', H = H_prev, mod_n = mod_n)',
                          sep='') ))

  message("Done.")

  return(list(H = H_prev, XW = XW_prev, last_dF = last_dF))

}

