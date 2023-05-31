## data generation function for Missing Completely At Random (MCAR) setting
#'
#' @title Data generation function for MCAR
#' @description This function generates the simulation data (2 modalities) under the setting of MCAR
#' @param nsubj number of subjects
#' @param nvoxel1 number of voxels in modality 1
#' @param nvoxel2 number of voxels in modality 2
#' @param true_ncomp number of true component
#' @param seed seed
#' @param n_miss number of missing subjects
#'
#' @return The resulting dataset has lists including the no-missing data (Y1, Y2), order of subjects who were implemented missing (subj1_miss, subj2_miss), missing data (Y1_miss, Y2_miss), and other intermediate parameters during the data generation process.
#' @export
#'
#' @examples
#' dat = data_generate_MCAR(nsubj = 100, seed = 1234, n_miss = 5); names(dat)
data_generate_MCAR = function(nsubj = 100, nvoxel1 = 1000, nvoxel2 = 3000, true_ncomp = 2, seed = 1234, n_miss = 5){
  # Y(k) = XW(k) H + E(k)

  # H: ncomp x nsubj
  set.seed(seed)
  H = matrix(rnorm(true_ncomp*nsubj, mean = 0, sd = 1), nrow = true_ncomp, ncol = nsubj) # column-wise fill-in N(0,1)

  # X^(k): nvoxel x ncomp
  comp1 = rep(0, nvoxel1 + nvoxel2)
  comp1[c(1:100, 1001:1100)] = 1
  comp2 = rep(0, nvoxel1 + nvoxel2)
  comp2[c(101:200, 1101:1200)] = 1
  temp = cbind(comp1, comp2)
  error = matrix(rnorm(nrow(temp)*ncol(temp), mean = 0, sd = 1), nrow = nrow(temp), ncol = ncol(temp)) # column-wise fill-in N(0,1) random error
  X_cbind = temp + error
  #cor.test(X_cbind[,1],X_cbind[,2])

  X1 = X_cbind[1:nvoxel1,]
  X2 = X_cbind[-c(1:nvoxel1),]

  # W^(k): 1 x ncomp
  W1 = W2 = matrix(1, nrow = 1, ncol = true_ncomp)

  # XW(k): nvoxel x ncomp
  XW1 = XW2 = NULL
  for (i in 1:true_ncomp) {
    x1 = X1[,i]; x2 = X2[,i]
    w1 = W1[,i]; w2 = W2[,i]
    comp1 = x1*w1; comp2 = x2*w2
    XW1 = cbind(XW1, comp1); XW2 = cbind(XW2, comp2)
  }
  colnames(XW1) = colnames(XW2) = paste0("true_comp",1:true_ncomp)

  # Y: nvoxel x nsubj
  Y1 = XW1 %*% H + matrix(rnorm(nvoxel1*nsubj, mean = 0, sd = 1), nrow = nvoxel1, ncol = nsubj)
  Y2 = XW2 %*% H + matrix(rnorm(nvoxel2*nsubj, mean = 0, sd = 1), nrow = nvoxel2, ncol = nsubj)


  # prepare missing subjects
  set.seed(seed); miss.subj = sample(1:nsubj, nsubj)
  subj1 = miss.subj[1:(nsubj/2)]
  subj2 = miss.subj[(nsubj/2+1):nsubj]

  # implement missing subjects
  subj1_miss = subj1[1:n_miss]
  subj2_miss = subj2[1:n_miss]

  # assign missing subj NA
  Y1_miss = data.frame(Y1); Y1_miss[,subj1_miss] = NA # make it data frame to keep the ordering of the subjects
  Y2_miss = data.frame(Y2); Y2_miss[,subj2_miss] = NA

  # output data
  list(Y1 = Y1, Y2 = Y2, # matrix (flica needs matrix input)
       H = H, X1 = X1, X2 = X2, W1 = W1, W2 = W2, XW1 = XW1, XW2 = XW2,
       subj1 = subj1, subj2 = subj2, subj1_miss = subj1_miss, subj2_miss = subj2_miss,
       Y1_miss = as.matrix(Y1_miss), Y2_miss = as.matrix(Y2_miss), # data frame
       seed = seed
  )
}


