## data generation function for Missing At Random (MAR) setting
#'
#' @title Data generation function for MAR (with continuous H)
#' @description This function generates the simulation data (2 modalities) for the setting of MAR, with two continuous covariates (C1,C2) and H
#' @param nsubj number of subjects
#' @param nvoxel1 number of voxels in modality 1
#' @param nvoxel2 number of voxels in modality 2
#' @param true_ncomp number of true component
#' @param seed seed
#' @param n_miss number of missing subjects
#' @param es_C1H1 correlation between the first covariate (C1) and the first component of H (H1)
#' @param es_C2H2 correlation between the second covariate (C2) and the second component of H (H2)
#'
#' @return The resulting dataset has lists including the no-missing data (Y1, Y2), order of subjects who were implemented missing (subj1_miss, subj2_miss), missing data (Y1_miss, Y2_miss), and other intermediate parameters during the data generation process.
#' @export
#'
#' @examples
#' dat = data_generate_MAR_ctnsH(nsubj = 100, seed = 1234, n_miss = 5); names(dat)
data_generate_MAR_ctnsH = function(nsubj = 100, nvoxel1 = 1000, nvoxel2 = 3000, true_ncomp = 2, seed = 1234, n_miss = 5, es_C1H1 = 0.5, es_C2H2 = 0.3){
  # Y(k) = XW(k) H + E(k)

  set.seed(seed)
  # covariate: nsubjx1, for H and missing subjects; C1,C2: a continuous variable, e.g., ~N(0,1)
  # H: ncomp x nsubj, comp1 correlated w/ C1, comp2 correlated w/ C2
  ## C1,C2,H1,H2
  mat = diag(1,4); mat[1,3]=mat[3,1]=es_C1H1; mat[2,4]=mat[4,2]=es_C2H2
  CH = mvtnorm::rmvnorm(n = nsubj, mean = rep(0, 4), sigma = mat) # sigma: covariance matrix
  C1 = CH[,1]
  C2 = CH[,2]
  H1 = CH[,3]
  H2 = CH[,4]
  #cor(CH)

  # H: ncomp x nsubj
  H = t(cbind(scale(H1),scale(H2))) ## H is expected to ~ N(0,1)
  #mean(H);sd(H)

  # X^(k): nvoxel x ncomp
  comp1 = rep(0, nvoxel1 + nvoxel2)
  comp1[c(1:100, nvoxel1 + (1:100))] = 1
  comp2 = rep(0, nvoxel1 + nvoxel2)
  comp2[c(101:200, nvoxel1 + (101:200))] = 1
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


  # prepare missing subjects (MAR: probability of missing is conditional on other variables)
  Z = -.6 + 0.5*C1 + 1.2*C2
  ## beta0: log odds of missing when C1 = 0 (at mean) C2 of 0 category
  ## beta1: log odds of missing with 1 unit change in C1, holding C2 fixed
  ## beta2: log odds of missing when C2 = 1 vs C2 = 0, holding C1 fixed
  p = 1/(1 + exp(-Z))

  # top n_miss % in p for each mod
  subj = sort.int(p, index.return = TRUE, decreasing = TRUE)$ix
  subj1 = subj[seq(1,nsubj,2)] # assign top 1,3,5, ... to mod 1
  subj2 = subj[seq(2,nsubj,2)] # assign top 2,4,6, ... to mod 2

  # implement missing subjects
  subj1_miss = subj1[1:n_miss]
  subj2_miss = subj2[1:n_miss]

  # assign missing subj NA
  Y1_miss = data.frame(Y1); Y1_miss[,subj1_miss] = NA # make it data frame to keep the ordering of the subjects
  Y2_miss = data.frame(Y2); Y2_miss[,subj2_miss] = NA

  # output data
  list(Y1 = Y1, Y2 = Y2, # matrix (flica needs matrix input)
       H = H, X1 = X1, X2 = X2, W1 = W1, W2 = W2, XW1 = XW1, XW2 = XW2,
       subj1 = subj1, subj2 = subj2, subj1_miss = subj1_miss, subj2_miss = subj2_miss, subj = subj,
       Y1_miss = as.matrix(Y1_miss), Y2_miss = as.matrix(Y2_miss), # data frame
       seed = seed,
       C1 = C1, C2 = C2,
       H1 = H1, H2 = H2
  )
}


