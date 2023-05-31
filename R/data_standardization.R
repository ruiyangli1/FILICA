## data standardization function
#'
#' @title Data standardization function
#' @description This function standardizes the input data
#' @param raw_data input data
#'
#' @return The resulting dataset is the standardized data
#' @export
#'
#' @examples
#' dat = data_generate_MCAR(nsubj = 100, seed = 1234, n_miss = 5); head(dat$Y1[,1:5])
#' Y1_std = data_standardization(dat$Y1); head(Y1_std[,1:5])
data_standardization = function(raw_data){
  rowmean = rowMeans(raw_data, na.rm = T)
  rms = sqrt(rowMeans((raw_data-rowmean)^2, na.rm = T))
  std_data = (raw_data - rowmean)/rms
  #apply(std_data[1:10,1:5],1,is.nan)
  #apply(std_data[1:10,1:5],2,is.nan)
  std_data[apply(std_data,2,is.nan)] = 0 # in case of any voxel has (0-0)/0 = NaN, make it 0
  std_data[apply(std_data,2,is.infinite)] = 0 # in case if any voxel has non-zero#/0 = +/-Inf, make it 0
  std_data = as.matrix(std_data) # flica needs matrix input
  return(std_data)
}


