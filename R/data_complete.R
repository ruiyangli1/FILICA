## function to keep the completers
#'
#' @title Function to keep the complete cases
#' @description This function keeps the complete cases from the input modality data
#' @param data_list input modality data (as a list)
#'
#' @return The resulting dataset is the data containing the complete cases (as a list)
#' @export
#'
#' @examples
#' # generate data
#' dat = data_generate_MAR(nsubj = 100, seed = 1234, n_miss = 20)
#' # missing data (stored in lists)
#' dat_miss = NULL; dat_miss[[1]] = dat$Y1_miss; dat_miss[[2]] = dat$Y2_miss
#' # keep complete cases (stored in lists)
#' dat_complete = data_complete(dat_miss)
data_complete = function(data_list){
  data_n = length(data_list)
  col_nona = lapply(1:data_n, function(x){
    as.numeric(which(colSums(is.na(data_list[[x]])) == 0))
  })
  col_keep = as.numeric(which(table(unlist(col_nona)) == data_n))
  complete_data = lapply(1:data_n, function(x){
    data_list[[x]][,col_keep]
  })
  return(complete_data)
}


