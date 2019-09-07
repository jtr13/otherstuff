get_dataset_dims <- function(packagename = NULL) {
  datasetnames <- data(package = packagename)$results[,3]
  nrow <- purrr::map_int(datasetnames, ~dim(get(.x))[1])
  ncol <- purrr::map_int(datasetnames, ~dim(get(.x))[2])
  data.frame(name = datasetnames, nrow, ncol)
}

