get_dataset_dims <- function(packagename = NULL) {
  datasetnames <- data(package = packagename)$results[,3]

  # get rid of everything after space in dataset name
  datasetnames <- unlist(purrr::map(strsplit(datasetnames, " "), ~.x[[1]]))

  nrow <- purrr::map(datasetnames, ~dim(get(.x))[1])
  ncol <- purrr::map(datasetnames, ~dim(get(.x))[2])

  # change NULLs to NAs
  nrow <- unlist(purrr::map(nrow, ~ifelse(is.null(.x), NA, .x)))
  ncol <- unlist(purrr::map(ncol, ~ifelse(is.null(.x), NA, .x)))
  firstclass <- unlist(purrr::map(datasetnames, ~class(get(.x))[1]))
  data.frame(name = datasetnames, nrow, ncol, firstclass = firstclass)
}

