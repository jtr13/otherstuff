# provides information on built in datasets in R

get_type_count <- function(dataset, datatype) {
  if("data.frame" %in% class(get(dataset))) {
    num_cols <- summary(as.factor(unlist(lapply(get(dataset), class))))[datatype]
    if (is.na(num_cols)) num_cols <- 0
  } else {
    num_cols <- NA
  }
  num_cols
}

get_dataset_dims <- function(packagename = NULL) {
  datasetnames <- data(package = packagename)$results[,3]
  datasetpackages <- data(package = packagename)$results[,1]

  if (!is.null(packagename)) library(packagename, character.only = TRUE)

  # load data from packages if not lazy load

  if (!exists(datasetnames[1])) data(list = datasetnames, package = packagename)

  # get rid of everything after space in dataset name
  datasetnames <- unlist(purrr::map(strsplit(datasetnames, " "), ~.x[[1]]))




  dim <- purrr::map_chr(datasetnames,
                        ~ifelse(length(dim(get(.x))) > 0,
                                paste(dim(get(.x)), collapse = "  "),
                                NA))

  ncol <- unlist(purrr::map(datasetnames,
                            ~ifelse(length(dim(get(.x))) == 2,
                                    dim(get(.x))[[2]],
                                    NA)))

  length <- unlist(purrr::map(datasetnames, ~ifelse(is.null(dim(get(.x))), length(get(.x)), NA)))

  first_class_listed <- unlist(purrr::map(datasetnames, ~class(get(.x))[1]))

  allclasses <- tibble::enframe(purrr::map(datasetnames, ~class(get(.x))),
                                name = NULL, value = "allclasses")

  n_cols <- unlist(purrr::map2(datasetnames, "numeric", get_type_count))
  i_cols <- unlist(purrr::map2(datasetnames, "integer", get_type_count))
  f_cols <- unlist(purrr::map2(datasetnames, "factor", get_type_count))
  c_cols <- unlist(purrr::map2(datasetnames, "character", get_type_count))
  d_cols <- unlist(purrr::map2(datasetnames, "Date", get_type_count))

  other_cols <- ncol - (n_cols + i_cols + f_cols + c_cols + d_cols)

  # this needs work
  cnames <- unlist(purrr::map(datasetnames, ~paste(colnames(data.frame(get(.x))), collapse = " ")))

  cbind(data.frame(package = datasetpackages, name = datasetnames, dim,
                   length, first_class_listed, n_cols, i_cols, f_cols,
                   c_cols, d_cols, other_cols), allclasses, cnames)

}

