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

get_nrow <- function(name) {
  nrow <- ifelse(sum(c("data.frame", "matrix") %in% class(get(name))) > 0,
                 dim(get(name))[[1]], NA)
}

get_ncol <- function(name) {
  nrow <- ifelse(sum(c("data.frame", "matrix") %in% class(get(name))) > 0,
                 dim(get(name))[[2]], NA)
}

get_dataset_dims <- function(packagename = NULL) {
  datasetnames <- data(package = packagename)$results[,3]

  datasetpackages <- data(package = packagename)$results[,1]

  if (!is.null(packagename)) library(packagename, character.only = TRUE)

  # get rid of everything after space in dataset name
  datasetnames <- unlist(purrr::map(strsplit(datasetnames, " "), ~.x[[1]]))

#  nrow <- unlist(purrr::map(datasetnames, get_nrow))

#  ncol <- unlist(purrr::map(datasetnames, get_ncol))

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

  numeric_cols <- unlist(purrr::map2(datasetnames, "numeric", get_type_count))
  integer_cols <- unlist(purrr::map2(datasetnames, "integer", get_type_count))
  factor_cols <- unlist(purrr::map2(datasetnames, "factor", get_type_count))
  character_cols <- unlist(purrr::map2(datasetnames, "character", get_type_count))
  date_cols <- unlist(purrr::map2(datasetnames, "Date", get_type_count))

  other_cols <- ncol - (numeric_cols + integer_cols + factor_cols +
                          character_cols + date_cols)

  cbind(data.frame(package = datasetpackages, name = datasetnames, dim,
                   length, first_class_listed, numeric_cols, integer_cols, factor_cols,
                   character_cols, date_cols, other_cols), allclasses)
}

