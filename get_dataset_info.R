#' Get detailed info on R package datasets
#'
#' (not part of a package at the moment)
#'
#' @description
#' finds all datasets in a specified package and returns a tibble with information on the dimensions of the datasets as well as data types. It is an alternative to `data()` which simply lists the datasets with short descriptions.
#'
#' @param packagenames a character vector providing the package(s) to look in for data sets, or `NULL`. If `NULL`, all loaded packages will be searched.
#'
#' @section Output columns
#' @md
#'
#' * `package` name of package
#' * `name` name of dataset
#' * `dim` dimensions of dataset or `NA` for vectors, lists
#' * `length` length (vectors only)
#' * `first_class_listed` first class listed
#' * `n_cols` number of numeric columns
#' * `i_cols` number of integer columns
#' * `f_cols` number of factor columns
#' * `c_cols` number of character columns
#' * `d_cols` number of date columns
#' * `other_cols` number of other columns
#' * `allclasses` full list of classes
#'
#'
#' @examples
#'
#' x <- get_dataset_info("ggplot2")
#' View(x)
#'
#' x <- get_dataset_info()
#' View(x)
#'
#' x <- get_dataset_info(c("fivethirtyeight", "pgmm"))
#' View(x)

get_type_count <- function(dataset, datatype) {
  if("data.frame" %in% class(get(dataset))) {
    num_cols <- summary(as.factor(unlist(lapply(get(dataset), class))))[datatype]
    if (is.na(num_cols)) num_cols <- 0
  } else {
    num_cols <- NA
  }
  num_cols
}

# load data if not lazy load
loaddata <- function(package) {
  datasets <- data(package = package)$results[,3]
  if (!exists(datasets[1])) data(list = datasets, package = package)
}

get_dataset_info <- function(packagenames = NULL) {
  datasetnames <- data(package = packagenames)$results[,3]
  datasetpackages <- data(package = packagenames)$results[,1]

  if (!is.null(packagenames)) lapply(packagenames, library, character.only = TRUE)

  # load data from packages since some might not be lazy load

  lapply(packagenames, loaddata)

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
#  cnames <- unlist(purrr::map(datasetnames, ~paste(colnames(data.frame(get(.x))), collapse = " ")))

  dplyr::bind_cols(tibble::tibble(package = datasetpackages, name = datasetnames, dim,
                   length, first_class_listed, n_cols, i_cols, f_cols,
                   c_cols, d_cols, other_cols), allclasses)

}

