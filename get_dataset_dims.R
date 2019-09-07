fnum <- function(dataset) ifelse("data.frame" %in% class(get(dataset)), summary(as.factor(unlist(lapply(get(dataset), class))))["numeric"], NA)

fchar <- function(dataset) ifelse("data.frame" %in% class(get(dataset)), summary(as.factor(unlist(lapply(get(dataset), class))))["character"], NA)

ffac <- function(dataset) ifelse("data.frame" %in% class(get(dataset)), summary(as.factor(unlist(lapply(get(dataset), class))))["factor"], NA)

get_dataset_dims <- function(packagename = NULL) {
  datasetnames <- data(package = packagename)$results[,3]

  datasetpackages <- data(package = packagename)$results[,1]

  if (!is.null(packagename)) library(packagename, character.only = TRUE)

  # get rid of everything after space in dataset name
  datasetnames <- unlist(purrr::map(strsplit(datasetnames, " "), ~.x[[1]]))

  nrow <- purrr::map(datasetnames, ~dim(get(.x))[1])
  ncol <- purrr::map(datasetnames, ~dim(get(.x))[2])

  # change NULLs to NAs
  nrow <- unlist(purrr::map(nrow, ~ifelse(is.null(.x), NA, .x)))
  ncol <- unlist(purrr::map(ncol, ~ifelse(is.null(.x), NA, .x)))
  firstclass <- unlist(purrr::map(datasetnames, ~class(get(.x))[1]))
  allclasses <- tibble::enframe(purrr::map(datasetnames, ~class(get(.x))), name = NULL, value = "allclasses")

  numericols <- unlist(purrr::map(datasetnames, fnum))
  charcols <- unlist(purrr::map(datasetnames, fchar))
  facols <- unlist(purrr::map(datasetnames, ffac))

  cbind(data.frame(name = datasetnames, package = datasetpackages, nrow, ncol, firstclass, numericols, charcols, facols), allclasses)
}
