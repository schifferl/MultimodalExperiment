#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics colnames<-
.enframe <- function(x, name = "name", value = "value") {
    enframedDataFrame <-
        DataFrame(x) |>
        .rowNamesToColumn()

    colnames(enframedDataFrame) <-
        c(name, value)

    enframedDataFrame
}

#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics unlist
#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics cbind
.bulkExperimentMap <- function(x) {
    bulkExperimentMap <-
        bulkExperiments(x) |>
        colnames() |>
        unlist() |>
        .enframe(name = "experiment", value = "sample")

    nRowBulkExperimentMap <-
        nrow(bulkExperimentMap)

    typeBulkExperimentMap <-
        rep_len("bulk", nRowBulkExperimentMap)

    cbind(type = typeBulkExperimentMap, bulkExperimentMap)
}

#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics unlist
#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics cbind
.singleCellExperimentMap <- function(x) {
    singleCellExperimentMap <-
        singleCellExperiments(x) |>
        colnames() |>
        unlist() |>
        .enframe(name = "experiment", value = "cell")

    nRowSingleCellExperimentMap <-
        nrow(singleCellExperimentMap)

    typeSingleCellExperimentMap <-
        rep_len("single-cell", nRowSingleCellExperimentMap)

    cbind(type = typeSingleCellExperimentMap, singleCellExperimentMap)
}
