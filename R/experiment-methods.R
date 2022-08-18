#' MultimodalExperiment Experiment Methods
#'
#' Extract or replace experiments of a [MultimodalExperiment-class] object by
#' index, name, or type.
#'
#' @param x a [MultimodalExperiment-class] object
#' @param i an integer or character index
#' @param value a replacement value
#'
#' @details The term matrix-like objects refers to [matrix][base::matrix]
#' objects or Bioconductor S4 objects that contain them (
#' [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class],
#' [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment-class],
#' etc.) where rows represent features and columns represent observations.
#'
#' @returns
#' `experiment` returns a matrix-like object.
#'
#' `bulkExperiments` returns an [ExperimentList] of matrix-like objects.
#'
#' `singleCellExperiments` returns an [ExperimentList] of matrix-like objects.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' experiment(ME, 2L) <-
#'     experiment(ME, 2L)[1:4, 1:4]
#'
#' experiment(ME, 2L)
#'
#' experiment(ME, "scRNAseq") <-
#'     experiment(ME, "scRNAseq")[1:4, 1:4]
#'
#' experiment(ME, "scRNAseq")
#'
#' bulkExperiments(ME) <-
#'     bulkExperiments(ME)[1L]
#'
#' bulkExperiments(ME)
#'
#' singleCellExperiments(ME) <-
#'     singleCellExperiments(ME)[2L]
#'
#' singleCellExperiments(ME)
#'
#' @name experiment-methods
NULL

#' @rdname experiment-methods
#'
#' @aliases experiment
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("experiment", "MultimodalExperiment", function(x, i) {
    experiments(x)[[i]]
})

#' @rdname experiment-methods
#'
#' @aliases experiment<-
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("experiment<-", "MultimodalExperiment", function(x, i, value) {
    experiments(x)[[i]] <-
        value

    x
})

#' @rdname experiment-methods
#'
#' @aliases bulkExperiments
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
setMethod("bulkExperiments", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    .experimentNames <-
        names(.experiments)

    .experimentMap <-
        experimentMap(x)

    bulkExperimentNames <-
        .experimentMap[.experimentMap[["type"]] %in% "bulk", "experiment"]

    .experiments[.experimentNames %in% bulkExperimentNames]
})

#' @rdname experiment-methods
#'
#' @aliases bulkExperiments<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics unlist
#' @importFrom S4Vectors unname
#' @importFrom BiocGenerics unique
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics setdiff
setMethod("bulkExperiments<-", "MultimodalExperiment", function(x, value) {
    valueNames <-
        names(value)

    valueNamesDataFrame <-
        DataFrame(experiment = valueNames)

    experimentData(x) <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "experiment") |>
        .outerJoin(valueNamesDataFrame) |>
        .columnToRowNames(colName = "experiment")

    valueColNames <-
        colnames(value) |>
        unlist() |>
        unname() |>
        unique()

    valueColNamesDataFrame <-
        DataFrame(sample = valueColNames)

    sampleData(x) <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "sample") |>
        .outerJoin(valueColNamesDataFrame) |>
        .columnToRowNames(colName = "sample")

    .experiments <-
        experiments(x)

    .experimentNames <-
        names(.experiments)

    .experimentMap <-
        experimentMap(x)

    bulkExperimentNames <-
        .experimentMap[.experimentMap[["type"]] %in% "bulk", "experiment"]

    bulkExperimentNames <-
        setdiff(bulkExperimentNames, valueNames)

    .experiments <-
        .experiments[!(.experimentNames %in% bulkExperimentNames)]

    .experimentMap[.experimentMap[["experiment"]] %in% valueNames, "type"] <-
        "bulk"

    lengthValueNames <-
        length(valueNames)

    newExperimentType <-
        rep_len("bulk", lengthValueNames)

    newExperimentMap <-
        DataFrame(type = newExperimentType, experiment = valueNames)

    experimentMap(x) <-
        .outerJoin(.experimentMap, newExperimentMap)

    subjectMap(x) <-
        subjectMap(x) |>
        .hemiJoin(valueNamesDataFrame)

    sampleMap(x) <-
        sampleMap(x) |>
        .hemiJoin(valueColNamesDataFrame)

    for (valueName in valueNames) {
        .experiments[[valueName]] <-
            value[[valueName]]
    }

    experiments(x) <-
        .experiments

    x
})

#' @rdname experiment-methods
#'
#' @aliases singleCellExperiments
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
setMethod("singleCellExperiments", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    .experimentNames <-
        names(.experiments)

    .experimentMap <-
        experimentMap(x)

    singleCellExperimentNames <-
        .experimentMap[.experimentMap[["type"]] %in% "single-cell", "experiment"]

    .experiments[.experimentNames %in% singleCellExperimentNames]
})

#' @rdname experiment-methods
#'
#' @aliases singleCellExperiments<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics unlist
#' @importFrom S4Vectors unname
#' @importFrom BiocGenerics unique
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics setdiff
setMethod("singleCellExperiments<-", "MultimodalExperiment", function(x, value) {
    valueNames <-
        names(value)

    valueNamesDataFrame <-
        DataFrame(experiment = valueNames)

    experimentData(x) <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "experiment") |>
        .outerJoin(valueNamesDataFrame) |>
        .columnToRowNames(colName = "experiment")

    valueColNames <-
        colnames(value) |>
        unlist() |>
        unname() |>
        unique()

    valueColNamesDataFrame <-
        DataFrame(cell = valueColNames)

    cellData(x) <-
        cellData(x) |>
        .rowNamesToColumn(colName = "cell") |>
        .outerJoin(valueColNamesDataFrame) |>
        .columnToRowNames(colName = "cell")

    .experiments <-
        experiments(x)

    .experimentNames <-
        names(.experiments)

    .experimentMap <-
        experimentMap(x)

    singleCellExperimentNames <-
        .experimentMap[.experimentMap[["type"]] %in% "single-cell", "experiment"]

    singleCellExperimentNames <-
        setdiff(singleCellExperimentNames, valueNames)

    .experiments <-
        .experiments[!(.experimentNames %in% singleCellExperimentNames)]

    .experimentMap[.experimentMap[["experiment"]] %in% valueNames, "type"] <-
        "single-cell"

    lengthValueNames <-
        length(valueNames)

    newExperimentType <-
        rep_len("single-cell", lengthValueNames)

    newExperimentMap <-
        DataFrame(type = newExperimentType, experiment = valueNames)

    experimentMap(x) <-
        .outerJoin(.experimentMap, newExperimentMap)

    subjectMap(x) <-
        subjectMap(x) |>
        .hemiJoin(valueNamesDataFrame)

    cellMap(x) <-
        cellMap(x) |>
        .hemiJoin(valueColNamesDataFrame)

    for (valueName in valueNames) {
        .experiments[[valueName]] <-
            value[[valueName]]
    }

    experiments(x) <-
        .experiments

    x
})
