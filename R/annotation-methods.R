#' MultimodalExperiment Annotation Methods
#'
#' `joinAnnotations` joins all annotations into an unnormalized
#' [DataFrame][S4Vectors::DataFrame-class] object.
#'
#' @param x a [MultimodalExperiment-class] object
#'
#' @returns `joinAnnotations` returns a [DataFrame][S4Vectors::DataFrame-class]
#' object.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' joinAnnotations(ME)
#'
#' @name annotation-methods
NULL

#' @rdname annotation-methods
#'
#' @aliases joinAnnotations
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics order
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics setdiff
#' @importFrom BiocGenerics unique
setMethod("joinAnnotations", "MultimodalExperiment", function(x) {
    joinedMaps <-
        joinMaps(x)

    experimentCols <-
        c("type", "experiment")

    .experimentMap <-
        unique(joinedMaps[, experimentCols])

    .experimentData <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "experiment") |>
        .rowNumberToColumn(colName = "rowNumber.experimentData")

    experimentAnnotations <-
        .outerJoin(.experimentMap, .experimentData) |>
        .innerJoin(.experimentData)

    rowOrder <-
        order(experimentAnnotations[["rowNumber.experimentData"]])

    colOrder <-
        colnames(experimentAnnotations) |>
        setdiff("rowNumber.experimentData")

    experimentAnnotations <-
        experimentAnnotations[rowOrder, colOrder, drop = FALSE]

    subjectCols <-
        c("type", "experiment", "subject")

    .subjectMap <-
        unique(joinedMaps[, subjectCols])

    .subjectData <-
        subjectData(x) |>
        .rowNamesToColumn(colName = "subject") |>
        .rowNumberToColumn(colName = "rowNumber.subjectData")

    subjectAnnotations <-
        .outerJoin(.subjectMap, .subjectData) |>
        .innerJoin(.subjectData)

    rowOrder <-
        order(subjectAnnotations[["rowNumber.subjectData"]])

    colOrder <-
        colnames(subjectAnnotations) |>
        setdiff("rowNumber.subjectData")

    subjectAnnotations <-
        subjectAnnotations[rowOrder, colOrder, drop = FALSE]

    sampleCols <-
        c("type", "experiment", "subject", "sample")

    .sampleMap <-
        unique(joinedMaps[, sampleCols])

    .sampleData <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "sample") |>
        .rowNumberToColumn(colName = "rowNumber.sampleData")

    sampleAnnotations <-
        .outerJoin(.sampleMap, .sampleData) |>
        .innerJoin(.sampleData)

    rowOrder <-
        order(sampleAnnotations[["rowNumber.sampleData"]])

    colOrder <-
        colnames(sampleAnnotations) |>
        setdiff("rowNumber.sampleData")

    sampleAnnotations <-
        sampleAnnotations[rowOrder, colOrder, drop = FALSE]

    cellCols <-
        c("type", "experiment", "subject", "sample", "cell")

    .cellMap <-
        unique(joinedMaps[, cellCols])

    .cellData <-
        cellData(x) |>
        .rowNamesToColumn(colName = "cell") |>
        .rowNumberToColumn(colName = "rowNumber.cellData")

    cellAnnotations <-
        .outerJoin(.cellMap, .cellData) |>
        .innerJoin(.cellData)

    rowOrder <-
        order(cellAnnotations[["rowNumber.cellData"]])

    colOrder <-
        colnames(cellAnnotations) |>
        setdiff("rowNumber.cellData")

    cellAnnotations <-
        cellAnnotations[rowOrder, colOrder, drop = FALSE]

    joinedAnnotations <-
        .outerJoin(experimentAnnotations, subjectAnnotations, by = experimentCols) |>
        .outerJoin(sampleAnnotations, by = subjectCols) |>
        .outerJoin(cellAnnotations, by = sampleCols)

    joinedAnnotations
})
