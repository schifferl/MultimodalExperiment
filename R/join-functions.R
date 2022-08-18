#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics union
#' @importFrom BiocGenerics intersect
#' @importFrom BiocGenerics setdiff
#' @importFrom BiocGenerics unique
.colNamesToKeep <- function(colNamesList, by) {
    if (is.null(by)) {
        colNamesToKeep <-
            Reduce(union, colNamesList)
    } else {
        duplicateColNames <-
            Reduce(intersect, colNamesList) |>
            setdiff(by)

        if (length(duplicateColNames) > 0L) {
            xColNames <-
                ifelse(colNamesList[[1L]] %in% duplicateColNames, ".x", "")

            xColNames <-
                paste0(colNamesList[[1L]], xColNames)

            yColNames <-
                ifelse(colNamesList[[2L]] %in% duplicateColNames, ".y", "")

            yColNames <-
                paste0(colNamesList[[2L]], yColNames)

            colNamesToKeep <-
                c(xColNames, yColNames) |>
                unique()
        } else {
            colNamesToKeep <-
                Reduce(union, colNamesList)
        }
    }

    colNamesToKeep
}

#' @importFrom BiocGenerics order
.rowNumberOrder <- function(joinedDataFrame) {
    order(joinedDataFrame[["rowNumber.x"]], joinedDataFrame[["rowNumber.y"]])
}

#' @importFrom BiocGenerics lapply
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics intersect
#' @importFrom S4Vectors merge
.outerJoin <- function(x, y, by = NULL) {
    colNamesList <-
        list(x, y) |>
        lapply(colnames)

    colNamesToKeep <-
        .colNamesToKeep(colNamesList, by)

    x <-
        .rowNumberToColumn(x)

    y <-
        .rowNumberToColumn(y)

    if (is.null(by)) {
        by <-
            Reduce(intersect, colNamesList)
    }

    joinedDataFrame <-
        merge(x, y, by = by, all = TRUE, sort = FALSE)

    rowNumberOrder <-
        .rowNumberOrder(joinedDataFrame)

    joinedDataFrame[rowNumberOrder, colNamesToKeep, drop = FALSE]
}

#' @importFrom BiocGenerics colnames
.hemiJoin <- function(x, y, by = NULL) {
    xColNames <-
        colnames(x)

    joinedDataFrame <-
        .outerJoin(x, y, by = by)

    joinedDataFrame[, xColNames, drop = FALSE]
}

#' @importFrom BiocGenerics lapply
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics intersect
#' @importFrom S4Vectors merge
.innerJoin <- function(x, y, by = NULL) {
    colNamesList <-
        list(x, y) |>
        lapply(colnames)

    colNamesToKeep <-
        .colNamesToKeep(colNamesList, by)

    x <-
        .rowNumberToColumn(x)

    y <-
        .rowNumberToColumn(y)

    if (is.null(by)) {
        by <-
            Reduce(intersect, colNamesList)
    }

    joinedDataFrame <-
        merge(x, y, by = by, all = FALSE, sort = FALSE)

    rowNumberOrder <-
        .rowNumberOrder(joinedDataFrame)

    joinedDataFrame[rowNumberOrder, colNamesToKeep, drop = FALSE]
}

#' @importFrom BiocGenerics colnames
.semiJoin <- function(x, y, by = NULL) {
    xColNames <-
        colnames(x)

    joinedDataFrame <-
        .innerJoin(x, y, by = by)

    joinedDataFrame[, xColNames, drop = FALSE]
}

#' @importFrom BiocGenerics lapply
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics intersect
#' @importFrom S4Vectors merge
.leftJoin <- function(x, y, by = NULL) {
    colNamesList <-
        list(x, y) |>
        lapply(colnames)

    colNamesToKeep <-
        .colNamesToKeep(colNamesList, by)

    x <-
        .rowNumberToColumn(x)

    y <-
        .rowNumberToColumn(y)

    if (is.null(by)) {
        by <-
            Reduce(intersect, colNamesList)
    }

    joinedDataFrame <-
        merge(x, y, by = by, all.x = TRUE, sort = FALSE)

    rowNumberOrder <-
        .rowNumberOrder(joinedDataFrame)

    joinedDataFrame[rowNumberOrder, colNamesToKeep, drop = FALSE]
}
