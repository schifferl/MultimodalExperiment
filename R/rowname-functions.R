#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics nrow
.rowNumberToColumn <- function(x, colName = "rowNumber") {
    colNames <-
        colnames(x)

    x[, colName] <-
        nrow(x) |>
        seq_len()

    colNames <-
        c(colName, colNames)

    x[, colNames, drop = FALSE]
}

#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics rownames
#' @importFrom BiocGenerics rownames<-
.rowNamesToColumn <- function(x, colName = "rowName") {
    colNames <-
        colnames(x)

    x[, colName] <-
        rownames(x) |>
        as.character()

    rownames(x) <-
        NULL

    colNames <-
        c(colName, colNames)

    x[, colNames, drop = FALSE]
}

#' @importFrom BiocGenerics rownames<-
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics match
.columnToRowNames <- function(x, colName = "rowName") {
    rownames(x) <-
        x[[colName]]

    colNames <-
        colnames(x)

    colIndex <-
        match(colName, colNames)

    x[, -colIndex, drop = FALSE]
}
