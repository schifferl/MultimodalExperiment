#' MultimodalExperiment Show Method
#'
#' Display details about a [MultimodalExperiment-class] object.
#'
#' @param object a [MultimodalExperiment-class] object
#'
#' @returns `show` returns `NULL` invisibly.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' show(ME)
#'
#' @name show-method
#'
#' @keywords internal
NULL

#' @importFrom methods is
#' @importFrom BiocGenerics paste
.classOrType <- function(object) {
    if (is(object, "Date") | is(object, "factor")) {
        classOrType <-
            class(object)
    } else {
        classOrType <-
            typeof(object)
    }

    paste("<", classOrType, ">", sep = "", collapse = "\n")
}

#' @importFrom BiocGenerics rownames
#' @importFrom BiocGenerics rownames<-
#' @importFrom BiocGenerics lapply
#' @importFrom S4Vectors DataFrame
#' @importFrom utils head
#' @importFrom BiocGenerics colnames<-
#' @importFrom BiocGenerics colnames
#' @importFrom utils tail
#' @importFrom BiocGenerics rbind
#' @importFrom BiocGenerics as.data.frame
#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics setdiff
#' @importFrom BiocGenerics union
#' @importFrom BiocGenerics cbind
#' @importFrom methods show
.catDataFrame <- function(object, nRows) {
    rowNames <-
        rownames(object)

    if (is.null(rowNames)) {
        rownames(object) <-
            seq_len(nRows) |>
            as.character()
    }

    showRowsT <-
        getOption("showRowsT", default = 2L)

    showRowsT <-
        ifelse(nRows <= showRowsT, nRows, showRowsT)

    showRowsB <-
        getOption("showRowsB", default = 2L)

    showRowsB <-
        ifelse(nRows - showRowsT < showRowsB, nRows - showRowsT, showRowsB)

    showColsL <-
        getOption("showColsL", default = 2L)

    showColsR <-
        getOption("showColsR", default = 2L)

    .type <-
        lapply(object, .classOrType) |>
        DataFrame(row.names = "")

    .head <-
        head(object, showRowsT)

    times <-
        length(.head)

    .dots <-
        rep("...", times) |>
        as.list(.dots) |>
        DataFrame(row.names = "...")

    colnames(.dots) <-
        colnames(.head)

    .tail <-
        tail(object, showRowsB)

    short <-
        rbind(.head, .tail) |>
        as.data.frame() |>
        format()

    .head <-
        head(short, showRowsT)

    .tail <-
        tail(short, showRowsB)

    nRowsHead <-
        nrow(.head)

    nRowsTail <-
        nrow(.tail)

    nRowsShow <-
        as.integer(nRowsHead + nRowsTail)

    if (nRows > nRowsShow) {
        short <-
            rbind(.type, .head, .dots, .tail) |>
            as.data.frame() |>
            format()
    } else {
        short <-
            rbind(.type, .head, .tail) |>
            as.data.frame() |>
            format()
    }

    colNamesShort <-
        colnames(short)

    keepColNamesL <-
        head(colNamesShort, showColsL)

    keepColNamesR <-
        tail(colNamesShort, showColsR) |>
        setdiff(keepColNamesL)

    colsL <-
        short[, keepColNamesL, drop = FALSE]

    times <-
        nrow(short)

    colsC <-
        data.frame(
            `...` = rep("   ...", times),
            row.names = rownames(short)
        )

    colsR <-
        short[, keepColNamesR, drop = FALSE]

    colNamesUnion <-
        union(keepColNamesL, keepColNamesR)

    if (setequal(colNamesShort, colNamesUnion)) {
        dotsDataFrame <-
            cbind(colsL, colsR)
    } else {
        dotsDataFrame <-
            cbind(colsL, colsC, colsR)
    }

    show(dotsDataFrame)
}

#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics ncol
.catExperimentData <- function(object) {
    .experimentData <-
        experimentData(object)

    .class <-
        class(.experimentData)

    if (.class == "DFrame") {
        .class <-
            as.character("DataFrame")
    }

    nRows <-
        nrow(.experimentData)

    nCols <-
        ncol(.experimentData)

    cat("experimentData:", .class, "with", nRows, "row(s) and", nCols, "column(s).", fill = TRUE)

    if (nRows == 0L | nCols == 0L) {
        return(invisible(NULL))
    }

    .catDataFrame(.experimentData, nRows)
}

#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics ncol
.catSubjectData <- function(object) {
    .subjectData <-
        subjectData(object)

    .class <-
        class(.subjectData)

    if (.class == "DFrame") {
        .class <-
            as.character("DataFrame")
    }

    nRows <-
        nrow(.subjectData)

    nCols <-
        ncol(.subjectData)

    cat("subjectData:", .class, "with", nRows, "row(s) and", nCols, "column(s).", fill = TRUE)

    if (nRows == 0L | nCols == 0L) {
        return(invisible(NULL))
    }

    .catDataFrame(.subjectData, nRows)
}

#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics ncol
.catSampleData <- function(object) {
    .sampleData <-
        sampleData(object)

    .class <-
        class(.sampleData)

    if (.class == "DFrame") {
        .class <-
            as.character("DataFrame")
    }

    nRows <-
        nrow(.sampleData)

    nCols <-
        ncol(.sampleData)

    cat("sampleData:", .class, "with", nRows, "row(s) and", nCols, "column(s).", fill = TRUE)

    if (nRows == 0L | nCols == 0L) {
        return(invisible(NULL))
    }

    .catDataFrame(.sampleData, nRows)
}

#' @importFrom BiocGenerics nrow
#' @importFrom BiocGenerics ncol
.catCellData <- function(object) {
    .cellData <-
        cellData(object)

    .class <-
        class(.cellData)

    if (.class == "DFrame") {
        .class <-
            as.character("DataFrame")
    }

    nRows <-
        nrow(.cellData)

    nCols <-
        ncol(.cellData)

    cat("cellData:", .class, "with", nRows, "row(s) and", nCols, "column(s).", fill = TRUE)

    if (nRows == 0L | nCols == 0L) {
        return(invisible(NULL))
    }

    .catDataFrame(.cellData, nRows)
}

.matrixOrClass <- function(object){
    if (is.matrix(object)) {
        matrixOrClass <-
            as.character("matrix")
    } else {
        matrixOrClass <-
            class(object)
    }

    matrixOrClass
}

#' @importFrom BiocGenerics lapply
#' @importFrom BiocGenerics paste
.catExperimentListElements <- function(object) {
    elementIndex <-
        seq(along.with = object)

    elementNames <-
        names(object)

    elementClass <-
        lapply(object, .matrixOrClass)

    elementNRows <-
        lapply(object, nrow)

    elementNCols <-
        lapply(object, ncol)

    elementPaste <-
        paste("[", elementIndex, "] ", elementNames, ": ", elementClass, " with ", elementNRows, " row(s) and ", elementNCols, " column(s).", sep = "", collapse = "\n")

    cat(elementPaste, fill = TRUE)
}

.catBulkExperiments <- function(.bulkExperiments, nBulkExperiments) {
    .class <-
        class(.bulkExperiments)

    cat("bulkExperiments:", .class, "with", nBulkExperiments, "bulk experiment(s).", fill = TRUE)

    if (nBulkExperiments == 0L) {
        return(invisible(NULL))
    }

    .catExperimentListElements(.bulkExperiments)
}

.catSingleCellExperiments <- function(.singleCellExperiments, nSingleCellExperiments) {
    .class <-
        class(.singleCellExperiments)

    cat("singleCellExperiments:", .class, "with", nSingleCellExperiments, "single-cell experiment(s).", fill = TRUE)

    if (nSingleCellExperiments == 0L) {
        return(invisible(NULL))
    }

    .catExperimentListElements(.singleCellExperiments)
}

#' @rdname show-method
#'
#' @aliases show
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("show", "MultimodalExperiment", function(object) {
    .class <-
        class(object)

    .bulkExperiments <-
        bulkExperiments(object)

    nBulkExperiments <-
        length(.bulkExperiments)

    .singleCellExperiments <-
        singleCellExperiments(object)

    nSingleCellExperiments <-
        length(.singleCellExperiments)

    cat(.class, "with", nBulkExperiments, "bulk and", nSingleCellExperiments, "single-cell experiment(s).", fill = TRUE)
    cat("", fill = TRUE)
    .catExperimentData(object)
    cat("", fill = TRUE)
    .catSubjectData(object)
    cat("", fill = TRUE)
    .catSampleData(object)
    cat("", fill = TRUE)
    .catCellData(object)
    cat("", fill = TRUE)
    .catBulkExperiments(.bulkExperiments, nBulkExperiments)
    cat("", fill = TRUE)
    .catSingleCellExperiments(.singleCellExperiments, nSingleCellExperiments)
    cat("", fill = TRUE)
    cat("Need help? Try browseVignettes(\"MultimodalExperiment\").", fill = TRUE)
    cat("Publishing? Cite with citation(\"MultimodalExperiment\").", fill = TRUE)
})
