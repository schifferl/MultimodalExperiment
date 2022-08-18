#' MultimodalExperiment Name Methods
#'
#' Extract or replace names of a [MultimodalExperiment-class] object.
#'
#' @param x a [MultimodalExperiment-class] object
#' @param value a replacement value
#'
#' @returns
#' `names` returns a [CharacterList][IRanges::AtomicList] object.
#'
#' `rownames` returns a [CharacterList][IRanges::AtomicList] object.
#'
#' `colnames` returns a [CharacterList][IRanges::AtomicList] object.
#'
#' `dimnames` returns a [list][base::list] object.
#'
#' `experimentNames` returns a [character][base::character] vector.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' names(ME) <-
#'     names(ME) |>
#'     tolower()
#'
#' names(ME)
#'
#' rownames(ME) <-
#'     rownames(ME) |>
#'     toupper()
#'
#' rownames(ME)
#'
#' colnames(ME) <-
#'     colnames(ME) |>
#'     tolower()
#'
#' colnames(ME)
#'
#' dimnames(ME)[[2L]] <-
#'     dimnames(ME)[[2L]] |>
#'     toupper()
#'
#' dimnames(ME)[[2L]]
#'
#' experimentNames(ME) <-
#'     experimentNames(ME) |>
#'     gsub(pattern = "seq", replacement = "-seq")
#'
#' experimentNames(ME)
#'
#' @name name-methods
NULL

#' @rdname name-methods
#'
#' @aliases names
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics rownames
setMethod("names", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    rownames(.experiments)
})

#' @rdname name-methods
#'
#' @aliases names<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics rownames
#' @importFrom S4Vectors mendoapply
#' @importFrom BiocGenerics rownames<-
setMethod("names<-", "MultimodalExperiment", function(x, value) {
    .experiments <-
        experiments(x)

    if (length(value) != length(.experiments)) {
        stop("length(value) != length(experiments(x))", call. = FALSE)
    }

    valueNames <-
        names(value)

    .experimentNames <-
        names(.experiments)

    if (!all(valueNames == .experimentNames)) {
        if (all(valueNames %in% .experimentNames)) {
            value <-
                value[.experimentNames]
        } else {
            stop("names(value) != experimentNames(x)", call. = FALSE)
        }
    }

    experimentRowNames <-
        rownames(.experiments)

    if (any(lengths(value) != lengths(experimentRowNames))) {
        stop("lengths(value) != lengths(rownames(x))", call. = FALSE)
    }

    experiments(x) <-
        mendoapply(`rownames<-`, .experiments, value)

    x
})

#' @rdname name-methods
#'
#' @aliases rownames
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics rownames
setMethod("rownames", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    rownames(.experiments)
})

#' @rdname name-methods
#'
#' @aliases rownames<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics rownames
#' @importFrom S4Vectors mendoapply
#' @importFrom BiocGenerics rownames<-
setMethod("rownames<-", "MultimodalExperiment", function(x, value) {
    .experiments <-
        experiments(x)

    if (length(value) != length(.experiments)) {
        stop("length(value) != length(experiments(x))", call. = FALSE)
    }

    valueNames <-
        names(value)

    .experimentNames <-
        names(.experiments)

    if (!all(valueNames == .experimentNames)) {
        if (all(valueNames %in% .experimentNames)) {
            value <-
                value[.experimentNames]
        } else {
            stop("names(value) != experimentNames(x)", call. = FALSE)
        }
    }

    experimentRowNames <-
        rownames(.experiments)

    if (any(lengths(value) != lengths(experimentRowNames))) {
        stop("lengths(value) != lengths(rownames(x))", call. = FALSE)
    }

    experiments(x) <-
        mendoapply(`rownames<-`, .experiments, value)

    x
})

#' @rdname name-methods
#'
#' @aliases colnames
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics colnames
setMethod("colnames", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    colnames(.experiments)
})

#' @rdname name-methods
#'
#' @aliases colnames<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics colnames
#' @importFrom BiocGenerics unlist
#' @importFrom S4Vectors unname
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics unique
#' @importFrom BiocGenerics duplicated
#' @importFrom BiocGenerics colnames<-
#' @importFrom S4Vectors mendoapply
setMethod("colnames<-", "MultimodalExperiment", function(x, value) {
    .experiments <-
        experiments(x)

    if (length(value) != length(.experiments)) {
        stop("length(value) != length(experiments(x))", call. = FALSE)
    }

    valueNames <-
        names(value)

    .experimentNames <-
        names(.experiments)

    if (!all(valueNames == .experimentNames)) {
        if (all(valueNames %in% .experimentNames)) {
            value <-
                value[.experimentNames]
        } else {
            stop("names(value) != experimentNames(x)", call. = FALSE)
        }
    }

    experimentColNames <-
        colnames(.experiments)

    if (any(lengths(value) != lengths(experimentColNames))) {
        stop("lengths(value) != lengths(colnames(x))", call. = FALSE)
    }

    oldColNames <-
        unlist(experimentColNames) |>
        unname()

    newColNames <-
        unlist(value) |>
        unname()

    colNamesMap <-
        DataFrame(
            oldColName = oldColNames,
            newColName = newColNames
        ) |>
        unique()

    oldColNameIsduplicated <-
        duplicated(colNamesMap[["oldColName"]])

    if (any(oldColNameIsduplicated)) {
        stop("sample and/or cell mappings are not one-to-one", call. = FALSE)
    }

    .sampleData <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "oldColName") |>
        .leftJoin(colNamesMap)

    .sampleData[["newColName"]] <-
        ifelse(is.na(.sampleData[["newColName"]]), .sampleData[["oldColName"]],
               .sampleData[["newColName"]])

    sampleData(x) <-
        .columnToRowNames(.sampleData, colName = "oldColName") |>
        .columnToRowNames(colName = "newColName")

    .cellData <-
        cellData(x) |>
        .rowNamesToColumn(colName = "oldColName") |>
        .leftJoin(colNamesMap)

    .cellData[["newColName"]] <-
        ifelse(is.na(.cellData[["newColName"]]), .cellData[["oldColName"]],
               .cellData[["newColName"]])

    cellData(x) <-
        .columnToRowNames(.cellData, colName = "oldColName") |>
        .columnToRowNames(colName = "newColName")

    .sampleMap <-
        sampleMap(x)

    colnames(.sampleMap) <-
        c("subject", "oldColName")

    .sampleMap <-
        .leftJoin(.sampleMap, colNamesMap)

    .sampleMap[["sample"]] <-
        ifelse(is.na(.sampleMap[["newColName"]]), .sampleMap[["oldColName"]],
               .sampleMap[["newColName"]])

    sampleMapColNames <-
        c("subject", "sample")

    sampleMap(x) <-
        .sampleMap[, sampleMapColNames]

    .cellMap <-
        cellMap(x)

    colnames(.cellMap) <-
        c("sample", "oldColName")

    .cellMap <-
        .leftJoin(.cellMap, colNamesMap)

    .cellMap[["cell"]] <-
        ifelse(is.na(.cellMap[["newColName"]]), .cellMap[["oldColName"]],
               .cellMap[["newColName"]])

    cellMapColNames <-
        c("sample", "cell")

    cellMap(x) <-
        .cellMap[, cellMapColNames]

    experiments(x) <-
        mendoapply(`colnames<-`, .experiments, value)

    x
})

#' @rdname name-methods
#'
#' @aliases dimnames
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("dimnames", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    dimnames(.experiments)
})

#' @rdname name-methods
#'
#' @aliases dimnames<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics %in%
#' @importFrom BiocGenerics unlist
#' @importFrom S4Vectors unname
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics unique
#' @importFrom BiocGenerics duplicated
#' @importFrom BiocGenerics colnames<-
#' @importFrom BiocGenerics Map
#' @importFrom S4Vectors mendoapply
setMethod("dimnames<-", "MultimodalExperiment", function(x, value) {
    .experiments <-
        experiments(x)

    valueOne <-
        value[[1L]]

    valueTwo <-
        value[[2L]]

    if (length(valueOne) != length(.experiments)) {
        stop("length(value[[1L]]) != length(experiments(x))", call. = FALSE)
    }

    if (length(valueTwo) != length(.experiments)) {
        stop("length(value[[2L]]) != length(experiments(x))", call. = FALSE)
    }

    valueNamesOne <-
        names(valueOne)

    valueNamesTwo <-
        names(valueTwo)

    .experimentNames <-
        names(.experiments)

    if (!all(valueNamesOne == .experimentNames)) {
        if (all(valueNamesOne %in% .experimentNames)) {
            valueOne <-
                valueOne[.experimentNames]
        } else {
            stop("names(value[[1L]]) != experimentNames(x)", call. = FALSE)
        }
    }

    if (!all(valueNamesTwo == .experimentNames)) {
        if (all(valueNamesTwo %in% .experimentNames)) {
            valueTwo <-
                valueTwo[.experimentNames]
        } else {
            stop("names(value[[2L]]) != experimentNames(x)", call. = FALSE)
        }
    }

    experimentRowNames <-
        dimnames(.experiments)[[1L]]

    if (any(lengths(valueOne) != lengths(experimentRowNames))) {
        stop("lengths(value[[1L]]) != lengths(dimnames(x)[[1L]])", call. = FALSE)
    }

    experimentColNames <-
        dimnames(.experiments)[[2L]]

    if (any(lengths(valueTwo) != lengths(experimentColNames))) {
        stop("lengths(value[[2L]]) != lengths(colnames(x))", call. = FALSE)
    }

    oldColNames <-
        unlist(experimentColNames) |>
        unname()

    newColNames <-
        unlist(valueTwo) |>
        unname()

    colNamesMap <-
        DataFrame(
            oldColName = oldColNames,
            newColName = newColNames
        ) |>
        unique()

    oldColNameIsduplicated <-
        duplicated(colNamesMap[["oldColName"]])

    if (any(oldColNameIsduplicated)) {
        stop("sample and/or cell mappings are not one-to-one", call. = FALSE)
    }

    .sampleData <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "oldColName") |>
        .leftJoin(colNamesMap)

    .sampleData[["newColName"]] <-
        ifelse(is.na(.sampleData[["newColName"]]), .sampleData[["oldColName"]],
               .sampleData[["newColName"]])

    sampleData(x) <-
        .columnToRowNames(.sampleData, colName = "oldColName") |>
        .columnToRowNames(colName = "newColName")

    .cellData <-
        cellData(x) |>
        .rowNamesToColumn(colName = "oldColName") |>
        .leftJoin(colNamesMap)

    .cellData[["newColName"]] <-
        ifelse(is.na(.cellData[["newColName"]]), .cellData[["oldColName"]],
               .cellData[["newColName"]])

    cellData(x) <-
        .columnToRowNames(.cellData, colName = "oldColName") |>
        .columnToRowNames(colName = "newColName")

    .sampleMap <-
        sampleMap(x)

    colnames(.sampleMap) <-
        c("subject", "oldColName")

    .sampleMap <-
        .leftJoin(.sampleMap, colNamesMap)

    .sampleMap[["sample"]] <-
        ifelse(is.na(.sampleMap[["newColName"]]), .sampleMap[["oldColName"]],
               .sampleMap[["newColName"]])

    sampleMapColNames <-
        c("subject", "sample")

    sampleMap(x) <-
        .sampleMap[, sampleMapColNames]

    .cellMap <-
        cellMap(x)

    colnames(.cellMap) <-
        c("sample", "oldColName")

    .cellMap <-
        .leftJoin(.cellMap, colNamesMap)

    .cellMap[["cell"]] <-
        ifelse(is.na(.cellMap[["newColName"]]), .cellMap[["oldColName"]],
               .cellMap[["newColName"]])

    cellMapColNames <-
        c("sample", "cell")

    cellMap(x) <-
        .cellMap[, cellMapColNames]

    dimnamesList <-
        Map(list, valueOne, valueTwo)

    experiments(x) <-
        mendoapply(`dimnames<-`, .experiments, dimnamesList)

    x
})

#' @rdname name-methods
#'
#' @aliases experimentNames
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("experimentNames", "MultimodalExperiment", function(x) {
    .experiments <-
        experiments(x)

    names(.experiments)
})

#' @rdname name-methods
#'
#' @aliases experimentNames<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics colnames<-
setMethod("experimentNames<-", "MultimodalExperiment", function(x, value) {
    .experiments <-
        experiments(x)

    .experimentNames <-
        names(.experiments)

    experimentNamesMap <-
        DataFrame(
            oldExperimentName = .experimentNames,
            newExperimentName = value
        )

    .experimentData <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "oldExperimentName") |>
        .leftJoin(experimentNamesMap)

    .experimentData[["newExperimentName"]] <-
        ifelse(is.na(.experimentData[["newExperimentName"]]),
               .experimentData[["oldExperimentName"]],
               .experimentData[["newExperimentName"]])

    experimentData(x) <-
        .columnToRowNames(.experimentData, colName = "oldExperimentName") |>
        .columnToRowNames(colName = "newExperimentName")

    .experimentMap <-
        experimentMap(x)

    colnames(.experimentMap) <-
        c("type", "oldExperimentName")

    .experimentMap <-
        .leftJoin(.experimentMap, experimentNamesMap)

    .experimentMap[["experiment"]] <-
        ifelse(is.na(.experimentMap[["newExperimentName"]]),
               .experimentMap[["oldExperimentName"]],
               .experimentMap[["newExperimentName"]])

    experimentMapColNames <-
        c("type", "experiment")

    experimentMap(x) <-
        .experimentMap[, experimentMapColNames]

    .subjectMap <-
        subjectMap(x)

    colnames(.subjectMap) <-
        c("oldExperimentName", "subject")

    .subjectMap <-
        .leftJoin(.subjectMap, experimentNamesMap)

    .subjectMap[["experiment"]] <-
        ifelse(is.na(.subjectMap[["newExperimentName"]]),
               .subjectMap[["oldExperimentName"]],
               .subjectMap[["newExperimentName"]])

    subjectMapColNames <-
        c("experiment", "subject")

    subjectMap(x) <-
        .subjectMap[, subjectMapColNames]

    names(.experiments) <-
        value

    experiments(x) <-
        .experiments

    x
})
