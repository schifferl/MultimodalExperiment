#' MultimodalExperiment Coordination Methods
#'
#' Propagate or harmonize indices of a [MultimodalExperiment-class] object.
#'
#' @param x a [MultimodalExperiment-class] object
#'
#' @details
#' `propagate` inserts experiment, subject, sample, and cell indices into all
#' relevant tables by taking their union and adding missing indices.
#'
#' `harmonize` deletes experiment, subject, sample, and cell indices from all
#' relevant tables by taking their intersection and removing extraneous indices.
#'
#' @returns
#' `propagate` returns a [MultimodalExperiment-class] object.
#'
#' `harmonize` returns a [MultimodalExperiment-class] object.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' isMonocyte <-
#'     cellData(ME)[["cellType"]] %in% "Monocyte"
#'
#' cellData(ME) <-
#'     cellData(ME)[isMonocyte, , drop = FALSE]
#'
#' harmonize(ME)
#'
#' @name coordination-methods
NULL

#' @rdname coordination-methods
#'
#' @aliases propagate
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics union
#' @importFrom S4Vectors complete.cases
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics unique
#' @importFrom BiocGenerics intersect
setMethod("propagate", "MultimodalExperiment", function(x) {
    .experimentData <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "experiment")

    .experimentMap <-
        experimentMap(x)

    .subjectMap <-
        subjectMap(x)

    experimentIndicesList <-
        list(
            .experimentData[["experiment"]],
            .experimentMap[["experiment"]],
            .subjectMap[["experiment"]]
        )

    experimentIndices <-
        Reduce(union, experimentIndicesList)

    .experimentNames <-
        experimentNames(x)

    experimentIndices <-
        union(experimentIndices, .experimentNames)

    experimentIndices <-
        experimentIndices[complete.cases(experimentIndices)]

    experimentIndicesMap <-
        DataFrame(experiment = experimentIndices)

    experimentData(x) <-
        .hemiJoin(.experimentData, experimentIndicesMap) |>
        .columnToRowNames(colName = "experiment")

    experimentMap(x) <-
        .hemiJoin(.experimentMap, experimentIndicesMap)

    bulkExperimentMap <-
        .bulkExperimentMap(x)

    .sampleMap <-
        sampleMap(x)

    bulkExperimentMap <-
        .outerJoin(bulkExperimentMap, .sampleMap) |>
        .innerJoin(bulkExperimentMap)

    .subjectMap <-
        .hemiJoin(.subjectMap, bulkExperimentMap) |>
        unique()

    singleCellExperimentMap <-
        .singleCellExperimentMap(x)

    .cellMap <-
        cellMap(x)

    singleCellExperimentMap <-
        .outerJoin(singleCellExperimentMap, .cellMap) |>
        .outerJoin(.sampleMap) |>
        .innerJoin(singleCellExperimentMap)

    .subjectMap <-
        .hemiJoin(.subjectMap, singleCellExperimentMap) |>
        unique()

    .subjectMap <-
        .hemiJoin(.subjectMap, experimentIndicesMap)

    .subjectData <-
        subjectData(x) |>
        .rowNamesToColumn(colName = "subject")

    subjectIndicesList <-
        list(
            .subjectData[["subject"]],
            .subjectMap[["subject"]],
            .sampleMap[["subject"]]
        )

    subjectIndices <-
        Reduce(union, subjectIndicesList)

    subjectIndices <-
        subjectIndices[complete.cases(subjectIndices)]

    subjectIndicesMap <-
        DataFrame(subject = subjectIndices)

    subjectData(x) <-
        .hemiJoin(.subjectData, subjectIndicesMap) |>
        .columnToRowNames(colName = "subject")

    subjectMap(x) <-
        .hemiJoin(.subjectMap, subjectIndicesMap)

    .sampleMap <-
        .hemiJoin(.sampleMap, subjectIndicesMap)

    .sampleData <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "sample")

    sampleIndicesList <-
        list(
            .sampleData[["sample"]],
            .sampleMap[["sample"]],
            .cellMap[["sample"]]
        )

    sampleIndices <-
        Reduce(union, sampleIndicesList)

    sampleIndices <-
        union(sampleIndices, bulkExperimentMap[["sample"]])

    sampleIndices <-
        sampleIndices[complete.cases(sampleIndices)]

    sampleIndicesMap <-
        DataFrame(sample = sampleIndices)

    sampleData(x) <-
        .hemiJoin(.sampleData, sampleIndicesMap) |>
        .columnToRowNames(colName = "sample")

    sampleMap(x) <-
        .hemiJoin(.sampleMap, sampleIndicesMap)

    sampleIndices <-
        intersect(sampleIndices, singleCellExperimentMap[["sample"]])

    sampleIndicesMap <-
        DataFrame(sample = sampleIndices)

    .cellMap <-
        .hemiJoin(.cellMap, sampleIndicesMap)

    .cellData <-
        cellData(x) |>
        .rowNamesToColumn(colName = "cell")

    cellIndicesList <-
        list(
            .cellData[["cell"]],
            .cellMap[["cell"]]
        )

    cellIndices <-
        Reduce(union, cellIndicesList)

    cellIndices <-
        union(cellIndices, singleCellExperimentMap[["cell"]])

    cellIndices <-
        cellIndices[complete.cases(cellIndices)]

    cellIndicesMap <-
        DataFrame(cell = cellIndices)

    cellData(x) <-
        .hemiJoin(.cellData, cellIndicesMap) |>
        .columnToRowNames(colName = "cell")

    cellMap(x) <-
        .hemiJoin(.cellMap, cellIndicesMap)

    x
})

#' @rdname coordination-methods
#'
#' @aliases harmonize
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics Reduce
#' @importFrom BiocGenerics unique
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics rownames
#' @importFrom S4Vectors split
#' @importFrom BiocGenerics lapply
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors mendoapply
setMethod("harmonize", "MultimodalExperiment", function(x) {
    bulkExperimentMap <-
        .bulkExperimentMap(x)

    .experimentMap <-
        experimentMap(x)

    .subjectMap <-
        subjectMap(x)

    .sampleMap <-
        sampleMap(x)

    bulkMapList <-
        list(
            bulkExperimentMap,
            .experimentMap,
            .subjectMap,
            .sampleMap
        )

    .experimentData <-
        experimentData(x) |>
        .rowNamesToColumn(colName = "experiment")

    .subjectData <-
        subjectData(x) |>
        .rowNamesToColumn(colName = "subject")

    .sampleData <-
        sampleData(x) |>
        .rowNamesToColumn(colName = "sample")

    bulkMap <-
        Reduce(.innerJoin, bulkMapList) |>
        .semiJoin(.experimentData) |>
        .semiJoin(.subjectData) |>
        .semiJoin(.sampleData)

    singleCellExperimentMap <-
        .singleCellExperimentMap(x)

    .cellMap <-
        cellMap(x)

    singleCellMapList <-
        list(
            singleCellExperimentMap,
            .experimentMap,
            .subjectMap,
            .sampleMap,
            .cellMap
        )

    .cellData <-
        cellData(x) |>
        .rowNamesToColumn(colName = "cell")

    singleCellMap <-
        Reduce(.innerJoin, singleCellMapList) |>
        .semiJoin(.experimentData) |>
        .semiJoin(.subjectData) |>
        .semiJoin(.sampleData) |>
        .semiJoin(.cellData)

    joinedMaps <-
        .outerJoin(bulkMap, singleCellMap)

    experimentIndices <-
        unique(joinedMaps[["experiment"]])

    experimentIndicesMap <-
        DataFrame(experiment = experimentIndices)

    experimentData(x) <-
        .semiJoin(.experimentData, experimentIndicesMap) |>
        .columnToRowNames(colName = "experiment")

    experimentMap(x) <-
        .semiJoin(.experimentMap, experimentIndicesMap)

    experiments(x) <-
        experiments(x)[experimentIndices]

    subjectIndices <-
        unique(joinedMaps[["subject"]])

    subjectIndicesMap <-
        DataFrame(subject = subjectIndices)

    subjectData(x) <-
        .semiJoin(.subjectData, subjectIndicesMap) |>
        .columnToRowNames(colName = "subject")

    subjectMap(x) <-
        .semiJoin(.subjectMap, subjectIndicesMap)

    sampleIndices <-
        unique(joinedMaps[["sample"]])

    sampleIndicesMap <-
        DataFrame(sample = sampleIndices)

    sampleData(x) <-
        .semiJoin(.sampleData, sampleIndicesMap) |>
        .columnToRowNames(colName = "sample")

    sampleMap(x) <-
        .semiJoin(.sampleMap, sampleIndicesMap)

    bulkExperimentIndices <-
        unique(bulkMap[["experiment"]])

    .bulkExperiments <-
        bulkExperiments(x)

    .bulkExperiments <-
        .bulkExperiments[bulkExperimentIndices]

    bulkExperimentRowNames <-
        rownames(.bulkExperiments)

    bulkExperimentRowNames <-
        bulkExperimentRowNames[bulkExperimentIndices]

    bulkExperimentColNames <-
        split(bulkMap, bulkMap[["experiment"]]) |>
        lapply(`[[`, "sample") |>
        CharacterList()

    bulkExperimentColNames <-
        bulkExperimentColNames[bulkExperimentIndices]

    bulkExperiments(x) <-
        mendoapply(`[`, .bulkExperiments, bulkExperimentRowNames, bulkExperimentColNames, drop = FALSE)

    cellIndices <-
        unique(joinedMaps[["cell"]])

    cellIndicesMap <-
        DataFrame(cell = cellIndices)

    cellData(x) <-
        .semiJoin(.cellData, cellIndicesMap) |>
        .columnToRowNames(colName = "cell")

    cellMap(x) <-
        .semiJoin(.cellMap, cellIndicesMap)

    singleCellExperimentIndices <-
        unique(singleCellMap[["experiment"]])

    .singleCellExperiments <-
        singleCellExperiments(x)

    .singleCellExperiments <-
        .singleCellExperiments[singleCellExperimentIndices]

    singleCellExperimentRowNames <-
        rownames(.singleCellExperiments)

    singleCellExperimentRowNames <-
        singleCellExperimentRowNames[singleCellExperimentIndices]

    singleCellExperimentColNames <-
        split(singleCellMap, singleCellMap[["experiment"]]) |>
        lapply(`[[`, "cell") |>
        CharacterList()

    singleCellExperimentColNames <-
        singleCellExperimentColNames[singleCellExperimentIndices]

    singleCellExperiments(x) <-
        mendoapply(`[`, .singleCellExperiments, singleCellExperimentRowNames, singleCellExperimentColNames, drop = FALSE)

    x
})
