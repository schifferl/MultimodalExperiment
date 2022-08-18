#' MultimodalExperiment Map Methods
#'
#' `joinMaps` joins all maps into an unnormalized
#' [DataFrame][S4Vectors::DataFrame-class] object.
#'
#' @param x a [MultimodalExperiment-class] object
#'
#' @returns `joinMaps` returns a [DataFrame][S4Vectors::DataFrame-class] object.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' joinMaps(ME)
#'
#' @name map-methods
NULL

#' @rdname map-methods
#'
#' @aliases joinMaps
#'
#' @export
#'
#' @importFrom methods setMethod
setMethod("joinMaps", "MultimodalExperiment", function(x) {
    bulkExperimentMap <-
        .bulkExperimentMap(x)

    .sampleMap <-
        sampleMap(x)

    bulkExperimentMap <-
        .outerJoin(bulkExperimentMap, .sampleMap) |>
        .innerJoin(bulkExperimentMap)

    singleCellExperimentMap <-
        .singleCellExperimentMap(x)

    .cellMap <-
        cellMap(x)

    singleCellExperimentMap <-
        .outerJoin(singleCellExperimentMap, .cellMap) |>
        .outerJoin(.sampleMap) |>
        .innerJoin(singleCellExperimentMap)

    joinedMaps <-
        .outerJoin(bulkExperimentMap, singleCellExperimentMap)

    joinedMaps <-
        .outerJoin(.sampleMap, joinedMaps)

    .experimentMap <-
        experimentMap(x)

    .outerJoin(.experimentMap, joinedMaps)
})
