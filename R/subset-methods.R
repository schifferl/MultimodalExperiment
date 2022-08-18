#' MultimodalExperiment Subset Methods
#'
#' Extract or replace parts of a [MultimodalExperiment-class] object.
#'
#' @param x a [MultimodalExperiment-class] object
#' @param i a [list][base::list], [List][S4Vectors::List-class],
#' [LogicalList][IRanges::AtomicList], [IntegerList][IRanges::AtomicList], or
#' [CharacterList][IRanges::AtomicList] of elements to extract or replace
#' @param j a [list][base::list], [List][S4Vectors::List-class],
#' [LogicalList][IRanges::AtomicList], [IntegerList][IRanges::AtomicList], or
#' [CharacterList][IRanges::AtomicList] of elements to extract or replace
#' @param ... ignored, required by generic
#' @param drop ignored, required by generic
#' @param value a replacement value
#'
#' @returns `[` returns a [MultimodalExperiment-class] object.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' i <-
#'     rownames(ME) |>
#'     endoapply(sample, 4L)
#'
#' j <-
#'     colnames(ME) |>
#'     endoapply(sample, 1L)
#'
#' ME[i, j] <-
#'     0L
#'
#' experiment(ME[i, j], "pbRNAseq")
#'
#' experiment(ME[i, j], "scADTseq")
#'
#' experiment(ME[i, j], "scRNAseq")
#'
#' @name subset-methods
NULL

#' @importFrom methods is
.isList <- function(x) {
    acceptedClasses <-
        c("list", "List", "LogicalList", "IntegerList", "CharacterList")

    logicalTemplate <-
        logical(length = 1L)

    isAcceptedClass <-
        vapply(acceptedClasses, is, logicalTemplate, object = x) |>
        any()

    isAcceptedClass
}

#' @rdname subset-methods
#'
#' @aliases [
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics rownames
#' @importFrom BiocGenerics colnames
#' @importFrom S4Vectors mendoapply
setMethod("[", c("MultimodalExperiment", "ANY", "ANY"), function(x, i, j, ..., drop = FALSE) {
    .experiments <-
        experiments(x)

    if (missing(i)) {
        i <-
            rownames(.experiments)
    } else {
        if (!.isList(i)) {
            stop("i must be a list, List, LogicalList, IntegerList, or CharacterList", call. = FALSE)
        }
    }

    if (missing(j)) {
        j <-
            colnames(.experiments)
    } else {
        if (!.isList(j)) {
            stop("j must be a list, List, LogicalList, IntegerList, or CharacterList", call. = FALSE)
        }
    }

    experiments(x) <-
        mendoapply(`[`, .experiments, i, j, drop = FALSE)

    x
})

#' @rdname subset-methods
#'
#' @aliases [<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom BiocGenerics rownames
#' @importFrom BiocGenerics colnames
#' @importFrom S4Vectors mendoapply
setMethod("[<-", c("MultimodalExperiment", "ANY", "ANY"), function(x, i, j, value) {
    .experiments <-
        experiments(x)

    if (missing(i)) {
        i <-
            rownames(.experiments)
    } else {
        if (!.isList(i)) {
            stop("i must be a list, List, LogicalList, IntegerList, or CharacterList", call. = FALSE)
        }
    }

    if (missing(j)) {
        j <-
            colnames(.experiments)
    } else {
        if (!.isList(j)) {
            stop("j must be a list, List, LogicalList, IntegerList, or CharacterList", call. = FALSE)
        }
    }

    experiments(x) <-
        mendoapply(`[<-`, .experiments, i, j, value)

    x
})
