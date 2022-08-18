#' MultimodalExperiment Slot Methods
#'
#' Extract or replace slots of a [MultimodalExperiment-class] object.
#'
#' @param object a [MultimodalExperiment-class] object
#' @param value a replacement value
#'
#' @returns Extract methods return the value of the slot.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @example examples/example-code.R
#'
#' @examples
#' experimentData(ME)
#'
#' subjectData(ME)
#'
#' sampleData(ME)
#'
#' cellData(ME)
#'
#' experimentMap(ME)
#'
#' subjectMap(ME)
#'
#' sampleMap(ME)
#'
#' cellMap(ME)
#'
#' experiments(ME)
#'
#' @name slot-methods
NULL

#' @rdname slot-methods
#'
#' @aliases experimentData
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("experimentData", "MultimodalExperiment", function(object) {
    slot(object, "experimentData")
})

#' @rdname slot-methods
#'
#' @aliases experimentData<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("experimentData<-", "MultimodalExperiment", function(object, value) {
    slot(object, "experimentData", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases subjectData
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("subjectData", "MultimodalExperiment", function(object) {
    slot(object, "subjectData")
})

#' @rdname slot-methods
#'
#' @aliases subjectData<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("subjectData<-", "MultimodalExperiment", function(object, value) {
    slot(object, "subjectData", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases sampleData
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("sampleData", "MultimodalExperiment", function(object) {
    slot(object, "sampleData")
})

#' @rdname slot-methods
#'
#' @aliases sampleData<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("sampleData<-", "MultimodalExperiment", function(object, value) {
    slot(object, "sampleData", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases cellData
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("cellData", "MultimodalExperiment", function(object) {
    slot(object, "cellData")
})

#' @rdname slot-methods
#'
#' @aliases cellData<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("cellData<-", "MultimodalExperiment", function(object, value) {
    slot(object, "cellData", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases experimentMap
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("experimentMap", "MultimodalExperiment", function(object) {
    slot(object, "experimentMap")
})

#' @rdname slot-methods
#'
#' @aliases experimentMap<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("experimentMap<-", "MultimodalExperiment", function(object, value) {
    slot(object, "experimentMap", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases subjectMap
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("subjectMap", "MultimodalExperiment", function(object) {
    slot(object, "subjectMap")
})

#' @rdname slot-methods
#'
#' @aliases subjectMap<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("subjectMap<-", "MultimodalExperiment", function(object, value) {
    slot(object, "subjectMap", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases sampleMap
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("sampleMap", "MultimodalExperiment", function(object) {
    slot(object, "sampleMap")
})

#' @rdname slot-methods
#'
#' @aliases sampleMap<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("sampleMap<-", "MultimodalExperiment", function(object, value) {
    slot(object, "sampleMap", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases cellMap
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("cellMap", "MultimodalExperiment", function(object) {
    slot(object, "cellMap")
})

#' @rdname slot-methods
#'
#' @aliases cellMap<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("cellMap<-", "MultimodalExperiment", function(object, value) {
    slot(object, "cellMap", check = TRUE) <-
        value

    object
})

#' @rdname slot-methods
#'
#' @aliases experiments
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot
setMethod("experiments", "MultimodalExperiment", function(object) {
    slot(object, "experiments")
})

#' @rdname slot-methods
#'
#' @aliases experiments<-
#'
#' @export
#'
#' @importFrom methods setMethod
#' @importFrom methods slot<-
setMethod("experiments<-", "MultimodalExperiment", function(object, value) {
    slot(object, "experiments", check = TRUE) <-
        value

    object
})
