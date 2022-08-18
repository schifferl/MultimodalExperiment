#' MultimodalExperiment Class Definition
#'
#' MultimodalExperiment is an S4 class that integrates bulk and single-cell
#' experiment data; it is optimally storage-efficient and its methods are
#' exceptionally fast. It effortlessly represents multimodal data of any nature
#' and features normalized experiment, subject, sample, and cell annotations
#' which are related to underlying biological experiments through maps. Its
#' coordination methods are opt-in and employ database-like join operations
#' internally to deliver fast and flexible management of multimodal data.
#'
#' @details The term matrix-like objects refers to [matrix][base::matrix]
#' objects or Bioconductor S4 objects that contain them (
#' [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class],
#' [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment-class],
#' etc.) where rows represent features and columns represent observations.
#'
#' @slot experimentData a [DataFrame][S4Vectors::DataFrame-class] of experiment
#' annotations with experiment indices as rownames
#' @slot subjectData a [DataFrame][S4Vectors::DataFrame-class] of subject
#' annotations with subject indices as rownames
#' @slot sampleData a [DataFrame][S4Vectors::DataFrame-class] of sample
#' annotations with sample indices as rownames
#' @slot cellData a [DataFrame][S4Vectors::DataFrame-class] of cell annotations
#' with cell indices as rownames
#' @slot experimentMap a [DataFrame][S4Vectors::DataFrame-class] of type (bulk
#' or single-cell) to experiment (index) mappings
#' @slot subjectMap a [DataFrame][S4Vectors::DataFrame-class] of experiment
#' (index) to subject (index) mappings
#' @slot sampleMap a [DataFrame][S4Vectors::DataFrame-class] of subject (index)
#' to sample (index) mappings
#' @slot cellMap a [DataFrame][S4Vectors::DataFrame-class] of sample (index) to
#' cell (index) mappings
#' @slot experiments an [ExperimentList] of matrix-like objects
#' @slot metadata a [list][base::list] of metadata objects
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @export
#'
#' @importFrom methods setClass
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom MultiAssayExperiment ExperimentList
#' @importClassesFrom S4Vectors Annotated
setClass("MultimodalExperiment",
         slots = list(
             experimentData = "DataFrame",
             subjectData = "DataFrame",
             sampleData = "DataFrame",
             cellData = "DataFrame",
             experimentMap = "DataFrame",
             subjectMap = "DataFrame",
             sampleMap = "DataFrame",
             cellMap = "DataFrame",
             experiments = "ExperimentList",
             metadata = "list"
         ),
         contains = "Annotated"
)

# slot-generics ################################################################

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentData", function(object) {
    standardGeneric("experimentData")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentData<-", function(object, value) {
    standardGeneric("experimentData<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("subjectData", function(object) {
    standardGeneric("subjectData")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("subjectData<-", function(object, value) {
    standardGeneric("subjectData<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("sampleData", function(object) {
    standardGeneric("sampleData")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("sampleData<-", function(object, value) {
    standardGeneric("sampleData<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("cellData", function(object) {
    standardGeneric("cellData")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("cellData<-", function(object, value) {
    standardGeneric("cellData<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentMap", function(object) {
    standardGeneric("experimentMap")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentMap<-", function(object, value) {
    standardGeneric("experimentMap<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("subjectMap", function(object) {
    standardGeneric("subjectMap")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("subjectMap<-", function(object, value) {
    standardGeneric("subjectMap<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("sampleMap", function(object) {
    standardGeneric("sampleMap")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("sampleMap<-", function(object, value) {
    standardGeneric("sampleMap<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("cellMap", function(object) {
    standardGeneric("cellMap")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("cellMap<-", function(object, value) {
    standardGeneric("cellMap<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experiments", function(object) {
    standardGeneric("experiments")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experiments<-", function(object, value) {
    standardGeneric("experiments<-")
})

# annotation-generics ##########################################################

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("joinAnnotations", function(x) {
    standardGeneric("joinAnnotations")
})

# map-generics #################################################################

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("joinMaps", function(x) {
    standardGeneric("joinMaps")
})

# experiment-generics ##########################################################
#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experiment", function(x, i) {
    standardGeneric("experiment")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experiment<-", function(x, i, value) {
    standardGeneric("experiment<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("bulkExperiments", function(x) {
    standardGeneric("bulkExperiments")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("bulkExperiments<-", function(x, value) {
    standardGeneric("bulkExperiments<-")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("singleCellExperiments", function(x) {
    standardGeneric("singleCellExperiments")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("singleCellExperiments<-", function(x, value) {
    standardGeneric("singleCellExperiments<-")
})

# name-generics ################################################################

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentNames", function(x) {
    standardGeneric("experimentNames")
})

#' @export
#'
#' @importFrom methods setGeneric
setGeneric("experimentNames<-", function(x, value) {
    standardGeneric("experimentNames<-")
})

# coordination-generics ########################################################

setGeneric("propagate", function(x) {
    standardGeneric("propagate")
})

setGeneric("harmonize", function(x) {
    standardGeneric("harmonize")
})
