#' MultimodalExperiment Constructor Function
#'
#' `MultimodalExperiment` constructs a [MultimodalExperiment-class] object.
#'
#' @usage
#' MultimodalExperiment(
#'     experimentData = DataFrame(),
#'     subjectData = DataFrame(),
#'     sampleData = DataFrame(),
#'     cellData = DataFrame(),
#'     experimentMap = DataFrame(
#'         type = character(),
#'         experiment = character()
#'     ),
#'     subjectMap = DataFrame(
#'         experiment = character(),
#'         subject = character()
#'     ),
#'     sampleMap = DataFrame(
#'         subject = character(),
#'         sample = character()
#'     ),
#'     cellMap = DataFrame(
#'         sample = character(),
#'         cell = character()
#'     ),
#'     experiments = ExperimentList(),
#'     metadata = list()
#' )
#'
#' @param experimentData a [DataFrame][S4Vectors::DataFrame-class] of experiment
#' annotations with experiment indices as rownames
#' @param subjectData a [DataFrame][S4Vectors::DataFrame-class] of subject
#' annotations with subject indices as rownames
#' @param sampleData a [DataFrame][S4Vectors::DataFrame-class] of sample
#' annotations with sample indices as rownames
#' @param cellData a [DataFrame][S4Vectors::DataFrame-class] of cell annotations
#' with cell indices as rownames
#' @param experimentMap a [DataFrame][S4Vectors::DataFrame-class] of type (bulk
#' or single-cell) to experiment (index) mappings
#' @param subjectMap a [DataFrame][S4Vectors::DataFrame-class] of experiment
#' (index) to subject (index) mappings
#' @param sampleMap a [DataFrame][S4Vectors::DataFrame-class] of subject (index)
#' to sample (index) mappings
#' @param cellMap a [DataFrame][S4Vectors::DataFrame-class] of sample (index) to
#' cell (index) mappings
#' @param experiments an [ExperimentList] of matrix-like objects
#' @param metadata a [list][base::list] of metadata objects
#'
#' @details The term matrix-like objects refers to [matrix][base::matrix]
#' objects or Bioconductor S4 objects that contain them (
#' [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class],
#' [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment-class],
#' etc.) where rows represent features and columns represent observations.
#'
#' @returns `MultimodalExperiment` returns a [MultimodalExperiment-class]
#' object.
#'
#' @seealso `browseVignettes("MultimodalExperiment")`
#'
#' @examples MultimodalExperiment()
#'
#' @export
#'
#' @importFrom S4Vectors DataFrame
#' @importFrom MultiAssayExperiment ExperimentList
#' @importFrom methods new
MultimodalExperiment <- function(
    experimentData = DataFrame(),
    subjectData = DataFrame(),
    sampleData = DataFrame(),
    cellData = DataFrame(),
    experimentMap = DataFrame(
      type = character(),
      experiment = character()
    ),
    subjectMap = DataFrame(
      experiment = character(),
      subject = character()
    ),
    sampleMap = DataFrame(
      subject = character(),
      sample = character()
    ),
    cellMap = DataFrame(
      sample = character(),
      cell = character()
    ),
    experiments = ExperimentList(),
    metadata = list()
    ) {
    new("MultimodalExperiment",
        experimentData = experimentData,
        subjectData = subjectData,
        sampleData = sampleData,
        cellData = cellData,
        experimentMap = experimentMap,
        subjectMap = subjectMap,
        sampleMap = sampleMap,
        cellMap = cellMap,
        experiments = experiments,
        metadata = metadata)
}
