#' MultimodalExperiment Example Data
#'
#' Human peripheral blood mononuclear cells (PBMCs) from a single healthy donor
#' were profiled by cellular indexing of transcriptomes and epitopes by
#' sequencing (CITE-seq) to generate single-cell antibody-derived tag sequencing
#' (`scADTseq`) and single-cell RNA sequencing (`scRNAseq`) data simultaneously;
#' the `scRNAseq` data was summed into pseudo-bulk RNA sequencing (`pbRNAseq`)
#' data. The dimensions of resulting matrices were reduced to conserve storage
#' because these data are only used to demonstrate the functionality of the
#' [MultimodalExperiment-class] class.
#'
#' @examples
#' pbRNAseq[1:4, 1:1, drop = FALSE]
#'
#' scADTseq[1:4, 1:4, drop = FALSE]
#'
#' scRNAseq[1:4, 1:4, drop = FALSE]
#'
#' @source *PBMCs of a Healthy Donor - 5' Gene Expression with a Panel of
#' TotalSeq™-C Antibodies*, Single Cell Immune Profiling Dataset by Cell Ranger
#' 3.0.0, 10x Genomics, (2018, November 19).
#'
#' @name example-data
NULL

#' @rdname example-data
"pbRNAseq"

#' @rdname example-data
"scADTseq"

#' @rdname example-data
"scRNAseq"
