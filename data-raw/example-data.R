tempFile <-
    base::tempfile()

utils::download.file("https://bit.ly/3za2mnP", tempFile)

untarDir <-
    base::tempdir()

utils::untar(tempFile, exdir = untarDir)

rowNames <-
    base::list.files(path = untarDir, full.names = TRUE, recursive = TRUE) |>
    stringr::str_subset("features.tsv.gz") |>
    readr::read_tsv(col_names = FALSE, col_types = "-c-", progress = FALSE) |>
    dplyr::pull()

colNames <-
    base::list.files(path = untarDir, full.names = TRUE, recursive = TRUE) |>
    stringr::str_subset("barcodes.tsv.gz") |>
    readr::read_tsv(col_names = FALSE, col_types = "c", progress = FALSE) |>
    dplyr::pull()

gzMatrix <-
    base::list.files(path = untarDir, full.names = TRUE, recursive = TRUE) |>
    stringr::str_subset("matrix.mtx.gz") |>
    base::gzfile(open = "rb")

scMatrix <-
    Matrix::readMM(gzMatrix) |>
    base::as.matrix()

base::close(gzMatrix)

base::dimnames(scMatrix) <-
    base::list(rowNames, colNames)

isRNAseq <-
    base::rownames(scMatrix) |>
    stringr::str_detect("_TotalSeqC", negate = TRUE)

isADTseq <-
    base::rownames(scMatrix) |>
    stringr::str_detect("_TotalSeqC", negate = FALSE)

base::rownames(scMatrix) <-
    base::rownames(scMatrix) |>
    stringr::str_remove("_TotalSeqC")

base::colnames(scMatrix) <-
    base::colnames(scMatrix) |>
    stringr::str_remove("-1")

keepCols <-
    base::colnames(scMatrix) |>
    base::sample(5000L) |>
    base::sort()

keepRows <-
    base::rownames(scMatrix) |>
    magrittr::extract(isADTseq) |>
    stringr::str_subset("^CD[0-9]{1,2}$") |>
    base::sample(8L) |>
    stringr::str_sort(numeric = TRUE)

scADTseq <-
    scMatrix[keepRows, keepCols]

keepRows <-
    base::rownames(scMatrix) |>
    magrittr::extract(isRNAseq) |>
    stringr::str_subset("^[A-Z0-9]{4}$") |>
    base::sample(3000L) |>
    stringr::str_sort(numeric = TRUE)

scRNAseq <-
    scMatrix[keepRows, keepCols]

sampleID <-
    base::rep("SAMPLE-1", 5000L)

pbRNAseq <-
    scuttle::summarizeAssayByGroup(scRNAseq, sampleID) |>
    SummarizedExperiment::assay("sum")

usethis::use_data(pbRNAseq, scRNAseq, scADTseq, overwrite = TRUE)
