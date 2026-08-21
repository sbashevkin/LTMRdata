#' Generate comparison report between two versions of a dataset
#'
#' Previously published data should remain the same in the newly downloaded data.
#' If this is not true, there can be structural changes to the database that
#' warrants exploring. This flags these differences using a hash function.
#'
#' @param new_data Newest data to update with
#' @param old_data Current or previous data to be updated
#' @param id_cols Data columns to identify individual rows
#'
#' @returns A data frame of rows from `new_data` that does not match `old_data`.
#'
#' @examples
#' \dontrun{
#' create_comparison_report(Salvage, LTMRdata::Salvage, id_cols = c("SampleID", "Taxa", "Length", "Count"))
#' }

create_comparison_report <- function(new_data, old_data,
                                     id_cols = c("SampleID", "Taxa", "Length", "Count")) {

  # Compute row hash and composite record ID; drop allData immediately after hashing
  prepareData <- function(data) {
    data %>%
      tidyr::unite("allData", dplyr::everything(), remove = FALSE, sep = "|") %>%
      dplyr::mutate(rowHash = purrr::map_chr(allData, rlang::hash)) %>%
      dplyr::select(-allData) %>%                                    # <- drop early
      tidyr::unite("recordId", dplyr::all_of(id_cols), remove = FALSE, sep = "-")
  }

  processedNew <- prepareData(new_data)
  processedOld <- prepareData(old_data)

  # 1. Identify New Records
  newIds <- setdiff(processedNew$recordId, processedOld$recordId)
  cat(length(newIds), "new datapoints to add.\n")

  # 2. Identify Changed Records — pull a vector directly, skip the intermediary data frame
  changedIds <- dplyr::inner_join(
    dplyr::select(processedNew, recordId, rowHash),
    dplyr::select(processedOld, recordId, rowHash),
    by = "recordId", suffix = c("_New", "_Old")
  ) %>%
    dplyr::filter(rowHash_New != rowHash_Old) %>%
    dplyr::pull(recordId)

  if (length(changedIds) == 0) {
    cat("No changed records found.\n")
    return(NULL)
  } else {
    cat(length(changedIds), "changed records found.\n")
  }

  # 3. Comparison Report
  # ID columns are structurally equal by construction — skip them, compare data cols only
  dataCols <- setdiff(names(new_data), id_cols)

  joined <- dplyr::inner_join(
    processedNew %>% dplyr::filter(recordId %in% changedIds) %>% dplyr::select(-rowHash),
    processedOld %>% dplyr::filter(recordId %in% changedIds) %>% dplyr::select(-rowHash),
    by = "recordId", suffix = c("_New", "_Old")
  )

  # Vectorised column-wise diff — replaces pivot_longer + group_by + summarise overhead
  diffCols <- purrr::map_dfc(dataCols, \(col) {
    new_v <- as.character(joined[[paste0(col, "_New")]])
    old_v <- as.character(joined[[paste0(col, "_Old")]])
    changed <- is.na(new_v) != is.na(old_v) |
      (!is.na(new_v) & !is.na(old_v) & new_v != old_v)
    dplyr::tibble(!!col := dplyr::if_else(changed, paste0("WAS: ", old_v, " | NOW: ", new_v), NA_character_))
  })

  dplyr::bind_cols(dplyr::select(joined, recordId), diffCols) %>%
    dplyr::select(recordId, tidyselect::where(~ !all(is.na(.))))
}


#' Load data from git commits
#'
#' Access a previously committed versions of a dataset (RDA file). Requires
#' files to be git-tracked, generated during the cloning process.
#'
#' @param name Name of dataset
#' @param ref Relative reference to the commit of interest, HEAD~N, where N
#' is the number of commits back from the present
#' @param dir Relative directory of the folder where the dataset is located
#'
#' @returns data.frame
#'
#' @examples
#' \dontrun{
#' loadDataFromGit(Salvage, "HEAD~1")
#' }

loadDataFromGit <- function(name, ref = "HEAD~1", dir = "data") {
  tmpFile <- tempfile(fileext = ".rda")
  gitPath <- paste0(ref, ":", dir, "/", name, ".rda")
  exitCode <- system2("git", c("show", gitPath), stdout = tmpFile, stderr = FALSE)
  if (exitCode != 0) stop("Could not retrieve ", name, " from git ref: ", ref)
  env <- new.env()
  load(tmpFile, envir = env)
  env[[name]]
}
