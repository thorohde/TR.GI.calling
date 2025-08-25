#' @export normalize_and_compute_LFC


normalize_and_compute_LFC <- function(data_list,
                                      library_col = "Library",
                                      screen_cols,
                                      metadata_cols,
                                      norm_cf1 = 100, norm_cf2 = 1,
                                      lib_rc_threshold = NA,
                                      verbose = F) {


  if (verbose) {message(paste0("normalizing sequencing depth to ", norm_cf1, "x coverage, fudge factor ", norm_cf2, "."))}

  output <- list(metadata = data_list[metadata_cols],
                 lib_raw = data_list[[library_col]],
                 lib_norm = normalizeReadcounts(data_list[[library_col]], cf1 = norm_cf1, cf2 = norm_cf2),
                 raw = as.list(data_list[screen_cols]))

  output$norm <- lapply(output$raw, \(.s) {normalizeReadcounts(.s, cf1 = norm_cf1, cf2 = norm_cf2)})
  names(output$norm) <- gsub("raw", "norm", names(output$norm))

  output$lfc <- lapply(output$norm, \(.s) {.s - output$lib_norm})
  names(output$lfc) <- gsub("norm", "lfc", names(output$norm))

  return(output)}
