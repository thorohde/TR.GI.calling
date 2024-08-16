normalize_and_compute_LFC <- \(data_list,
                               library_col = "Library",
                               screen_cols,
                               metadata_cols,
                               norm_cf1, norm_cf2,
                               lib_rc_threshold = NA) {

  output <- list(metadata = data_list[metadata_cols],
                 lib_raw = data_list[[library_col]],
                 lib_norm = normalizeReadcounts(data_list[[library_col]], cf1 = norm_cf1, cf2 = norm_cf2),
                 raw = as.list(data_list[screen_cols])
  )

  normalizeReadcounts <- \(readcounts, cf1 = 100, cf2 = 1) {
    # replaced cf1 = 1e6 with 100 x length(readcounts), replaced cf2 = 0.5 with 1
    x <- log2((readcounts / sum(readcounts, na.rm = T)) * cf1 * length(readcounts) + cf2) #NA will stay NA
    if (!all(is.na(x))) { #not run if replicate is missing (= all NA)
      x <- x - min(x, na.rm = T)} #smallest value is 0 regardless of cf2
    x}




  output$norm <- lapply(output$raw, \(.s) {normalizeReadcounts(.s, cf1 = norm_cf1, cf2 = norm_cf2)})
  output$lfc <- lapply(output$norm, \(.s) {.s - output$lib_norm})

  if (!is.na(lib_rc_threshold)) {
    .i <- which(output$lib_raw <= lib_rc_threshold)
    output$norm <- lapply(output$norm, \(.s) {.s[.i] <- NA; .s})
    output$lfc <- lapply(output$lfc, \(.s) {.s[.i] <- NA; .s})

  }
  return(output)}
