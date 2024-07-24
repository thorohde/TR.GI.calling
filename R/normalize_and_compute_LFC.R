normalize_and_compute_LFC <- \(data_list, 
                               library_col = "Library", 
                               screen_cols, 
                               metadata_cols, 
                               norm_cf1 = 1e6, norm_cf2 = 0.5, 
                               lib_rc_threshold = NA) {
  
  output <- list(metadata = data_list[metadata_cols], 
                 lib_raw = data_list[[library_col]], 
                 lib_norm = normalizeReadcounts(data_list[[library_col]], cf1 = norm_cf1, cf2 = norm_cf2), 
                 raw = as.list(data_list[screen_cols])
  )
  
  output$norm <- lapply(output$raw, \(.s) {normalizeReadcounts(.s, cf1 = norm_cf1, cf2 = norm_cf2)})
  output$lfc <- lapply(output$norm, \(.s) {.s - output$lib_norm})
  
  if (!is.na(lib_rc_threshold)) {
    .i <- which(output$lib_raw <= lib_rc_threshold)
    output$norm <- lapply(output$norm, \(.s) {.s[.i] <- NA; .s})
    output$lfc <- lapply(output$lfc, \(.s) {.s[.i] <- NA; .s})
    
  }
  return(output)}