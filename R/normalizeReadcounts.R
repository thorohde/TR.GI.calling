#' @export normalizeReadcounts

## ---- normalizeReadcounts function ----

normalizeReadcounts <- \(readcounts, cf1 = 100, cf2 = 1) {
  # replaced 1e6 with 100 x length(readcounts), cf1 = 1e6, cf2 = 0.5
  x <- log2((readcounts / sum(readcounts, na.rm = T)) * cf1 * length(readcounts) + cf2) #NA will stay NA
  if (!all(is.na(x))) { #not run if replicate is missing (= all NA)
    x <- x - min(x, na.rm = T)} #smallest value is 0 regardless of cf2
  x}
