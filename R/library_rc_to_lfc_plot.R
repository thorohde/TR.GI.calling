#' @import data.table
#' @export library_rc_to_lfc_plot


library_rc_to_lfc_plot <- \(lib_readcounts,
                            lfc_list,
                            rc_max,
                            lfc_min,
                            lfc_max,
                            rc_binwidth = 50,
                            lfc_binwidth = 0.5) {

  if (missing(rc_max)) {rc_max <- max(lib_readcounts, na.rm = T)}
  if (missing(lfc_max)) {lfc_max <- base::max(base::sapply(lfc_list, base::max, na.rm = T))}
  if (missing(lfc_min)) {lfc_min <- base::min(base::sapply(lfc_list, base::min, na.rm = T))}

  lfc_ths_pos <- base::seq(0, lfc_max, lfc_binwidth)
  lfc_ths_neg <- base::seq(lfc_min, -lfc_binwidth, lfc_binwidth)

  rc_ths_low <- c(0, base::seq(rc_binwidth, rc_max, by = rc_binwidth))
  rc_ths_high <- c(base::seq(rc_binwidth, rc_max, by = rc_binwidth), 10*rc_max)

  plot_array <- list(base::as.character(c(lfc_ths_neg, lfc_ths_pos)),
                     base::as.character(c(0, base::seq(rc_binwidth, rc_max, by = rc_binwidth))))

  plot_array <- base::array(data = NA, dim = base::sapply(plot_array, base::length), dimnames = plot_array)

  .d <- data.table(lib_rc = lib_readcounts,
                   lfc_min = base::Reduce(base::pmin, lfc_list),
                   lfc_max = base::Reduce(base::pmax, lfc_list))

  for (i in base::seq_along(1:base::length(rc_ths_low))) {
    .d_in_bin <- .d[!is.na(get("lib_rc")) & get("lib_rc") %between% c(rc_ths_low[i], rc_ths_high[i])]
    if (.d_in_bin[, .N] == 0) {
      plot_array[base::as.character(c(lfc_ths_neg, lfc_ths_pos)),i] <- 0
    } else {
      plot_array[as.character(lfc_ths_pos),i] <- base::sapply(lfc_ths_pos, \(.x) .d_in_bin[, sum(lfc_max > .x, na.rm = T) / .N], simplify = T)
      plot_array[as.character(lfc_ths_neg),i] <- base::sapply(lfc_ths_neg, \(.x) .d_in_bin[, sum(lfc_min < .x, na.rm = T) / .N], simplify = T)
    }}
  return(plot_array)
}
