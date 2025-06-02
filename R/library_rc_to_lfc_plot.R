#' @import data.table
#'
#' @export library_rc_to_lfc_plot

lib_rc_to_lfc_plot <- \(lib_readcounts,
                        lfc_list,
                        rc_max = 1000,
                        lfc_min = 5, lfc_max = 10,
                        rc_binwidth = 50,
                        lfc_binwidth = 0.5) {

  lfc_ths_pos <- base::seq(0, lfc_max, lfc_binwidth)
  lfc_ths_neg <- base::seq(lfc_min, -lfc_binwidth, lfc_binwidth)

  rc_ths_low <- c(0, base::seq(rc_binwidth, rc_max, by = rc_binwidth))
  rc_ths_high <- c(base::seq(rc_binwidth, rc_max, by = rc_binwidth), 10*rc_max)

  plot_array <- empty_array(base::list(base::as.character(c(lfc_ths_neg, lfc_ths_pos)),
                                       base::as.character(c(0, base::seq(rc_binwidth, rc_max, by = rc_binwidth)))))

  .d <- data.table(lib_rc = lib_readcounts,
                   lfc_min = base::do.call(\(...) base::pmin(..., na.rm = T), lfc_list),
                   lfc_max = base::do.call(\(...) base::pmax(..., na.rm = T), lfc_list))

  for (i in base::seq_along(1:base::length(rc_ths_low))) {
    .d_in_bin <- .d[!is.na(lib_rc) & lib_rc %between% c(rc_ths_low[i], rc_ths_high[i])]
    if (.d_in_bin[, .N] == 0) {
      plot_array[base::as.character(c(lfc_ths_neg, lfc_ths_pos)),i] <- 0
    } else {
      plot_array[as.character(lfc_ths_pos),i] <- base::sapply(lfc_ths_pos, \(.) {.d_in_bin[, sum(lfc_max > ., na.rm = T) / .N]})
      plot_array[as.character(lfc_ths_neg),i] <- base::sapply(lfc_ths_neg, \(.) {.d_in_bin[, sum(lfc_min < ., na.rm = T) / .N]})
    }}
  return(plot_array)
}
