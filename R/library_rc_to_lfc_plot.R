library_rc_to_lfc_plot <- \(.library_rc, .lfc, .lfc_extreme = 8, .stepsize = 0.5) {

  .th_seq = c(0, seq(20, 400, by = 10))
  .th_seq_up = c(seq(20, 400, by = 10), 10000)

  x <- data.table(lib = .library_rc, lfc_min = apply(.lfc, 1, min, na.rm = T), lfc_max = apply(.lfc, 1, max, na.rm = T))

  qc <- list(pos = copy(Reduce(bind_rows, lapply(seq(.stepsize, .lfc_extreme, .stepsize), \(.) data.table(th_seq = .th_seq, th_seq_up = .th_seq_up, th_lfc = .)))),
             neg = copy(Reduce(bind_rows, lapply(seq(-.lfc_extreme, -.stepsize, .stepsize), \(.) data.table(th_seq = .th_seq, th_seq_up = .th_seq_up, th_lfc = .)))))

  qc$pos[, count := mapply(\(.th1, .th2, .th3) {x[lib > .th1 & lib < .th2 & lfc_max > .th3, .N] / x[lib > .th1 & lib < .th2, .N]}, th_seq, th_seq_up, th_lfc)]
  qc$neg[, count := mapply(\(.th1, .th2, .th3) {x[lib > .th1 & lib < .th2 & lfc_min < .th3, .N] / x[lib > .th1 & lib < .th2, .N]}, th_seq, th_seq_up, th_lfc)]

  Reduce(dplyr::bind_rows, qc)[, count := round(count, 2)]
}


# if lfc are given as list:

library_rc_to_lfc_plot <- \(.library_rc, lfc_list, .lfc_extreme = 8, .stepsize = 0.5) {

  .th_seq = c(0, seq(20, 400, by = 10))
  .th_seq_up = c(seq(20, 400, by = 10), 10000)

  x <- data.table(lib = .library_rc,
                  lfc_min = do.call(\(...) pmin(..., na.rm = TRUE), lfc_list),
                  lfc_max = do.call(\(...) pmax(..., na.rm = TRUE), lfc_list))

  qc <- list(pos = copy(Reduce(bind_rows, lapply(seq(.stepsize, .lfc_extreme, .stepsize), \(.) data.table(th_seq = .th_seq, th_seq_up = .th_seq_up, th_lfc = .)))),
             neg = copy(Reduce(bind_rows, lapply(seq(-.lfc_extreme, -.stepsize, .stepsize), \(.) data.table(th_seq = .th_seq, th_seq_up = .th_seq_up, th_lfc = .)))))

  qc$pos[, count := mapply(\(.th1, .th2, .th3) {x[lib > .th1 & lib < .th2 & lfc_max > .th3, .N] / x[lib > .th1 & lib < .th2, .N]}, th_seq, th_seq_up, th_lfc)]
  qc$neg[, count := mapply(\(.th1, .th2, .th3) {x[lib > .th1 & lib < .th2 & lfc_min < .th3, .N] / x[lib > .th1 & lib < .th2, .N]}, th_seq, th_seq_up, th_lfc)]

  Reduce(dplyr::bind_rows, qc)[, count := round(count, 2)]
}


