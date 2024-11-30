#' Creates the plots from contraceptive use
#'
#' @param dat tibble which contains mCPR observations. Columns: iso, year, cp
#' @param est tibble which contains mCPR estimates. Columns: “Country or area”, iso, Year, Median, U95, L95
#' @param iso_code country iso code
#' @param CI confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)
#' @return ggplot object with data and estimates
#' @examples
#' plot_cp(dat, est, iso_code = 4)

plot_cp <- function(dat, est, iso_code, CI = 95) {
  est_filtered <- est %>%
    filter(iso == iso_code)

  observed_filtered <- dat %>%
    filter(division_numeric_code == iso_code & is_in_union == "Y") %>%
    mutate(
      Year = (start_date + end_date) / 2,
      cp = contraceptive_use_modern * 100
    )

  p <- ggplot(est_filtered, aes(x = Year, y = Median)) +
    geom_line(color = "blue") +
    geom_point(data = observed_filtered, aes(x = Year, y = cp), color = "black", size = 2) +
    labs(
      x = "Time", y = "Modern use (%)",
      title = unique(est_filtered$`Country or area`)
    )

  if (!is.na(CI)) {
    if (CI == 95) {
      p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.2, fill = "blue")
    } else if (CI == 80) {
      p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), alpha = 0.2, fill = "blue")
    }
  }

  return(p)
}
