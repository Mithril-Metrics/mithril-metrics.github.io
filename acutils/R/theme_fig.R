# Theme, palette, captions and figure export --------------------------------------
# Categorical palette: 8 hues in a fixed order, validated for colour-vision deficiency
# (worst adjacent CVD Delta E 9.1 on the light surface). Slots 3-5 (aqua, yellow, pink)
# have < 3:1 contrast on white: label those marks directly or add a table.

.ac_cols <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100",
              magenta = "#e87ba4", green = "#008300", violet = "#6250d6", red = "#e34948")
.ac_surface <- "#fcfcfb"
.ac_blue_ramp <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#2a78d6", "#256abf",
                   "#184f95", "#0d366b")

#' AgEcon Practical Experience colours
#'
#' @param n Number of categorical colours (max 8). Colours are assigned in a fixed
#'   order; a 9th series should become "Other" or a small multiple, not a new hue.
#' @return Character vector of hex colours.
#' @export
ac_palette <- function(n = 8) {
  if (n > length(.ac_cols)) stop("At most 8 categorical colours: group the rest into 'Other' ",
                                 "or use small multiples.", call. = FALSE)
  unname(.ac_cols[seq_len(n)])
}

#' Discrete colour and fill scales with the case-study palette
#'
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#' @export
scale_colour_ac <- function(...) ggplot2::scale_colour_manual(values = ac_palette(8), ...)

#' @rdname scale_colour_ac
#' @export
scale_fill_ac <- function(...) ggplot2::scale_fill_manual(values = ac_palette(8), ...)

#' Diverging (blue-grey-red) and sequential (one-hue blue) continuous scales
#'
#' @param midpoint Neutral value of the diverging scale (e.g. 0 for changes).
#' @param reverse For the diverging scale: `FALSE` puts red at negative values.
#' @param ... Passed to the underlying ggplot2 scale.
#' @export
scale_fill_ac_div <- function(midpoint = 0, reverse = FALSE, ...) {
  lo <- if (reverse) .ac_cols[["blue"]] else .ac_cols[["red"]]
  hi <- if (reverse) .ac_cols[["red"]] else .ac_cols[["blue"]]
  ggplot2::scale_fill_gradient2(low = lo, mid = "#e4e3df", high = hi, midpoint = midpoint, ...)
}

#' @rdname scale_fill_ac_div
#' @export
scale_colour_ac_div <- function(midpoint = 0, reverse = FALSE, ...) {
  lo <- if (reverse) .ac_cols[["blue"]] else .ac_cols[["red"]]
  hi <- if (reverse) .ac_cols[["red"]] else .ac_cols[["blue"]]
  ggplot2::scale_colour_gradient2(low = lo, mid = "#e4e3df", high = hi, midpoint = midpoint, ...)
}

#' @rdname scale_fill_ac_div
#' @export
scale_fill_ac_seq <- function(...) ggplot2::scale_fill_gradientn(colours = .ac_blue_ramp, ...)

#' Clean ggplot2 theme, readable on a phone
#'
#' Large base size, finding-style title aligned with the plot edge, recessive grid,
#' legend on top, left-aligned source caption.
#'
#' @param base_size Base font size in points (14 works for 1600×1000 px exports).
#' @param base_family Font family ("" = device default).
#' @param grid `"y"`, `"x"`, `"xy"` or `"none"`: which major gridlines to keep.
#' @export
theme_ac <- function(base_size = 14, base_family = "", grid = "y") {
  g <- ggplot2::element_line(colour = "#e4e3df", linewidth = 0.35)
  blank <- ggplot2::element_blank()
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25),
                                         colour = "#1c1c1a", margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(colour = "#4a4a46", size = ggplot2::rel(0.95),
                                            margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(colour = "#6b6b66", size = ggplot2::rel(0.7),
                                           hjust = 0, margin = ggplot2::margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(fill = .ac_surface, colour = NA),
      plot.margin = ggplot2::margin(16, 20, 12, 16),
      panel.grid.minor = blank,
      panel.grid.major.x = if (grepl("x", grid)) g else blank,
      panel.grid.major.y = if (grepl("y", grid)) g else blank,
      axis.text = ggplot2::element_text(colour = "#4a4a46"),
      axis.title = ggplot2::element_text(colour = "#4a4a46", size = ggplot2::rel(0.85)),
      axis.ticks = blank,
      legend.position = "top",
      legend.justification = "left",
      legend.title = blank,
      legend.text = ggplot2::element_text(colour = "#1c1c1a"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, colour = "#1c1c1a")
    )
}

#' Build a standard source line for figure captions
#'
#' @param source Dataset name(s); several are joined with "; ".
#' @param accessed Access date (Date or string); `NULL` omits it.
#' @param note Optional extra note (e.g. "Nominal USD." or "Group means, not farms.").
#' @param author Analysis credit.
#' @return A single string.
#' @examples
#' source_caption("USDA ERS International Agricultural Productivity (2026 release)",
#'                accessed = "2026-11-18")
#' @export
source_caption <- function(source, accessed = NULL, note = NULL,
                           author = "Pietro Sala, AgEcon Practical Experience") {
  s <- paste0("Source: ", paste(source, collapse = "; "))
  if (!is.null(accessed)) s <- paste0(s, " (accessed ", format(as.Date(accessed)), ")")
  s <- paste0(s, ".")
  if (!is.null(note)) s <- paste(s, note)
  paste0(s, "\nAnalysis: ", author)
}

#' Save a figure as PNG (1600×1000 px) and SVG
#'
#' @param plot A ggplot object.
#' @param name File name without extension, e.g. `"fig1_hero"`.
#' @param dir Output directory (default `"output"`).
#' @param width_px,height_px PNG size in pixels.
#' @param dpi Resolution; with the default 200 the plot is 8 × 5 inches, which keeps
#'   `theme_ac()` text readable on a phone.
#' @param svg Also write an SVG (uses `svglite` if installed).
#' @return Invisibly, the paths written.
#' @export
save_fig <- function(plot, name, dir = "output", width_px = 1600, height_px = 1000,
                     dpi = 200, svg = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  w <- width_px / dpi
  h <- height_px / dpi
  png <- file.path(dir, paste0(name, ".png"))
  ggplot2::ggsave(png, plot, width = w, height = h, units = "in", dpi = dpi, bg = .ac_surface)
  paths <- png
  if (svg) {
    f <- file.path(dir, paste0(name, ".svg"))
    dev <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg
    ggplot2::ggsave(f, plot, width = w, height = h, units = "in", device = dev, bg = .ac_surface)
    paths <- c(paths, f)
  }
  message("Saved ", paste(basename(paths), collapse = " + "))
  invisible(paths)
}
