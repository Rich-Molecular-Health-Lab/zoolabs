# palettes.R
# Utility functions for managing color palettes across plots and pedigrees

#' Generate fill color mapping for pedigree plots
#'
#' @param pedigree A pedigree object created by `pedtools::ped()`
#' @param studbook A tibble of studbook data
#' @return A named list mapping individuals to colors by sex and status
#' @export
#'
#' @importFrom dplyr intersect
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @importFrom pedtools females
#' @importFrom pedtools males
#' @importFrom purrr list_assign
set_ped_fills <- function(pedigree, studbook) {
  female   <- intersect(living(studbook), females(pedigree))
  male     <- intersect(living(studbook), males(pedigree))
  undet    <- setdiff(living(studbook), union(females(pedigree), males(pedigree)))

  fills        <- list(female, male, undet)
  names(fills) <- keep_at(colors, c("f", "m", "u"))

  female.d <- intersect(deceased(studbook), females(pedigree))
  male.d   <- intersect(deceased(studbook), males(pedigree))
  undet.d  <- setdiff(deceased(studbook), union(females(pedigree), males(pedigree)))

  ped.fills <- list("#D5328870" = female.d, "#3F459B70" = male.d, "#21B14B70" = undet.d)
  fills     <- list_assign(fills, !!!ped.fills)
  return(fills)
}

#' Convert a named list palette into named vector format
#'
#' @param palette A named list of color values
#' @return A named vector suitable for plotly or reactable inputs
#' @export
#'
#' @importFrom purrr keep_at
#' @importFrom stats setNames
set_plotly_pal <- function(palette) {
  col.pal <- keep_at(palette, c("f", "m", "u")) %>% unlist()
  col.pal <- setNames(col.pal, c("F", "M", "Total"))
  return(col.pal)
}

#' Lighten color palette by replacing hex alpha value
#'
#' @param palette A named list or vector of colors
#' @param hex A two-character hex string (e.g., "33")
#' @return Palette with adjusted transparency
#' @export
#'
#' @importFrom purrr map_depth
lighten_palette <- function(palette, hex) {
  if (is.list(palette)) {
    new <- map_depth(palette, 1, \(x) gsub("FF", hex, x))
  } else {
    new <- gsub("FF", hex, palette)
  }
  return(new)
}

#' Lighten color palette for plotly usage
#'
#' @param palette A named vector of colors
#' @param hex A two-character hex code representing transparency
#' @return A named vector of lightened colors
#' @export
lighten_plotly_pal <- function(palette, hex = "33") {
  gsub("FF", hex, palette)
}

#' Define standardized color set for zoo biology plots
#'
#' @return A named list of color hex codes
#' @export
palette_zoolabs <- function() {
  list(
    f    = "#D53288FF",
    m    = "#3F459BFF",
    u    = "#21B14BFF",
    sire = "#3F459B33",
    dam  = "#D5328833",
    emph = "#DC8045FF",
    seq  = "rcartocolor::Sunset",
    div  = "rcartocolor::Temps",
    rand = "khroma::stratigraphy"
  )
}
