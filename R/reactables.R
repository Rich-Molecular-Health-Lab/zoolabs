# reactables.R
# Reactable column definitions and summary table generators

#' Reactable Column Defs
#'
#' These helper functions generate formatted column definitions for `reactable`
#' tables to summarize studbook, pedigree, and demographic data.
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom tippy tippy
ID <- function(df) {
  colDef(
    header = tippy("ID", tooltip = "Studbook ID color-coded by sex (maroon = F, blue = M, green = Undetermined)"),
    maxWidth = 70,
    cell = color_tiles(
      data       = df,
      color_ref  = "color",
      box_shadow = TRUE,
      bold_text  = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmlwidgets JS
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Status <- function() {
  colDef(
    header = tippy("Status", tooltip = "Alive or Deceased"),
    maxWidth = 100,
    style = JS("
      function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Status') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Status'] === prevRow['Status']) {
            return { visibility: 'hidden' }
          }
        }
      }")
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom scales label_date
#' @importFrom tippy tippy
DateBirth <- function(df) {
  colDef(
    header = tippy("Birthdate", tooltip = "Date of birth (captive-born) or capture (wild-born)"),
    maxWidth = 200,
    cell = color_tiles(
      data                = df,
      colors              = paletteer_d(colors$seq),
      opacity             = 0.4,
      color_by            = "BirthYear",
      brighten_text_color = "black",
      box_shadow          = TRUE,
      number_fmt          = label_date()
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom scales label_date
#' @importFrom tippy tippy
DateDeath <- function(df) {
  colDef(
    header = tippy("Death Date", tooltip = "Date of death (NA for living individuals)"),
    maxWidth = 200,
    cell = color_tiles(
      data                = df,
      colors              = paletteer_d(colors$seq),
      opacity             = 0.4,
      color_by            = "YearLast",
      brighten_text_color = "black",
      box_shadow          = TRUE,
      number_fmt          = label_date()
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div
#' @importFrom htmltools span
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom tippy tippy
LocBirth <- function(df) {
  colDef(
    header = tippy("Born", tooltip = "Location of birth (captive-born) or capture (wild-born)"),
    maxWidth = 100,
    cell = function(value, index) {
      flag <- df$LocBirth_icon[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$LocBirth_name[index]))
      date <- div(style = "display:inline",
                  span(style = "font-weight: 600; font-size:12pt", df$BirthYear[index]),
                  span(style = "font-size:10pt", paste0(" (", df$MonthBirth[index], ")")))
      div(style = "display:grid; row-gap:2px", div(style = "display:inline-block", flag, name), date)
    }
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div
#' @importFrom htmltools span
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom tippy tippy
LocLast <- function(df) {
  colDef(
    header = tippy("Last", tooltip = "Current institution (Alive) or institution and date of death (Deceased)"),
    maxWidth = 100,
    cell = function(value, index) {
      flag <- df$LocLast_icon[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$LocLast_name[index]))
      date <- div(style = "font-weight: 600; font-size:12pt", df$YearDeath[index])
      div(style = "display:grid; row-gap:2px", div(style = "display:inline-block", flag, name), date)
    }
  )
}

#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div
#' @importFrom htmltools span
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Current_Location <- function(df) {
  colDef(
    header = tippy("Current Location", tooltip = "Current institution"),
    maxWidth = 70,
    cell = function(value, index) {
      flag <- df$LocLast_icon[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$LocLast_name[index]))
      div(style = "display:inline-block", flag, name)
    }
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom scales label_date
#' @importFrom tippy tippy
AgeLast <- function(df) {
  colDef(
    header = tippy("Age", tooltip = "Now (Alive) or at time of death (Deceased)"),
    maxWidth = 50,
    align = "center",
    cell = pill_buttons(
      data       = df,
      colors     = paletteer_d(colors$seq),
      opacity    = 0.4,
      brighten_text_color = "black",
      box_shadow = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#' @param name The name of the column to be shown in the table
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr bubble_grid
#' @importFrom tippy tippy
bubble_count <- function(df, name) {
  colDef(
    name = name,
    maxWidth = 250,
    align = "center",
    cell = bubble_grid(
      data = df,
      text_color = "#ffffff",
      bold_text = TRUE,
      brighten_text = TRUE,
      colors = paletteer_d(colors$seq)
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom tippy tippy
Sire <- function(df) {
  colDef(
    header = tippy("Father", tooltip = "Studbook ID of Sire (0 if wildborn or unknown)"),
    maxWidth = 70,
    cell = pill_buttons(
      data       = df,
      colors     = colors$sire,
      opacity    = 0.6,
      brighten_text_color = "black",
      box_shadow = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom tippy tippy
Dam <- function(df) {
  colDef(
    header = tippy("Mother", tooltip = "Studbook ID of Dam (0 if wildborn or unknown)"),
    maxWidth = 70,
    cell = pill_buttons(
      data       = df,
      colors     = colors$dam,
      opacity    = 0.6,
      brighten_text_color = "black",
      box_shadow = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr data_bars
#' @importFrom scales label_percent
#' @importFrom tippy tippy
Rel_Contribution <- function(df) {
  colDef(
    header = tippy("Relative Contribution", tooltip = "Individual's contribution to living population relative to total founder representation"),
    maxWidth = 200,
    cell = data_bars(
      data          = df,
      text_position = "outside-base",
      fill_color    = paletteer_d(colors$seq),
      number_fmt    = label_percent(),
      background    = "white",
      box_shadow    = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom paletteer paletteer_d
#' @importFrom reactable colDef
#' @importFrom reactablefmtr data_bars
#' @importFrom scales label_number
#' @importFrom tippy tippy
inbred <- function(df) {
  colDef(
    header = tippy("F", tooltip = "Inbreeding coefficient: probability two alleles are identical by descent"),
    align = "center",
    maxWidth = 300,
    cell = data_bars(
      data          = df,
      text_position = "inside-end",
      box_shadow    = TRUE,
      fill_color    = paletteer_d(colors$div),
      background    = "#ffffff00",
      text_color    = "#ffffff",
      bold_text     = TRUE,
      brighten_text = TRUE,
      number_fmt    = label_number(accuracy = 0.001)
    )
  )
}
