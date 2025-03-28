theme_set(theme_classic())
thematic_rmd()
thematic_on(accent = "#8785B2FF", fg = "black")

opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE,
               comment = "")

palette <- c("#D53288FF", "#DC8045FF", "#21B14BFF", "#008AC2FF", "#3F459BFF")
colors  <- list(
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

col.pal <- keep_at(colors, c("f", "m", "u")) %>% unlist()
col.pal <- setNames(col.pal, c("F", "M", "U"))
fill.pal   <- gsub("FF", "33", col.pal)

deceased.col <- function(color) {
  gsub("FF", "33", color)
}

font_add_google("Noto Sans Symbols", family = "NotoSym")
showtext_auto()

