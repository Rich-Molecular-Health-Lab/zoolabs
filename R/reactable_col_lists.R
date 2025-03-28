# reactable_col_lists.R
# Column list generators for use in reactable tables

#' Column definitions for full studbook summary tables
#'
#' @param df A filtered founder summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
studbook.cols <- function(df) {
  list(
    Status            = Status(),
    ID                = ID(df),
    LocBirth          = LocBirth(df),
    AgeLast           = AgeLast(df),
    LocLast           = LocLast(df),
    Sire              = Sire(df),
    Dam               = Dam(df),
    DateDeath         = colDef(show = FALSE),
    DateBirth         = colDef(show = FALSE),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    BirthYear         = colDef(show = FALSE),
    MonthBirth        = colDef(show = FALSE),
    YearLast          = colDef(show = FALSE),
    YearDeath         = colDef(show = FALSE),
    LocBirth_icon     = colDef(show = FALSE),
    LocBirth_color    = colDef(show = FALSE),
    LocLast_icon      = colDef(show = FALSE),
    LocLast_color     = colDef(show = FALSE),
    LocBirth_name     = colDef(show = FALSE),
    LocLast_name      = colDef(show = FALSE),
    sex_ped           = colDef(show = FALSE),
    sex_kinship       = colDef(show = FALSE)
  )
}

#' Column definitions for founder summary tables
#'
#' @param df A filtered founder summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
founder.cols <- function(df) {
  list(
    Status           = Status(),
    ID               = ID(df),
    LocBirth         = LocBirth(df),
    AgeDeath         = AgeLast(df),
    LocLast          = LocLast(df),
    Rel_Contribution = Rel_Contribution(df),
    Sire             = colDef(show = FALSE),
    Dam              = colDef(show = FALSE),
    DateDeath        = colDef(show = FALSE),
    DateBirth        = colDef(show = FALSE),
    Sex              = colDef(show = FALSE),
    color            = colDef(show = FALSE),
    BirthYear        = colDef(show = FALSE),
    MonthBirth       = colDef(show = FALSE),
    YearLast         = colDef(show = FALSE),
    YearDeath        = colDef(show = FALSE),
    LocBirth_icon    = colDef(show = FALSE),
    LocBirth_color   = colDef(show = FALSE),
    LocLast_icon     = colDef(show = FALSE),
    LocLast_color    = colDef(show = FALSE),
    LocBirth_name    = colDef(show = FALSE),
    LocLast_name     = colDef(show = FALSE),
    sex_ped          = colDef(show = FALSE),
    sex_kinship      = colDef(show = FALSE)
  )
}

#' Column definitions for living individual summaries
#'
#' @param df A living population summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
living.cols <- function(df) {
  list(
    LocCurrent    = Current_Location(df),
    ID            = ID(df),
    LocBirth      = LocBirth(df),
    Age           = AgeLast(df),
    Sire          = Sire(df),
    Dam           = Dam(df),
    inbred        = inbred(df),
    N_Ancestors   = bubble_count(df, "Ancestors"),
    N_Descendants = bubble_count(df, "Descendants"),
    N_Children    = bubble_count(df, "Offspring"),
    N_Siblings    = bubble_count(df, "Siblings"),
    DateBirth     = colDef(show = FALSE),
    Sex           = colDef(show = FALSE),
    color         = colDef(show = FALSE),
    BirthYear     = colDef(show = FALSE),
    MonthBirth    = colDef(show = FALSE),
    YearLast      = colDef(show = FALSE),
    LocBirth_icon = colDef(show = FALSE),
    LocBirth_color= colDef(show = FALSE),
    LocLast_icon  = colDef(show = FALSE),
    LocLast_color = colDef(show = FALSE),
    LocBirth_name = colDef(show = FALSE),
    LocLast_name  = colDef(show = FALSE),
    sex_ped       = colDef(show = FALSE),
    sex_kinship   = colDef(show = FALSE)
  )
}

#' Column groupings for living summary table
#'
#' @return A named list of column groups
#' @export
#'
#' @importFrom reactable colGroup
living.groups <- function() {
  list(colGroup(name = "Cumulative Counts", columns = c("N_Ancestors", "N_Children", "N_Siblings")))
}

#' Column definitions for kinship summary tables
#'
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactable colFormat
#' @importFrom tippy tippy
kin.cols <- function() {
  list(
    N              = colDef(header = tippy("N"          , tooltip = "Number of currently living individuals" )),
    Ne             = colDef(header = tippy("Ne"         , tooltip = "Effective population size"              ), format = colFormat(digits = 0)),
    Ne_over_N      = colDef(header = tippy("Ne/N"       , tooltip = "Effective population size relative to N"), format = colFormat(digits = 1)),
    n_founder_reps = colDef(header = tippy("Founders"   , tooltip = "Number of represented founders"         )),
    FGE            = colDef(header = tippy("FGE"        , tooltip = "Founder genome equivalents"             ), format = colFormat(digits = 1)),
    F_mean         = colDef(header = tippy("F"          , tooltip = "Mean inbreeding coefficient"            )),
    mean_gen       = colDef(header = tippy("Generations", tooltip = "Mean generation of living population"   ), format = colFormat(digits = 1)),
    delta_F        = colDef(header = tippy("\u0394F"    , tooltip = "Per-generation inbreeding increase"     )),
    GD             = colDef(header = tippy("GD"         , tooltip = "Gene diversity retained"                ), format = colFormat(percent = TRUE)),
    MK             = colDef(header = tippy("MK"         , tooltip = "Population mean kinship"                ))
  )
}
