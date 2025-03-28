# life_tables.R
# Functions for calculating and formatting life tables

#' Build a full demographic life table from cohort data
#'
#' @param df A cohort-formatted tibble with Births, Nx, Age, Cohort
#' @return A life table with survivorship, mortality, and reproductive values
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom tidyr fill
#' @importFrom tidyr replace_na
lifeTab <- function(df) {
  df %>%
    arrange(Cohort, Age) %>%
    mutate(across(c(Births, Nx), ~ replace_na(., 0)), RiskQx = Nx, RiskMx = Nx) %>%
    group_by(Cohort) %>%
    mutate(
      Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
      N0     = if_else(Age == 0, Nx, NA),
      N1     = if_else(Age == 1, Nx, NA),
      Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx)
    ) %>%
    fill(N0, N1, .direction = "downup") %>%
    ungroup() %>%
    mutate(
      Lx1 = if_else(N1 == 0, 0, Nx / N1),
      Lx  = if_else(N0 == 0, 0, Nx / N0),
      Qx  = if_else(RiskQx == 0, 0, Deaths / RiskQx),
      Mx  = if_else(RiskMx == 0, 0, (Births / RiskMx) / 2),
      Qx1 = if_else(Age == 0, Qx, NA),
      Fx  = Mx * Lx,
      numT = Age * Fx
    ) %>%
    group_by(Cohort) %>%
    mutate(
      MLE        = if_else(max(N1, na.rm = TRUE) < 1, 0, MLE_age1(Lx1, Age)),
      FirstRepro = min(Age[Mx > 0]),
      LastRepro  = max(Age[Mx > 0]),
      MaxLongev  = max(Age[Deaths > 0]),
      R0   = sum(Fx),
      Tnum = sum(numT)
    ) %>%
    ungroup() %>%
    mutate(
      T = if_else(R0 > 0, Tnum / R0, 0),
      lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)
    ) %>%
    select(-Tnum) %>%
    relocate(RiskQx, RiskMx, N0, Nx, Px, Lx, Lx1, Qx, Qx1, Mx, R0, T, MLE, lambda, FirstRepro, LastRepro, MaxLongev, .after = Age)
}

#' Generate life table from studbook and timeline over cohorts
#'
#' @param timeline Timeline tibble
#' @param studbook Studbook tibble
#' @param minYear First cohort year
#' @param maxYear Last cohort year
#' @param span Years per cohort
#' @param maxAge Maximum age
#' @return A tibble with cohort life table summary
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
cohort_lifeTab <- function(timeline, studbook, minYear, maxYear, span, maxAge) {
  life.table.sexes <- count_births(timeline, studbook) %>%
    count(BirthYear, Sex, Age, name = "Nx") %>%
    group_by(BirthYear, Sex, Age) %>%
    summarise(Births = sum(Births), .groups = "drop") %>%
    make_cohorts(minYear = minYear, maxYear = maxYear, span = span, maxAge = maxAge, include_sex = TRUE) %>%
    group_by(CohortLabel, BirthCohort, Sex, Cohort, Age) %>%
    summarise(Births = sum(Births), Nx = sum(Nx), .groups = "drop") %>%
    arrange(BirthCohort, Sex, Age) %>%
    lifeTab()

  life.table.totals <- count_births(timeline, studbook) %>%
    count(BirthYear, Age, name = "Nx") %>%
    group_by(BirthYear, Age) %>%
    summarise(Births = sum(Births), .groups = "drop") %>%
    make_cohorts(minYear = minYear, maxYear = maxYear, span = span, maxAge = maxAge, include_sex = FALSE) %>%
    group_by(CohortLabel, BirthCohort, Cohort, Sex, Age) %>%
    summarise(Births = sum(Births), Nx = sum(Nx), .groups = "drop") %>%
    arrange(BirthCohort, Age) %>%
    lifeTab()

  bind_rows(life.table.totals, life.table.sexes) %>%
    arrange(BirthCohort, Age, Sex)
}

#' Reduce full life table to summary stats per cohort
#'
#' @param df Life table tibble
#' @return Condensed summary table with lambda and vital rates
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
lifeTab_static <- function(df) {
  df %>%
    mutate(across(c(Px:lambda), ~ round(., 3))) %>%
    select(Cohort, CohortLabel, Sex, N0, Qx1, R0, T, MLE, lambda, FirstRepro, LastRepro, MaxLongev) %>%
    filter(N0 > 0) %>%
    distinct() %>%
    arrange(Cohort, Sex)
}
