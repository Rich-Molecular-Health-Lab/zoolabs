# pedigree_stats.R
# Pedigree-based statistics and lineage summary functions

#' Return list of founder IDs represented in living population
#'
#' @param pedigree Full pedigree object
#' @param pedigree.living Pedigree for living individuals
#' @param studbook Studbook tibble
#' @return A character vector of founder IDs
#' @export
#'
#' @importFrom dplyr intersect
#' @importFrom pedtools descendants
#' @importFrom pedtools founders
#' @importFrom purrr compact
#' @importFrom purrr keep
#' @importFrom purrr list_flatten
#' @importFrom purrr map
#' @importFrom purrr map_chr
founder_reps <- function(pedigree, pedigree.living, studbook) {
  founder.descendants <- map(as.list(founders(pedigree)), \(x) as.list(descendants(pedigree, x, inclusive = TRUE))) %>% compact()
  names(founder.descendants) <- map_chr(founder.descendants, ~ .x[[1]])
  keep(founder.descendants, function(sublist) {
    any(unlist(sublist) %in% living(studbook))
  }) %>% list_flatten(name_spec = "") %>% unique() %>% intersect(founders(pedigree.living))
}

#' Calculate founder contributions to living population
#'
#' @param studbook Studbook tibble
#' @param pedigree.living Pedigree for living individuals
#' @return A named numeric vector of founder contribution weights
#' @export
#'
#' @importFrom pedtools descentPaths
#' @importFrom pedtools founders
#' @importFrom purrr keep
#' @importFrom purrr map_dbl
founder_contributions <- function(studbook, pedigree.living) {
  living <- living(studbook)
  dp <- descentPaths(pedigree.living)
  fc <- map_dbl(founders(pedigree.living), function(f) {
    valid_paths <- keep(dp[[f]], ~ tail(.x, 1) %in% living)
    sum(map_dbl(valid_paths, ~ 0.5^(length(.x) - 1)))
  })
  names(fc) <- founders(pedigree.living)
  fc
}

#' Calculate relative founder contributions
#'
#' @param studbook Studbook tibble
#' @param pedigree.living Pedigree for living individuals
#' @return A numeric vector of relative contributions summing to 1
#' @export
rel_founder_contributions <- function(studbook, pedigree.living) {
  p <- founder_contributions(studbook, pedigree.living)
  p / sum(p)
}

#' Summarize family-level stats for each individual
#'
#' @param pedigree.living Pedigree object for living individuals
#' @param studbook Studbook tibble
#' @return A tibble with family metrics and metadata
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom ribd inbreeding
#' @importFrom tibble tibble
family_history <- function(pedigree.living, studbook) {
  generations <- enframe(gen_numbers(pedigree.living), name = "ID", value = "Generations") %>% mutate(ID = as.integer(ID))
  inbred.df <- inbreeding(pedigree.living) %>% enframe(name = "ID", value = "inbred") %>% mutate(ID = as.integer(ID))
  kin.matrix <- kin_matrix_living(pedigree.living, studbook)
  ids <- rownames(kin.matrix)

  ancestors <- map(ids, ~ as.list(ancestors(pedigree.living, .x))) %>% set_names(ids)
  children <- map(ids, ~ as.list(children(pedigree.living, .x))) %>% set_names(ids)
  descendants <- map(ids, ~ as.list(descendants(pedigree.living, .x))) %>% set_names(ids)
  siblings <- map(ids, ~ as.list(siblings(pedigree.living, .x))) %>% set_names(ids)

  df <- tibble(ID = as.integer(ids)) %>%
    left_join(enframe(ancestors, name = "ID", value = "Ancestors") %>% mutate(ID = as.integer(ID)), by = "ID") %>%
    left_join(enframe(descendants, name = "ID", value = "Descendants") %>% mutate(ID = as.integer(ID)), by = "ID") %>%
    left_join(enframe(children, name = "ID", value = "Children") %>% mutate(ID = as.integer(ID)), by = "ID") %>%
    left_join(enframe(siblings, name = "ID", value = "Siblings") %>% mutate(ID = as.integer(ID)), by = "ID") %>%
    left_join(generations, by = "ID") %>%
    rowwise() %>%
    mutate(
      N_Children = length(Children),
      N_Descendants = length(Descendants),
      N_Siblings = length(Siblings),
      N_Ancestors = length(Ancestors)
    ) %>%
    ungroup() %>%
    left_join(inbred.df, by = "ID") %>%
    left_join(studbook, by = "ID") %>%
    mutate(LocCurrent = LocLast, Age = AgeLast) %>%
    select(
      LocCurrent, ID, LocBirth, Age, Sire, Dam, DateBirth, Sex, color,
      BirthYear, MonthBirth, YearLast, LocBirth_icon, LocBirth_color,
      LocLast_icon, LocLast_color, LocBirth_name, LocLast_name,
      sex_ped, sex_kinship, inbred, N_Children, N_Descendants, N_Siblings, N_Ancestors
    ) %>%
    arrange(LocCurrent, Sex, Age)

  return(df)
}

#' Calculate inbreeding vector from a pedigree
#'
#' @param pedigree.living A pedigree object
#' @param studbook Studbook tibble
#' @return Numeric vector of inbreeding coefficients
#' @export
F_vector <- function(pedigree.living, studbook) {
  kin.matrix <- kin_matrix_living(pedigree.living, studbook)
  F_vec <- 2 * diag(kin.matrix) - 1
  return(F_vec)
}

#' Subset kinship matrix to living individuals
#'
#' @param pedigree.living A pedigree object
#' @param studbook Studbook tibble
#' @return Sub-matrix of pairwise kinship among living individuals
#' @export
#'
#' @importFrom dplyr intersect
#' @importFrom kinship2 kinship
kin_matrix_living <- function(pedigree.living, studbook) {
  living_ids <- living(studbook)
  kin <- kinship(pedigree.living)
  subset_ids <- intersect(living_ids, rownames(kin))
  kin[subset_ids, subset_ids]
}

#' Return number of generations for individuals
#'
#' @param pedigree.living Pedigree object
#' @return Named vector of generation numbers
#' @export
#'
#' @importFrom pedtools generations
gen_numbers <- function(pedigree.living) {
  gens <- generations(pedigree.living, what = "indiv")
  if (length(gens) == 0) {
    warning("No generation numbers returned using 'indiv'. Trying 'depth'.")
    gens <- generations(pedigree.living, what = "depth")
  }
  gens
}

#' Get generation numbers for living individuals
#'
#' @param pedigree.living Pedigree object
#' @param studbook Studbook tibble
#' @return Named vector of generations for living IDs
#' @export
#'
#' @importFrom tibble tibble
gen_numbers_living <- function(pedigree.living, studbook) {
  gens <- gen_numbers(pedigree.living)
  gens[names(gens) %in% living(studbook)]
}

#' Summarize pedigree-wide statistics
#'
#' @param pedigree Full pedigree object
#' @param pedigree.living Living-only pedigree object
#' @param studbook Studbook tibble
#' @return A tibble of demographic/genetic statistics
#' @export
#'
#' @importFrom tibble tibble
kinship_summary <- function(pedigree, pedigree.living, studbook) {
  p <- rel_founder_contributions(studbook, pedigree.living)
  N <- length(living(studbook))
  kin <- kin_matrix_living(pedigree.living, studbook)
  F_vec <- F_vector(pedigree.living, studbook)
  F_mean <- mean(F_vec)
  mean_gen <- mean(gen_numbers_living(pedigree.living, studbook), na.rm = TRUE)
  delta_F <- 1 - (1 - F_mean)^(1/mean_gen)
  Ne <- 1 / (2 * delta_F)
  MK <- mean(kin[upper.tri(kin)])
  FGE <- 1 / sum(p^2)
  GD <- 1 - sum(p^2)
  tibble(
    N = N,
    n_founder_reps = length(founder_reps(pedigree, pedigree.living, studbook)),
    FGE = FGE,
    delta_F = delta_F,
    Ne = Ne,
    Ne_over_N = Ne / N,
    mean_gen = mean_gen,
    F_mean = F_mean,
    MK = MK,
    GD = GD
  )
}
