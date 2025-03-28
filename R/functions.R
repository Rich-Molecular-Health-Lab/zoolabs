#' Calculate Age from Dates
#' This returns age in years as an integer given a date of birth and a relative end date, both formatted as date vectors.
calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(interval(floor_date(birth, "month"), ceiling_date(date, "month")), unit = "years"), "years"))
}
#' Subset Studbook IDs
#' These functions generate character vectors with a list of all studbook IDs in a particular category. They are helper functions for data wrangling and visuzalization.
#' @rdname studbook_ids
#' @export
living <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" | Status == "A") %>% pull(ID) %>% unique()
  return(ids)
}
#' @rdname studbook_ids
#' @export
deceased <- function(studbook) {
  ids  <- filter(studbook, Status == "Deceased" | Status == "D" | Status == "H" | Status == "Hypothetical") %>% pull(ID) %>% unique()
  return(ids)
}
#' @rdname studbook_ids
#' @export
living.males <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" & (Sex == "M" | Sex == "Male")) %>%
    pull(ID) %>% unique()
  return(ids)
}
#' @rdname studbook_ids
#' @export
living.females <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" & (Sex == "F" | Sex == "Female")) %>%
    pull(ID) %>% unique()
  return(ids)
}
#' Organize Studbook Data
#' These functions help with importing, cleaning, and wrangling studbook data to group stats based on custom cohorts like birth year, sex, or location.
#' @rdname wrangle_studbook
#' @export
make_cohorts <- function(df, minYear, maxYear, span, maxAge, include_sex = TRUE) {
  N_letters <- (maxYear - minYear + 1)/span
  cohorts   <- expand_grid(
    Age       = 0:maxAge,
    Sex       = c("M", "F"),
    BirthYear = 1985:2024) %>%
    full_join(tibble(BirthYear   = minYear:maxYear,
                     BirthCohort = rep(LETTERS[1:N_letters], each = span)),
              by = join_by(BirthYear)) %>%
    mutate(CohortMin = min(BirthYear),
           CohortMax = max(BirthYear), .by = BirthCohort) %>%
    mutate(CohortLabel = case_when(
      CohortMin <= 2013 & CohortMax >= 2013 ~ as.character(str_glue("{CohortMin}", "-", "{CohortMax}", "\n(Culi)")),
      CohortMin <= 2017 & CohortMax >= 2013  ~ as.character(str_glue("{CohortMin}", "-", "{CohortMax}", "\n(Warble)")),
      .default = as.character(str_glue("{CohortMin}", "-", "{CohortMax}"))),
      .keep = "unused") %>%
    select(CohortLabel, BirthCohort, BirthYear, Sex, Age) %>%
    arrange(BirthCohort, BirthYear, Sex, Age) %>%
    filter(BirthYear + Age <= 2025)
  if (include_sex == TRUE) {
    df <- df %>%
      right_join(cohorts, by = join_by(BirthYear, Sex, Age)) %>%
      mutate(Cohort = as.character(str_glue("{Sex}", "{BirthCohort}")),
             across(where(is.numeric), ~ replace_na(., 0)))
  } else if (include_sex == FALSE) {
    df <- df %>%
      right_join(distinct(cohorts,
                          Age,
                          BirthYear,
                          BirthCohort,
                          CohortLabel), by = join_by(BirthYear, Age)) %>%
      mutate(Cohort = BirthCohort,
             across(where(is.numeric), ~ replace_na(., 0)),
             Sex = "Total") %>%
      relocate(CohortLabel, BirthCohort, Sex, Age)
  }
  return(df)
}
#' @rdname wrangle_studbook
#' @export
nest_timeline <- function(timeline, groupBy = NULL) {
  if (is.null(groupBy)) {
    nested <-  timeline %>%
      group_by(Date) %>%
      summarise(Individuals = list(tibble(ID, Sex, BirthYear, Age)),
                .groups = "drop") %>%
      split(.$Date)
  } else if (groupBy == "Location") {
    nested <-  timeline %>%
      group_by(Date, Location) %>%
      summarize(Individuals = list(tibble(ID, Sex, BirthYear, Age))) %>%
      group_by(Date) %>%
      summarize(Locations = split(Individuals, Location)) %>%
      split(.$Date) %>%
      map(~ .x$Locations)
  } else if (groupBy == "Age") {
    nested <-  timeline %>%
      group_by(Date, Age) %>%
      summarize(Individuals = list(tibble(ID, Sex, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Ages = split(Individuals, Age)) %>%
      split(.$Date) %>%
      map(~ .x$Ages)
  } else if (groupBy == "Sex") {
    nested <-  timeline %>%
      group_by(Date, Sex) %>%
      summarize(Individuals = list(tibble(ID, Age, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Sexes = split(Individuals, Sex)) %>%
      split(.$Date) %>%
      map(~ .x$Sexes)
  } else if (groupBy == "AgeSex") {
    nested <-  timeline %>%
      group_by(Date, Sex, Age) %>%
      summarize(Individuals = list(tibble(ID, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Classes = split(Individuals, Sex, Age)) %>%
      split(.$Date) %>%
      map(~ .x$Classes)
  }
  return(nested)
}
#' Censusing Count Data
#' Select a time unit (either months or years) over which to count the presence/absence of individuals or occurrence of events from the studbook, beginning with the earliest date represented in the timeline and ending with today.
#' These functions require two tibbles - a timeline tibble and a studbook.
#' The timeline should contain one column with a unique identifier matching the unique identifiers used for each individual in the studbook.
#' The timeline should also contain a column for the factor "event". Each individual should have one row with a "Birth" event and one row with an "End" event in the timeline.
#' The Date column on the timeline should contain a formatted date for the event represented by that row.
#' The studbook should contain one row per individual identifier, and each row should also contain the column "Sex" with factor levels "M", "F", or "U".
#' Functions produce summary tibbles with count results for chosen period of time, or location, or demographic category.
#' @rdname census_counts
#' @export
count_births <- function(timeline, studbook) {
  births <- timeline %>%
    filter(TypeEvent == "Breed") %>%
    select(ID, Date) %>% distinct() %>%
    mutate(Year = year(Date)) %>%
    summarize(Births = n(), .by = c(ID, Year)) %>%
    ungroup()
  counts <- studbook %>%
    select(
      ID,
      Sex,
      BirthYear,
      Start     = DateBirth,
      End       = DateDeath
    ) %>%
    filter(Sex != "U") %>%
    mutate(Start = floor_date(Start, "years"),
           End   = if_else(!is.na(End),
                           floor_date(End, "years"),
                           floor_date(today(), "years"))
    ) %>%
    mutate(Years = pmap(list(Start, End), \(x, y) seq(x, y, by = "years"))) %>%
    unnest(Years) %>%
    mutate(Year = year(Years),
           Age  = calculate_age(Start, Years)) %>%
    select(ID,
           Sex,
           BirthYear,
           Age,
           Year) %>%
    left_join(births, by = join_by(ID, Year)) %>%
    mutate(Births = replace_na(Births, 0)) %>%
    distinct() %>%
    select(ID,
           BirthYear,
           Sex,
           Age,
           Births)
  return(counts)
}
#' @rdname census_counts
#' @export
census <- function(timeline, studbook, period) {
  if (period == "years") {
    counts <- timeline %>%
      filter(event %in% c("Birth", "End")) %>%
      distinct(ID, Date, event) %>%
      pivot_wider(id_cols     = "ID",
                  names_from  = "event",
                  values_from = "Date") %>%
      distinct() %>%
      mutate(Birth = floor_date(Birth, "years"),
             End   = ceiling_date(End, "years")) %>%
      mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = "year"))) %>%
      unnest(Dates) %>%
      select(ID, Date = Dates) %>%
      mutate(Age = row_number() - 1, .by = ID) %>%
      left_join(select(studbook,
                       ID,
                       Sex), by = join_by(ID)) %>%
      summarize(N = n(), .by = c(Sex, Date)) %>%
      distinct() %>%
      arrange(Date)
  } else if (period == "months") {
    counts <- timeline %>%
      filter(event %in% c("Birth", "End")) %>%
      distinct(ID, Date, event) %>%
      pivot_wider(id_cols     = "ID",
                  names_from  = "event",
                  values_from = "Date") %>%
      distinct() %>%
      mutate(Birth = floor_date(Birth, "months"),
             End   = ceiling_date(End, "months")) %>%
      mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = "month"))) %>%
      unnest(Dates) %>%
      select(ID, Date = Dates) %>%
      mutate(Age = row_number() - 1, .by = ID) %>%
      left_join(select(studbook,
                       ID,
                       Sex), by = join_by(ID)) %>%
      summarize(N = n(), .by = c(Sex, Date)) %>%
      distinct() %>%
      arrange(Date)
  }
  census <- counts %>%
    pivot_wider(id_cols     = "Date",
                names_from  = "Sex",
                values_from = "N") %>%
    rename(Females      = F,
           Males        = M,
           Unidentified = U) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  return(census)
}
#' Generate Life Tables
#' These functions will calculate basic demographic summary statistics and build life tables in tibble format, starting from the formatted studbook and timeline tibbles.
#' Some functions may include parameters to summarize based on age group cohorts, sexes, location, etc.
#' @rdname life_tabs
#' @export
lifeTab <- function(df) {
  life.table <- df %>% arrange(Cohort, Age) %>%
    mutate(across(c(Births, Nx), ~ replace_na(., 0)),
           RiskQx = Nx,
           RiskMx = Nx) %>%
    group_by(Cohort) %>%
    mutate(Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
           N0     = if_else(Age == 0, Nx, NA),
           N1     = if_else(Age == 1, Nx, NA),
           Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx)) %>%
    fill(N0, N1, .direction = "downup")  %>%
    ungroup() %>%
    mutate(Lx1 = if_else(    N1 == 0, 0, Nx/N1),
           Lx  = if_else(    N0 == 0, 0, Nx/N0),
           Qx  = if_else(RiskQx == 0, 0, Deaths/RiskQx),
           Mx  = if_else(RiskMx == 0, 0, (Births/RiskMx)/2)) %>%
    mutate(Qx1 = if_else(Age == 0, Qx, NA),
           Fx  = Mx * Lx) %>%
    mutate(numT  = Age * Fx) %>%
    group_by(Cohort) %>%
    mutate(MLE        = if_else(max(N1) < 1, 0, MLE_age1(Lx1, Age)),
           FirstRepro = min(Age[Mx>0]),
           LastRepro  = max(Age[Mx>0]),
           MaxLongev  = max(Age[Deaths>0])
    ) %>%
    mutate(R0   = sum(Fx),
           Tnum = sum(numT),
           .keep = "unused") %>%
    fill(Qx1) %>%
    ungroup() %>%
    mutate(T = if_else(R0 > 0, Tnum/R0, 0)) %>%
    mutate(lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)) %>%
    select(-Tnum) %>%
    relocate(
      RiskQx,
      RiskMx,
      N0,
      Nx,
      Px,
      Lx,
      Lx1,
      Qx,
      Qx1,
      Mx,
      R0,
      T,
      MLE,
      lambda,
      FirstRepro,
      LastRepro,
      MaxLongev,
      .after = Age
    )
  return(life.table)
}
#' @rdname life_tabs
#' @export
cohort_lifeTab <- function(timeline, studbook, minYear, maxYear, span, maxAge) {
  life.table.sexes <- count_births(timeline, studbook) %>%
    summarize(Births = sum(Births),
              Nx     = n(),
              .by    = c(BirthYear, Sex, Age)
    ) %>%
    ungroup() %>%
    make_cohorts(., minYear, maxYear, span, maxAge, TRUE) %>%
    summarize(Births = sum(Births),
              Nx      = sum(Nx),
              .by     = c(CohortLabel, BirthCohort, Sex, Cohort, Age)) %>%
    arrange(BirthCohort, Sex, Age) %>%
    lifeTab()

  lifeTab <- count_births(timeline, studbook) %>%
    summarize(Births = sum(Births),
              Nx     = n(),
              .by    = c(BirthYear, Age)
    ) %>%
    ungroup() %>%
    make_cohorts(., minYear, maxYear, span, maxAge, FALSE) %>%
    summarize(Births = sum(Births),
              Nx      = sum(Nx),
              .by     = c(CohortLabel, BirthCohort, Cohort, Sex, Age)) %>%
    arrange(BirthCohort, Age) %>%
    lifeTab() %>%
    bind_rows(life.table.sexes) %>%
    arrange(BirthCohort, Age, Sex)
  return(lifeTab)
}
#' @rdname life_tabs
#' @export
lifeTab_static <- function(df) {
  df <- df  %>%
    mutate(across(c(Px:lambda), ~ round(., digits = 3))) %>%
    select(
      Cohort,
      CohortLabel,
      Sex,
      N0,
      Qx1,
      R0,
      T,
      MLE,
      lambda,
      FirstRepro,
      LastRepro,
      MaxLongev
    ) %>%
    filter(N0 > 0) %>%
    distinct() %>%
    arrange(Cohort, Sex)
  return(df)
}
#' Generate Pedigree Statistics
#' These functions will calculate basic pedigree summary statistics for use in visualization or modeling.
#' Most functions require the original studbook and either the full list of pedigree objects created by running ped() with pedtools (pedigree), a single pedigree from that list for the living representation (pedigree.living), or both.
#' @rdname ped_stats
#' @export
founder_reps   <- function(pedigree, pedigree.living, studbook) {
  founder.descendants        <- map(as.list(founders(pedigree)), \(x) as.list(descendants(pedigree, x, inclusive = TRUE))) %>% compact()
  names(founder.descendants) <- map(founder.descendants, \(x) x[[1]])
  founderReps                <- keep(founder.descendants, function(sublist) {
    flat  <- unlist(sublist)
    any(flat %in% living(studbook))
  }) %>% list_flatten(name_spec = "") %>% unique() %>% intersect(founders(pedigree.living))
  return(founderReps)
}
#' @rdname ped_stats
#' @export
founder_contributions  <- function(studbook, pedigree.living) {
  living         <- living(studbook)
  dp             <- descentPaths(pedigree.living)
  founderContribution <- map_dbl(founders(pedigree.living), function(f) {
    paths_list <- dp[[f]]
    valid_paths <- keep(paths_list, ~ tail(.x, 1) %in% living)
    sum(map_dbl(valid_paths, ~ 0.5^(length(.x) - 1)))
  })
  names(founderContribution) <- founders(pedigree.living)
  return(founderContribution)
}
#' @rdname ped_stats
#' @export
rel_founder_contributions <- function(studbook, pedigree.living) {
  founderContribution <- founder_contributions(studbook, pedigree.living)
  result              <- founderContribution / sum(founderContribution)
  return(result)
}
#' @rdname ped_stats
#' @export
founder_summary    <- function(pedigree, pedigree.living, studbook) {
  founderContribution <- founder_contributions(studbook, pedigree.living)
  living              <- living(studbook)
  founderReps         <- founder_reps(pedigree, pedigree.living, studbook)
  n_founder_reps      <- length(founderReps)
  founderRepsIDs      <- founderReps %>% list_c()
  dp                  <- descentPaths(pedigree.living)
  p                   <- founder_contributions(studbook, pedigree.living)
  p.tbl               <- enframe(p, name  = "ID", value = "Rel_Contribution") %>% mutate(ID = as.integer(ID))
  founder.summary     <- enframe(founderContribution, name = "ID", value = "Contribution")  %>% mutate(ID = as.integer(ID)) %>%
    left_join(p.tbl) %>% left_join(studbook) %>%
    select(Status           ,
           ID               ,
           LocBirth         ,
           AgeDeath = AgeLast ,
           LocLast          ,
           Sire             ,
           Dam              ,
           Rel_Contribution ,
           DateDeath        ,
           DateBirth        ,
           Sex              ,
           color            ,
           BirthYear        ,
           MonthBirth       ,
           YearLast         ,
           YearDeath        ,
           LocBirth_icon    ,
           LocBirth_color   ,
           LocLast_icon     ,
           LocLast_color    ,
           LocBirth_name    ,
           LocLast_name     ,
           sex_ped          ,
           sex_kinship
    )  %>%
    arrange(Status, LocLast, desc(Rel_Contribution))
  return(founder.summary)
}
#' @rdname ped_stats
#' @export
gen_numbers  <- function(pedigree.living) {
  gen_numbers <- generations(pedigree.living, what = "indiv")
  if (length(gen_numbers) == 0) {
    warning("No generation numbers returned using what = 'indiv'. Trying what = 'depth' instead.")
    gen_numbers <- generations(pedigree.living, what = "depth")
  }
  return(gen_numbers)
}
#' @rdname ped_stats
#' @export
gen_numbers_living <- function(pedigree.living, studbook) {
  living      <- living(studbook)
  gens        <- gen_numbers(pedigree.living)
  living_gen  <- gens[names(gens) %in% living]
  return(living_gen)
}
#' @rdname ped_stats
#' @export
kin_matrix_living  <- function(pedigree.living, studbook) {
  living         <- living(studbook)
  kinship.ped    <- kinship(pedigree.living)
  living.ped     <- intersect(living, rownames(kinship.ped))
  living.kinship <- kinship.ped[living.ped, living.ped]
}
#' @rdname ped_stats
#' @export
family_history     <- function(pedigree.living, studbook) {
  generations <- enframe(gen_numbers(pedigree.living),
                         name  = "ID",
                         value = "Generations") %>%
    mutate(ID = as.integer(ID))
  inbred.df <- inbreeding(pedigree.living) %>%
    enframe(name = "ID", value = "inbred")
  kin.matrix <- kin_matrix_living(pedigree.living, studbook)
  inbred.df  <- inbred.df  %>%
    mutate(ID = as.integer(ID))
  ancestors <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(ancestors(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Ancestors") %>%
    mutate(ID = as.integer(ID))
  children <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(children(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Children") %>%
    mutate(ID = as.integer(ID))
  descendants <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(descendants(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Descendants") %>%
    mutate(ID = as.integer(ID))
  siblings <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(siblings(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Siblings") %>%
    mutate(ID = as.integer(ID))
  df <- ancestors %>%
    left_join(descendants) %>%
    left_join(children) %>%
    left_join(siblings) %>%
    left_join(generations) %>%
    rowwise() %>%
    mutate(N_Children    = length(Children),
           N_Descendants = length(Descendants),
           N_Siblings    = length(Siblings),
           N_Ancestors   = length(Ancestors)) %>%
    select(ID, starts_with("N_")) %>%
    left_join(inbred.df) %>%
    left_join(studbook) %>%
    select(
      LocCurrent = LocLast,
      ID               ,
      LocBirth         ,
      Age = AgeLast    ,
      Sire             ,
      Dam              ,
      DateBirth        ,
      Sex              ,
      color            ,
      BirthYear        ,
      MonthBirth       ,
      YearLast         ,
      LocBirth_icon    ,
      LocBirth_color   ,
      LocLast_icon     ,
      LocLast_color    ,
      LocBirth_name    ,
      LocLast_name     ,
      sex_ped          ,
      sex_kinship      ,
      inbred           ,
      starts_with("N_")) %>%
    arrange(LocCurrent, Sex, Age)
  return(df)
}
#' @rdname ped_stats
#' @export
F_vector           <- function(pedigree.living, studbook) {
  kin.matrix     <- kin_matrix_living(pedigree.living, studbook)
  F_vec          <- 2 * diag(kin.matrix) - 1
  return(F_vec)
}
#' @rdname ped_stats
#' @export
kinship_summary    <- function(pedigree, pedigree.living, studbook) {
  p              <- rel_founder_contributions(studbook, pedigree.living)
  living         <- living(studbook)
  n_founder_reps <- length(founder_reps(pedigree, pedigree.living, studbook))
  kin.matrix     <- kin_matrix_living(pedigree.living, studbook)
  mean_gen       <- mean(gen_numbers_living(pedigree.living, studbook), na.rm = TRUE)
  F_vec          <- F_vector(pedigree.living, studbook)
  F_mean         <- mean(F_vec)
  delta_F        <- 1 - (1 - F_mean)^(1/mean_gen)
  Ne             <- 1 / (2 * delta_F)
  N              <- length(living)
  Ne_over_N      <- Ne / N
  FGE            <- 1 / sum(p^2)
  GD             <- 1 - sum(p^2)
  MK             <- mean(kin.matrix[upper.tri(kin.matrix)])
  result <- tibble(N,
                   n_founder_reps,
                   FGE,
                   delta_F,
                   Ne,
                   Ne_over_N,
                   mean_gen,
                   F_mean,
                   MK,
                   GD)
  return(result)
}
