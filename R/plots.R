#' @importFrom dplyr intersect
#' @importFrom dplyr setdiff
#' @importFrom htmlwidgets saveWidget
#' @importFrom pedtools plot.ped
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom purrr keep_at
#' @importFrom purrr map_depth
#' @importFrom purrr list_assign
#' @importFrom stats setNames
#' Standardize Color Palettes for Population Data
#' Create color palettes to standardize color coding based on sex, status, and location across plots, pedigrees, and tables.
#' These functions will transfer standardized palettes between list and vector formats to integrate smoothly across pedtools, plotly, ggplot, and reactable.
#' You should start by naming a list of colors where each color value is named "m" for males, "f" for females and "u" for undetermined.
#' The palette functions containing the tag "fill" will lighten your color palette for more opaque versions of the same colors.
#' @rdname color_pals
#' @export
set_ped_fills <- function(pedigree, studbook) {
  female       <- intersect(living(studbook), females(pedigree))
  male         <- intersect(living(studbook), males(pedigree))
  undet        <- setdiff(living(studbook), males(pedigree)) %>% setdiff(females(pedigree))
  fills        <- list(c(female), c(male), c(undet))
  names(fills) <- keep_at(colors, c("f", "m", "u"))
  female.d     <- intersect(deceased(studbook), females(pedigree))
  male.d       <- intersect(deceased(studbook), males(pedigree))
  undet.d      <- setdiff(deceased(studbook), males(pedigree)) %>% setdiff(females(pedigree))
  ped.fills    <- list("#D5328870" = c(female.d), "#3F459B70" = c(male.d), "#21B14B70" = c(undet.d))
  fills        <- list_assign(fills, !!!ped.fills)
  return(fills)
}
#' @rdname color_pals
#' @export
set_plotly_pal <- function(palette) {
  col.pal <- keep_at(palette, c("f", "m", "u")) %>% unlist()
  col.pal <- setNames(col.pal, c("F", "M", "Total"))
  return(col.pal)
}
#' @rdname color_pals
#' @export
lighten_palette <- function(palette, hex) {
  if (is.list(palette)) {
    new <-  map_depth(palette, 1, \(x) gsub("FF", hex, x))
  } else {
    new <- gsub("FF", hex, palette)
  }
  return(new)
}
#' Plot Pedigree Objects Produced by pedtools.
#' This will produce formatted plots from the pedigree objects created by the ped() function in pedtools.
#' @family pedigree_plots
#' @export
plot_pedigrees <- function(pedigree, name, studbook, ...) {
  fills    <- set_ped_fills(pedigree, studbook)
  deceased <- deceased(studbook)
  plot(pedigree,
       title        = paste0(name),
       cex          = 0.4,
       deceased     = deceased,
       labs         = NULL,
       fill         = fills,
       lwd          = 0.3,
       col          = "black",
       pty          = "m", ...)
}
#' Line Graphs of Demographic Status using Plotly
#' These functions work with tibbles produced in other functions here to summarize basic demographic Life Table statistics in the form of line graphs using the package plotly.
#' Some of the functions will automatically export an html version of the plot to your working directory.
#' These also work with the other color palette functions in this package so that if you provide the original list version of a color palette, they will do the work of converting between lists and vectors and renaming values or decreasing opacity for fills.
#' @rdname demog_linely
#' @export
annotate_lambda <- function(df, lambda = lambda) {
  df <- df %>%
    mutate(hover_lambda = abs(round((lambda - 1)*100, digits = 1))) %>%
    mutate(
      hover_lambda = if_else(
        lambda >= 1,
        as.character(str_glue("Population growing by ", "{hover_lambda}", "%")),
        as.character(str_glue("Population declining by ", "{hover_lambda}", "%"))
      )
    )
  return(df)
}
#' @rdname demog_linely
#' @export
hline <- function(y = 0, color = "#444444") {
  list(
    type = "line",
    x0   = 0,
    x1   = 1,
    xref = "paper",
    y0   = y,
    y1   = y,
    line = list(color   = color,
                width   = 0.9,
                dash    = "dot")
  )
}
#' @rdname demog_linely
#' @export
plot_census <- function(cesus_df, palette, title) {
  fill.col <- lighten_pal_list(palette, 26)
  colors   <- palette
  plot <- plot_ly(
    data         = cesus_df,
    x            = ~Date,
    y            = ~Unidentified,
    name         = "Sex Unidentified",
    type         = "scatter",
    mode         = "lines",
    stackgroup   = "one",
    hoveron      = "points+fills",
    opacity      = 0.5,
    line         = list(
      color     = colors$u,
      shape     = "spline",
      smoothing = 0.8,
      width     = 1.5
    ),
    fillcolor    = fill.col$u
  ) %>%
    add_trace(
      x            = ~Date,
      y            = ~Females,
      name         = "Females",
      mode         = "lines",
      stackgroup   = "one",
      hoveron      = "points+fills",
      opacity      = 0.5,
      line         = list(
        color     = colors$f,
        shape     = "spline",
        smoothing = 0.8,
        width     = 1.5
      ),
      fillcolor    = fill.col$f
    ) %>%
    add_trace(
      x            = ~Date,
      y            = ~Males,
      name         = "Males",
      mode         = "lines",
      stackgroup   = "one",
      hoveron      = "points+fills",
      opacity      = 0.5,
      line         = list(
        color     = colors$m,
        shape     = "spline",
        smoothing = 0.8,
        width     = 1.5
      ),
      fillcolor    = fill.col$m
    ) %>%
    layout(title        = title,
           plot_bgcolor = "#ffffff",
           yaxis = list(title         = "Individuals Alive",
                        showline      = T,
                        showgrid      = F),
           xaxis = list(title         = "Date",
                        showline      = F,
                        showgrid      = T,
                        rangeslider   = list(visible = T)))
  saveWidget(plot, "PopulationTrends.html")
  return(plot)
}
#' @rdname demog_linely
#' @export
plot_lambda <- function(life_table, title) {
  col.pal           <- set_plotly_pal(colors)
  fills             <- lighten_plotly_pal(col.pal, "33")
  life.table.static <- lifeTab_static(life_table) %>% annotate_lambda()
  lambda.annotation <-  list(
    x          = 0.9,
    xref       = "paper",
    y          = 1,
    yref       = "y",
    text       = "Stable if \u03BB = 1.0",
    showarrow  = TRUE,
    arrowhead  = 4,
    arrowcolor = "#444444",
    arrowsize  = 1,
    arrowwidth = 0.7,
    ax         = 10,
    ay         = 40,
    font       = list(color  = "#444444",
                      size   = 12,
                      style  = "italic")
  )
  plot <- plot_ly(
    data        = filter(life.table.static, Sex != "Total"),
    type        = "scatter",
    x           = ~CohortLabel,
    y           = ~lambda,
    color       = ~Sex,
    colors      = col.pal,
    text        = ~hover.text,
    mode        = "lines+markers",
    connectgaps = TRUE,
    line        = list(
      shape     = "spline",
      color     = ~Sex,
      colors    = col.pal,
      width     = 1.5
    ),
    marker      = list(
      fill      = ~Sex,
      colors    = fills,
      size      = 6,
      opacity   = 0.7,
      line      = list(
        color     = ~Sex,
        colors    = col.pal,
        width     = 1.0
      )
    )) %>%
    layout(title        = title,
           plot_bgcolor = "#ffffff",
           barmode      = "grouped",
           shapes       = list(hline(1.0)),
           annotations  = lambda.annotation,
           yaxis = list(title         = "Lambda (\u03BB)",
                        showline      = T,
                        zerolinewidth = 2,
                        zerolinecolor = "black",
                        showgrid      = F),
           xaxis = list(title         = "Birth Cohort",
                        showline      = T,
                        zerolinewidth = 2,
                        zerolinecolor = "black",
                        showgrid      = T))
  saveWidget(plot, "Lambda_Cohorts.html")
  return(plot)
}
#' Heatmaps of Matrices using Heatmaply
#' These functions work with matrix objects produced in other functions here to summarize pairwise relatedness metrics between individuals using heatmaply heatmaps.
#' Some of the functions will automatically export an html version of the plot to your working directory.
#' These also work with the other color palette functions in this package so that if you provide the original list version of a color palette, they will do the work of converting between lists and vectors and renaming values or decreasing opacity for fills.
#' @importFrom heatmaply heatmaply
#' @rdname kinmaply
#' @export
annotate_kin_matrix <- function(matrix, pedigree.living, studbook) {
  F   <- F_vector(pedigree.living, studbook) %>%
  enframe(name  = "ID", value = "F_vec") %>%
  mutate(ID = as.integer(ID))
living.data <- family_history(pedigree.living, studbook)
annotate    <- living.data %>%
  mutate(ID = as.integer(ID)) %>%
  left_join(F, by = join_by(ID)) %>%
  mutate(hoverText = str_glue(
    "{LocCurrent}", "<br>",
    "{Age}", " yrs (Born ", "{DateBirth}", ")<br>",
    "Mother: ", "{Dam}", ", Father: ", "{Sire}", "<br>",
    "{N_Siblings}"," Siblings, ", "{N_Children}", " Offspring, ", "{N_Descendants}", " Descendants"
  )
  ) %>% arrange(ID)
kinship_btp  <- subset_matrix_living(matrix, studbook)
text_males   <- intersect(as.character(living.males(studbook))  , rownames(kinship_btp))
text_females <- intersect(as.character(living.females(studbook)), colnames(kinship_btp))
annotate.m   <- filter(annotate, Sex == "Male") %>%
  select(ID, hoverText) %>%
  filter(ID %in% text_males) %>%
  arrange(match(ID, text_males))
annotate.f   <- filter(annotate, Sex == "Female") %>%
  select(ID, hoverText) %>%
  filter(ID %in% text_females) %>%
  arrange(match(ID, text_females))
hover_matrix <- outer(
  annotate.m$hoverText,
  annotate.f$hoverText,
  FUN = function(m_text, f_text) {
    I(paste0("<br>Male:<br>", m_text, "<br><br>",
             "Female:<br>", f_text))
  }
)
rownames(hover_matrix) <- annotate.m$ID
colnames(hover_matrix) <- annotate.f$ID
return(hover_matrix)
}
#' @rdname kinmaply
#' @export
matrix.heatmap <- function(matrix, title, key.title, xlab = "Females", ylab = "Males") {
  filename <- paste0("heatmap_", str_remove_all(title, "\s"), ".html")
  plot     <- heatmaply(
    matrix,
    dendrogram       = "none",
    main             = title,
    scale            = "none",
    colors           = paletteer_d(colors$div),
    margins          = c(60, 100, 40, 20),
    grid_color       = "white",
    grid_width       = 0.00001,
    label_format_fun = function(value) round(value, digits = 3),
    titleX           = TRUE,
    hide_colorbar    = FALSE,
    key.title        = key.title,
    branches_lwd     = 0.1,
    fontsize_row     = 10,
    fontsize_col     = 10,
    labCol           = colnames(matrix),
    labRow           = rownames(matrix),
    heatmap_layers   = theme(axis.line = element_blank())
  )
  saveWidget(plot, filename)
  return(plot)
}
#' @rdname kinmaply
#' @export
subset_matrix_living <- function(matrix, studbook) {
  living_males   <- intersect(as.character(living.males(studbook))  , rownames(matrix))
  living_females <- intersect(as.character(living.females(studbook)), rownames(matrix))
  result         <- matrix[living_males, living_females]
  return(result)
}
#' Visualize Pedigrees with visNetwork
#' This series of helpers and functions will start with a pedigree object produced by pedtools and its precurser studbook and timeline tibbles and use them to create interactive graphics.
#' These are a more elaborate and interactive alternative for visualizing the pedigree plots.
#' They can organize the graphics as a clustered network or a hierarchy by generation.
#' Some of the functions automatically export an html version of the graphic to your working directory.
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visSave
#' @importFrom visNetwork visNodes
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork addFontAwesome
#' @importFrom pedtools founders
#' @importFrom pedtools leaves
#' @importFrom pedtools nonfounders
#' @importFrom pedtools parents
#' @importFrom pedtools commonDescendants
#' @rdname ped_visnet
#' @export
pedigree_levels <- function(pedigree, studbook) {
  studbook <- studbook %>%
    mutate(across(c(Sire, Dam), ~ as.character(.)))
  levels <- as.list(gen_numbers(pedigree)) %>%
    set_names(pedigree$ID) %>%
    enframe(name = "id", value = "level") %>%
    mutate(level = as.integer(level)) %>%
    mutate(level = if_else(id %in% founders(pedigree), level, level + 2)) %>%
    mutate(level = if_else(id %in% leaves(pedigree), level + 1, level)) %>%
    distinct()
  return(levels)
}
#' @rdname ped_visnet
#' @export
pedigree_pairs   <- function(pedigree, studbook) {
  studbook <- studbook %>%
    mutate(across(c(Sire, Dam), ~ as.character(.)))
  pairs <- map(as.list(nonfounders(pedigree)), \(x) as.list(parents(pedigree, id = x))) %>%
    enframe(name = NULL) %>%
    unnest_wider(value, names_sep = "_") %>%
    distinct() %>%
    mutate(pid = paste0("0", as.character(row_number()))) %>%
    left_join(studbook, by = join_by(value_1 == Sire, value_2 == Dam)) %>%
    select(pid,
           fid = value_1,
           mid = value_2,
           color = LocBirth_color,
           label = LocBirth) %>%
    distinct() %>%
    group_by(pid) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup()
  return(pairs)
}
#' @rdname ped_visnet
#' @export
pedigree_edges  <- function(pedigree, studbook) {
  pairs  <- pedigree_pairs(pedigree, studbook)
  sire   <- pull(pairs, fid, pid) %>% as.list()
  dam    <- pull(pairs, mid, pid) %>% as.list()
  pairs.edf <- pairs  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "from", names_to = NULL) %>%
    select(from, to = pid, color) %>%
    mutate(dashes = TRUE,
           shadow = FALSE,
           width  = 0.5,
           arrows = "to") %>% distinct()
  combined         <- c(sire, dam)
  combined_grouped <- split(combined, names(combined)) %>% map(unlist)
  offspring.edf <- combined_grouped %>%
    imap(function(x, idx) {
      list(idx, as.list(commonDescendants(pedigree, ids = x, maxGen = 2)))
    }) %>%
    enframe(name = NULL, value = "id") %>%
    unnest_wider(id, names_sep = "_") %>%
    unnest_longer(id_2, values_to = "id") %>%
    select(from = id_1, to = id) %>%
    arrange(from, to) %>%
    distinct() %>%
    left_join(select(pairs, from = pid, color), by = join_by(from)) %>%
    mutate(dashes = FALSE,
           shadow = TRUE,
           width  = 0.7,
           arrows = NULL) %>%
    distinct()

  edges <- bind_rows(pairs.edf, offspring.edf) %>%
    distinct()

  return(edges)
}
#' @rdname ped_visnet
#' @export
pedigree_nodes  <- function(pedigree, studbook) {
  levels    <- pedigree_levels(pedigree, studbook)
  pairs.ndf <- pedigree_pairs(pedigree, studbook)  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "mateID", names_to = NULL) %>%
    left_join(levels, by = join_by(mateID == id)) %>%
    distinct() %>%
    group_by(pid, color, label) %>%
    mutate(level = sum(max(level), 1)) %>% ungroup() %>%
    group_by(pid) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    mutate(
      group = "pair",
      shape = "icon",
      icon  = map(color, \(x) list(code = "\uf068", color = x, face = "'Font Awesome 5 Free'", weight = 700, size = 35)),
      font  = map(color, \(x) list(size = 18, color = x, strokeColor = "#FFFFFF", strokeWidth = 3))
    ) %>%
    select(id = pid, level, group, shape, icon, label, font) %>%
    distinct()
  nodes <- as.data.frame(pedigree) %>%
    mutate(id    = as.character(id),
           group = as.character(sex)) %>%
    select(id, group) %>%
    distinct() %>%
    group_by(id) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    left_join(levels, by = join_by(id)) %>%
    distinct() %>%
    group_by(id) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    mutate(color = case_when(
      group == "0" ~ colors$u,
      group == "1" ~ colors$m,
      group == "2" ~ colors$f),
      code = case_when(
        group == "0" ~ "\uf04b",
        group == "1" ~ "\uf0c8",
        group == "2" ~ "\uf111"),
      shape = "icon",
      label = id
    ) %>%
    mutate(icon = map2(color, code, \(x, y) list(code = y,
                                                 color = x,
                                                 face = "'Font Awesome 5 Free'",
                                                 weight = 700,
                                                 size = 30)),
           font = map(color, \(x) list(size = 12,
                                       color = x,
                                       strokeColor = "#FFFFFF",
                                       strokeWidth = 3))
    ) %>%
    select(id, level, group, shape, icon, label, font) %>%
    bind_rows(pairs.ndf) %>%
    distinct(id, .keep_all = TRUE) %>%
    ungroup()
  return(nodes)
}
#' @rdname ped_visnet
#' @export
ped_network <- function(pedigree, studbook) {
  edges <- pedigree_edges(pedigree, studbook) %>% distinct()
  nodes <- pedigree_nodes(pedigree, studbook) %>% distinct()
  legend <- nodes %>%
    select(group, shape, icon) %>%
    distinct() %>%
    mutate(icon = if_else(group %in% c("0", "1", "2"), icon, map(icon, \(x) locations.color(x)))) %>%
    mutate(label = case_when(
      group == "0" ~ "Sex Undet",
      group == "1" ~ "Male",
      group == "2" ~ "Female",
      .default = "Pair colored/labeled by Location"
    )) %>% distinct()
  ped.net <- visNetwork(nodes, edges, width = "100%", height = "700px") %>%
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE, fixed  = list(x = FALSE, y = FALSE))  %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE) %>%
    visLegend(addNodes = legend, ncol = 1, useGroups = FALSE)
  visSave(ped.net, file = "Pedigree_Hierarchical_.html")
  return(ped.net)
}
#' @rdname ped_visnet
#' @export
subset_network_btp <- function(pedigree.living, pair, studbook) {
  edges        <- pedigree_edges(pedigree.living, studbook) %>% distinct()
  nodes        <- pedigree_nodes(pedigree.living, studbook) %>% distinct()
  result       <- verbalise(pedigree.living, ids = pair)
  related.pair <- setdiff(pedigree.living[["ID"]],
                          c(unrelated(pedigree.living, male),
                            unrelated(pedigree.living, female))) %>% unique()
  connections <- edges %>%
    filter(to %in% c(pair, result[[1]][["v1"]], result[[1]][["v2"]])) %>%
    pull(from) %>% unique()
  paths       <- edges %>%
    filter(to %in% c(related.pair, pair) | from %in% c(related.pair, pair)) %>%
    filter(to %in% c(pair, result[[1]][["v1"]], result[[1]][["v2"]], connections)) %>%
    mutate(color  = colors$emph,
           shadow = TRUE,
           width  = 3) %>%
    distinct()
  pair.edges <- edges %>%
    filter(to %in% c(related.pair, pair) | from %in% c(related.pair, pair)) %>%
    anti_join(paths) %>%
    bind_rows(paths) %>%
    distinct()
  node.ids <- c(pull(pair.edges, from), pull(pair.edges, to)) %>% unique()
  icons.size <- function(x, group) {
    if (group == "match.m" | group == "match.f") {
      list_assign(x, size = 55, )
    } else if (group == "match.anc") {
      list_assign(x, size = 45)
    } else {
      return(x)
    }
  }
  nodes.color <- function(group) {
    if (group == "match.m" | group == "match.f" | group == "match.anc") {
      list(background = colors$emph, border = "#000000")
    } else {
      list(NULL)
    }
  }
  font.match <- function(x, group) {
    if (group == "match.m" | group == "match.f") {
      list_assign(x, size = 18, background = colors$emph)
    } else if (group == "match.anc") {
      list_assign(x, size = 18, background = colors$emph)
    } else if (group == "match.rel.m" | group == "match.rel.f") {
      list_assign(x, size = 14)
    } else {
      return(x)
    }
  }
  pair.nodes <- filter(nodes, id %in% node.ids) %>%
    mutate(
      label = case_when(
        id %in% male                 ~ "Male Partner",
        id %in% female               ~ "Female Partner",
        id %in% result[[1]][["anc"]] ~ "Shared Ancestor",
        .default = label),
      group = case_when(
        id %in% male                 ~ "match.m",
        id %in% female               ~ "match.f",
        id %in% result[[1]][["anc"]] ~ "match.anc",
        id %in% result[[1]][["v1"]]  ~ "match.rel.m",
        id %in% result[[1]][["v2"]]  ~ "match.rel.f",
        .default = group)) %>%
    mutate(icon = map2(icon, group, \(x, y) icons.size(x, y)),
           font = map2(font, group, \(x, y) font.match(x, y)),
           color= map(group, \(x) nodes.color(x)),
           borderWidth = case_when(
             group %in% c("match.m"    , "match.f")     ~ 3,
             group %in% c("match.rel.m", "match.rel.f") ~ 1.5,
             group     == "match.anc"                   ~ 2,
             .default = 1
           )) %>% distinct()
  subtitle <- paste0(male, " & ", female, " are ", result[[1]][["rel"]],
                     "<br>Path Connecting Pair: ", result[[1]][["path"]])
  pair.net <- visNetwork(pair.nodes,
                         pair.edges,
                         width   = "100%",
                         height  = "700px",
                         main    = "Relatives of Proposed Match",
                         submain = subtitle) %>%
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE, fixed  = list(x = FALSE, y = FALSE))  %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE)
  visSave(pair.net, file = "Pedigree_Network_PairLineage.html")
  return(pair.net)
}
