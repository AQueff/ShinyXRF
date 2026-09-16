
## 1. LOAD REQUIRED PACKAGES #####

required_packages <- c(
  "shiny", "readxl", "nexus", "dimensio", "isopleuros",
  "ggplot2", "plotly", "ggtern", "grid", "DT"
)

# Check and load missing packages
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

# Install missing packages
if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  message("Installing...")
  install.packages(missing_packages, dependencies = TRUE)
}

# Laod all packages
lapply(required_packages, library, character.only = TRUE)


# 2. GLOBAL CONSTANTS (colors, themes, etc.) #####

app_dark <- "#242424"
app_dark_soft <- "#3A3A3A"
app_muted <- "#6F6A63"
app_bg <- "#F7F3EF"
app_surface <- "#FFFFFF"
app_surface_soft <- "#FBFAF7"
app_border <- "#E7E2DA"
app_grid <- "#E9E2D8"
app_grid_light <- "#F2EDE7"

# 3. UTILITY FUNCTIONS ######

### --- Safe operators and helpers ####
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

###  Themes ####
theme_xrf_clean <- function(base_size = 11, base_family = "serif") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey35", size = 0.35),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "grey20", size = 0.30),
      axis.title = ggplot2::element_text(colour = "grey10", face = "plain"),
      axis.text = ggplot2::element_text(colour = "grey15"),
      plot.title = ggplot2::element_text(colour = "grey10", face = "plain", size = base_size + 1, hjust = 0.5),
      legend.title = ggplot2::element_text(colour = "grey10", face = "plain"),
      legend.text = ggplot2::element_text(colour = "grey15"),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      strip.background = ggplot2::element_rect(fill = "white", colour = "grey35", size = 0.35),
      strip.text = ggplot2::element_text(colour = "grey10", face = "plain")
    )
}

### Export helpers ####
sanitize_file_part <- function(x) {
  x <- paste(x, collapse = "_")
  x <- iconv(x, to = "ASCII//TRANSLIT")
  
  if (is.na(x)) {
    x <- "diagram"
  }
  
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  
  if (is.null(x) || !nzchar(x)) {
    "diagram"
  } else {
    substr(x, 1, 120)
  }
}

safe_export_number <- function(value, default, min_value, max_value = Inf) {
  value <- suppressWarnings(as.numeric(value))
  
  if (length(value) != 1 || !is.finite(value) || value < min_value) {
    return(default)
  }
  
  min(value, max_value)
}

safe_axis_text_size <- function(value, default = 11, min_value = 6, max_value = 32) {
  value <- suppressWarnings(as.numeric(value))
  
  if (length(value) != 1 || !is.finite(value)) {
    return(default)
  }
  
  min(max(value, min_value), max_value)
}

axis_title_size <- function(axis_text_size) {
  safe_axis_text_size(axis_text_size, default = 11) + 2
}

apply_gg_axis_text_size <- function(p = NULL, axis_text_size = 11) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  
  axis_theme <- ggplot2::theme(
    axis.text = ggplot2::element_text(size = axis_text_size),
    axis.title = ggplot2::element_text(size = axis_title_size(axis_text_size))
  )
  
  if (is.null(p)) {
    return(axis_theme)
  }
  
  p + axis_theme
}

axis_text_controls_ui <- function(id, title = "Size of text") {
  tags$div(
    class = "axis-text-panel",
    tags$span(class = "axis-text-label", title),
    tags$div(
      class = "axis-text-controls",
      actionButton(paste0(id, "_axis_text_minus"), "A−", class = "axis-text-button"),
      uiOutput(paste0(id, "_axis_text_value"), inline = TRUE),
      actionButton(paste0(id, "_axis_text_plus"), "A+", class = "axis-text-button")
    )
  )
}

safe_plot_title <- function(value, default = "Plot", max_chars = 220) {
  default <- paste(default %||% "Plot", collapse = " ")
  default <- trimws(as.character(default))
  
  if (!nzchar(default)) {
    default <- "Plot"
  }
  
  value <- paste(value %||% "", collapse = " ")
  value <- trimws(as.character(value))
  
  if (!nzchar(value)) {
    value <- default
  }
  
  substr(value, 1, max_chars)
}

safe_plot_label <- function(value, default = "Label", max_chars = 220) {
  default <- paste(default %||% "Label", collapse = " ")
  default <- trimws(as.character(default))
  
  if (!nzchar(default)) {
    default <- "Label"
  }
  
  value <- paste(value %||% "", collapse = " ")
  value <- trimws(as.character(value))
  
  if (!nzchar(value)) {
    value <- default
  }
  
  substr(value, 1, max_chars)
}

plot_label_field <- function(label_overrides, field, default = "Label") {
  if (is.list(label_overrides) && !is.null(label_overrides[[field]])) {
    return(safe_plot_label(label_overrides[[field]], default))
  }
  
  safe_plot_label(NULL, default)
}

plot_title_controls_ui <- function(id) {
  uiOutput(paste0(id, "_title_ui"))
}

plot_label_controls_ui <- function(id) {
  uiOutput(paste0(id, "_labels_ui"))
}

plot_text_controls_ui <- function(id) {
  uiOutput(paste0(id, "_text_ui"))
}

export_controls_ui <- function(id, title = "Export plot", grid_choice = FALSE) {
  tagList(
    tags$div(
      class = "export-panel",
      h5(title),
      fluidRow(
        column(
          4,
          numericInput(
            paste0(id, "_export_width"),
            "Width (inches)",
            value = 8,
            min = 2,
            max = 30,
            step = 0.5
          )
        ),
        column(
          4,
          numericInput(
            paste0(id, "_export_height"),
            "Height (inches)",
            value = 6,
            min = 2,
            max = 30,
            step = 0.5
          )
        ),
        column(
          4,
          numericInput(
            paste0(id, "_export_dpi"),
            "DPI image",
            value = 300,
            min = 72,
            max = 1200,
            step = 50
          )
        )
      ),
      if (isTRUE(grid_choice)) {
        tags$div(
          class = "export-grid-choice",
          checkboxInput(
            paste0(id, "_export_show_grid"),
            "Display grid in export",
            value = TRUE
          ),
          tags$small(
            class = "text-muted",
            "Uncheck to export with white background, without grid."
          )
        )
      },
      downloadButton(paste0("download_", id, "_pdf"), "vector PDF"),
      downloadButton(paste0("download_", id, "_png"), "PNG")
    )
  )
}

export_plot_file <- function(file, draw_fun, format = c("pdf", "png"), width = 8, height = 6, dpi = 300) {
  format <- match.arg(format)
  width <- safe_export_number(width, default = 8, min_value = 2, max_value = 30)
  height <- safe_export_number(height, default = 6, min_value = 2, max_value = 30)
  dpi <- safe_export_number(dpi, default = 300, min_value = 72, max_value = 1200)
  
  if (identical(format, "pdf")) {
    grDevices::pdf(file, width = width, height = height, onefile = FALSE, useDingbats = FALSE, family = "serif")
  } else {
    grDevices::png(file, width = width, height = height, units = "in", res = dpi)
  }
  
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_fun()
  invisible(file)
}

check_columns <- function(df, cols, where = "data") {
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s) in ", where, ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

parse_cols_spec <- function(x, n_cols) {
  x <- gsub("\\s+", "", x)
  if (is.null(x) || !nzchar(x)) {
    stop("Please fill in information columns, for example 1:3,7", call. = FALSE)
  }
  
  parts <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  idx <- integer(0)
  
  for (part in parts) {
    if (grepl("^\\d+:\\d+$", part)) {
      bounds <- as.integer(strsplit(part, ":", fixed = TRUE)[[1]])
      idx <- c(idx, seq(bounds[1], bounds[2]))
    } else if (grepl("^\\d+$", part)) {
      idx <- c(idx, as.integer(part))
    } else {
      stop("Invalid format for information columns: ", part, call. = FALSE)
    }
  }
  
  idx <- unique(idx)
  if (any(is.na(idx)) || any(idx < 1) || any(idx > n_cols)) {
    stop("Information columns must be comprised between 1 and ", n_cols, ".", call. = FALSE)
  }
  
  idx
}

safe_selected <- function(choices, wanted, min_n = 1) {
  selected <- intersect(wanted, choices)
  if (length(selected) >= min_n) {
    selected
  } else {
    head(choices, min(length(choices), max(min_n, 1)))
  }
}

### Reactive value helpers ####
remembered_selected <- function(choices, current, default = character(0), min_n = 1, max_n = Inf) {
  current <- if (is.null(current)) character(0) else as.character(current)
  current <- current[!is.na(current) & nzchar(current)]
  
  selected <- intersect(current, choices)
  
  if (length(selected) < min_n) {
    selected <- safe_selected(choices, default, min_n = min_n)
  }
  
  if (is.finite(max_n)) {
    selected <- head(selected, max_n)
  }
  
  selected
}
remembered_choice <- function(choices, current, default) {
  selected <- remembered_selected(choices, current, default = default, min_n = 1, max_n = 1)
  
  if (length(selected) == 0) {
    default
  } else {
    selected[[1]]
  }
}
remembered_bool <- function(current, default = FALSE) {
  if (is.null(current)) {
    default
  } else {
    isTRUE(current)
  }
}
remembered_number <- function(current, default) {
  value <- suppressWarnings(as.numeric(current))
  
  if (length(value) != 1 || !is.finite(value)) {
    default
  } else {
    value
  }
}

numeric_like_columns <- function(df, exclude = character(0), min_non_missing = 1) {
  candidates <- setdiff(colnames(df), exclude)
  
  candidates[vapply(candidates, function(nm) {
    x <- suppressWarnings(as.numeric(df[[nm]]))
    sum(is.finite(x), na.rm = TRUE) >= min_non_missing
  }, logical(1))]
}

### Palette helpers ####
make_group_palette <- function(groups, palette_name = "Okabe-Ito") {
  group_levels <- levels(droplevels(as.factor(groups)))
  n_groups <- length(group_levels)
  
  if (n_groups == 0) {
    return(character(0))
  }
  
  if (is.null(palette_name) || !nzchar(palette_name)) {
    palette_name <- "Okabe-Ito"
  }

  make_infinite_hcl <- function(n) {
    hues <- seq(15, 375, length.out = n + 1)[seq_len(n)]
    grDevices::hcl(h = hues, c = 85, l = 55, fixup = TRUE)
  }
  
  if (palette_name %in% c("Infinie (HCL)")) {
    colours <- make_infinite_hcl(n_groups)
  } else if (palette_name %in% c("rainbow", "Rainbow")) {
    colours <- grDevices::rainbow(n_groups)
  } else {
    colours <- tryCatch(
      grDevices::palette.colors(n_groups, palette = palette_name),
      error = function(e) make_infinite_hcl(n_groups)
    )
  }
  
  names(colours) <- group_levels
  colours
}

make_group_shapes <- function(groups, shape_palette_name = "Classic") {
  group_levels <- levels(droplevels(as.factor(groups)))
  n_groups <- length(group_levels)
  
  if (n_groups == 0) {
    return(integer(0))
  }
  
  shape_palettes <- list(
    "Classic" = c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, 5, 6, 7, 9, 10, 11, 12, 13, 14),
    "Open shapes" = c(1, 2, 0, 5, 6, 7, 9, 10, 11, 12, 13, 14),
    "Black and white mix" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
  )
  
  if (!shape_palette_name %in% names(shape_palettes)) {
    shape_palette_name <- "Classic"
  }
  
  shapes <- rep(shape_palettes[[shape_palette_name]], length.out = n_groups)
  names(shapes) <- group_levels
  shapes
}

### Compositional data helpers ####
replace_lod <- function(df, factor = 0.65) {
  out <- as.data.frame(df, check.names = FALSE)
  
  for (nm in names(out)) {
    x <- suppressWarnings(as.numeric(out[[nm]]))
    x[x <= 0] <- NA_real_
    
    min_x <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.finite(min_x)) {
      x[is.na(x)] <- min_x * factor
    }
    
    out[[nm]] <- x
  }
  
  out
}

as_clr_matrix <- function(clr_object) {
  if (isS4(clr_object) && ".Data" %in% slotNames(clr_object)) {
    return(clr_object@.Data)
  }
  
  as.matrix(clr_object)
}

make_selected_clr_matrix <- function(df, elements, group_col) {
  if (length(elements) < 3) {
    warning("For a PCA, one should choose at least 3 elements.")
  }
  
  check_columns(df, c(group_col, elements))
  
  selected_data <- df[, c(group_col, elements), drop = FALSE]
  selected_compo <- nexus::as_composition(selected_data, groups = 1)
  selected_clr <- nexus::transform_clr(selected_compo)
  
  clr_matrix <- as_clr_matrix(selected_clr)
  if (is.null(colnames(clr_matrix))) {
    colnames(clr_matrix) <- elements
  }
  
  clr_matrix
}

axis_terms <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

axis_spec <- function(numerator, denominator = character(0)) {
  numerator <- axis_terms(numerator)
  denominator <- axis_terms(denominator)
  
  if (length(numerator) == 0) {
    return(character(0))
  }
  
  list(
    numerator = numerator,
    denominator = denominator
  )
}

axis_spec_columns <- function(variable_spec) {
  if (is.list(variable_spec)) {
    return(axis_terms(c(variable_spec$numerator, variable_spec$denominator)))
  }
  
  axis_terms(variable_spec)
}

axis_part_sum <- function(df, columns, part_name = "numérateur") {
  columns <- axis_terms(columns)
  
  if (length(columns) == 0) {
    stop("The ", part_name, " must contain at least one variable.", call. = FALSE)
  }
  
  check_columns(df, columns)
  
  values_matrix <- as.data.frame(
    lapply(columns, function(nm) suppressWarnings(as.numeric(df[[nm]]))),
    check.names = FALSE
  )
  
  values <- rowSums(values_matrix, na.rm = FALSE)
  
  label <- if (length(columns) == 1) {
    columns[1]
  } else {
    paste0("(", paste(columns, collapse = " + "), ")")
  }
  
  list(values = values, label = label)
}

make_element_or_ratio <- function(df, variable_spec) {
  if (is.list(variable_spec)) {
    numerator <- axis_terms(variable_spec$numerator)
    denominator <- axis_terms(variable_spec$denominator)
  } else {
    variable_spec <- axis_terms(variable_spec)
    
    if (!length(variable_spec) %in% c(1, 2)) {
      stop(
        "Each axis must be defined for one variable, a sum of variables, ",
        "or a ratio like (A+B)/(C+D).",
        call. = FALSE
      )
    }
    
    numerator <- variable_spec[1]
    denominator <- if (length(variable_spec) == 2) variable_spec[2] else character(0)
  }
  
  numerator_part <- axis_part_sum(df, numerator, part_name = "numerator")
  
  if (length(denominator) == 0) {
    values <- numerator_part$values
    label <- numerator_part$label
  } else {
    denominator_part <- axis_part_sum(df, denominator, part_name = "denominator")
    
    if (any(denominator_part$values == 0, na.rm = TRUE)) {
      warning(
        "Denominator contains at least one 0: ",
        paste(denominator, collapse = " + ")
      )
    }
    
    values <- numerator_part$values / denominator_part$values
    label <- paste(numerator_part$label, denominator_part$label, sep = "/")
  }
  
  list(values = values, label = label)
}

### Ternary plot helpers ####
ternary_axis_spec <- function(numerator, denominator_type = "none", denominator_element = "", denominator_number = 1) {
  list(
    numerator = numerator,
    denominator_type = denominator_type,
    denominator_element = denominator_element,
    denominator_number = denominator_number
  )
}
make_ternary_axis_variable <- function(df, spec) {
  numerator <- spec$numerator
  denominator_type <- spec$denominator_type
  denominator_element <- spec$denominator_element
  denominator_number <- spec$denominator_number
  
  if (is.null(numerator) || !nzchar(numerator)) {
    stop("Each summit must have a numerator element.", call. = FALSE)
  }
  
  if (is.null(denominator_type) || !nzchar(denominator_type)) {
    denominator_type <- "none"
  }
  
  check_columns(df, numerator)
  values <- as.numeric(df[[numerator]])
  label <- numerator
  
  if (denominator_type == "element") {
    if (is.null(denominator_element) || !nzchar(denominator_element)) {
      stop("Choose an element in the denominator ", numerator, ".", call. = FALSE)
    }
    
    check_columns(df, denominator_element)
    denominator <- as.numeric(df[[denominator_element]])
    
    if (any(denominator == 0, na.rm = TRUE)) {
      warning("Denominator contains at least one 0: ", denominator_element)
    }
    
    values <- values / denominator
    label <- paste(numerator, denominator_element, sep = "/")
  } else if (denominator_type == "number") {
    divisor <- suppressWarnings(as.numeric(denominator_number))
    
    if (length(divisor) != 1 || is.na(divisor) || !is.finite(divisor) || divisor <= 0) {
      stop("Numerical divided of ", numerator, " must be positive.", call. = FALSE)
    }
    
    values <- values / divisor
    label <- paste0(numerator, "/", format(divisor, scientific = FALSE, trim = TRUE))
  } else if (denominator_type != "none") {
    stop("Unknown type of denominator: ", denominator_type, call. = FALSE)
  }
  
  list(values = values, label = label)
}

### CLR Biplot ####

plot_clr_biplot <- function(
    clr_matrix,
    ids,
    groups,
    title = "CLR biplot",
    axes = c(1, 2),
    colour_label = "Group",
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    label_overrides = NULL,
    show_confidence_ellipses = FALSE
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  title <- safe_plot_title(title, "CLR biplot")
  pca <- prcomp(clr_matrix, center = TRUE, scale. = FALSE)
  var_pca <- pca$sdev^2 / sum(pca$sdev^2)
  
  axis_x <- axes[1]
  axis_y <- axes[2]
  x_name <- paste0("PC", axis_x)
  y_name <- paste0("PC", axis_y)
  x_default_label <- paste0(x_name, " (", round(var_pca[axis_x] * 100, 1), " % )")
  y_default_label <- paste0(y_name, " (", round(var_pca[axis_y] * 100, 1), " % )")
  x_axis_label <- plot_label_field(label_overrides, "x", x_default_label)
  y_axis_label <- plot_label_field(label_overrides, "y", y_default_label)
  legend_title <- plot_label_field(label_overrides, "legend", colour_label)
  
  individuals <- data.frame(
    x = as.numeric(pca$x[, axis_x]),
    y = as.numeric(pca$x[, axis_y]),
    Sample = ids,
    Group = as.factor(groups),
    stringsAsFactors = FALSE
  )
  
  individuals$hover_text <- paste0(
    "Sample: ", individuals$Sample,
    "<br>", x_axis_label, ": ", signif(individuals$x, 4),
    "<br>", y_axis_label, ": ", signif(individuals$y, 4),
    "<br>", legend_title, ": ", individuals$Group
  )
  
  variables <- data.frame(
    x = as.numeric(pca$rotation[, axis_x]),
    y = as.numeric(pca$rotation[, axis_y]),
    Element = rownames(pca$rotation),
    stringsAsFactors = FALSE
  )
  
  scale_x <- max(abs(individuals$x), na.rm = TRUE) / max(abs(variables$x), na.rm = TRUE)
  scale_y <- max(abs(individuals$y), na.rm = TRUE) / max(abs(variables$y), na.rm = TRUE)
  arrow_scale <- 0.8 * min(scale_x, scale_y)
  
  if (!is.finite(arrow_scale) || arrow_scale <= 0) {
    arrow_scale <- 1
  }
  
  variables$xend <- variables$x * arrow_scale
  variables$yend <- variables$y * arrow_scale
  
  p <- ggplot(individuals, aes(x = x, y = y, text = hover_text)) +
    geom_hline(yintercept = 0, colour = "grey70", size = 0.25) +
    geom_vline(xintercept = 0, colour = "grey70", size = 0.25)
  
  if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(individuals$Group, shape_palette_name = shape_palette_name)
    p <- p +
      geom_point(aes(shape = Group), colour = "grey10", fill = "white", size = 1.8, alpha = 0.95) +
      scale_shape_manual(values = group_shapes, na.translate = FALSE, drop = FALSE) +
      labs(shape = legend_title)
  } else {
    group_palette <- make_group_palette(individuals$Group, palette_name = palette_name)
    group_colors <- unname(group_palette)  
    
    p <- p +
      geom_point(aes(colour = Group, fill = Group), size = 1.8, alpha = 0.95) +
      scale_colour_manual(values = group_palette, na.translate = FALSE, drop = FALSE) +
      scale_fill_manual(values = group_colors, na.translate = FALSE, drop = FALSE) + 
      labs(colour = legend_title, fill = legend_title) 
  }
  
  if (isTRUE(show_confidence_ellipses)) {
    p <- p +
      stat_ellipse(
        data = individuals,
        aes(x = x, y = y, group = Group, fill = Group, colour = Group),
        level = 0.95,
        alpha = 0.8,  
        size = 0.5    
      )
  }
  
  
  p +
    geom_segment(
      data = variables,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.2, "cm")),
      colour = "grey20"
    ) +
    geom_text(
      data = variables,
      aes(x = xend, y = yend, label = Element),
      inherit.aes = FALSE,
      nudge_x = 0.08,
      nudge_y = 0.08,
      size = 3
    ) +
    coord_fixed() +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label
    )
}

### ALR Biplot ####
plot_alr_biplot <- function(
    alr_matrix,
    ids,
    groups,
    title = "ALR biplot",
    axes = c(1, 2),
    colour_label = "Group",
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    label_overrides = NULL,
    show_confidence_ellipses = FALSE
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  title <- safe_plot_title(title, "ALR biplot")
  pca <- prcomp(alr_matrix, center = TRUE, scale. = FALSE)
  var_pca <- pca$sdev^2 / sum(pca$sdev^2)
  
  axis_x <- axes[1]
  axis_y <- axes[2]
  x_name <- paste0("PC", axis_x)
  y_name <- paste0("PC", axis_y)
  x_default_label <- paste0(x_name, " (", round(var_pca[axis_x] * 100, 1), " % )")
  y_default_label <- paste0(y_name, " (", round(var_pca[axis_y] * 100, 1), " % )")
  x_axis_label <- plot_label_field(label_overrides, "x", x_default_label)
  y_axis_label <- plot_label_field(label_overrides, "y", y_default_label)
  legend_title <- plot_label_field(label_overrides, "legend", colour_label)
  
  individuals <- data.frame(
    x = as.numeric(pca$x[, axis_x]),
    y = as.numeric(pca$x[, axis_y]),
    Sample = ids,
    Group = as.factor(groups),
    stringsAsFactors = FALSE
  )
  
  individuals$hover_text <- paste0(
    "Sample: ", individuals$Sample,
    "<br>", x_axis_label, ": ", signif(individuals$x, 4),
    "<br>", y_axis_label, ": ", signif(individuals$y, 4),
    "<br>", legend_title, ": ", individuals$Group
  )
  
  variables <- data.frame(
    x = as.numeric(pca$rotation[, axis_x]),
    y = as.numeric(pca$rotation[, axis_y]),
    Element = rownames(pca$rotation),
    stringsAsFactors = FALSE
  )
  
  scale_x <- max(abs(individuals$x), na.rm = TRUE) / max(abs(variables$x), na.rm = TRUE)
  scale_y <- max(abs(individuals$y), na.rm = TRUE) / max(abs(variables$y), na.rm = TRUE)
  arrow_scale <- 0.8 * min(scale_x, scale_y)
  
  if (!is.finite(arrow_scale) || arrow_scale <= 0) {
    arrow_scale <- 1
  }
  
  variables$xend <- variables$x * arrow_scale
  variables$yend <- variables$y * arrow_scale
  
  p <- ggplot(individuals, aes(x = x, y = y, text = hover_text)) +
    geom_hline(yintercept = 0, colour = "grey70", size = 0.25) +
    geom_vline(xintercept = 0, colour = "grey70", size = 0.25)
  
  if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(individuals$Group, shape_palette_name = shape_palette_name)
    p <- p +
      geom_point(aes(shape = Group), colour = "grey10", fill = "white", size = 1.8, alpha = 0.95) +
      scale_shape_manual(values = group_shapes, na.translate = FALSE, drop = FALSE) +
      labs(shape = legend_title)
  } else {
    group_palette <- make_group_palette(individuals$Group, palette_name = palette_name)
    group_colors <- unname(group_palette)
    
    p <- p +
      geom_point(aes(colour = Group, fill = Group), size = 1.8, alpha = 0.95) +
      scale_colour_manual(values = group_palette, na.translate = FALSE, drop = FALSE) +
      scale_fill_manual(values = group_colors, na.translate = FALSE, drop = FALSE) +
      labs(colour = legend_title, fill = legend_title)
  }
  
  if (isTRUE(show_confidence_ellipses)) {
    p <- p +
      stat_ellipse(
        data = individuals,
        aes(x = x, y = y, group = Group, fill = Group, colour = Group),
        level = 0.95,
        alpha = 0.8,
        size = 0.5
      )
  }
  
  p +
    geom_segment(
      data = variables,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.2, "cm")),
      colour = "grey20"
    ) +
    geom_text(
      data = variables,
      aes(x = xend, y = yend, label = Element),
      inherit.aes = FALSE,
      nudge_x = 0.08,
      nudge_y = 0.08,
      size = 3
    ) +
    coord_fixed() +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label
    )
}

### ILR Biplot ####
plot_ilr_biplot <- function(
    ilr_matrix,
    ids,
    groups,
    title = "ILR biplot",
    axes = c(1, 2),
    colour_label = "Group",
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    label_overrides = NULL,
    show_confidence_ellipses = FALSE
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  title <- safe_plot_title(title, "ILR biplot")
  pca <- prcomp(ilr_matrix, center = TRUE, scale. = FALSE)
  var_pca <- pca$sdev^2 / sum(pca$sdev^2)
  
  axis_x <- axes[1]
  axis_y <- axes[2]
  x_name <- paste0("PC", axis_x)
  y_name <- paste0("PC", axis_y)
  x_default_label <- paste0(x_name, " (", round(var_pca[axis_x] * 100, 1), " % )")
  y_default_label <- paste0(y_name, " (", round(var_pca[axis_y] * 100, 1), " % )")
  x_axis_label <- plot_label_field(label_overrides, "x", x_default_label)
  y_axis_label <- plot_label_field(label_overrides, "y", y_default_label)
  legend_title <- plot_label_field(label_overrides, "legend", colour_label)
  
  individuals <- data.frame(
    x = as.numeric(pca$x[, axis_x]),
    y = as.numeric(pca$x[, axis_y]),
    Sample = ids,
    Group = as.factor(groups),
    stringsAsFactors = FALSE
  )
  
  individuals$hover_text <- paste0(
    "Sample: ", individuals$Sample,
    "<br>", x_axis_label, ": ", signif(individuals$x, 4),
    "<br>", y_axis_label, ": ", signif(individuals$y, 4),
    "<br>", legend_title, ": ", individuals$Group
  )
  
  variables <- data.frame(
    x = as.numeric(pca$rotation[, axis_x]),
    y = as.numeric(pca$rotation[, axis_y]),
    Element = rownames(pca$rotation),
    stringsAsFactors = FALSE
  )
  
  scale_x <- max(abs(individuals$x), na.rm = TRUE) / max(abs(variables$x), na.rm = TRUE)
  scale_y <- max(abs(individuals$y), na.rm = TRUE) / max(abs(variables$y), na.rm = TRUE)
  arrow_scale <- 0.8 * min(scale_x, scale_y)
  
  if (!is.finite(arrow_scale) || arrow_scale <= 0) {
    arrow_scale <- 1
  }
  
  variables$xend <- variables$x * arrow_scale
  variables$yend <- variables$y * arrow_scale
  
  p <- ggplot(individuals, aes(x = x, y = y, text = hover_text)) +
    geom_hline(yintercept = 0, colour = "grey70", size = 0.25) +
    geom_vline(xintercept = 0, colour = "grey70", size = 0.25)
  
  if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(individuals$Group, shape_palette_name = shape_palette_name)
    p <- p +
      geom_point(aes(shape = Group), colour = "grey10", fill = "white", size = 1.8, alpha = 0.95) +
      scale_shape_manual(values = group_shapes, na.translate = FALSE, drop = FALSE) +
      labs(shape = legend_title)
  } else {
    group_palette <- make_group_palette(individuals$Group, palette_name = palette_name)
    group_colors <- unname(group_palette)
    
    p <- p +
      geom_point(aes(colour = Group, fill = Group), size = 1.8, alpha = 0.95) +
      scale_colour_manual(values = group_palette, na.translate = FALSE, drop = FALSE) +
      scale_fill_manual(values = group_colors, na.translate = FALSE, drop = FALSE) +
      labs(colour = legend_title, fill = legend_title)
  }
  
  if (isTRUE(show_confidence_ellipses)) {
    p <- p +
      stat_ellipse(
        data = individuals,
        aes(x = x, y = y, group = Group, fill = Group, colour = Group),
        level = 0.95,
        alpha = 0.8,
        size = 0.5
      )
  }
  
  p +
    geom_segment(
      data = variables,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.2, "cm")),
      colour = "grey20"
    ) +
    geom_text(
      data = variables,
      aes(x = xend, y = yend, label = Element),
      inherit.aes = FALSE,
      nudge_x = 0.08,
      nudge_y = 0.08,
      size = 3
    ) +
    coord_fixed() +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label
    )
}

### Set 2D plot ####
valid_axis_step <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  
  if (length(value) != 1 || !is.finite(value) || value <= 0) {
    NA_real_
  } else {
    value
  }
}

plot2d_nice_step <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  
  if (length(value) != 1 || !is.finite(value) || value <= 0) {
    return(NA_real_)
  }
  
  exponent <- floor(log10(value))
  base <- 10^exponent
  fraction <- value / base
  
  nice_fraction <- if (fraction <= 1) {
    1
  } else if (fraction <= 2) {
    2
  } else if (fraction <= 2.5) {
    2.5
  } else if (fraction <= 5) {
    5
  } else {
    10
  }
  
  nice_fraction * base
}

plot2d_axis_data_range <- function(values) {
  values <- values[is.finite(values)]
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  data_range <- range(values, na.rm = TRUE)
  
  if (!is.finite(diff(data_range)) || diff(data_range) <= 0) {
    padding <- max(abs(data_range[1]) * 0.05, 1)
    data_range <- data_range + c(-padding, padding)
  }
  
  data_range
}

plot2d_breaks_from_step <- function(values, requested_step, max_breaks = 12) {
  step <- valid_axis_step(requested_step)
  data_range <- plot2d_axis_data_range(values)
  
  empty <- list(
    breaks = NULL,
    step = NA_real_,
    limits = data_range,
    adjusted = FALSE
  )
  
  if (!is.finite(step) || is.null(data_range)) {
    return(empty)
  }
  
  make_breaks <- function(current_step) {
    lower <- floor(data_range[1] / current_step) * current_step
    upper <- ceiling(data_range[2] / current_step) * current_step
    
    if (!is.finite(lower) || !is.finite(upper) || lower > upper) {
      return(NULL)
    }
    
    seq(lower, upper, by = current_step)
  }
  
  breaks <- make_breaks(step)
  current_step <- step
  adjusted <- FALSE
  
  if (!is.null(breaks) && length(breaks) > max_breaks) {
    target_step <- plot2d_nice_step(diff(data_range) / max(1, max_breaks - 1))
    
    if (is.finite(target_step)) {
      current_step <- max(step, target_step)
      breaks <- make_breaks(current_step)
      adjusted <- TRUE
    }
  }
  
  if (is.null(breaks) || length(breaks) == 0 || length(breaks) > max_breaks + 5) {
    breaks <- pretty(data_range, n = max_breaks)
    breaks <- breaks[breaks >= data_range[1] & breaks <= data_range[2]]
    
    if (length(breaks) >= 2) {
      current_step <- stats::median(diff(breaks))
    }
    
    adjusted <- TRUE
  }
  
  limits <- range(c(data_range, breaks), na.rm = TRUE)
  
  list(
    breaks = breaks,
    step = current_step,
    limits = limits,
    adjusted = adjusted
  )
}

plot2d_remove_major_breaks <- function(minor_breaks, major_breaks) {
  if (is.null(minor_breaks) || is.null(major_breaks)) {
    return(minor_breaks)
  }
  
  keep <- vapply(minor_breaks, function(x) {
    !any(abs(x - major_breaks) <= sqrt(.Machine$double.eps) * pmax(1, abs(x), abs(major_breaks)))
  }, logical(1))
  
  minor_breaks[keep]
}

plot2d_tick_settings <- function(mode = "auto", x_major = NA, x_minor = NA, y_major = NA, y_minor = NA) {
  mode <- mode %||% "auto"
  
  if (identical(mode, "major_1")) {
    return(list(mode = "custom", x_major = 1, x_minor = NA, y_major = 1, y_minor = NA))
  }
  
  if (identical(mode, "major_5_minor_1")) {
    return(list(mode = "custom", x_major = 5, x_minor = 1, y_major = 5, y_minor = 1))
  }
  
  if (identical(mode, "custom")) {
    return(list(
      mode = "custom",
      x_major = valid_axis_step(x_major),
      x_minor = valid_axis_step(x_minor),
      y_major = valid_axis_step(y_major),
      y_minor = valid_axis_step(y_minor)
    ))
  }
  
  list(mode = "auto", x_major = NA, x_minor = NA, y_major = NA, y_minor = NA)
}

plot2d_axis_breaks_for_axis <- function(
    values,
    major_step,
    minor_step = NA,
    max_major_breaks = 14,
    max_minor_breaks = 80
) {
  values <- values[is.finite(values)]
  
  if (length(values) == 0) {
    return(list(major = NULL, minor = NULL, limits = NULL, major_step = NA_real_, minor_step = NA_real_))
  }
  
  major_info <- plot2d_breaks_from_step(values, major_step, max_breaks = max_major_breaks)
  major_breaks <- major_info$breaks
  
  minor_breaks <- NULL
  effective_minor_step <- NA_real_
  minor_step <- valid_axis_step(minor_step)
  
  if (is.finite(minor_step) && !is.null(major_breaks) && length(major_breaks) > 0) {
    minor_info <- plot2d_breaks_from_step(values, minor_step, max_breaks = max_minor_breaks)
    
    if (!is.null(minor_info$breaks) && length(minor_info$breaks) > 0) {
      effective_minor_step <- minor_info$step
      
      if (is.finite(effective_minor_step) &&
          is.finite(major_info$step) &&
          effective_minor_step < major_info$step) {
        minor_breaks <- plot2d_remove_major_breaks(minor_info$breaks, major_breaks)
      }
    }
  }
  
  limits <- range(c(values, major_breaks, minor_breaks), na.rm = TRUE)
  
  if (!all(is.finite(limits)) || diff(limits) <= 0) {
    limits <- plot2d_axis_data_range(values)
  }
  
  list(
    major = major_breaks,
    minor = minor_breaks,
    limits = limits,
    major_step = major_info$step,
    minor_step = effective_minor_step
  )
}

plot2d_axis_breaks_for_plot <- function(plot_df, axis_breaks = NULL) {
  if (is.null(axis_breaks) || !identical(axis_breaks$mode, "custom") || nrow(plot_df) == 0) {
    return(NULL)
  }
  
  list(
    x = plot2d_axis_breaks_for_axis(plot_df$x_value, axis_breaks$x_major, axis_breaks$x_minor),
    y = plot2d_axis_breaks_for_axis(plot_df$y_value, axis_breaks$y_major, axis_breaks$y_minor)
  )
}

apply_plot2d_axis_breaks <- function(p, plot_df, axis_breaks = NULL) {
  breaks <- plot2d_axis_breaks_for_plot(plot_df, axis_breaks)
  
  if (is.null(breaks)) {
    return(p)
  }
  
  if (!is.null(breaks$x$major) && length(breaks$x$major) > 0) {
    p <- p + scale_x_continuous(
      breaks = breaks$x$major,
      labels = plot2d_format_axis_labels(breaks$x$major),
      minor_breaks = breaks$x$minor,
      limits = breaks$x$limits,
      expand = ggplot2::expansion(mult = 0.02),
      guide = plot2d_axis_guide()
    )
  }
  
  if (!is.null(breaks$y$major) && length(breaks$y$major) > 0) {
    p <- p + scale_y_continuous(
      breaks = breaks$y$major,
      labels = plot2d_format_axis_labels(breaks$y$major),
      minor_breaks = breaks$y$minor,
      limits = breaks$y$limits,
      expand = ggplot2::expansion(mult = 0.02),
      guide = plot2d_axis_guide()
    )
  }
  
  p <- p +
    theme(
      axis.ticks.length = grid::unit(4, "pt"),
      axis.ticks = ggplot2::element_line(colour = "grey20", size = 0.30),
      panel.grid.major = ggplot2::element_line(colour = "#D0C7BC", size = 0.28)
    )
  
  if (plot2d_has_minor_breaks(breaks)) {
    p <- p + theme(
      panel.grid.minor = ggplot2::element_line(colour = app_grid_light, size = 0.18)
    )
  }
  
  p
}

plot2d_format_axis_labels <- function(values) {
  values <- as.numeric(values)
  
  if (length(values) == 0) {
    return(character(0))
  }
  
  if (all(abs(values - round(values)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
    return(formatC(values, format = "f", digits = 0))
  }
  
  format(values, trim = TRUE, scientific = FALSE)
}

plot2d_axis_guide <- function() {
  guide_formals <- names(formals(ggplot2::guide_axis))
  
  if ("minor.ticks" %in% guide_formals) {
    ggplot2::guide_axis(minor.ticks = TRUE)
  } else {
    ggplot2::guide_axis()
  }
}

plot2d_has_minor_breaks <- function(breaks) {
  (!is.null(breaks$x$minor) && length(breaks$x$minor) > 0) ||
    (!is.null(breaks$y$minor) && length(breaks$y$minor) > 0)
}

plotly_axis_options_from_plot2d_breaks <- function(axis_breaks, values, axis = c("x", "y")) {
  axis <- match.arg(axis)
  
  if (is.null(axis_breaks) || !identical(axis_breaks$mode, "custom")) {
    return(NULL)
  }
  
  major_step <- if (identical(axis, "x")) axis_breaks$x_major else axis_breaks$y_major
  minor_step <- if (identical(axis, "x")) axis_breaks$x_minor else axis_breaks$y_minor
  axis_info <- plot2d_axis_breaks_for_axis(values, major_step, minor_step)
  
  axis_options <- list(
    ticks = "outside",
    showgrid = TRUE,
    gridcolor = "#D0C7BC",
    gridwidth = 1
  )
  
  if (!is.null(axis_info$limits) && all(is.finite(axis_info$limits))) {
    axis_options$range <- axis_info$limits
  }
  
  if (!is.null(axis_info$major) && length(axis_info$major) > 0) {
    axis_options$tickmode <- "array"
    axis_options$tickvals <- axis_info$major
    axis_options$ticktext <- plot2d_format_axis_labels(axis_info$major)
  }
  
  if (!is.null(axis_info$minor) &&
      length(axis_info$minor) > 0 &&
      is.finite(axis_info$minor_step)) {
    axis_options$minor <- list(
      ticks = "outside",
      tick0 = min(axis_info$minor, na.rm = TRUE),
      dtick = axis_info$minor_step,
      ticklen = 3,
      showgrid = FALSE
    )
  }
  
  axis_options
}

plotly_axis_text_options <- function(axis_text_size, axis_options = NULL, title = NULL) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  
  font_options <- list(
    tickfont = list(size = axis_text_size, family = "serif", color = "#1A1A1A"),
    titlefont = list(size = axis_title_size(axis_text_size), family = "serif", color = "#1A1A1A")
  )
  
  if (!is.null(title)) {
    font_options$title <- list(
      text = safe_plot_label(title, "Axis"),
      font = list(size = axis_title_size(axis_text_size), family = "serif", color = "#1A1A1A")
    )
  }
  
  utils::modifyList(axis_options %||% list(), font_options)
}

plotly_legend_options <- function(title = "Legend", axis_text_size = 11) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  
  list(
    title = list(
      text = safe_plot_label(title, "Legend"),
      font = list(size = axis_title_size(axis_text_size), family = "serif", color = "#1A1A1A")
    ),
    font = list(size = axis_text_size, family = "serif", color = "#1A1A1A")
  )
}

plotly_title_options <- function(title, axis_text_size = 11) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  
  list(
    text = safe_plot_title(title, "Plot"),
    x = 0.5,
    xanchor = "center",
    font = list(
      family = "serif",
      color = "#1A1A1A",
      size = axis_title_size(axis_text_size) + 1
    )
  )
}

make_plot2d_r2_info <- function(df, x, y, log_axes = FALSE) {
  x_var <- make_element_or_ratio(df, x)
  y_var <- make_element_or_ratio(df, y)
  
  r2_df <- data.frame(
    x_value = x_var$values,
    y_value = y_var$values,
    stringsAsFactors = FALSE
  )
  
  valid_rows <- is.finite(r2_df$x_value) & is.finite(r2_df$y_value)
  
  if (isTRUE(log_axes)) {
    valid_rows <- valid_rows & r2_df$x_value > 0 & r2_df$y_value > 0
    r2_name <- "R² log-log"
  } else {
    r2_name <- "R²"
  }
  
  r2_df <- r2_df[valid_rows, , drop = FALSE]
  
  if (nrow(r2_df) < 2) {
    label <- paste0(r2_name, " non calculable")
    return(list(
      label = label,
      label_html = gsub("\n", "<br>", label, fixed = TRUE),
      r2 = NA_real_,
      n = nrow(r2_df)
    ))
  }
  
  if (isTRUE(log_axes)) {
    fit_x <- log10(r2_df$x_value)
    fit_y <- log10(r2_df$y_value)
  } else {
    fit_x <- r2_df$x_value
    fit_y <- r2_df$y_value
  }
  
  complete_r2 <- complete.cases(fit_x, fit_y)
  fit_x <- fit_x[complete_r2]
  fit_y <- fit_y[complete_r2]
  
  if (length(fit_x) >= 2 &&
      stats::sd(fit_x) > 0 &&
      stats::sd(fit_y) > 0) {
    fit <- stats::lm(fit_y ~ fit_x)
    r2_value <- summary(fit)$r.squared
    label <- paste0(
      r2_name, " = ", formatC(r2_value, format = "f", digits = 3),
      "\nn = ", length(fit_x)
    )
  } else {
    r2_value <- NA_real_
    label <- paste0(r2_name, " non calculable")
  }
  
  list(
    label = label,
    label_html = gsub("\n", "<br>", label, fixed = TRUE),
    r2 = r2_value,
    n = length(fit_x)
  )
}

plot_2d_elements <- function(
    df,
    x,
    y,
    group,
    label = NULL,
    log_axes = FALSE,
    point_size = 2,
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    line_only = FALSE,
    show_r2 = FALSE,
    axis_breaks = NULL,
    axis_text_size = 11,
    title = NULL,
    label_overrides = NULL
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  check_columns(df, c(group, axis_spec_columns(x), axis_spec_columns(y)))
  
  if (!is.null(label)) {
    check_columns(df, label)
  }
  
  x_var <- make_element_or_ratio(df, x)
  y_var <- make_element_or_ratio(df, y)
  x_axis_label <- plot_label_field(label_overrides, "x", x_var$label)
  y_axis_label <- plot_label_field(label_overrides, "y", y_var$label)
  legend_title <- plot_label_field(label_overrides, "legend", group)
  
  plot_df <- data.frame(
    x_value = x_var$values,
    y_value = y_var$values,
    Group = as.factor(df[[group]]),
    Label = if (is.null(label)) seq_len(nrow(df)) else df[[label]],
    Point_order = seq_len(nrow(df)),
    stringsAsFactors = FALSE
  )
  
  valid_rows <- is.finite(plot_df$x_value) &
    is.finite(plot_df$y_value)
  
  if (!isTRUE(line_only)) {
    valid_rows <- valid_rows & !is.na(plot_df$Group)
  }
  
  plot_df <- plot_df[valid_rows, , drop = FALSE]
  plot_df <- plot_df[order(plot_df$Point_order), , drop = FALSE]
  
  if (isTRUE(line_only)) {
    plot_df$hover_text <- paste0(
      "Sample: ", plot_df$Label,
      "<br>", x_axis_label, ": ", signif(plot_df$x_value, 4),
      "<br>", y_axis_label, ": ", signif(plot_df$y_value, 4)
    )
  } else {
    plot_df$hover_text <- paste0(
      "Sample: ", plot_df$Label,
      "<br>", x_axis_label, ": ", signif(plot_df$x_value, 4),
      "<br>", y_axis_label, ": ", signif(plot_df$y_value, 4),
      "<br>", legend_title, ": ", plot_df$Group
    )
  }
  
  plot_title <- safe_plot_title(title, paste("Plot of", y_var$label, "as a function of", x_var$label))
  
  p <- ggplot(plot_df, aes(x = x_value, y = y_value, text = hover_text)) +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(
      x = x_axis_label,
      y = y_axis_label,
      title = plot_title
    )
  
  if (isTRUE(line_only)) {
    p <- p +
      geom_path(aes(group = 1), colour = "black", size = 0.45, alpha = 1) +
      theme(legend.position = "none")
  } else if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(plot_df$Group, shape_palette_name = shape_palette_name)
    p <- p +
      geom_point(aes(shape = Group), colour = "grey10", fill = "white", size = point_size, alpha = 0.95) +
      scale_shape_manual(values = group_shapes, na.translate = FALSE, drop = FALSE) +
      labs(shape = legend_title)
  } else {
    group_palette <- make_group_palette(plot_df$Group, palette_name = palette_name)
    p <- p +
      geom_point(aes(colour = Group), size = point_size, alpha = 0.95) +
      scale_colour_manual(values = group_palette, na.translate = FALSE, drop = FALSE) +
      labs(colour = legend_title)
  }
  
  if (isTRUE(show_r2)) {
    regression_df <- plot_df
    
    if (isTRUE(log_axes)) {
      regression_df <- regression_df[regression_df$x_value > 0 & regression_df$y_value > 0, , drop = FALSE]
      fit_x <- log10(regression_df$x_value)
      fit_y <- log10(regression_df$y_value)
      r2_name <- "R² log-log"
    } else {
      fit_x <- regression_df$x_value
      fit_y <- regression_df$y_value
      r2_name <- "R²"
    }
    
    complete_r2 <- complete.cases(fit_x, fit_y)
    fit_x <- fit_x[complete_r2]
    fit_y <- fit_y[complete_r2]
    regression_df <- regression_df[complete_r2, , drop = FALSE]
    
    if (length(fit_x) >= 2 &&
        stats::sd(fit_x) > 0 &&
        stats::sd(fit_y) > 0) {
      fit <- stats::lm(fit_y ~ fit_x)
      r2_value <- summary(fit)$r.squared
      
      if (isTRUE(log_axes)) {
        x_seq <- seq(min(fit_x), max(fit_x), length.out = 100)
        pred_y <- stats::predict(fit, newdata = data.frame(fit_x = x_seq))
        line_df <- data.frame(
          x_value = 10^x_seq,
          y_value = 10^pred_y
        )
        
        x_range <- range(fit_x, na.rm = TRUE)
        y_range <- range(fit_y, na.rm = TRUE)
        label_x <- 10^(x_range[1] + 0.03 * diff(x_range))
        label_y <- 10^(y_range[2] - 0.05 * diff(y_range))
      } else {
        x_seq <- seq(min(fit_x), max(fit_x), length.out = 100)
        pred_y <- stats::predict(fit, newdata = data.frame(fit_x = x_seq))
        line_df <- data.frame(
          x_value = x_seq,
          y_value = pred_y
        )
        
        x_range <- range(fit_x, na.rm = TRUE)
        y_range <- range(fit_y, na.rm = TRUE)
        label_x <- x_range[1] + 0.03 * diff(x_range)
        label_y <- y_range[2] - 0.05 * diff(y_range)
      }
      
      r2_label <- paste0(
        r2_name, " = ", formatC(r2_value, format = "f", digits = 3),
        "\nn = ", length(fit_x)
      )
      
      label_df <- data.frame(
        x_value = label_x,
        y_value = label_y,
        label = r2_label,
        stringsAsFactors = FALSE
      )
      
      p <- p +
        geom_line(
          data = line_df,
          aes(x = x_value, y = y_value),
          inherit.aes = FALSE,
          colour = "grey10",
          linetype = "dashed",
          size = 0.65,
          alpha = 0.95
        ) +
        geom_label(
          data = label_df,
          aes(x = x_value, y = y_value, label = label),
          inherit.aes = FALSE,
          hjust = 0,
          vjust = 1,
          size = 3.5,
          label.size = 0.25,
          fill = "white",
          colour = "grey10"
        )
    } else {
      if (nrow(regression_df) > 0) {
        if (isTRUE(log_axes)) {
          label_x <- min(regression_df$x_value, na.rm = TRUE)
          label_y <- max(regression_df$y_value, na.rm = TRUE)
        } else {
          label_x <- min(plot_df$x_value, na.rm = TRUE)
          label_y <- max(plot_df$y_value, na.rm = TRUE)
        }
        
        label_df <- data.frame(
          x_value = label_x,
          y_value = label_y,
          label = paste0(r2_name, " non calculable"),
          stringsAsFactors = FALSE
        )
        
        p <- p +
          geom_label(
            data = label_df,
            aes(x = x_value, y = y_value, label = label),
            inherit.aes = FALSE,
            hjust = 0,
            vjust = 1,
            size = 3.5,
            label.size = 0.25,
            fill = "white",
            colour = "grey10"
          )
      }
    }
  }
  
  if (isTRUE(log_axes)) {
    p <- p + scale_x_log10() + scale_y_log10()
  } else {
    p <- apply_plot2d_axis_breaks(p, plot_df, axis_breaks = axis_breaks)
  }
  
  p
}

### Ternary isopleuros ####
plot_ternary_isopleuros <- function(
    data_compo,
    df,
    elements,
    group_col,
    point_size = 1.2,
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    title = NULL,
    label_overrides = NULL
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  axis_cex <- axis_text_size / 11
  
  old_par <- par(
    bg = "white",
    fg = "grey20",
    col.axis = "grey20",
    col.lab = "grey10",
    col.main = "grey10",
    family = "serif",
    font.main = 1,
    cex.main = max(1, axis_cex),
    cex.lab = axis_cex,
    cex.axis = axis_cex
  )
  on.exit(par(old_par), add = TRUE)
  
  if (length(elements) != 3) {
    stop("The ternary diagram must use precisely three elements.", call. = FALSE)
  }
  
  check_columns(df, c(elements, group_col))
  
  groups <- trimws(as.character(df[[group_col]]))
  valid_rows <- !is.na(groups) & groups != "" & groups != "NA"
  
  ternary_compo <- data_compo[valid_rows, elements]
  ternary_groups <- droplevels(as.factor(groups[valid_rows]))
  ternary_levels <- levels(ternary_groups)
  
  if (identical(aesthetic_mode, "shape")) {
    ternary_shapes <- make_group_shapes(ternary_groups, shape_palette_name = shape_palette_name)
    point_shapes <- ternary_shapes[as.character(ternary_groups)]
    point_colours <- rep("grey10", length(ternary_groups))
    legend_shapes <- ternary_shapes[ternary_levels]
    legend_colours <- rep("grey10", length(ternary_levels))
  } else {
    ternary_palette <- make_group_palette(ternary_groups, palette_name = palette_name)
    point_shapes <- rep(16, length(ternary_groups))
    point_colours <- ternary_palette[as.character(ternary_groups)]
    legend_shapes <- rep(16, length(ternary_levels))
    legend_colours <- ternary_palette[ternary_levels]
  }
  
  x_axis_label <- plot_label_field(label_overrides, "x", elements[1])
  y_axis_label <- plot_label_field(label_overrides, "y", elements[2])
  z_axis_label <- plot_label_field(label_overrides, "z", elements[3])
  legend_title <- plot_label_field(label_overrides, "legend", group_col)
  
  ternary_plot(
    ternary_compo,
    pch = point_shapes,
    cex = point_size,
    col = point_colours,
    xlab = x_axis_label,
    ylab = y_axis_label,
    zlab = z_axis_label,
    main = safe_plot_title(title, paste(elements, collapse = " - ")),
    panel.first = ternary_grid()
  )
  
  legend(
    "topright",
    legend = ternary_levels,
    title = legend_title,
    col = legend_colours,
    pch = legend_shapes,
    bty = "n",
    cex = 0.8 * axis_cex
  )
}

### Ternary ggtern ####
plot_ternary_ggtern <- function(
    df,
    axis_specs,
    group_col,
    point_size = 2,
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    title = NULL,
    label_overrides = NULL
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  
  if (length(axis_specs) != 3) {
    stop("The ternary diagram must use precisely three summits.", call. = FALSE)
  }
  
  check_columns(df, group_col)
  
  axes <- lapply(axis_specs, function(spec) make_ternary_axis_variable(df, spec))
  axis_labels <- vapply(axes, function(x) x$label, character(1))
  x_axis_label <- plot_label_field(label_overrides, "x", axis_labels[1])
  y_axis_label <- plot_label_field(label_overrides, "y", axis_labels[2])
  z_axis_label <- plot_label_field(label_overrides, "z", axis_labels[3])
  legend_title <- plot_label_field(label_overrides, "legend", group_col)
  
  plot_df <- data.frame(
    X = axes[[1]]$values,
    Y = axes[[2]]$values,
    Z = axes[[3]]$values,
    Group = trimws(as.character(df[[group_col]])),
    stringsAsFactors = FALSE
  )
  
  plot_df$Group[plot_df$Group == "" | plot_df$Group == "NA"] <- NA
  
  valid_rows <- complete.cases(plot_df[, c("X", "Y", "Z", "Group")]) &
    apply(plot_df[, c("X", "Y", "Z")], 1, function(x) all(is.finite(x))) &
    rowSums(plot_df[, c("X", "Y", "Z")], na.rm = TRUE) > 0
  
  plot_df <- plot_df[valid_rows, , drop = FALSE]
  plot_df$Group <- droplevels(as.factor(plot_df$Group))
  
  plot_title <- safe_plot_title(title, paste(axis_labels, collapse = " - "))
  
  p <- ggtern(plot_df, aes(x = X, y = Y, z = Z)) +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(
      title = plot_title,
      x = x_axis_label,
      y = y_axis_label,
      z = z_axis_label
    )
  
  if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(plot_df$Group, shape_palette_name = shape_palette_name)
    p <- p +
      geom_point(aes(shape = Group), colour = "grey10", fill = "white", size = point_size, alpha = 0.95, na.rm = TRUE) +
      scale_shape_manual(values = group_shapes, na.translate = FALSE, drop = FALSE) +
      labs(shape = legend_title)
  } else {
    group_palette <- make_group_palette(plot_df$Group, palette_name = palette_name)
    p <- p +
      geom_point(aes(colour = Group), size = point_size, alpha = 0.95, na.rm = TRUE) +
      scale_colour_manual(values = group_palette, na.translate = FALSE, drop = FALSE) +
      labs(colour = legend_title)
  }
  
  p
}

### Boxplot helpers ####
plot_boxplots <- function(
    df,
    elements,
    group_col,
    title = "Group boxplots",
    palette_name = "Okabe-Ito",
    aesthetic_mode = "colour",
    shape_palette_name = "Classic",
    axis_text_size = 11,
    label_overrides = NULL,
    show_points = FALSE
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  title <- safe_plot_title(title, "Group boxplots")
  
  check_columns(df, c(group_col, elements))
  
  # Preparation of data
  n <- nrow(df)
  df_melted <- data.frame(
    Element = rep(elements, each = n),
    Value = as.vector(unlist(df[, elements, drop = FALSE])),
    Group = rep(df[[group_col]], length(elements)),
    stringsAsFactors = FALSE
  )
  
  # Filtering of NA
  df_melted <- df_melted[is.finite(df_melted$Value), ]
  if (nrow(df_melted) == 0) stop("No valid data for boxplots.", call. = FALSE)
  
  # Convertir en facteurs (CRUCIAL)
  df_melted$Element <- factor(df_melted$Element, levels = elements)
  df_melted$Group <- factor(df_melted$Group)
  
  x_axis_label <- plot_label_field(label_overrides, "x", "Element")
  y_axis_label <- plot_label_field(label_overrides, "y", "Concentration")
  legend_title <- plot_label_field(label_overrides, "legend", group_col)
  
  if (identical(aesthetic_mode, "shape")) {
    group_shapes <- make_group_shapes(df_melted$Group, shape_palette_name = shape_palette_name)
    group_palette <- make_group_palette(df_melted$Group, palette_name)
    p <- ggplot(df_melted, aes(x = Element, y = Value, fill = Group)) +
      geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = ifelse(show_points, 16, NA)) +
      scale_fill_manual(values = group_palette, na.translate = FALSE) +
      labs(fill = legend_title)
  } else {
    group_palette <- make_group_palette(df_melted$Group, palette_name)
    p <- ggplot(df_melted, aes(x = Element, y = Value, fill = Group)) +
      geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = ifelse(show_points, 16, NA)) +
      scale_fill_manual(values = group_palette, na.translate = FALSE) +
      labs(fill = legend_title)
  }
  
  p +
    theme_xrf_clean(base_size = axis_text_size) +
    apply_gg_axis_text_size(axis_text_size = axis_text_size) +
    labs(title = title, x = x_axis_label, y = y_axis_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


### Correlation matrix helpers ####
plot_correlation_matrix <- function(
    df,
    elements,
    title = "Correlation matrix",
    method = "pearson",
    axis_text_size = 11,
    label_overrides = NULL,
    show_values = TRUE,
    palette_name = "RdBu"
) {
  axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
  title <- safe_plot_title(title, "Correlation matrix")
  
  check_columns(df, elements)
  
  # Sélection des données numériques
  cor_data <- df[, elements, drop = FALSE]
  cor_data <- as.data.frame(apply(cor_data, 2, as.numeric))
  
  # Calcul de la matrice de corrélation
  cor_matrix <- cor(cor_data, method = method, use = "complete.obs")
  
  # Vérification
  if (nrow(cor_matrix) == 0 || ncol(cor_matrix) == 0) {
    stop("Impossible to calculate the correlation matrix.", call. = FALSE)
  }
  
  # Creation of the heatmap with plotly
  p <- plotly::plot_ly(
    x = rownames(cor_matrix),
    y = colnames(cor_matrix),
    z = as.matrix(cor_matrix),
    type = "heatmap",
    colorscale = palette_name,
    zmin = -1,
    zmax = 1,
    hoverongaps = FALSE
  )
  
  # Add the values
  if (isTRUE(show_values)) {
    values_text <- matrix(
      paste0("<b>", round(cor_matrix, 2), "</b>"),
      nrow = nrow(cor_matrix),
      ncol = ncol(cor_matrix)
    )
    p <- p %>%
      plotly::add_trace(
        x = rownames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        text = values_text,
        texttemplate = "%{text}",
        textfont = list(size = axis_text_size * 0.8),
        showscale = FALSE,
        hoverinfo = "skip"
      )
  }
  
  # Display
  p <- p %>%
    plotly::layout(
      title = list(
        text = title,
        x = 0.5,
        xanchor = "center",
        font = list(
          family = "serif",
          color = "#1A1A1A",
          size = axis_title_size(axis_text_size) + 1
        )
      ),
      xaxis = list(
        title = list(
          text = plot_label_field(label_overrides, "x", "Element"),
          font = list(size = axis_title_size(axis_text_size), family = "serif", color = "#1A1A1A")
        ),
        tickfont = list(size = axis_text_size, family = "serif", color = "#1A1A1A"),
        side = "bottom",
        tickangle = -45
      ),
      yaxis = list(
        title = list(
          text = plot_label_field(label_overrides, "y", "Element"),
          font = list(size = axis_title_size(axis_text_size), family = "serif", color = "#1A1A1A")
        ),
        tickfont = list(size = axis_text_size, family = "serif", color = "#1A1A1A"),
        autorange = "reversed"
      ),
      margin = list(l = 100 + (ncol(cor_matrix) * 5), b = 100 + (nrow(cor_matrix) * 5)),
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    )
  
  p
}