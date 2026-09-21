# =============================================
# SERVER LOGIC
# =============================================

# Helper Functions####
# These are functions used ONLY in the server logic

# --- Reactive Data and State ####
server <- function(input, output, session) {
#### 1. DATA LOADING AND PREPROCESSING ####

  raw_data <- reactive({
    req(input$input_file)
    as.data.frame(readxl::read_excel(input$input_file$datapath, .name_repair = "unique"), check.names = FALSE)
  })
  
  info_cols <- reactive({
    req(input$sample_col, input$group_col, input$info_cols)
    raw <- raw_data()
    idx <- tryCatch(
      parse_cols_spec(input$info_cols, ncol(raw)),
      error = function(e) integer(0)
    )
    extra <- c(input$sample_col, input$group_col)
    unique(c(idx, match(extra, colnames(raw))))
  })
  
  chem_choices <- reactive({
    raw <- raw_data()
    setdiff(colnames(raw), colnames(raw)[info_cols()])
  })
  
  plot2d_choices <- reactive({
    raw <- raw_data()
    req(input$sample_col, input$group_col)
    
    numeric_like_columns(
      raw,
      exclude = input$sample_col,
      min_non_missing = 1
    )
  })
  
  #### 2. GLOBAL SETTINGS AND UI CONTROLS ####
  group_aesthetic_mode <- reactive({
    if (is.null(input$group_use_colour) || isTRUE(input$group_use_colour)) "colour" else "shape"
  })
  
  group_shape_palette_name <- reactive({
    if (is.null(input$group_shape_palette_name) || !nzchar(input$group_shape_palette_name)) {
      "Classique"
    } else {
      input$group_shape_palette_name
    }
  })
  
  plot2d_axis_breaks <- reactive({
    plot2d_tick_settings(
      mode = input$plot2d_axis_breaks_mode %||% "auto",
      x_major = input$plot2d_x_major_step,
      x_minor = input$plot2d_x_minor_step,
      y_major = input$plot2d_y_major_step,
      y_minor = input$plot2d_y_minor_step
    )
  })
  
  #### 3. DATA PREPARATION ####
  
  prepared <- reactive({
    raw <- raw_data()
    req(input$sample_col, input$group_col)
    
    idx <- info_cols()
    sample_info <- raw[, idx, drop = FALSE]
    chem_data <- raw[, -idx, drop = FALSE]
    chem_data <- replace_lod(chem_data, factor = input$lod_factor)
    
    data <- cbind(sample_info, chem_data)
    data[[input$group_col]] <- trimws(as.character(data[[input$group_col]]))
    
    check_columns(data, c(input$sample_col, input$group_col))
    
    compo_data <- cbind(data[, input$group_col, drop = FALSE], chem_data)
    data_compo <- nexus::as_composition(compo_data, groups = 1)
    
    max_alr <- ncol(chem_data)
    validate(need(alr_denominator_index() <= max_alr, paste0("ALR index must be <= ", max_alr, ".")))
    
    list(
      raw = raw,
      sample_info = sample_info,
      chem_data = chem_data,
      data = data,
      data_compo = data_compo,
      object_clr = nexus::transform_clr(data_compo),
      object_alr = nexus::transform_alr(data_compo, j = alr_denominator_index()),
      object_ilr = nexus::transform_ilr(data_compo)
    )
  })
  
  
  observe({
    req(raw_data())
    raw <- raw_data()
    idx <- info_cols()
    chem_cols <- setdiff(colnames(raw), colnames(raw)[idx])
    updateSelectInput(session, "alr_denominator", choices = chem_cols)
  })
  
  alr_denominator_index <- reactive({
    req(input$alr_denominator, raw_data())
    raw <- raw_data()
    idx <- info_cols()
    chem_cols <- setdiff(colnames(raw), colnames(raw)[idx])
    match(input$alr_denominator, chem_cols)
  })
  
  selected_row_indices <- reactive({
    p <- prepared()
    
    
    selected <- input$data_preview_rows_selected
    
    if (is.null(selected)) {
      selected <- seq_len(nrow(p$data))
    }
    
    idx <- suppressWarnings(as.integer(selected))
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(p$data)]
    idx <- unique(idx)
    
    validate(need(length(idx) > 0, "Select at least one sample."))
    
    idx
  })
  
  selected_prepared <- reactive({
    p <- prepared()
    idx <- selected_row_indices()
    
    raw <- p$raw[idx, , drop = FALSE]
    sample_info <- p$sample_info[idx, , drop = FALSE]
    chem_data <- p$chem_data[idx, , drop = FALSE]
    data <- p$data[idx, , drop = FALSE]
    data[[input$group_col]] <- trimws(as.character(data[[input$group_col]]))
    
    compo_data <- cbind(data[, input$group_col, drop = FALSE], chem_data)
    data_compo <- nexus::as_composition(compo_data, groups = 1)
    
    list(
      raw = raw,
      sample_info = sample_info,
      chem_data = chem_data,
      data = data,
      data_compo = data_compo
    )
  })
  

  selected_clr_matrix <- reactive({
    req(input$clr_biplot_elements)
    selected_data <- selected_prepared()$data
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the CLR biplot."))
    validate(need(length(input$clr_biplot_elements) >= 2, "Choose at least two elements for the CLR biplot."))
    
    make_selected_clr_matrix(
      df = selected_data,
      elements = input$clr_biplot_elements,
      group_col = input$group_col
    )
  })
  
  selected_alr_matrix <- reactive({
    req(input$alr_biplot_elements)
    selected_data <- selected_prepared()$data
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the ALR biplot."))
    validate(need(length(input$alr_biplot_elements) >= 2, "Choisissez au moins 2 éléments ALR."))
    
    check_columns(selected_data, c(input$group_col, input$alr_biplot_elements))
    
    selected_data <- selected_data[, c(input$group_col, input$alr_biplot_elements), drop = FALSE]
    selected_compo <- nexus::as_composition(selected_data, groups = 1)
    selected_alr <- nexus::transform_alr(selected_compo, j = alr_denominator_index())
    
    alr_matrix <- as_clr_matrix(selected_alr)
    if (is.null(colnames(alr_matrix))) {
      colnames(alr_matrix) <- input$alr_biplot_elements
    }
    
    alr_matrix
  })
  
  selected_ilr_matrix <- reactive({
    req(input$ilr_biplot_elements)
    selected_data <- selected_prepared()$data
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the ILR biplot."))
    validate(need(length(input$ilr_biplot_elements) >= 2, "Choisissez au moins 2 éléments ILR."))
    
    check_columns(selected_data, c(input$group_col, input$ilr_biplot_elements))
    
    selected_data <- selected_data[, c(input$group_col, input$ilr_biplot_elements), drop = FALSE]
    selected_compo <- nexus::as_composition(selected_data, groups = 1)
    selected_ilr <- nexus::transform_ilr(selected_compo)
    
    ilr_matrix <- as_clr_matrix(selected_ilr)
    if (is.null(colnames(ilr_matrix))) {
      colnames(ilr_matrix) <- input$ilr_biplot_elements
    }
    
    ilr_matrix
  })
  
  data_preview_proxy <- DT::dataTableProxy("data_preview")
  
  observeEvent(input$select_all_samples, {
    req(prepared())
    DT::selectRows(data_preview_proxy, seq_len(nrow(prepared()$data)))
  })
  
  observeEvent(input$clear_samples, {
    DT::selectRows(data_preview_proxy, NULL)
  })
  
  #### 4. DYNAMIC UI ELEMENTS ####
  output$data_mapping_ui <- renderUI({
    raw <- raw_data()
    cols <- colnames(raw)
    
    tagList(
      h4("Main columns"),
      selectInput("sample_col", "Sample column", choices = cols, selected = if ("Sample" %in% cols) "Sample" else cols[1]),
      selectInput("group_col", "Group column", choices = cols, selected = if ("Sample" %in% cols) "Sample" else cols[min(2, length(cols))])
    )
  })
  
  output$clr_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "clr_biplot_elements",
        "CLR elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$clr_biplot_elements),
          default = c("P", "Al", "Fe"),
          min_n = 3
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      numericInput(
        "clr_axis_x",
        "X axis",
        value = remembered_number(isolate(input$clr_axis_x), 1),
        min = 1,
        step = 1
      ),
      numericInput(
        "clr_axis_y",
        "Y axis",
        value = remembered_number(isolate(input$clr_axis_y), 2),
        min = 1,
        step = 1
      )
    )
  })
  
  output$alr_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "alr_biplot_elements",
        "ALR elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$alr_biplot_elements),
          default = head(choices, min(5, length(choices))),
          min_n = 2
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      numericInput(
        "alr_axis_x",
        "X axis",
        value = remembered_number(isolate(input$alr_axis_x), 1),
        min = 1,
        step = 1
      ),
      numericInput(
        "alr_axis_y",
        "Y axis",
        value = remembered_number(isolate(input$alr_axis_y), 2),
        min = 1,
        step = 1
      )
    )
  })
  
  output$ilr_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "ilr_biplot_elements",
        "ILR elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$ilr_biplot_elements),
          default = head(choices, min(5, length(choices))),
          min_n = 2
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      numericInput(
        "ilr_axis_x",
        "X axis",
        value = remembered_number(isolate(input$ilr_axis_x), 1),
        min = 1,
        step = 1
      ),
      numericInput(
        "ilr_axis_y",
        "Y axis",
        value = remembered_number(isolate(input$ilr_axis_y), 2),
        min = 1,
        step = 1
      )
    )
  })
  
  output$plot2d_ui <- renderUI({
    choices <- plot2d_choices()
    
    tagList(
      selectizeInput(
        "plot2d_x_num",
        "X - numerator",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$plot2d_x_num),
          default = character(0),
          min_n = 1
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Choose one or more variables")
      ),
      selectizeInput(
        "plot2d_x_den",
        "X - denominator",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$plot2d_x_den),
          default = character(0),
          min_n = 0
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "No denominator")
      ),
      selectizeInput(
        "plot2d_y_num",
        "Y - numerator",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$plot2d_y_num),
          default = character(0),
          min_n = 1
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Choose one or more variables")
      ),
      selectizeInput(
        "plot2d_y_den",
        "Y - denominator",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$plot2d_y_den),
          default = character(0),
          min_n = 0
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "No denominator")
      ),
      checkboxInput(
        "plot2d_log_axes",
        "Logarithmic scale",
        value = remembered_bool(isolate(input$plot2d_log_axes), FALSE)
      ),
      checkboxInput(
        "plot2d_show_r2",
        "Display regression",
        value = remembered_bool(isolate(input$plot2d_show_r2), FALSE)
      ),
      conditionalPanel(
        condition = "input.plot2d_log_axes === false",
        tags$hr(),
        h5("Graduation des axes"),
        selectInput(
          "plot2d_axis_breaks_mode",
          "Numérotation",
          choices = c(
            "Automatique" = "auto",
            "De 1 en 1" = "major_1",
            "Personnalisé" = "custom"
          ),
          selected = remembered_choice(
            c("auto", "major_1", "custom"),
            isolate(input$plot2d_axis_breaks_mode),
            "auto"
          )
        ),
        helpText(
          "Si le pas choisi est trop fin pour l'échelle des données, il est automatiquement agrandi pour éviter les ralentissements et garder l'axe lisible."
        ),
        conditionalPanel(
          condition = "input.plot2d_axis_breaks_mode == 'custom'",
          fluidRow(
            column(
              6,
              numericInput(
                "plot2d_x_major_step",
                "X - Graduation principale",
                value = remembered_number(isolate(input$plot2d_x_major_step), 5),
                min = .Machine$double.eps,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                "plot2d_x_minor_step",
                "X - Graduation secondaire",
                value = remembered_number(isolate(input$plot2d_x_minor_step), 1),
                min = .Machine$double.eps,
                step = 1
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "plot2d_y_major_step",
                "Y - Graduation principale",
                value = remembered_number(isolate(input$plot2d_y_major_step), 5),
                min = .Machine$double.eps,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                "plot2d_y_minor_step",
                "Y - Graduation secondaire",
                value = remembered_number(isolate(input$plot2d_y_minor_step), 1),
                min = .Machine$double.eps,
                step = 1
              )
            )
          )
        )
      )
    )
  })
  
  # === Group filter ===
  
  # Reactive : List of groups
  available_groups <- reactive({
    req(prepared(), input$group_col)
    unique(na.omit(prepared()$data[[input$group_col]]))
  })
  
  # Dynamic UI for groups checkboxes
  output$group_filter_ui <- renderUI({
    groups <- available_groups()
    if (length(groups) == 0) return(NULL)
    
    checkboxGroupInput(
      "selected_groups",
      NULL,
      choices = groups,
      selected = groups,
      inline = TRUE
    )
  })
  
  # Observe: apply/remove the group filter
  observeEvent(input$apply_group_filter, {
    req(prepared(), input$group_col, input$selected_groups)
    p <- prepared()
    group_values <- p$data[[input$group_col]]
    idx <- which(group_values %in% input$selected_groups)
    DT::selectRows(data_preview_proxy, idx)
  })
  
  observeEvent(input$clear_group_filter, {
    req(prepared())
    groups <- available_groups()
    updateCheckboxGroupInput(session, "selected_groups", selected = groups)
    DT::selectRows(data_preview_proxy, seq_len(nrow(prepared()$data)))
  })
  
  # === Value filter ===
  
  observe({
    req(prepared())
    p <- prepared()
    numeric_cols <- numeric_like_columns(
      p$data,
      exclude = c(input$sample_col, input$group_col, info_cols())
    )
    updateSelectInput(session, "filter_column", choices = numeric_cols)
  })
  
  # Dynamic UI for values range
  output$filter_range_ui <- renderUI({
    req(input$filter_column)
    p <- prepared()
    col_data <- suppressWarnings(as.numeric(p$data[[input$filter_column]]))
    col_data <- col_data[!is.na(col_data)]
    
    if (length(col_data) == 0) return(NULL)
    
    min_val <- min(col_data, na.rm = TRUE)
    max_val <- max(col_data, na.rm = TRUE)
    step_val <- if (min_val == max_val) 1 else (max_val - min_val) / 100
    
    sliderInput(
      "filter_range",
      NULL,
      min = min_val,
      max = max_val,
      value = c(min_val, max_val),
      step = step_val,
      width = "100%"
    )
  })
  
  # Apply the value filter
  observeEvent(input$apply_value_filter, {
    req(input$filter_column, input$filter_range)
    p <- prepared()
    col_data <- suppressWarnings(as.numeric(p$data[[input$filter_column]]))
    range <- input$filter_range
    
    idx <- which(!is.na(col_data) & col_data >= range[1] & col_data <= range[2])
    DT::selectRows(data_preview_proxy, idx)
  })
  
  # Remove the value filter
  observeEvent(input$clear_value_filter, {
    req(prepared())
    updateSelectInput(session, "filter_column", selected = NULL)
    DT::selectRows(data_preview_proxy, seq_len(nrow(prepared()$data)))
  })
  
  output$boxplot_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "boxplot_elements",
        "Chemical elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$boxplot_elements),
          default = head(choices, min(5, length(choices))),
          min_n = 1
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      checkboxInput(
        "boxplot_show_points",
        "Show points",
        value = remembered_bool(isolate(input$boxplot_show_points), FALSE)
      )
    )
  })
  
  output$ternary_isopleuros_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "ternary_isopleuros_elements",
        "3 elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$ternary_isopleuros_elements),
          default = c("P", "Al", "Fe"),
          min_n = 3,
          max_n = 3
        ),
        multiple = TRUE,
        options = list(maxItems = 3, plugins = list("remove_button"))
      )
    )
  })
  
  output$ternary_ggtern_ui <- renderUI({
    choices <- chem_choices()
    denominator_modes <- c(
      "Element" = "none",
      "Ratio" = "element",
      "Multiplier" = "number"
    )
    
    ternary_axis_ui <- function(prefix, label, selected) {
      current_num <- isolate(input[[paste0(prefix, "_num")]])
      current_type <- isolate(input[[paste0(prefix, "_type")]])
      current_den <- isolate(input[[paste0(prefix, "_den")]])
      current_divisor <- isolate(input[[paste0(prefix, "_divisor")]])
      
      tagList(
        h5(label),
        selectInput(
          paste0(prefix, "_num"),
          "Element",
          choices = choices,
          selected = remembered_selected(
            choices,
            current_num,
            default = selected,
            min_n = 1,
            max_n = 1
          )
        ),
        selectInput(
          paste0(prefix, "_type"),
          "Type",
          choices = denominator_modes,
          selected = remembered_choice(
            c("none", "element", "number"),
            current_type,
            "none"
          )
        ),
        conditionalPanel(
          condition = paste0("input.", prefix, "_type == 'element'"),
          selectInput(
            paste0(prefix, "_den"),
            "Element denominator",
            choices = choices,
            selected = remembered_selected(
              choices,
              current_den,
              default = selected,
              min_n = 1,
              max_n = 1
            )
          )
        ),
        conditionalPanel(
          condition = paste0("input.", prefix, "_type == 'number'"),
          numericInput(
            paste0(prefix, "_divisor"),
            "Multiplier",
            value = remembered_number(current_divisor, 1),
            min = .Machine$double.eps,
            step = 1
          )
        )
      )
    }
    
    tagList(
      fluidRow(
        column(4, ternary_axis_ui("ternary_ggtern_x", "Summit X", "Fe")),
        column(4, ternary_axis_ui("ternary_ggtern_y", "Summit Y", "Mn")),
        column(4, ternary_axis_ui("ternary_ggtern_z", "Summit Z", "Ca"))
      )
    )
  })
  
  #### 5. TEXT AND AXIS CONTROLS ####
  export_dimensions <- function(id, default_width = 8, default_height = 6, default_dpi = 300) {
    list(
      width = safe_export_number(input[[paste0(id, "_export_width")]] %||% default_width, default_width, 2, 30),
      height = safe_export_number(input[[paste0(id, "_export_height")]] %||% default_height, default_height, 2, 30),
      dpi = safe_export_number(input[[paste0(id, "_export_dpi")]] %||% default_dpi, default_dpi, 72, 1200)
    )
  }
  
  register_axis_text_controls <- function(id, default = 11, min_size = 6, max_size = 32, step = 1) {
    value <- shiny::reactiveVal(safe_axis_text_size(default, default = 11, min_value = min_size, max_value = max_size))
    
    observeEvent(input[[paste0(id, "_axis_text_minus")]], {
      value(max(min_size, value() - step))
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0(id, "_axis_text_plus")]], {
      value(min(max_size, value() + step))
    }, ignoreInit = TRUE)
    
    output[[paste0(id, "_axis_text_value")]] <- renderUI({
      tags$span(class = "axis-text-value", paste0(value(), " pt"))
    })
    
    value
  }
  
  axis_text_sizes <- list(
    clr_biplot = register_axis_text_controls("clr_biplot", default = 12),
    alr_biplot = register_axis_text_controls("alr_biplot", default = 12),
    ilr_biplot = register_axis_text_controls("ilr_biplot", default = 12),
    dimensio_scree = register_axis_text_controls("dimensio_scree", default = 11),
    dimensio_individuals = register_axis_text_controls("dimensio_individuals", default = 11),
    dimensio_variables = register_axis_text_controls("dimensio_variables", default = 11),
    plot_2d = register_axis_text_controls("plot_2d", default = 12),
    ternary_isopleuros = register_axis_text_controls("ternary_isopleuros", default = 11),
    ternary_ggtern = register_axis_text_controls("ternary_ggtern", default = 12),
    boxplot = register_axis_text_controls("boxplot", default = 12),
    correlation_matrix = register_axis_text_controls("correlation_matrix", default = 11),
    texture_ternary = register_axis_text_controls("texture_ternary", default = 12),
    texture_classes_plot = register_axis_text_controls("texture_classes_plot", default = 11)
  )
  
  register_plot_text_controls <- function(id, default_title, default_labels, fields = c("x", "y", "legend")) {
    field_labels <- c(
      x = "Name of X axis",
      y = "Name of Y axis",
      z = "Name of Z axis",
      legend = "Legend title"
    )
    
    custom_title <- shiny::reactiveVal(NULL)
    custom_labels <- shiny::reactiveVal(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
    editing <- shiny::reactiveVal(FALSE)
    
    default_title_value <- reactive({
      value <- tryCatch({
        if (is.function(default_title)) default_title() else default_title
      }, error = function(e) "Graph")
      
      safe_plot_title(value, "Graph")
    })
    
    current_title <- reactive({
      safe_plot_title(custom_title(), default_title_value())
    })
    
    default_labels_value <- reactive({
      value <- tryCatch({
        if (is.function(default_labels)) default_labels() else default_labels
      }, error = function(e) list())
      
      if (is.null(value) || !is.list(value)) {
        value <- list()
      }
      
      out <- list()
      for (field in fields) {
        out[[field]] <- safe_plot_label(value[[field]], field_labels[[field]])
      }
      out
    })
    
    current_labels <- reactive({
      defaults <- default_labels_value()
      custom <- custom_labels()
      
      if (is.null(custom) || !is.list(custom)) {
        custom <- list()
      }
      
      out <- defaults
      for (field in fields) {
        candidate <- custom[[field]]
        candidate <- paste(candidate %||% "", collapse = " ")
        candidate <- trimws(as.character(candidate))
        
        if (nzchar(candidate)) {
          out[[field]] <- safe_plot_label(candidate, defaults[[field]])
        }
      }
      out
    })
    
    observeEvent(input[[paste0(id, "_text_edit")]], {
      updateTextInput(session, paste0(id, "_title_input"), value = current_title())
      labels <- current_labels()
      for (field in fields) {
        updateTextInput(session, paste0(id, "_", field, "_label_input"), value = labels[[field]])
      }
      editing(TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0(id, "_text_apply")]], {
      custom_title(input[[paste0(id, "_title_input")]])
      
      values <- custom_labels()
      if (is.null(values) || !is.list(values)) {
        values <- list()
      }
      
      for (field in fields) {
        values[[field]] <- input[[paste0(id, "_", field, "_label_input")]]
      }
      
      custom_labels(values)
      editing(FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0(id, "_text_cancel")]], {
      editing(FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0(id, "_text_reset")]], {
      custom_title(NULL)
      custom_labels(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
      updateTextInput(session, paste0(id, "_title_input"), value = default_title_value())
      labels <- default_labels_value()
      for (field in fields) {
        updateTextInput(session, paste0(id, "_", field, "_label_input"), value = labels[[field]])
      }
      editing(FALSE)
    }, ignoreInit = TRUE)
    
    output[[paste0(id, "_text_ui")]] <- renderUI({
      labels <- current_labels()
      
      tags$div(
        class = "plot-text-editor",
        if (isTRUE(editing())) {
          edit_inputs <- c(
            list(
              textInput(
                paste0(id, "_title_input"),
                "Title",
                value = current_title(),
                placeholder = default_title_value()
              )
            ),
            lapply(fields, function(field) {
              textInput(
                paste0(id, "_", field, "_label_input"),
                field_labels[[field]],
                value = labels[[field]],
                placeholder = default_labels_value()[[field]]
              )
            })
          )
          
          tags$div(
            class = "plot-text-edit-panel",
            do.call(tags$div, c(list(class = "plot-text-fields"), edit_inputs)),
            tags$div(
              class = "plot-text-actions",
              actionButton(paste0(id, "_text_apply"), "Apply"),
              actionButton(paste0(id, "_text_cancel"), "Cancel"),
              actionButton(paste0(id, "_text_reset"), "Initialize")
            )
          )
        } else {
          actionButton(
            paste0(id, "_text_edit"),
            "Clic to modify Title and labels",
            class = "plot-text-toggle"
          )
        }
      )
    })
    
    list(
      title = current_title,
      labels = current_labels
    )
  }
  
  plot2d_default_title <- function() {
    x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
    y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
    
    tryCatch({
      x_label <- make_element_or_ratio(selected_prepared()$data, x)$label
      y_label <- make_element_or_ratio(selected_prepared()$data, y)$label
      paste("Plot of", y_label, "as a function of", x_label)
    }, error = function(e) "2D plot")
  }
  
  correlation_default_title <- function() {
    elements <- input$correlation_elements %||% character(0)
    if (length(elements) == 0) {
      "Correlation matrix"
    } else {
      paste("Correlation matrix -", paste(elements, collapse = ", "))
    }
  }
  
  correlation_default_labels <- function() {
    list(
      x = "Element",
      y = "Element",
      legend = "Correlation"
    )
  }
  
  
  ternary_ggtern_default_title <- function() {
    tryCatch({
      axis_specs <- list(
        ternary_axis_spec(input$ternary_ggtern_x_num, input$ternary_ggtern_x_type, input$ternary_ggtern_x_den, input$ternary_ggtern_x_divisor),
        ternary_axis_spec(input$ternary_ggtern_y_num, input$ternary_ggtern_y_type, input$ternary_ggtern_y_den, input$ternary_ggtern_y_divisor),
        ternary_axis_spec(input$ternary_ggtern_z_num, input$ternary_ggtern_z_type, input$ternary_ggtern_z_den, input$ternary_ggtern_z_divisor)
      )
      axes <- lapply(axis_specs, function(spec) make_ternary_axis_variable(selected_prepared()$data, spec))
      paste(vapply(axes, function(x) x$label, character(1)), collapse = " - ")
    }, error = function(e) "Ternary plot (ggtern)")
  }
  
  clr_biplot_default_labels <- function() {
    tryCatch({
      req(selected_clr_matrix(), input$clr_biplot_elements)
      
      clr_matrix <- selected_clr_matrix()
      axes <- c(input$clr_axis_x %||% 1, input$clr_axis_y %||% 2)
      axes <- suppressWarnings(as.integer(axes))
      
      if (length(axes) != 2 || any(is.na(axes)) || max(axes) > ncol(clr_matrix) || length(unique(axes)) != 2) {
        stop("No CLR axes")
      }
      
      if (!is.null(input$pca_calculation_groups) && length(input$pca_calculation_groups) > 0) {
        req(selected_prepared(), input$group_col)
        
        selected_data <- selected_prepared()$data
        split_result <- split_data_for_projection(
          data = selected_data,
          group_col = input$group_col,
          calculation_groups = input$pca_calculation_groups_clr
        )
        
        calc_indices <- split_result$calculation_indices
        
        if (length(calc_indices) >= 2) {
          calc_matrix <- clr_matrix[calc_indices, , drop = FALSE]
          pca <- prcomp(calc_matrix, center = TRUE, scale. = FALSE)
        } else {
          pca <- prcomp(clr_matrix, center = TRUE, scale. = FALSE)
        }
      } else {
        pca <- prcomp(clr_matrix, center = TRUE, scale. = FALSE)
      }
      
      var_pca <- pca$sdev^2 / sum(pca$sdev^2)
      
      list(
        x = paste0("PC", axes[1], " (", round(var_pca[axes[1]] * 100, 1), " % )"),
        y = paste0("PC", axes[2], " (", round(var_pca[axes[2]] * 100, 1), " % )"),
        legend = input$group_col %||% "Group"
      )
    }, error = function(e) {
      list(x = "PC1", y = "PC2", legend = input$group_col %||% "Group")
    })
  }
  
  alr_biplot_default_labels <- function() {
    tryCatch({
      req(selected_alr_matrix(), input$alr_biplot_elements)
      
      alr_matrix <- selected_alr_matrix()
      axes <- c(input$alr_axis_x %||% 1, input$alr_axis_y %||% 2)
      axes <- suppressWarnings(as.integer(axes))
      
      if (length(axes) != 2 || any(is.na(axes)) || max(axes) > ncol(alr_matrix) || length(unique(axes)) != 2) {
        stop("No ALR axes")
      }
      
      if (!is.null(input$pca_calculation_groups) && length(input$pca_calculation_groups) > 0) {
        req(selected_prepared(), input$group_col)
        
        selected_data <- selected_prepared()$data
        split_result <- split_data_for_projection(
          data = selected_data,
          group_col = input$group_col,
          calculation_groups = input$pca_calculation_groups_alr
        )
        
        calc_indices <- split_result$calculation_indices
        
        if (length(calc_indices) >= 2) {
          calc_matrix <- alr_matrix[calc_indices, , drop = FALSE]
          pca <- prcomp(calc_matrix, center = TRUE, scale. = FALSE)
        } else {
          pca <- prcomp(alr_matrix, center = TRUE, scale. = FALSE)
        }
      } else {
        pca <- prcomp(alr_matrix, center = TRUE, scale. = FALSE)
      }
      
      var_pca <- pca$sdev^2 / sum(pca$sdev^2)
      
      list(
        x = paste0("PC", axes[1], " (", round(var_pca[axes[1]] * 100, 1), " % )"),
        y = paste0("PC", axes[2], " (", round(var_pca[axes[2]] * 100, 1), " % )"),
        legend = input$group_col %||% "Group"
      )
    }, error = function(e) {
      list(x = "PC1", y = "PC2", legend = input$group_col %||% "Group")
    })
  }
  
  ilr_biplot_default_labels <- function() {
    tryCatch({
      req(selected_ilr_matrix(), input$ilr_biplot_elements)
      
      ilr_matrix <- selected_ilr_matrix()
      axes <- c(input$ilr_axis_x %||% 1, input$ilr_axis_y %||% 2)
      axes <- suppressWarnings(as.integer(axes))
      
      if (length(axes) != 2 || any(is.na(axes)) || max(axes) > ncol(ilr_matrix) || length(unique(axes)) != 2) {
        stop("No ILR axes")
      }
      
      if (!is.null(input$pca_calculation_groups) && length(input$pca_calculation_groups) > 0) {
        req(selected_prepared(), input$group_col)
        
        selected_data <- selected_prepared()$data
        split_result <- split_data_for_projection(
          data = selected_data,
          group_col = input$group_col,
          calculation_groups = input$pca_calculation_groups_ilr
        )
        
        calc_indices <- split_result$calculation_indices
        
        if (length(calc_indices) >= 2) {
          calc_matrix <- ilr_matrix[calc_indices, , drop = FALSE]
          pca <- prcomp(calc_matrix, center = TRUE, scale. = FALSE)
        } else {
          pca <- prcomp(ilr_matrix, center = TRUE, scale. = FALSE)
        }
      } else {
        pca <- prcomp(ilr_matrix, center = TRUE, scale. = FALSE)
      }
      
      var_pca <- pca$sdev^2 / sum(pca$sdev^2)
      
      list(
        x = paste0("PC", axes[1], " (", round(var_pca[axes[1]] * 100, 1), " % )"),
        y = paste0("PC", axes[2], " (", round(var_pca[axes[2]] * 100, 1), " % )"),
        legend = input$group_col %||% "Group"
      )
    }, error = function(e) {
      list(x = "PC1", y = "PC2", legend = input$group_col %||% "Group")
    })
  }
  
  plot2d_default_labels <- function() {
    x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
    y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
    
    tryCatch({
      x_label <- make_element_or_ratio(selected_prepared()$data, x)$label
      y_label <- make_element_or_ratio(selected_prepared()$data, y)$label
      list(x = x_label, y = y_label, legend = input$group_col %||% "Group")
    }, error = function(e) {
      list(x = "X axis", y = "Y axis", legend = input$group_col %||% "Group")
    })
  }
  
  ternary_isopleuros_default_labels <- function() {
    elements <- input$ternary_isopleuros_elements %||% character(0)
    elements <- c(elements, "X axis", "Y axis", "Z axis")[seq_len(3)]
    
    list(
      x = elements[1],
      y = elements[2],
      z = elements[3],
      legend = input$group_col %||% "Group"
    )
  }
  
  ternary_ggtern_default_labels <- function() {
    tryCatch({
      axis_specs <- list(
        ternary_axis_spec(input$ternary_ggtern_x_num, input$ternary_ggtern_x_type, input$ternary_ggtern_x_den, input$ternary_ggtern_x_divisor),
        ternary_axis_spec(input$ternary_ggtern_y_num, input$ternary_ggtern_y_type, input$ternary_ggtern_y_den, input$ternary_ggtern_y_divisor),
        ternary_axis_spec(input$ternary_ggtern_z_num, input$ternary_ggtern_z_type, input$ternary_ggtern_z_den, input$ternary_ggtern_z_divisor)
      )
      axes <- lapply(axis_specs, function(spec) make_ternary_axis_variable(selected_prepared()$data, spec))
      labels <- vapply(axes, function(x) x$label, character(1))
      
      list(
        x = labels[1],
        y = labels[2],
        z = labels[3],
        legend = input$group_col %||% "Group"
      )
    }, error = function(e) {
      list(x = "Element 1", y = "Element 2", z = "Element 3", legend = input$group_col %||% "Group")
    })
  }
  
    plot_text_controls <- list(
    clr_biplot = register_plot_text_controls("clr_biplot", function() paste("CLR PCA biplot:", paste(input$clr_biplot_elements, collapse = " - ")), clr_biplot_default_labels),
    alr_biplot = register_plot_text_controls("alr_biplot", function() paste("ALR PCA biplot:", paste(input$alr_biplot_elements, collapse = " - ")), alr_biplot_default_labels),
    ilr_biplot = register_plot_text_controls("ilr_biplot", function() paste("ILR PCA biplot:", paste(input$ilr_biplot_elements, collapse = " - ")), ilr_biplot_default_labels),
    dimensio_scree = register_plot_text_controls("dimensio_scree", "Screeplot of eigenvalues", list(x = "Principal components", y = "Eigenvalues", legend = "Legend")),
    dimensio_individuals = register_plot_text_controls("dimensio_individuals", "Individuals PCA", list(x = "Dimension 1", y = "Dimension 2", legend = "Group")),
    dimensio_variables = register_plot_text_controls("dimensio_variables", "Variables PCA", list(x = "Dimension 1", y = "Dimension 2", legend = "Variables")),
    plot_2d = register_plot_text_controls("plot_2d", plot2d_default_title, plot2d_default_labels),
    boxplot = register_plot_text_controls("boxplot", boxplot_default_title, boxplot_default_labels),
    correlation_matrix = register_plot_text_controls("correlation_matrix", correlation_default_title, correlation_default_labels),
    ternary_isopleuros = register_plot_text_controls("ternary_isopleuros", function() paste(input$ternary_isopleuros_elements %||% character(0), collapse = " - "), ternary_isopleuros_default_labels, fields = c("x", "y", "z", "legend")),
    ternary_ggtern = register_plot_text_controls("ternary_ggtern", ternary_ggtern_default_title, ternary_ggtern_default_labels, fields = c("x", "y", "z", "legend"))
    )
  
  plot_titles <- lapply(plot_text_controls, function(control) control$title)
  plot_labels <- lapply(plot_text_controls, function(control) control$labels)
  
  
  #### 6. PLOT RENDERING ####
  ##### CLR Biplot ####
  clr_biplot_plot <- reactive({
    req(input$clr_biplot_elements, selected_prepared())
    
    selected_data <- selected_prepared()$data
    group_col <- input$group_col
    sample_col <- input$sample_col
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the CLR biplot."))
    validate(need(length(input$clr_biplot_elements) >= 2, "Choose at least two elements for the CLR biplot."))
    
    clr_matrix <- make_selected_clr_matrix(
      df = selected_data,
      elements = input$clr_biplot_elements,
      group_col = group_col
    )
    
    split_result <- split_data_for_projection(
      data = selected_data,
      group_col = group_col,
      calculation_groups = input$pca_calculation_groups_clr
    )
    
    calc_indices <- split_result$calculation_indices
    
    validate(need(length(calc_indices) >= 2, 
                  "At least 2 samples from calculation groups are required for PCA."))
    
    create_biplot_with_projection(
      matrix_data = as.matrix(clr_matrix),
      group_values = selected_data[[group_col]],
      sample_ids = selected_data[[sample_col]],
      calculation_indices = calc_indices,
      axes = c(input$clr_axis_x, input$clr_axis_y),
      colour_label = input$group_col,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      axis_text_size = axis_text_sizes$clr_biplot(),
      label_overrides = plot_labels$clr_biplot(),
      show_confidence_ellipses = input$show_confidence_ellipses,
      title = plot_titles$clr_biplot()
    )
  })
  
  output$clr_biplot <- renderPlotly({
    axis_size <- axis_text_sizes$clr_biplot()
    labels <- plot_labels$clr_biplot()
    
    plotly::layout(
      ggplotly(clr_biplot_plot(), tooltip = "text"),
      font = list(family = "serif", color = "#1A1A1A", size = axis_size),
      title = plotly_title_options(plot_titles$clr_biplot(), axis_size),
      xaxis = plotly_axis_text_options(axis_size, title = labels$x),
      yaxis = plotly_axis_text_options(axis_size, title = labels$y),
      legend = plotly_legend_options(labels$legend, axis_size),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  })
  
  ##### ALR Biplot ####
  alr_biplot_plot <- reactive({
    req(input$alr_biplot_elements, selected_prepared())
    
    selected_data <- selected_prepared()$data 
    group_col <- input$group_col
    sample_col <- input$sample_col
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the ALR biplot."))
    validate(need(length(input$alr_biplot_elements) >= 2, "Choose at least two elements for ALR."))
    
    check_columns(selected_data, c(group_col, input$alr_biplot_elements))
    
    
    alr_data <- selected_data[, c(group_col, input$alr_biplot_elements), drop = FALSE]
    selected_compo <- nexus::as_composition(alr_data, groups = 1)
    selected_alr <- nexus::transform_alr(selected_compo, j = alr_denominator_index())
    alr_matrix <- as_clr_matrix(selected_alr)
    if (is.null(colnames(alr_matrix))) {
      colnames(alr_matrix) <- input$alr_biplot_elements
    }
    
      split_result <- split_data_for_projection(
      data = selected_data,  
      group_col = group_col,
      calculation_groups = input$pca_calculation_groups_alr
    )
    
    calc_indices <- split_result$calculation_indices
    
    validate(need(length(calc_indices) >= 2, 
                  "At least 2 samples from calculation groups are required for PCA."))
    
    create_biplot_with_projection(
      matrix_data = alr_matrix,  
      group_values = selected_data[[group_col]], 
      sample_ids = selected_data[[sample_col]], 
      calculation_indices = calc_indices,
      axes = c(input$alr_axis_x, input$alr_axis_y),
      colour_label = input$group_col,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      axis_text_size = axis_text_sizes$alr_biplot(),
      label_overrides = plot_labels$alr_biplot(),
      show_confidence_ellipses = input$show_confidence_ellipses_alr,
      title = paste("ALR biplot (Denominator:", input$alr_denominator, ")")
    )
  })
  
  output$alr_biplot <- renderPlotly({
    axis_size <- axis_text_sizes$alr_biplot()
    labels <- plot_labels$alr_biplot()
    
    plotly::layout(
      ggplotly(alr_biplot_plot(), tooltip = "text"),
      font = list(family = "serif", color = "#1A1A1A", size = axis_size),
      title = plotly_title_options(paste("ALR Biplot (Denominator: ", input$alr_denominator, ")"), axis_size),
      xaxis = plotly_axis_text_options(axis_size, title = labels$x),
      yaxis = plotly_axis_text_options(axis_size, title = labels$y),
      legend = plotly_legend_options(input$group_col, axis_size),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  })
  
  ##### ILR Biplot ####
  ilr_biplot_plot <- reactive({
    req(input$ilr_biplot_elements, selected_prepared())
    
    selected_data <- selected_prepared()$data
    group_col <- input$group_col
    sample_col <- input$sample_col
    
    validate(need(nrow(selected_data) >= 2, "Choose at least 2 samples for the ILR biplot."))
    validate(need(length(input$ilr_biplot_elements) >= 2, "Choose at least two elements for ILR."))
    
    check_columns(selected_data, c(group_col, input$ilr_biplot_elements))
    
    ilr_data <- selected_data[, c(group_col, input$ilr_biplot_elements), drop = FALSE]
    selected_compo <- nexus::as_composition(ilr_data, groups = 1)
    selected_ilr <- nexus::transform_ilr(selected_compo)
    ilr_matrix <- as_clr_matrix(selected_ilr)
    if (is.null(colnames(ilr_matrix))) {
      colnames(ilr_matrix) <- input$ilr_biplot_elements
    }
    
    split_result <- split_data_for_projection(
      data = selected_data,
      group_col = group_col,
      calculation_groups = input$pca_calculation_groups_ilr
    )
    
    calc_indices <- split_result$calculation_indices
    
    validate(need(length(calc_indices) >= 2, 
                  "At least 2 samples from calculation groups are required for PCA."))
    
    create_biplot_with_projection(
      matrix_data = ilr_matrix,
      group_values = selected_data[[group_col]],
      sample_ids = selected_data[[sample_col]],
      calculation_indices = calc_indices,
      axes = c(input$ilr_axis_x, input$ilr_axis_y),
      colour_label = input$group_col,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      axis_text_size = axis_text_sizes$ilr_biplot(),
      label_overrides = plot_labels$ilr_biplot(),
      show_confidence_ellipses = input$show_confidence_ellipses_alr,
      title = "ILR Biplot"
    )
  })
  
  output$ilr_biplot <- renderPlotly({
    axis_size <- axis_text_sizes$ilr_biplot()
    labels <- plot_labels$ilr_biplot()
    
    plotly::layout(
      ggplotly(ilr_biplot_plot(), tooltip = "text"),
      font = list(family = "serif", color = "#1A1A1A", size = axis_size),
      title = plotly_title_options("ILR Biplot", axis_size),
      xaxis = plotly_axis_text_options(axis_size, title = labels$x),
      yaxis = plotly_axis_text_options(axis_size, title = labels$y),
      legend = plotly_legend_options(input$group_col, axis_size),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  })
  
  ##### Dimensio plots ####
  print_dimensio_plot <- function(p, axis_text_size = 11, title = NULL, label_overrides = NULL) {
    axis_text_size <- safe_axis_text_size(axis_text_size, default = 11)
    
    if (inherits(p, "ggplot")) {
      p <- p + theme_xrf_clean(base_size = axis_text_size)
      p <- apply_gg_axis_text_size(p, axis_text_size = axis_text_size)
      
      if (!is.null(title) && nzchar(trimws(as.character(title)))) {
        p <- p + ggplot2::labs(title = title)
      }
      
      if (!is.null(label_overrides)) {
        x_default <- p$labels$x %||% "X axis"
        y_default <- p$labels$y %||% "Y axis"
        legend_default <- p$labels$colour %||% p$labels$color %||% p$labels$fill %||% p$labels$shape %||% "Legend"
        
        p <- p + ggplot2::labs(
          x = plot_label_field(label_overrides, "x", x_default),
          y = plot_label_field(label_overrides, "y", y_default),
          colour = plot_label_field(label_overrides, "legend", legend_default),
          color = plot_label_field(label_overrides, "legend", legend_default),
          fill = plot_label_field(label_overrides, "legend", legend_default),
          shape = plot_label_field(label_overrides, "legend", legend_default)
        )
      }
    }
    
    print(p)
  }
  
  draw_dimensio_scree <- function(axis_text_size = axis_text_sizes$dimensio_scree(), title = plot_titles$dimensio_scree(), label_overrides = plot_labels$dimensio_scree()) {
    clr_pca_dimensio <- dimensio::pca(selected_clr_matrix(), scale = FALSE)
    print_dimensio_plot(dimensio::screeplot(clr_pca_dimensio), axis_text_size = axis_text_size, title = title, label_overrides = label_overrides)
  }
  
  draw_dimensio_individuals <- function(axis_text_size = axis_text_sizes$dimensio_individuals(), title = plot_titles$dimensio_individuals(), label_overrides = plot_labels$dimensio_individuals()) {
    clr_pca_dimensio <- dimensio::pca(selected_clr_matrix(), scale = FALSE)
    print_dimensio_plot(dimensio::viz_individuals(clr_pca_dimensio, pch = 16), axis_text_size = axis_text_size, title = title, label_overrides = label_overrides)
  }
  
  draw_dimensio_variables <- function(axis_text_size = axis_text_sizes$dimensio_variables(), title = plot_titles$dimensio_variables(), label_overrides = plot_labels$dimensio_variables()) {
    clr_pca_dimensio <- dimensio::pca(selected_clr_matrix(), scale = FALSE)
    print_dimensio_plot(dimensio::viz_variables(clr_pca_dimensio), axis_text_size = axis_text_size, title = title, label_overrides = label_overrides)
  }
  
  output$dimensio_scree <- renderPlot({
    draw_dimensio_scree()
  })
  
  output$dimensio_individuals <- renderPlot({
    draw_dimensio_individuals()
  })
  
  output$dimensio_variables <- renderPlot({
    draw_dimensio_variables()
  })
  
  ##### Boxplots ###############
  boxplot_default_title <- function() {
    elements <- input$boxplot_elements %||% character(0)
    if (length(elements) == 0) {
      "Group boxplots"
    } else {
      paste("Boxplots -", paste(elements, collapse = ", "))
    }
  }
  
  boxplot_default_labels <- function() {
    list(
      x = "Element",
      y = "Concentration",
      legend = input$group_col %||% "Group"
    )
  }
  
  output$boxplot <- renderPlotly({
    req(input$boxplot_elements, input$group_col)
    axis_size <- axis_text_sizes$boxplot()
    labels <- plot_labels$boxplot()
    
    # Prepare data
    df <- selected_prepared()$data
    elements <- input$boxplot_elements
    group_col <- input$group_col
    
    n <- nrow(df)
    df_melted <- data.frame(
      Element = rep(elements, each = n),
      Value = as.vector(unlist(df[, elements, drop = FALSE])),
      Group = rep(df[[group_col]], length(elements)),
      stringsAsFactors = FALSE
    )
    
    # Filtering
    df_melted <- df_melted[is.finite(df_melted$Value), ]
    
    # Convert to factors
    df_melted$Element <- factor(df_melted$Element, levels = elements)
    df_melted$Group <- factor(df_melted$Group)
    
    # Palette
    group_palette <- make_group_palette(df_melted$Group, input$group_palette_name)
    color_map <- setNames(group_palette, levels(df_melted$Group))
    
    # Plot with plot_ly (not ggplotly)
    p <- plotly::plot_ly(
      df_melted,
      x = ~Element,
      y = ~Value,
      color = ~Group,
      colors = color_map,  
      type = 'box',
      boxpoints = ifelse(input$boxplot_show_points, "all", FALSE),
      name = ~Group
    ) %>%
      plotly::layout(
        font = list(family = "serif", color = "#1A1A1A", size = axis_size),
        title = plotly_title_options(plot_titles$boxplot(), axis_size),
        xaxis = plotly_axis_text_options(axis_size, title = labels$x),
        yaxis = plotly_axis_text_options(axis_size, title = labels$y),
        legend = plotly_legend_options(labels$legend, axis_size),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        boxmode = "group"  
      )
    
    p
  })
  
  ##### Correlation plot ###############
  output$correlation_ui <- renderUI({
    choices <- chem_choices()
    tagList(
      selectizeInput(
        "correlation_elements",
        "Chemical elements",
        choices = choices,
        selected = remembered_selected(
          choices,
          isolate(input$correlation_elements),
          default = head(choices, min(8, length(choices))),
          min_n = 2
        ),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectInput(
        "correlation_method",
        "Correlation method",
        choices = c(
          "Pearson" = "pearson",
          "Spearman" = "spearman",
          "Kendall" = "kendall"
        ),
        selected = "pearson"
      ),
      checkboxInput(
        "correlation_show_values",
        "Display values",
        value = TRUE
      ),
      selectInput(
        "correlation_palette",
        "Color palette",
        choices = c(
          "Red-Blue" = "RdBu",
          "Blue-Red" = "Bluered",
          "Viridis" = "Viridis",
          "Plasma" = "Plasma",
          "Jet" = "Jet",
          "Hot" = "Hot"
        ),
        selected = "RdBu"
      )
    )
  })
  
  output$correlation_matrix <- renderPlotly({
    req(input$correlation_elements, length(input$correlation_elements) >= 2)
    
    axis_size <- axis_text_sizes$correlation_matrix()
    labels <- plot_labels$correlation_matrix()
    
    plot_correlation_matrix(
      df = selected_prepared()$data,
      elements = input$correlation_elements,
      title = plot_titles$correlation_matrix(),
      method = input$correlation_method,
      axis_text_size = axis_size,
      label_overrides = labels,
      show_values = input$correlation_show_values,
      palette_name = input$correlation_palette
    )
  })
  
  ##### 2D plot ###############
  plot_2d_plot <- reactive({
    x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
    y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
    validate(need(length(x) > 0 && length(y) > 0, "Select X and Y axes."))
    
    plot_2d_elements(
      df = selected_prepared()$data,
      x = x,
      y = y,
      group = input$group_col,
      label = input$sample_col,
      log_axes = input$plot2d_log_axes,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      line_only = isTRUE(input$plot2d_line_only),
      show_r2 = isTRUE(input$plot2d_show_r2),
      axis_breaks = plot2d_axis_breaks(),
      axis_text_size = axis_text_sizes$plot_2d(),
      title = plot_titles$plot_2d(),
      label_overrides = plot_labels$plot_2d()
    )
  })
  
  output$plot_2d <- renderPlotly({
    axis_size <- axis_text_sizes$plot_2d()
    labels <- plot_labels$plot_2d()
    
    p2d <- plotly::layout(
      ggplotly(plot_2d_plot(), tooltip = "text"),
      font = list(family = "serif", color = "#1A1A1A", size = axis_size),
      title = plotly_title_options(plot_titles$plot_2d(), axis_size),
      xaxis = plotly_axis_text_options(axis_size, title = labels$x),
      yaxis = plotly_axis_text_options(axis_size, title = labels$y),
      legend = plotly_legend_options(labels$legend, axis_size),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
    
    if (!isTRUE(input$plot2d_log_axes)) {
      axis_breaks_settings <- plot2d_axis_breaks()
      if (identical(axis_breaks_settings$mode, "custom")) {
        x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
        y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
        plot2d_data <- selected_prepared()$data
        x_values <- make_element_or_ratio(plot2d_data, x)$values
        y_values <- make_element_or_ratio(plot2d_data, y)$values
        valid_axis_values <- is.finite(x_values) & is.finite(y_values)
        
        p2d <- plotly::layout(
          p2d,
          xaxis = plotly_axis_text_options(
            axis_size,
            plotly_axis_options_from_plot2d_breaks(axis_breaks_settings, x_values[valid_axis_values], "x"),
            title = labels$x
          ),
          yaxis = plotly_axis_text_options(
            axis_size,
            plotly_axis_options_from_plot2d_breaks(axis_breaks_settings, y_values[valid_axis_values], "y"),
            title = labels$y
          )
        )
      }
    }
    
    if (isTRUE(input$plot2d_show_r2)) {
      x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
      y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
      
      r2_info <- make_plot2d_r2_info(
        df = selected_prepared()$data,
        x = x,
        y = y,
        log_axes = input$plot2d_log_axes
      )
      
      p2d <- plotly::add_annotations(
        p2d,
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        text = r2_info$label_html,
        showarrow = FALSE,
        align = "left",
        xanchor = "left",
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.90)",
        bordercolor = "rgba(60,60,60,0.80)",
        borderwidth = 1,
        borderpad = 4,
        font = list(family = "serif", color = "#1A1A1A", size = max(13, axis_size))
      )
    }
    
    p2d
  })
  
  ##### Ternary isopleuros ####
  
  draw_ternary_isopleuros <- function(axis_text_size = axis_text_sizes$ternary_isopleuros()) {
    validate(need(length(input$ternary_isopleuros_elements) == 3, "Select 3 elements."))
    
    plot_ternary_isopleuros(
      data_compo = selected_prepared()$data_compo,
      df = selected_prepared()$data,
      elements = input$ternary_isopleuros_elements,
      group_col = input$group_col,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      axis_text_size = axis_text_size,
      title = plot_titles$ternary_isopleuros(),
      label_overrides = plot_labels$ternary_isopleuros()
    )
  }
  
  output$ternary_isopleuros <- renderPlot({
    draw_ternary_isopleuros()
  })
  
  ##### Ternary ggtern ####
  ternary_ggtern_plot <- reactive({
    validate(need(!is.null(input$ternary_ggtern_x_num), "Choose the element 1."))
    validate(need(!is.null(input$ternary_ggtern_y_num), "Choose the element 2."))
    validate(need(!is.null(input$ternary_ggtern_z_num), "Choose the element 3."))
    
    axis_specs <- list(
      ternary_axis_spec(
        numerator = input$ternary_ggtern_x_num,
        denominator_type = input$ternary_ggtern_x_type,
        denominator_element = input$ternary_ggtern_x_den,
        denominator_number = input$ternary_ggtern_x_divisor
      ),
      ternary_axis_spec(
        numerator = input$ternary_ggtern_y_num,
        denominator_type = input$ternary_ggtern_y_type,
        denominator_element = input$ternary_ggtern_y_den,
        denominator_number = input$ternary_ggtern_y_divisor
      ),
      ternary_axis_spec(
        numerator = input$ternary_ggtern_z_num,
        denominator_type = input$ternary_ggtern_z_type,
        denominator_element = input$ternary_ggtern_z_den,
        denominator_number = input$ternary_ggtern_z_divisor
      )
    )
    
    plot_ternary_ggtern(
      df = selected_prepared()$data,
      axis_specs = axis_specs,
      group_col = input$group_col,
      palette_name = input$group_palette_name,
      aesthetic_mode = group_aesthetic_mode(),
      shape_palette_name = group_shape_palette_name(),
      axis_text_size = axis_text_sizes$ternary_ggtern(),
      title = plot_titles$ternary_ggtern(),
      label_overrides = plot_labels$ternary_ggtern()
    )
  })
  
  output$ternary_ggtern <- renderPlot({
    print(ternary_ggtern_plot())
  })
  
  # Fonction helper pour éviter la duplication de code
  update_pca_groups <- function(session, input_id, prepared_data, group_col) {
    req(prepared_data, group_col)
    groups <- unique(na.omit(prepared_data[[group_col]]))
    updateCheckboxGroupInput(session, input_id, choices = groups, selected = groups)
  }
  
  # Observateur pour CLR
  observe({
    req(prepared(), input$group_col)
    update_pca_groups(session, "pca_calculation_groups_clr", prepared()$data, input$group_col)
  })
  
  # Observateur pour ALR
  observe({
    req(prepared(), input$group_col)
    update_pca_groups(session, "pca_calculation_groups_alr", prepared()$data, input$group_col)
  })
  
  # Observateur pour ILR
  observe({
    req(prepared(), input$group_col)
    update_pca_groups(session, "pca_calculation_groups_ilr", prepared()$data, input$group_col)
  })
  
  # Sélection rapide: tous les groupes pour CLR
  observeEvent(input$select_all_for_pca_clr, {
    req(prepared(), input$group_col)
    groups <- unique(na.omit(prepared()$data[[input$group_col]]))
    updateCheckboxGroupInput(session, "pca_calculation_groups_clr", selected = groups)
  })
  
  # Sélection rapide: tous les groupes pour ALR
  observeEvent(input$select_all_for_pca_alr, {
    req(prepared(), input$group_col)
    groups <- unique(na.omit(prepared()$data[[input$group_col]]))
    updateCheckboxGroupInput(session, "pca_calculation_groups_alr", selected = groups)
  })
  
  # Sélection rapide: tous les groupes pour ILR
  observeEvent(input$select_all_for_pca_ilr, {
    req(prepared(), input$group_col)
    groups <- unique(na.omit(prepared()$data[[input$group_col]]))
    updateCheckboxGroupInput(session, "pca_calculation_groups_ilr", selected = groups)
  })
  
   #### 7. DATA TABLES ####
  output$data_summary <- renderPrint({
    p <- prepared()
    cat("File:", input$input_file$name, "\n")
    selected_n <- if (is.null(input$data_preview_rows_selected)) nrow(p$data) else length(input$data_preview_rows_selected)
    cat("Number of samples:", nrow(p$data), "\n")
    cat("Selected samples:", selected_n, "/", nrow(p$data), "\n")
    cat("Number of information columns:", ncol(p$sample_info), "\n")
    cat("Number of chemical variables:", ncol(p$chem_data), "\n")
    cat("Sample column:", input$sample_col, "\n")
    cat("Group column:", input$group_col, "\n")
    cat("Groups:", paste(levels(droplevels(as.factor(p$data[[input$group_col]]))), collapse = ", "), "\n")
  })
  
  output$data_preview <- renderDT({
    p <- prepared()
    preview_data <- cbind(Afficher = "", p$data)
    
    DT::datatable(
      preview_data,
      rownames = FALSE,
      escape = TRUE,
      extensions = "Select",
      class = "stripe hover compact",
      selection = list(
        mode = "multiple",
        selected = seq_len(nrow(preview_data)),
        target = "row"
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 100,
        select = list(
          style = "multi",
          selector = "td:first-child"
        ),
        columnDefs = list(
          list(
            targets = 0,
            className = "select-checkbox dt-center",
            orderable = FALSE,
            searchable = FALSE,
            width = "46px"
          )
        )
      )
    )
  }, server = FALSE)
  
  output$texture_classes_table <- renderDT({
    classes <- classify_texture_points(texture_data(), class.sys = input$texture_class_system)
    DT::datatable(classes, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  #### 8. DOWNLOAD HANDLERS ####
  output$download_prepared <- downloadHandler(
    filename = function() "prepared_data.csv",
    content = function(file) write.csv(prepared()$data, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$download_clr <- downloadHandler(
    filename = function() "clr.csv",
    content = function(file) write.csv(as.data.frame(as_clr_matrix(prepared()$object_clr)), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$download_alr <- downloadHandler(
    filename = function() "alr.csv",
    content = function(file) write.csv(as.data.frame(as_clr_matrix(prepared()$object_alr)), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  #### 9. PLOT EXPORT REGISTRATION ####
  register_plot_downloads <- function(id, filename_root, draw_fun, default_width = 8, default_height = 6, default_dpi = 300) {
    filename_value <- function(extension) {
      root <- if (is.function(filename_root)) filename_root() else filename_root
      paste0(sanitize_file_part(root), ".", extension)
    }
    
    output[[paste0("download_", id, "_pdf")]] <- downloadHandler(
      filename = function() filename_value("pdf"),
      content = function(file) {
        dims <- export_dimensions(id, default_width, default_height, default_dpi)
        export_plot_file(
          file = file,
          draw_fun = draw_fun,
          format = "pdf",
          width = dims$width,
          height = dims$height,
          dpi = dims$dpi
        )
      }
    )
    
    output[[paste0("download_", id, "_png")]] <- downloadHandler(
      filename = function() filename_value("png"),
      content = function(file) {
        dims <- export_dimensions(id, default_width, default_height, default_dpi)
        export_plot_file(
          file = file,
          draw_fun = draw_fun,
          format = "png",
          width = dims$width,
          height = dims$height,
          dpi = dims$dpi
        )
      }
    )
  }
  
  register_plot_downloads(
    id = "clr_biplot",
    filename_root = function() paste("clr_biplot", paste(input$clr_biplot_elements, collapse = "_"), sep = "_"),
    draw_fun = function() print(clr_biplot_plot())
  )
  
  register_plot_downloads(
    id = "dimensio_scree",
    filename_root = "dimensio_scree",
    draw_fun = draw_dimensio_scree,
    default_width = 7,
    default_height = 4
  )
  
  register_plot_downloads(
    id = "dimensio_individuals",
    filename_root = "dimensio_individuals",
    draw_fun = draw_dimensio_individuals
  )
  
  register_plot_downloads(
    id = "dimensio_variables",
    filename_root = "dimensio_variables",
    draw_fun = draw_dimensio_variables
  )
  
  register_plot_downloads(
    id = "boxplot",
    filename_root = function() paste("boxplot", paste(input$boxplot_elements, collapse = "_"), sep = "_"),
    draw_fun = function() {
      p <- plot_boxplots(
        df = selected_prepared()$data,
        elements = input$boxplot_elements,
        group_col = input$group_col,
        title = plot_titles$boxplot(),
        palette_name = input$group_palette_name,
        aesthetic_mode = group_aesthetic_mode(),
        shape_palette_name = group_shape_palette_name(),
        axis_text_size = axis_text_sizes$boxplot(),
        label_overrides = plot_labels$boxplot()
      )
      
      if (!isTRUE(input$boxplot_export_show_grid %||% TRUE)) {
        p <- p + theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      }
      
      print(p)
    }
  )
  
  register_plot_downloads(
    id = "correlation_matrix",
    filename_root = function() paste("correlation_matrix", paste(input$correlation_elements, collapse = "_"), sep = "_"),
    draw_fun = function() {
      p <- plot_correlation_matrix(
        df = selected_prepared()$data,
        elements = input$correlation_elements,
        title = plot_titles$correlation_matrix(),
        method = input$correlation_method,
        axis_text_size = axis_text_sizes$correlation_matrix(),
        label_overrides = plot_labels$correlation_matrix(),
        show_values = input$correlation_show_values,
        palette_name = input$correlation_palette
      )
      print(p)
    },
    default_width = 10,
    default_height = 8
  )
  
  register_plot_downloads(
    id = "plot_2d",
    filename_root = function() {
      x <- axis_spec(input$plot2d_x_num, input$plot2d_x_den)
      y <- axis_spec(input$plot2d_y_num, input$plot2d_y_den)
      x_label <- tryCatch(make_element_or_ratio(selected_prepared()$data, x)$label, error = function(e) paste(input$plot2d_x_num, collapse = "+"))
      y_label <- tryCatch(make_element_or_ratio(selected_prepared()$data, y)$label, error = function(e) paste(input$plot2d_y_num, collapse = "+"))
      paste("2d_plot", y_label, "vs", x_label, sep = "_")
    },
    draw_fun = draw_plot_2d_export
  )
  
  register_plot_downloads(
    id = "ternary_isopleuros",
    filename_root = function() paste("ternary_isopleuros", paste(input$ternary_isopleuros_elements, collapse = "_"), sep = "_"),
    draw_fun = draw_ternary_isopleuros
  )
  
  register_plot_downloads(
    id = "ternary_ggtern",
    filename_root = "ternary_ggtern",
    draw_fun = function() print(ternary_ggtern_plot())
  )
  
  register_plot_downloads(
    id = "alr_biplot",
    filename_root = function() paste("alr_biplot", paste(input$alr_biplot_elements, collapse = "_"), sep = "_"),
    draw_fun = function() print(alr_biplot_plot())
  )
  
  register_plot_downloads(
    id = "ilr_biplot",
    filename_root = function() paste("ilr_biplot", paste(input$ilr_biplot_elements, collapse = "_"), sep = "_"),
    draw_fun = function() print(ilr_biplot_plot())
  )
  
  draw_plot_2d_export <- function() {
    
    p <- plot_2d_plot()
    
    if (!isTRUE(input$plot_2d_export_show_grid %||% TRUE)) {
      p <- p + theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    }
    
    print(p)
  }
  
  
  
  
}