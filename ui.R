ui <- fluidPage(
  ### Global styles (CSS) ####
  tags$head(
    tags$style(HTML("
      :root {
        --xrf-dark: #242424;
        --xrf-dark-soft: #3A3A3A;
        --xrf-muted: #6F6A63;
        --xrf-bg: #F7F3EF;
        --xrf-surface: #FFFFFF;
        --xrf-surface-soft: #FBFAF7;
        --xrf-border: #E7E2DA;
        --xrf-mint: #DDEEE4;
        --xrf-lilac: #EADFF2;
        --xrf-peach: #F7DCCB;
        --xrf-blue: #DCE8F4;
        --xrf-shadow: 0 18px 45px rgba(36, 36, 36, 0.08);
      }

      body {
        background:
          linear-gradient(135deg, #F7FCF8 0%, #EEF7F0 48%, #DDEDE2 100%);
        color: var(--xrf-dark);
        font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
        letter-spacing: -0.01em;
      }

      .container-fluid {
        max-width: 1580px;
      }

      .app-hero {
        margin: 20px 0 18px 0;
        padding: 26px 30px;
        border: 1px solid rgba(231, 226, 218, 0.9);
        border-radius: 28px;
        background: rgba(255, 255, 255, 0.78);
        box-shadow: var(--xrf-shadow);
        backdrop-filter: blur(10px);
      }

      .app-hero h1 {
        margin: 0;
        color: var(--xrf-dark);
        font-size: 34px;
        font-weight: 800;
        letter-spacing: -0.04em;
      }

      .app-hero p {
        margin: 8px 0 0 0;
        color: var(--xrf-muted);
        font-size: 15px;
      }

      .app-kicker {
        display: inline-flex;
        align-items: center;
        margin-bottom: 10px;
        padding: 5px 10px;
        border-radius: 999px;
        background: var(--xrf-mint);
        color: var(--xrf-dark);
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }

      h4, h5, .control-label {
        color: var(--xrf-dark);
        font-weight: 750;
      }

      h4 {
        margin-top: 4px;
      }

      .well, .export-panel {
        border: 1px solid rgba(231, 226, 218, 0.95);
        border-radius: 22px;
        background: rgba(255, 255, 255, 0.82);
        box-shadow: 0 12px 28px rgba(36, 36, 36, 0.06);
      }

      .sidebar .well {
        position: sticky;
        top: 16px;
      }

      .export-panel {
        margin-top: 14px;
        margin-bottom: 20px;
        padding: 16px;
        background: rgba(251, 250, 247, 0.9);
      }

      .export-panel h5 {
        margin-top: 0;
      }

      .export-panel .btn {
        margin-right: 8px;
        margin-top: 6px;
      }

      .export-grid-choice {
        margin: 8px 0 12px 0;
        padding: 10px 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 16px;
        background: rgba(255, 255, 255, 0.65);
      }

      .export-grid-choice .form-group,
      .export-grid-choice .checkbox {
        margin: 0;
      }

      .export-grid-choice small {
        display: block;
        margin-top: 4px;
        color: var(--xrf-muted);
      }

      .axis-text-panel {
        display: inline-flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 10px;
        margin: 6px 0 12px 0;
        padding: 10px 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(255, 255, 255, 0.82);
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .axis-text-label {
        color: var(--xrf-muted);
        font-weight: 750;
      }

      .axis-text-controls {
        display: inline-flex;
        align-items: center;
        gap: 8px;
      }

      .axis-text-button.btn {
        min-width: 44px;
        padding: 6px 12px;
      }

      .axis-text-value {
        min-width: 54px;
        text-align: center;
        color: var(--xrf-dark);
        font-weight: 800;
      }

      .plot-title-editor {
        margin: 6px 0 8px 0;
      }

      .plot-title-display,
      .plot-title-display:hover,
      .plot-title-display:focus {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        max-width: 100%;
        padding: 9px 14px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(255, 255, 255, 0.86);
        color: var(--xrf-dark);
        font-weight: 800;
        text-decoration: none;
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .plot-title-display::after {
        content: '✎';
        color: var(--xrf-muted);
        font-size: 13px;
      }

      .plot-title-hint {
        color: var(--xrf-muted);
        font-size: 12px;
        font-weight: 650;
      }

      .plot-title-edit-panel {
        display: flex;
        flex-wrap: wrap;
        align-items: end;
        gap: 8px;
        padding: 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(251, 250, 247, 0.92);
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .plot-title-edit-panel .form-group {
        flex: 1 1 320px;
        margin-bottom: 0;
      }

      .plot-title-edit-panel .btn {
        margin-bottom: 0;
      }

      .plot-labels-editor {
        margin: 4px 0 8px 0;
      }

      .plot-labels-display,
      .plot-labels-display:hover,
      .plot-labels-display:focus {
        display: inline-flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 7px;
        max-width: 100%;
        padding: 8px 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(251, 250, 247, 0.90);
        color: var(--xrf-dark);
        font-weight: 700;
        text-decoration: none;
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.04);
      }

      .plot-labels-display::after {
        content: '✎';
        color: var(--xrf-muted);
        font-size: 13px;
      }

      .plot-label-chip {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        padding: 3px 8px;
        border-radius: 999px;
        background: var(--xrf-mint);
        color: var(--xrf-dark-soft);
        font-size: 12px;
        font-weight: 750;
      }

      .plot-label-chip strong {
        color: var(--xrf-dark);
      }

      .plot-labels-edit-panel {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
        gap: 10px;
        padding: 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(251, 250, 247, 0.92);
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .plot-labels-edit-panel .form-group {
        margin-bottom: 0;
      }

      .plot-label-actions {
        display: flex;
        flex-wrap: wrap;
        align-items: end;
        gap: 8px;
      }

      .plot-text-editor {
        margin: 6px 0 10px 0;
      }

      .plot-text-toggle,
      .plot-text-toggle:hover,
      .plot-text-toggle:focus {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 9px 16px;
        border: 1px solid var(--xrf-border);
        border-radius: 999px;
        background: rgba(255, 255, 255, 0.88);
        color: var(--xrf-dark);
        font-weight: 800;
        text-decoration: none;
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .plot-text-toggle::after {
        content: '✎';
        color: var(--xrf-muted);
        font-size: 13px;
      }

      .plot-text-edit-panel {
        padding: 12px;
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: rgba(251, 250, 247, 0.92);
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.05);
      }

      .plot-text-fields {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        gap: 10px;
      }

      .plot-text-edit-panel .form-group {
        margin-bottom: 0;
      }

      .plot-text-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 10px;
      }

      .nav-tabs {
        border-bottom: 0;
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-bottom: 16px;
      }

      .nav-tabs > li {
        margin-bottom: 0;
      }

      .nav-tabs > li > a {
        border: 1px solid transparent;
        border-radius: 999px;
        background: rgba(255, 255, 255, 0.72);
        color: var(--xrf-dark-soft);
        font-weight: 650;
      }

      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        border-color: var(--xrf-border);
        background: var(--xrf-blue);
        color: var(--xrf-dark);
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        border: 1px solid var(--xrf-dark);
        background: var(--xrf-dark);
        color: #FFFFFF;
      }

      .tab-content {
        padding: 4px 2px 18px 2px;
      }

      .form-control,
      .selectize-input,
      .selectize-control.single .selectize-input,
      .selectize-control.multi .selectize-input,
      input[type='number'] {
        min-height: 40px;
        border: 1px solid var(--xrf-border);
        border-radius: 14px;
        background: var(--xrf-surface-soft);
        color: var(--xrf-dark);
        box-shadow: none;
      }

      .form-control:focus,
      .selectize-input.focus,
      .selectize-control.single .selectize-input.focus,
      input[type='number']:focus {
        border-color: var(--xrf-dark);
        box-shadow: 0 0 0 3px rgba(234, 223, 242, 0.9);
      }

      .selectize-dropdown {
        border-color: var(--xrf-border);
        border-radius: 14px;
        box-shadow: var(--xrf-shadow);
      }

      .selectize-control.multi .selectize-input > div {
        border-radius: 999px;
        background: var(--xrf-lilac);
        color: var(--xrf-dark);
      }

      .btn,
      .btn-default,
      .shiny-download-link {
        border: 0;
        border-radius: 999px;
        background: var(--xrf-lilac);
        color: var(--xrf-dark);
        font-weight: 750;
        box-shadow: 0 8px 18px rgba(36, 36, 36, 0.08);
        transition: transform 0.15s ease, box-shadow 0.15s ease, background 0.15s ease;
      }

      .btn:hover,
      .btn:focus,
      .btn-default:hover,
      .btn-default:focus,
      .shiny-download-link:hover,
      .shiny-download-link:focus {
        background: var(--xrf-peach);
        color: var(--xrf-dark);
        transform: translateY(-1px);
        box-shadow: 0 12px 24px rgba(36, 36, 36, 0.12);
      }

      .btn:active,
      .btn-default:active,
      .shiny-download-link:active {
        transform: translateY(0);
      }

      hr {
        border-top: 1px solid var(--xrf-border);
      }

      .mode-toggle-wrapper { margin-bottom: 14px; }
      .mode-toggle-line { display: flex; align-items: center; gap: 10px; color: var(--xrf-muted); font-weight: 650; }
      .mode-switch .form-group { margin-bottom: 0; }
      .mode-switch .checkbox { margin-top: 0; margin-bottom: 0; }
      .mode-switch label { padding-left: 0; margin-bottom: 0; }
      .mode-switch input[type='checkbox'] { opacity: 0; position: absolute; }
      .mode-switch input[type='checkbox'] + span {
        display: inline-block;
        width: 58px;
        height: 30px;
        border-radius: 999px;
        background: var(--xrf-mint);
        border: 1px solid var(--xrf-border);
        position: relative;
        cursor: pointer;
        font-size: 0;
        vertical-align: middle;
        box-shadow: inset 0 1px 3px rgba(36, 36, 36, 0.08);
      }
      .mode-switch input[type='checkbox'] + span::before {
        content: '';
        position: absolute;
        width: 24px;
        height: 24px;
        left: 3px;
        top: 2px;
        border-radius: 50%;
        background: #FFFFFF;
        box-shadow: 0 3px 8px rgba(36, 36, 36, 0.20);
        transition: transform 0.18s ease-in-out;
      }
      .mode-switch input[type='checkbox']:checked + span { background: var(--xrf-dark); }
      .mode-switch input[type='checkbox']:checked + span::before { transform: translateX(27px); }

      .sample-table-filter-actions {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 8px;
        margin: 14px 0 12px 0;
        padding: 14px 16px;
        border: 1px solid var(--xrf-border);
        border-radius: 22px;
        background: rgba(255, 255, 255, 0.82);
        box-shadow: 0 10px 24px rgba(36, 36, 36, 0.05);
      }

      .sample-table-filter-actions .sample-filter-help {
        flex-basis: 100%;
        color: var(--xrf-muted);
        margin: 0 0 4px 0;
      }

      .sample-row-checkbox {
        width: 18px;
        height: 18px;
        cursor: pointer;
        accent-color: var(--xrf-dark);
      }

      table.dataTable tbody td.dt-center,
      table.dataTable thead th.dt-center {
        text-align: center;
        vertical-align: middle;
      }

      .shiny-plot-output,
      .plotly,
      .html-widget,
      .dataTables_wrapper {
        border-radius: 22px;
        background: rgba(255, 255, 255, 0.78);
      }

      .dataTables_wrapper {
        padding: 14px;
        border: 1px solid var(--xrf-border);
        box-shadow: 0 10px 24px rgba(36, 36, 36, 0.05);
      }

      table.dataTable thead th {
        border-bottom: 1px solid var(--xrf-border) !important;
        color: var(--xrf-dark);
      }

      table.dataTable tbody tr {
        background-color: var(--xrf-surface-soft) !important;
      }

      table.dataTable tbody tr:hover {
        background-color: var(--xrf-mint) !important;
      }


      table.dataTable tbody tr.selected,
      table.dataTable tbody tr.selected > td,
      table.dataTable tbody tr.selected > th {
        background-color: var(--xrf-surface-soft) !important;
        color: var(--xrf-dark) !important;
        box-shadow: none !important;
      }

      table.dataTable tbody tr.selected:hover,
      table.dataTable tbody tr.selected:hover > td,
      table.dataTable tbody tr.selected:hover > th {
        background-color: var(--xrf-mint) !important;
        color: var(--xrf-dark) !important;
        box-shadow: none !important;
      }

      table.dataTable tbody td.select-checkbox::before,
      table.dataTable tbody th.select-checkbox::before {
        border: 1px solid var(--xrf-border) !important;
        border-radius: 5px !important;
        background: #FFFFFF !important;
      }

      table.dataTable tbody tr.selected td.select-checkbox::before,
      table.dataTable tbody tr.selected th.select-checkbox::before {
        border-color: #2F6FDB !important;
        background: #2F6FDB !important;
      }

      table.dataTable tbody tr.selected td.select-checkbox::after,
      table.dataTable tbody tr.selected th.select-checkbox::after {
        color: #FFFFFF !important;
        text-shadow: none !important;
      }

      pre {
        border: 1px solid var(--xrf-border);
        border-radius: 18px;
        background: var(--xrf-surface-soft);
        color: var(--xrf-dark);
      }
    "))
  ),
  
  # App header ####
  tags$div(
    class = "app-hero",
    h1("Geochemical data treatment")
  ),
  
  # Layout: sidebar + main panel ####
  sidebarLayout(
    sidebarPanel(
      
      #### File and global parameters ####
      h4("File and global parameters"),
      fileInput(
        "input_file",
        "Excel file",
        accept = c(".xlsx", ".xls")
      ),
      
      textInput("info_cols", "Information columns", value = "1:3"),
      numericInput("lod_factor", "Factor LOD", value = 0.65, min = 0, step = 0.05),
      selectInput("alr_denominator", "ALR denominator:", choices = NULL),
      
      #### Group display mode ####
      tags$div(
        class = "mode-toggle-wrapper",
        tags$label("Group display", class = "control-label"),
        tags$div(
          class = "mode-toggle-line",
          tags$span("Symbols"),
          tags$div(
            class = "mode-switch",
            checkboxInput("group_use_colour", label = HTML("&nbsp;"), value = TRUE)
          ),
          tags$span("Colors")
        )
      ),
      
      #### Color/shape palettes ####
      conditionalPanel(
        condition = "input.group_use_colour === true",
        selectInput(
          "group_palette_name",
          "Color palette",
          choices = c("Infinite (HCL)", "Okabe-Ito", "Polychromatic 36"),
          selected = "Okabe-Ito"
        )
      ),
      conditionalPanel(
        condition = "input.group_use_colour === false",
        selectInput(
          "group_shape_palette_name",
          "Shape palette",
          choices = c("Classic", "Open shapes", "Black and white mix"),
          selected = "Black and white mix"
        )
      ),
      
      #### Data mapping UI (dynamic) ####
      hr(),
      uiOutput("data_mapping_ui"),
      
      width = 3
    ),
    
    # MAIN PANEL (outputs) ####
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data summary",
          br(),
          verbatimTextOutput("data_summary"),
          fluidRow(
            # Column 1
            column(
              6,
              tags$div(
                class = "sample-table-filter-actions",
                style = "width: 100%; display: flex; flex-direction: column; gap: 10px;",
                tags$p(
                  class = "sample-filter-help",
                  "By default, all samples are selected."
                ),
                div(
                  style = "display: flex; gap: 8px; width: 100%;",
                  actionButton("select_all_samples", "Select all"),
                  actionButton("clear_samples", "Clear all")
                ),
                conditionalPanel(
                  condition = "input.group_col !== null && input.group_col !== ''",
                  div(
                    h5("Filter by group:"),
                    uiOutput("group_filter_ui"),
                    tags$hr(style = "margin: 4px 0; border-top: 1px solid var(--xrf-border); width: 100%;"),
                    div(
                      style = "display: flex; gap: 8px; width: 100%;",
                      actionButton("apply_group_filter", "Apply", class = "btn-primary"),
                      actionButton("clear_group_filter", "Clear", class = "btn-default")
                    )
                  )
                )
              )
            ),
            
            # Column 2
            column(
              6,
              tags$div(
                class = "sample-table-filter-actions",
                style = "width: 100%; display: flex; flex-direction: column; gap: 8px;",
                h5("Filter by value"),
                selectInput("filter_column", "Column:", choices = NULL, width = "100%"),
                div(
                  style = "width: 100%;",
                  uiOutput("filter_range_ui")
                ),
                tags$hr(style = "margin: 4px 0; border-top: 1px solid var(--xrf-border); width: 100%;"),
                div(
                  style = "display: flex; gap: 8px; width: 100%;",
                  actionButton("apply_value_filter", "Apply", class = "btn-primary"),
                  actionButton("clear_value_filter", "Clear", class = "btn-default")
                )
              )
            )
          ),
          DTOutput("data_preview")
        ),
        
        # CLR Biplot Tab ####
        tabPanel(
          "CLR biplot",
          br(),
          wellPanel(
            h4("Choice of CLR biplot"),
            uiOutput("clr_ui"),
            checkboxInput("show_dimensio_plots", "Display dimensio plots", value = FALSE),
            checkboxInput("show_confidence_ellipses", "Show Confidence Ellipses", value = FALSE)
          ),
          plot_text_controls_ui("clr_biplot"),
          axis_text_controls_ui("clr_biplot"),
          plotlyOutput("clr_biplot", height = "650px"),
          export_controls_ui("clr_biplot", "Export CLR biplot"),
          conditionalPanel(
            condition = "input.show_dimensio_plots === true",
            plot_text_controls_ui("dimensio_scree"),
            axis_text_controls_ui("dimensio_scree"),
            plotOutput("dimensio_scree", height = "350px"),
            export_controls_ui("dimensio_scree", "Export (dimensio)"),
            plot_text_controls_ui("dimensio_individuals"),
            axis_text_controls_ui("dimensio_individuals"),
            plotOutput("dimensio_individuals", height = "450px"),
            export_controls_ui("dimensio_individuals", "Export individuals (dimensio)"),
            plot_text_controls_ui("dimensio_variables"),
            axis_text_controls_ui("dimensio_variables"),
            plotOutput("dimensio_variables", height = "450px"),
            export_controls_ui("dimensio_variables", "Export variables (dimensio)")
          )
        ),
        
        # ALR Biplot Tab
        tabPanel(
          "ALR biplot",
          br(),
          wellPanel(
            h4("Choice of ALR biplot"),
            uiOutput("alr_ui"),
            checkboxInput("show_confidence_ellipses_alr", "Show Confidence Ellipses", value = FALSE)
          ),
          plot_text_controls_ui("alr_biplot"),
          axis_text_controls_ui("alr_biplot"),
          plotlyOutput("alr_biplot", height = "650px"),
          export_controls_ui("alr_biplot", "Export ALR biplot")
        ),
        
        # ILR Biplot Tab
        tabPanel(
          "ILR biplot",
          br(),
          wellPanel(
            h4("Choice of ILR biplot"),
            uiOutput("ilr_ui"),
            checkboxInput("show_confidence_ellipses_alr", "Show Confidence Ellipses", value = FALSE)
          ),
          plot_text_controls_ui("ilr_biplot"),
          axis_text_controls_ui("ilr_biplot"),
          plotlyOutput("ilr_biplot", height = "650px"),
          export_controls_ui("ilr_biplot", "Export ILR biplot")
        ),
        
        # Boxplots Tab ####
        tabPanel(
          "Boxplots",
          br(),
          wellPanel(
            h4("Comparison of element concentration by groups"),
            uiOutput("boxplot_ui")
          ),
          plot_text_controls_ui("boxplot"),
          axis_text_controls_ui("boxplot"),
          plotlyOutput("boxplot", height = "650px"),
          export_controls_ui("boxplot", "Export Boxplot", grid_choice = TRUE)
        ),
        
        # Correlation Matrix Tab ####
        tabPanel(
          "Correlation matrix",
          br(),
          wellPanel(
            h4("Correlation matrix"),
            uiOutput("correlation_ui")
          ),
          plot_text_controls_ui("correlation_matrix"),
          axis_text_controls_ui("correlation_matrix"),
          plotlyOutput("correlation_matrix", height = "750px"),
          export_controls_ui("correlation_matrix", "Export correlation matrix")
        ),
        
        # 2D Plot Tab ####
        tabPanel(
          "2D plot",
          br(),
          wellPanel(
            h4("Choice of 2D plot"),
            uiOutput("plot2d_ui")
          ),
          plot_text_controls_ui("plot_2d"),
          axis_text_controls_ui("plot_2d"),
          plotlyOutput("plot_2d", height = "650px"),
          export_controls_ui("plot_2d", "Export 2D plot", grid_choice = TRUE)
        ),
        
        # Ternary Isopleuros Tab ####
        tabPanel(
          "Ternary plot (isopleuros)",
          br(),
          wellPanel(
            h4("Choice of the ternary plot with isopleuros"),
            uiOutput("ternary_isopleuros_ui")
          ),
          plot_text_controls_ui("ternary_isopleuros"),
          axis_text_controls_ui("ternary_isopleuros"),
          plotOutput("ternary_isopleuros", height = "650px"),
          export_controls_ui("ternary_isopleuros", "Export the ternary diagram")
        ),
        
        # Ternary ggtern Tab ####
        tabPanel(
          "Ternary plot (ggtern)",
          br(),
          wellPanel(
            h4("Choice of the ternary plot with ggtern"),
            uiOutput("ternary_ggtern_ui")
          ),
          plot_text_controls_ui("ternary_ggtern"),
          axis_text_controls_ui("ternary_ggtern"),
          plotOutput("ternary_ggtern", height = "650px"),
          export_controls_ui("ternary_ggtern", "Export the ternary diagram")
        ),
        
        # Export data Tab ####
        tabPanel(
          "Exports",
          br(),
          downloadButton("download_prepared", "Download the prepared data"),
          downloadButton("download_clr", "Download CLR"),
          downloadButton("download_alr", "Download ALR")
        )
      ),
      width = 9
    )
  )
)