# ==============================================================================
#   A/B TESTING ENGINE: EXECUTIVE DECISION DASHBOARD
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(shinycssloaders)
library(plotly)
library(lubridate)
library(jsonlite)
library(readxl) 

options(shiny.maxRequestSize = 50 * 1024^2)

# --- GLOBAL SETUP (LIVE CURRENCY API) ---
get_live_rates <- function() {
  tryCatch({
    req <- jsonlite::fromJSON("https://open.er-api.com/v6/latest/USD")
    req$rates
  }, error = function(e) {
    list(USD = 1, EUR = 0.92, GBP = 0.79, PKR = 278.50, JPY = 150.0, 
         AUD = 1.5, CAD = 1.35, CHF = 0.9, INR = 83.0, CNY = 7.2) 
  })
}
exchange_rates <- get_live_rates()
# ---------------------------------------------

# --- UI DEFINITION ---
ui <- fluidPage(
  theme = bs_theme(
    version = 5, 
    bootswatch = "zephyr", 
    primary = "#2c3e50", 
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  # --- CSS STYLING ENGINE ---
  tags$head(
    tags$style(HTML("
      /* 1. Deep Modern Background (SaaS Mesh Gradient) */
      body {
        background: linear-gradient(135deg, #f0f4f8 0%, #e2e8f0 100%);
        font-family: 'Inter', sans-serif;
      }
      
      /* 2. Glassmorphism Sidebar */
      .well {
        background: rgba(255, 255, 255, 0.6) !important;
        backdrop-filter: blur(12px) !important;
        -webkit-backdrop-filter: blur(12px) !important; /* Safari support */
        border: 1px solid rgba(255, 255, 255, 0.8) !important;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.05) !important;
        border-radius: 16px !important;
      }
      
     /* 3. Base animations for ALL elements */
      .shiny-html-output > div {
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      /* Smart Card Targeting: Only apply white background if there is no custom color */
      .shiny-html-output > div:not([style*='background-color']) {
        background: #ffffff;
        border-radius: 16px !important;
        border: 1px solid #edf2f7 !important;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03) !important;
      }
      
      /* Hover Lift for ALL Main Containers (Including the Bayesian Badge) */
      .shiny-html-output > div:hover {
        transform: translateY(-4px);
        box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.15), 0 10px 10px -5px rgba(0, 0, 0, 0.04) !important;
      }
      
      /* 4. Hyper-Polished Inputs */
      .form-control, .selectize-input {
        background-color: #f8fafc !important;
        border: 1px solid #e2e8f0 !important;
        border-radius: 8px !important;
        color: #1e293b !important;
        transition: all 0.2s ease !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        background-color: #ffffff !important;
        border-color: #3b82f6 !important;
        box-shadow: 0 0 0 4px rgba(59, 130, 246, 0.15) !important;
      }
      
      /* 5. Animated 'Action' Button */
      .btn-outline-primary {
        background-size: 200% auto;
        background-image: linear-gradient(to right, #2563eb 0%, #4f46e5 51%, #2563eb 100%);
        color: white !important;
        border: none !important;
        border-radius: 8px !important;
        font-weight: 600 !important;
        letter-spacing: 0.5px;
        box-shadow: 0 4px 15px rgba(37, 99, 235, 0.4) !important;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1) !important;
      }
      
      .btn-outline-primary:hover {
        background-position: right center; /* Triggers the gradient animation */
        box-shadow: 0 6px 20px rgba(37, 99, 235, 0.6) !important;
        transform: translateY(-2px);
      }
      
      /* 6. Fix Stacking Context & Z-Index War */
      .selectize-dropdown {
        z-index: 9999 !important;
        border-radius: 8px !important;
        box-shadow: 0 10px 25px rgba(0,0,0,0.1) !important;
      }
      .form-group, .shiny-input-container {
        transform: none !important;
        transition: none !important;
        box-shadow: none !important;
        background: transparent !important;
      }
      
      /* Plotly Graph Mobile Responsiveness */
      .js-plotly-plot {
        max-width: 100% !important;
      }
    "))
  ),
  # ----------------------------------------
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      style = "background-color: #f8f9fc; border-right: 1px solid #e3e6f0;",
      h4(icon("bullseye"), " Campaign Data"),
      
      # Data Source Toggle
      radioButtons("data_source", "Select Data Source:", 
                   choices = c("Manual Entry" = "manual", "Raw data Upload" = "csv"), 
                   inline = TRUE),
      hr(),
      
      # Conditional UI: File Upload
      conditionalPanel(
        condition = "input.data_source == 'csv'",
        div(style = "background-color: #e8fbf3; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
            p(strong("Data Format Required:"), br(), "One row per visitor. Must contain a 'Group' column (e.g., A/B) and a 'Conversion' column (1 or 0).", style="font-size: 0.85em; color: #0f6848; margin-bottom: 0;")
        ),
        fileInput("file1", "Upload Raw Data (.csv, .xlsx, .tsv)", accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt")),
        helpText(em("Note: Pre-loaded with sample e-commerce data. Upload your own file to override.")),
        uiOutput("csv_mapping_ui"),
        uiOutput("segmentation_ui"),
        hr()
      ),
      
      # Conditional UI: Manual Entry
      conditionalPanel(
        condition = "input.data_source == 'manual'",
        h5("Variation A (Control)"),
        numericInput("visitors_A", "Total Visitors / Emails Sent:", value = 1500, min = 1),
        numericInput("conversions_A", "Total Conversions / Clicks:", value = 120, min = 0),
        hr(),
        
        h5("Variation B (Challenger)"),
        numericInput("visitors_B", "Total Visitors / Emails Sent:", value = 1550, min = 1),
        numericInput("conversions_B", "Total Conversions / Clicks:", value = 165, min = 0)
      )
      
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "pills",
        
        # --- TAB 1: EXECUTIVE DASHBOARD ---
        tabPanel("Executive Dashboard", icon = icon("chart-bar"),
                 br(),
                 
                 # Horizontal Business Projection Toolbar
                 div(style = "background-color: #fffaf0; border: 1px solid #fce8b2; border-left: 4px solid #f6c23e; border-radius: 0.5rem; padding: 15px 20px 0px 20px; margin-bottom: 20px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.05);",
                     fluidRow(
                       column(3, h5(icon("chart-line"), " Business Projections", style="color: #b8860b; margin-top: 10px; font-weight: bold;")),
                       column(3, selectInput("currency", "Currency:", 
                                             choices = c("$ (USD)" = "$", "₨ (PKR)" = "₨", "€ (EUR)" = "€", 
                                                         "£ (GBP)" = "£", "¥ (JPY)" = "¥", "A$ (AUD)" = "A$", 
                                                         "C$ (CAD)" = "C$", "₣ (CHF)" = "₣", "₹ (INR)" = "₹", 
                                                         "元 (CNY)" = "元"), selected = "$")),
                       column(3, numericInput("future_traffic", "Est. Future Traffic:", value = 100000, min = 100)),
                       column(3, numericInput("aov", "Avg. Order Value ($):", value = 50, min = 1))
                     )
                 ),
                 
                 div(style = "background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 25px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15); margin-bottom: 25px;",
                     h4(icon("gavel"), " Executive Verdict", style="color: #4e73df; font-weight: bold; border-bottom: 2px solid #eaecf4; padding-bottom: 10px;"),
                     uiOutput("executive_verdict_ui"),
                     div(style = "text-align: right; margin-top: 15px;",
                         downloadButton("download_report", "Download Executive Report (.pdf)", class = "btn-outline-primary")
                     )
                 ),
                 fluidRow(
                   column(6,
                          div(style = "background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 20px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);",
                              h5("Conversion Rates (95% CI)", style="text-align:center;"),
                              # Wrapped the plot in a loading spinner
                              withSpinner(plotlyOutput("conversion_plot", height = "300px"), type = 8, color = "#4e73df")
                          )
                   ),
                   column(6,
                          div(style = "background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 20px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);",
                              h5("Bayesian Probability Curves", style="text-align:center;"),
                              withSpinner(plotlyOutput("bayesian_plot", height = "300px"), type = 8, color = "#1cc88a")
                          )
                   )
                 ),
                 # Container for the Trend Line Chart
                 br(),
                 uiOutput("trend_chart_ui")
        ),
        
        # --- TAB 2: POWER ANALYSIS (HOW MUCH LONGER?) ---
        tabPanel("Power Analysis", icon = icon("hourglass-half"),
                 br(),
                 fluidRow(
                   column(5,
                          div(style = "background-color: #f8f9fc; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 20px;",
                              h5(icon("cogs"), " Experiment Parameters"),
                              p("What are we trying to detect?", style="color:#666; font-size:0.9em;"),
                              
                              numericInput("mde", "Minimum Detectable Effect (% relative):", value = 5, min = 0.1),
                              helpText("The smallest relative improvement that makes a business impact (e.g., 'Boost sales by at least 5%').", style="margin-top:-10px; font-size:0.85em;"),
                              
                              numericInput("target_power", "Statistical Power (%):", value = 80, min = 50, max = 99),
                              helpText("Your safety net against missing a true winner. 80% is the industry standard.", style="margin-top:-10px; font-size:0.85em;"),
                              
                              hr(),
                              numericInput("daily_traffic", "Average Daily Visitors:", value = 500, min = 1)
                          )
                   ),
                   column(7,
                          div(style = "background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 25px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);",
                              h4(icon("calendar-check"), " Timeline Verdict", style="color: #4e73df; font-weight: bold; border-bottom: 2px solid #eaecf4; padding-bottom: 10px;"),
                              uiOutput("power_verdict_ui")
                          )
                   )
                 )
        )
      )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # --- AUTO-CONVERTING CURRENCY ENGINE ---
  # Track the active currency to convert the AOV box dynamically
  prev_currency <- reactiveVal("USD")
  
  observeEvent(input$currency, {
    req(input$aov)
    code_map <- c("$" = "USD", "₨" = "PKR", "€" = "EUR", "£" = "GBP", 
                  "¥" = "JPY", "A$" = "AUD", "C$" = "CAD", "₣" = "CHF", 
                  "₹" = "INR", "元" = "CNY")
    new_code <- code_map[[input$currency]]
    old_code <- prev_currency()
    
    # Only run the conversion if the currency actually changed
    if (new_code != old_code) {
      # Calculate the ratio between the two currencies using our live API data
      ratio <- exchange_rates[[new_code]] / exchange_rates[[old_code]]
      new_aov <- input$aov * ratio
      
      # Automatically rewrite the input box label and converted value
      updateNumericInput(session, "aov", 
                         label = paste0("Avg. Order Value (", input$currency, "):"), 
                         value = round(new_aov, 2))
      
      # Lock in the new currency state
      prev_currency(new_code)
    }
  })
  # --------------------------------------------
  
  # Reactive Math Engine
  # Reactive Data Loader with Pre-loaded Default
  raw_data <- reactive({
    
    # 1. If NO file is uploaded, load the local portfolio dataset
    if (is.null(input$file1)) {
      if (file.exists("portfolio_ab_data.csv")) {
        return(read.csv("portfolio_ab_data.csv", stringsAsFactors = FALSE))
      } else {
        return(NULL) # Failsafe if the file is missing from the folder
      }
    }
    
    # 2. If a file IS uploaded, parse it based on extension
    ext <- tools::file_ext(input$file1$name)
    
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file1$datapath)
      } else if (ext %in% c("tsv", "txt")) {
        read.delim(input$file1$datapath, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    }, error = function(e) return(NULL))
    
    return(df)
  })
  
  # Dynamic Column Selectors 
  # 1. Core Column Selectors 
  output$csv_mapping_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    col_names <- names(df)
    
    tagList(
      # A blank default to force the user to make a choice before the math runs
      selectInput("var_col", "Which column identifies the Variation? (A/B)", 
                  choices = c("Select column..." = "", col_names), selected = ""),
      
      uiOutput("baseline_ui"),
      
      selectInput("conv_col", "Which column shows Conversions? (1/0)", 
                  choices = c("Select column..." = "", col_names), selected = ""),
      
      selectInput("guard_col", "Guardrail Metric (Negative Event) [Optional]:", 
                  choices = c("None", col_names), selected = "None"),
      helpText("A negative event (e.g., unsubscribes, errors) you want to ensure Variation B doesn't increase. Must be a 1/0 or Yes/No column.", style="margin-top:-10px; font-size:0.85em;"),
      
      selectInput("date_col", "Which column has Dates? (Optional)", 
                  choices = c("None", col_names), selected = "None")
    )
  })
  
  # 2. Smart Baseline Selector
  output$baseline_ui <- renderUI({
    req(raw_data(), input$var_col)
    df <- raw_data()
    
    unique_vars <- unique(df[[input$var_col]])
    unique_vars <- unique_vars[!is.na(unique_vars)]
    
    shiny::validate(shiny::need(
      length(unique_vars) <= 2, 
      "Consultant Alert: You selected a column with too many unique values (like an ID column). The Variation (A/B) column must contain exactly 2 groups."
    ))
    
    selectInput("baseline_var", "Which group is the Control (Baseline)?", choices = unique_vars)
  })
  
  # 3. Dynamic Segmentation UI (Prevents Logical Paradoxes & Browser Freezes)
  output$segmentation_ui <- renderUI({
    req(raw_data(), input$var_col, input$conv_col)
    df <- raw_data()
    
    # Automatically remove the core columns from the filter choices
    forbidden_cols <- c(input$var_col, input$conv_col, input$date_col)
    candidate_cols <- setdiff(names(df), forbidden_cols)
    
    # Prevent browser freezes by ignoring massive ID columns
    # Only keep columns that have fewer than 50 unique categorical values
    avail_cols <- Filter(function(col) {
      length(unique(df[[col]])) > 1 && length(unique(df[[col]])) < 50
    }, candidate_cols)
    
    tagList(
      hr(),
      h6(icon("filter"), " Audience Segmentation (Optional)", style="color: #4e73df; font-weight: bold;"),
      selectInput("seg_col", "Select a Demographic Column:", choices = c("None", avail_cols)),
      uiOutput("seg_val_ui") # Renders the specific category choices
    )
  })
  
  # 4. Dynamic Segment Value Selector
  output$seg_val_ui <- renderUI({
    req(raw_data(), input$seg_col)
    if(input$seg_col == "None") return(NULL)
    
    df <- raw_data()
    unique_vals <- unique(df[[input$seg_col]])
    unique_vals <- unique_vals[!is.na(unique_vals)] # Drop blank categories
    
    selectInput("seg_val", paste("Filter by", input$seg_col, ":"), choices = c("All", as.character(unique_vals)))
  })
  
  # Reactive Math Engine (With Guardrail Safety)
  ab_math_base <- reactive({
    
    # Initialize defaults so Manual Entry mode doesn't crash
    guardrail_alert <- FALSE
    guard_A_rate <- 0
    guard_B_rate <- 0
    simpsons_alert <- FALSE
    simpsons_text <- ""
    
    # Pathway 1: Manual Entry
    if (input$data_source == "manual") {
      req(input$visitors_A, input$conversions_A, input$visitors_B, input$conversions_B)
      
      # --- NaN CRASH SHIELDS ---
      shiny::validate(shiny::need(input$conversions_A <= input$visitors_A, "Consultant Error: Conversions cannot exceed total visitors for Variation A."))
      shiny::validate(shiny::need(input$conversions_B <= input$visitors_B, "Consultant Error: Conversions cannot exceed total visitors for Variation B."))
      # ------------------------------
      
      v_A <- input$visitors_A
      c_A <- input$conversions_A
      v_B <- input$visitors_B
      c_B <- input$conversions_B
      
      # Pathway 2: CSV Upload
    } else {
      req(raw_data(), input$var_col, input$conv_col)
      df <- raw_data()
      
      # --- SHIELD 1: SAME COLUMN SELECTION ---
      shiny::validate(shiny::need(
        input$var_col != input$conv_col,
        "Consultant Alert: You selected the exact same column for both Variation and Conversions. Please select two distinct columns."
      ))
      # -------------------------------------------
      
      # --- SANITIZATION SHIELD ---
      df <- df[!is.na(df[[input$var_col]]) & !is.na(df[[input$conv_col]]), ]
      c_data <- df[[input$conv_col]]
      
      # Extract unique values to check for typos/outliers
      c_unique <- unique(c_data[!is.na(c_data)])
      shiny::validate(shiny::need(
        length(c_unique) <= 2, 
        "Consultant Error: Data anomaly detected in the conversion column. Expected exactly 2 states, but found more. Please check your data for typos."
      ))
      
      if(is.character(c_data) || is.factor(c_data)) {
        c_data <- tolower(trimws(as.character(c_data)))
        df[[input$conv_col]] <- ifelse(c_data %in% c("yes", "true", "t", "y", "1", "success"), 1, 0)
      } else if (is.logical(c_data)) {
        df[[input$conv_col]] <- as.numeric(c_data)
      } else if (is.numeric(c_data) || is.integer(c_data)) {
        c_max <- max(c_data, na.rm = TRUE)
        df[[input$conv_col]] <- ifelse(c_data == c_max, 1, 0)
      }
      
      # Final Safety Check
      shiny::validate(shiny::need(all(df[[input$conv_col]] %in% c(0, 1)), 
                                  "Consultant Error: The conversion column could not be processed."))
      
      # --- SHIELD 2: ZERO CONVERSIONS TRAP ---
      # Prevents NaN cascade if a user selects a text column with no recognized "success" words
      total_convs <- sum(df[[input$conv_col]], na.rm = TRUE)
      shiny::validate(shiny::need(
        total_convs > 0,
        "Consultant Alert: Zero valid conversions found. If you selected a text column, ensure it contains recognizable success values like '1', 'yes', 'true', or 'success'."
      ))
      # -------------------------------------------
      
      # --- SEGMENTATION FILTER ---
      if (!is.null(input$seg_col) && input$seg_col != "None" && input$seg_col %in% names(df) && !is.null(input$seg_val) && input$seg_val != "All") {
        df <- df[df[[input$seg_col]] == input$seg_val, ]
        shiny::validate(shiny::need(nrow(df) > 0, "Consultant Alert: This specific segment has no data. Please select another."))
        
        # Collinearity & Skew Shields
        var_counts <- table(as.character(df[[input$var_col]]))
        
        # Check 1: Perfect Collinearity (100% overlap)
        shiny::validate(shiny::need(length(var_counts) == 2 && min(var_counts) > 0, 
                                    "Consultant Alert: Perfect Collinearity. Every user in this segment saw the exact same variation. A comparison is impossible."))
        
        # Check 2: Extreme Skew (High correlation)
        shiny::validate(shiny::need(min(var_counts) >= 5, 
                                    "Consultant Alert: Extreme Skew. One variation has almost no users in this segment, making a reliable statistical comparison impossible."))
      }
      
      # Find the two unique variations and lock the baseline
      variations <- unique(df[[input$var_col]])
      variations <- variations[!is.na(variations)]
      shiny::validate(shiny::need(length(variations) == 2, "Error: The Variation column must contain exactly 2 unique groups (e.g., Control and Challenger). If you have more, check your raw data for spelling errors or accidental outliers."))
      
      req(input$baseline_var)
      # Protect against baseline ghost variables when swapping CSVs
      shiny::validate(shiny::need(input$baseline_var %in% variations, "Updating baseline engine..."))
      
      var1 <- input$baseline_var
      var2 <- setdiff(variations, var1)[1] 
      
      # Filter data and calculate totals
      df_A <- df[df[[input$var_col]] == var1, ]
      df_B <- df[df[[input$var_col]] == var2, ]
      
      v_A <- nrow(df_A)
      c_A <- sum(as.numeric(df_A[[input$conv_col]]), na.rm = TRUE)
      
      v_B <- nrow(df_B)
      c_B <- sum(as.numeric(df_B[[input$conv_col]]), na.rm = TRUE)
      
      # --- GUARDRAIL SAFETY ENGINE ---
      if (!is.null(input$guard_col) && input$guard_col != "None") {
        # --- SHIELD GUARDRAIL OVERLAP TRAP ---
        shiny::validate(shiny::need(
          input$guard_col != input$var_col,
          "Consultant Alert: You selected your Variation (A/B) column as the Guardrail. A guardrail must be a completely separate column tracking negative events."
        ))
        
        shiny::validate(shiny::need(
          input$guard_col != input$conv_col,
          "Consultant Alert: You selected your Conversion column as the Guardrail. Your primary success metric cannot also be your negative failure metric."
        ))
        # --------------------------------------------
        # Sanitize the guardrail data 
        g_data <- df[[input$guard_col]]
        # Reject non-binary columns (prevents users from selecting landing_page)
        g_unique <- unique(g_data[!is.na(g_data)])
        shiny::validate(shiny::need(length(g_unique) <= 2, 
                                    "Consultant Alert: The Guardrail metric must be binary. You selected a column with too many unique values."))
        if(is.character(g_data) || is.factor(g_data)) {
          g_data <- tolower(trimws(as.character(g_data)))
          df[[input$guard_col]] <- ifelse(g_data %in% c("yes", "true", "t", "y", "1", "error", "fail"), 1, 0)
        } else if (is.logical(g_data)) {
          df[[input$guard_col]] <- as.numeric(g_data)
        }
        
        # Calculate negative events for both groups
        g_A <- sum(as.numeric(df_A[[input$guard_col]]), na.rm = TRUE)
        g_B <- sum(as.numeric(df_B[[input$guard_col]]), na.rm = TRUE)
        
        guard_A_rate <- g_A / v_A
        guard_B_rate <- g_B / v_B
        
        # If Variation B has a higher error rate, run a 1-sided test
        if (guard_B_rate > guard_A_rate) {
          g_test <- tryCatch({
            prop.test(x = c(g_A, g_B), n = c(v_A, v_B), alternative = "less") 
          }, error = function(e) NULL)
          
          if (!is.null(g_test) && g_test$p.value < 0.05) {
            guardrail_alert <- TRUE
          }
        }
      }
      # ------------------------------------
    }
    
    # Calculate Rates
    rate_A <- c_A / v_A
    rate_B <- c_B / v_B
    uplift <- (rate_B - rate_A) / rate_A
    
    # --- SIMPSON'S PARADOX DETECTOR (Time-Stratification) ---
    if (!is.null(input$date_col) && input$date_col != "None" && input$date_col %in% names(df)) {
      # 1. Quick background parse of the dates
      raw_d <- trimws(as.character(df[[input$date_col]]))
      clean_d <- gsub("\\.[0-9]+", "", raw_d)
      parsed_d <- suppressWarnings(lubridate::parse_date_time(
        clean_d, orders = c("ymd_HMS", "ymd_HM", "ymd", "dmy", "mdy"), quiet = TRUE
      ))
      df$MathDate <- as.Date(parsed_d)
      
      if (any(!is.na(df$MathDate))) {
        # 2. Aggregate into daily cohorts
        d_conv <- aggregate(as.formula(paste(input$conv_col, "~ MathDate +", input$var_col)), data = df, FUN = sum, na.rm = TRUE)
        d_vis <- aggregate(as.formula(paste(input$conv_col, "~ MathDate +", input$var_col)), data = df, FUN = length)
        d_math <- merge(d_conv, d_vis, by = c("MathDate", input$var_col))
        names(d_math) <- c("Date", "Variation", "Conversions", "Visitors")
        d_math$Rate <- d_math$Conversions / d_math$Visitors
        
        # 3. Pivot and compare A vs B per day
        d_A <- d_math[d_math$Variation == var1, c("Date", "Rate")]
        d_B <- d_math[d_math$Variation == var2, c("Date", "Rate")]
        d_comp <- merge(d_A, d_B, by = "Date", suffixes = c("_A", "_B"))
        
        if (nrow(d_comp) > 2) {
          days_A_won <- sum(d_comp$Rate_A > d_comp$Rate_B, na.rm = TRUE)
          days_B_won <- sum(d_comp$Rate_B > d_comp$Rate_A, na.rm = TRUE)
          total_days <- days_A_won + days_B_won
          
          # 4. The Paradox Logic: Overall winner loses the majority of individual days
          if (rate_B > rate_A && days_A_won > days_B_won) {
            simpsons_alert <- TRUE
            simpsons_text <- paste0("Variation B won overall, but Variation A won on ", days_A_won, " out of ", total_days, " days.")
          } else if (rate_A > rate_B && days_B_won > days_A_won) {
            simpsons_alert <- TRUE
            simpsons_text <- paste0("Variation A won overall, but Variation B won on ", days_B_won, " out of ", total_days, " days.")
          }
        }
      }
    }
    # --------------------------------------------------------
    
    # --- THE BAYESIAN ENGINE (EMPIRICAL PRIOR) ---
    set.seed(42)
    
    # 1. Calculate the Empirical Prior (Centered on reality)
    # We use the pooled conversion rate of the entire experiment
    pool_rate <- (c_A + c_B) / (v_A + v_B)
    
    # Assign a weak "weight" to this prior (e.g., 50 pseudo-visitors)
    # This stabilizes the math in the early days of a test without overpowering actual data
    prior_weight <- 50 
    prior_alpha <- pool_rate * prior_weight
    prior_beta <- (1 - pool_rate) * prior_weight
    
    # 2. Define Posterior Beta Distributions (Prior + Actual Data)
    alpha_A <- prior_alpha + c_A
    beta_A <- prior_beta + v_A - c_A
    alpha_B <- prior_alpha + c_B
    beta_B <- prior_beta + v_B - c_B
    
    # 3. Monte Carlo Simulation for Probability of Winning
    sim_A <- rbeta(100000, alpha_A, beta_A)
    sim_B <- rbeta(100000, alpha_B, beta_B)
    prob_B_wins <- mean(sim_B > sim_A)
    
    # 4. Calculate 95% Bayesian Credible Intervals mathematically
    ci_lower_A <- qbeta(0.025, alpha_A, beta_A)
    ci_upper_A <- qbeta(0.975, alpha_A, beta_A)
    ci_lower_B <- qbeta(0.025, alpha_B, beta_B)
    ci_upper_B <- qbeta(0.975, alpha_B, beta_B)
    # ---------------------------------------------
    
    list(
      rate_A = rate_A, rate_B = rate_B, uplift = uplift,
      prob_B_wins = prob_B_wins,
      ci_lower_A = ci_lower_A, ci_upper_A = ci_upper_A,
      ci_lower_B = ci_lower_B, ci_upper_B = ci_upper_B,
      v_A = v_A, c_A = c_A, v_B = v_B, c_B = c_B,
      guardrail_alert = guardrail_alert,
      guard_A_rate = guard_A_rate, guard_B_rate = guard_B_rate,
      simpsons_alert = simpsons_alert,      
      simpsons_text = simpsons_text
    )
  })
  
  # Wait 400ms after the user stops typing before calculating
  ab_math_debounced <- debounce(ab_math_base, 400)
  ab_math <- reactive({
    res <- ab_math_debounced()
    req(!is.null(res))   # Silently holds all render functions until debounce has fired
    res
  })
  
  # Executive Translation UI
  output$executive_verdict_ui <- renderUI({
    res <- ab_math()
    req(res$prob_B_wins, input$future_traffic, input$aov)
    
    rate_A_pct <- round(res$rate_A * 100, 2)
    rate_B_pct <- round(res$rate_B * 100, 2)
    uplift_pct <- round(res$uplift * 100, 2)
    bayesian_pct <- round(res$prob_B_wins * 100, 1) # Format the new metric
    
    proj_conv_A <- input$future_traffic * res$rate_A
    proj_conv_B <- input$future_traffic * res$rate_B
    
    # PURE MATH: The AOV box is already localized by the Auto-Converter
    proj_rev_A <- proj_conv_A * input$aov
    proj_rev_B <- proj_conv_B * input$aov
    rev_diff <- proj_rev_B - proj_rev_A
    
    formatted_diff <- paste0(input$currency, format(abs(round(rev_diff, 0)), big.mark = ","))
    
    # Guardrail Alert Banner HTML
    guardrail_html <- ""
    if (res$guardrail_alert) {
      guardrail_html <- paste0(
        "<div style='background-color: #fff4f4; border-left: 5px solid #e74c3c; padding: 15px; margin-bottom: 20px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);'>",
        "<h4 style='color: #c0392b; margin-top: 0; font-weight: bold;'><i class='fas fa-exclamation-triangle'></i> CRITICAL GUARDRAIL FAILURE</h4>",
        "<p style='color: #c0392b; font-size: 1.05em; margin-bottom: 0;'>Variation B won the primary test, but caused a statistically significant spike in negative events (",
        round(res$guard_A_rate * 100, 2), "% vs ", round(res$guard_B_rate * 100, 2), "%). <b>Deployment is highly risky.</b></p>",
        "</div>"
      )
    }
    
    # Simpson's Paradox HTML Banner
    simpsons_html <- ""
    if (res$simpsons_alert) {
      simpsons_html <- paste0(
        "<div style='background-color: #fff8e1; border-left: 5px solid #f6c23e; padding: 15px; margin-bottom: 20px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);'>",
        "<h4 style='color: #b8860b; margin-top: 0; font-weight: bold;'><i class='fas fa-project-diagram'></i> SIMPSON'S PARADOX DETECTED</h4>",
        "<p style='color: #856404; font-size: 1.05em; margin-bottom: 0;'>", res$simpsons_text,
        " This indicates a severe traffic allocation skew (e.g., one variation received disproportionate traffic on high-converting days). <b>The overall mathematical verdict is highly misleading.</b></p>",
        "</div>"
      )
    }
    
    # The Bayesian HTML Badge
    bayesian_badge <- paste0(
      "<div style='background-color: #2c3e50; color: white; padding: 15px; border-radius: 5px; margin-top: 20px; text-align: center; box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);'>",
      "<p style='margin: 0; font-size: 0.9em; color: #aeb8c2; text-transform: uppercase; letter-spacing: 1px;'>Bayesian Probability</p>",
      "<h3 style='margin: 5px 0 0 0; font-weight: 900; color: #1cc88a;'>", bayesian_pct, "% Chance Variation B is Better</h3>",
      "</div>"
    )
    
    # Evaluate Bayesian Probability Thresholds
    if (res$prob_B_wins >= 0.95) {
      # B Wins
      HTML(paste0(
        simpsons_html,
        guardrail_html,
        "<h2 style='color: #1cc88a; font-weight: 800; text-align: center; margin-bottom: 5px;'>Variation B Wins!</h2>",
        "<p style='font-size: 1.1em; color: #6c757d; text-align: center; margin-bottom: 20px;'>We are over 95% confident that Variation B outperforms Variation A.</p>",
        
        # Flexbox KPI Cards
        "<div style='display: flex; gap: 15px; margin-bottom: 20px;'>",
        "<div style='flex: 1; background: #f8f9fc; padding: 15px; border-radius: 8px; border-left: 4px solid #4e73df;'>",
        "<h6 style='margin: 0; color: #5a5c69; font-size: 0.85em; text-transform: uppercase; font-weight: 700;'>Relative Uplift</h6>",
        "<h3 style='margin: 5px 0 0 0; color: #2c3e50; font-weight: 800;'>+", uplift_pct, "%</h3>",
        "</div>",
        "<div style='flex: 1; background: #f8f9fc; padding: 15px; border-radius: 8px; border-left: 4px solid #1cc88a;'>",
        "<h6 style='margin: 0; color: #5a5c69; font-size: 0.85em; text-transform: uppercase; font-weight: 700;'>Revenue Impact</h6>",
        "<h3 style='margin: 5px 0 0 0; color: #1cc88a; font-weight: 800;'>+", formatted_diff, "</h3>",
        "</div>",
        "</div>",
        
        "<p style='font-size: 1.05em; color: #2c3e50;'><b>Actionable Advice:</b> Shift your remaining traffic and budget entirely to Variation B.</p>",
        bayesian_badge
      ))
    } else if (res$prob_B_wins <= 0.05) {
      # A Wins
      HTML(paste0(
        simpsons_html,
        "<h2 style='color: #e74c3c; font-weight: 800; text-align: center; margin-bottom: 5px;'>Stick with Variation A.</h2>",
        "<p style='font-size: 1.1em; color: #6c757d; text-align: center; margin-bottom: 20px;'>Variation B performed significantly worse.</p>",
        
        # Flexbox KPI Cards
        "<div style='display: flex; gap: 15px; margin-bottom: 20px;'>",
        "<div style='flex: 1; background: #fff4f4; padding: 15px; border-radius: 8px; border-left: 4px solid #e74c3c;'>",
        "<h6 style='margin: 0; color: #5a5c69; font-size: 0.85em; text-transform: uppercase; font-weight: 700;'>Relative Loss</h6>",
        "<h3 style='margin: 5px 0 0 0; color: #e74c3c; font-weight: 800;'>-", abs(uplift_pct), "%</h3>",
        "</div>",
        "<div style='flex: 1; background: #fff4f4; padding: 15px; border-radius: 8px; border-left: 4px solid #e74c3c;'>",
        "<h6 style='margin: 0; color: #5a5c69; font-size: 0.85em; text-transform: uppercase; font-weight: 700;'>Revenue Risk</h6>",
        "<h3 style='margin: 5px 0 0 0; color: #e74c3c; font-weight: 800;'>-", formatted_diff, "</h3>",
        "</div>",
        "</div>",
        
        "<p style='font-size: 1.05em; color: #2c3e50;'><b>Actionable Advice:</b> Discard Variation B immediately to prevent further lost revenue.</p>",
        bayesian_badge
      ))
      
    } else {
      # Inconclusive
      HTML(paste0(
        simpsons_html,
        "<h2 style='color: #f6c23e; font-weight: 800; text-align: center; margin-bottom: 5px;'>Inconclusive</h2>",
        "<p style='font-size: 1.1em; color: #6c757d; text-align: center; margin-bottom: 20px;'>No statistically significant winner.</p>",
        
        "<div style='background: #f8f9fc; padding: 15px; border-radius: 8px; border-left: 4px solid #f6c23e; margin-bottom: 20px;'>",
        "<h6 style='margin: 0; color: #5a5c69; font-size: 0.85em; text-transform: uppercase; font-weight: 700;'>Projected Difference (Not Guaranteed)</h6>",
        "<h3 style='margin: 5px 0 0 0; color: #f6c23e; font-weight: 800;'>", formatted_diff, "</h3>",
        "</div>",
        
        "<p style='font-size: 1.05em; color: #2c3e50;'><b>Actionable Advice:</b> Check the Power Analysis tab to see if more traffic is needed, or stick with whichever variation is cheaper to maintain.</p>",
        bayesian_badge
      ))
    }
  })
  
  # Interactive Bar Chart
  output$conversion_plot <- renderPlotly({
    res <- ab_math()
    req(res$ci_lower_A)
    
    df <- data.frame(
      Variation = c("A (Control)", "B (Challenger)"),
      Rate = c(res$rate_A, res$rate_B),
      Lower = c(res$ci_lower_A, res$ci_lower_B),
      Upper = c(res$ci_upper_A, res$ci_upper_B)
    )
    
    p_bar <- ggplot(df, aes(x = Variation, y = Rate, fill = Variation, 
                            text = paste("Variation:", Variation, 
                                         "<br>Rate:", scales::percent(Rate, accuracy=0.01), 
                                         "<br>95% CI: [", scales::percent(Lower, accuracy=0.01), " - ", scales::percent(Upper, accuracy=0.01), "]"))) +
      geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15, linewidth = 1, color = "#2c3e50") +
      scale_fill_manual(values = c("A (Control)" = "#858796", "B (Challenger)" = "#4e73df")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title = "Conversion Rates (95% Credible Intervals)",
           subtitle = "Derived mathematically from the posterior Beta distribution.") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", color = "#2c3e50"),
            plot.subtitle = element_text(color = "#e74c3c", face = "italic", margin = margin(b = 15)),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    # Wrap in ggplotly and turn off the floating menu bar for a cleaner look
    ggplotly(p_bar, tooltip = "text") %>%
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE) 
  })
  
  # Bayesian Density "Mountain" Plot
  output$bayesian_plot <- renderPlotly({
    res <- ab_math()
    req(res$prob_B_wins)
    
    # 1. Align the plot exactly with the Empirical Prior math used in the engine
    pool_rate <- (res$c_A + res$c_B) / (res$v_A + res$v_B)
    prior_weight <- 50 
    prior_alpha <- pool_rate * prior_weight
    prior_beta <- (1 - pool_rate) * prior_weight
    
    alpha_A <- prior_alpha + res$c_A
    beta_A <- prior_beta + res$v_A - res$c_A
    alpha_B <- prior_alpha + res$c_B
    beta_B <- prior_beta + res$v_B - res$c_B
    
    # 2. Calculate mathematically sound Standard Deviations to set the zoom
    sd_A <- sqrt((alpha_A * beta_A) / (((alpha_A + beta_A)^2) * (alpha_A + beta_A + 1)))
    sd_B <- sqrt((alpha_B * beta_B) / (((alpha_B + beta_B)^2) * (alpha_B + beta_B + 1)))
    mean_A <- alpha_A / (alpha_A + beta_A)
    mean_B <- alpha_B / (alpha_B + beta_B)
    
    # Zoom in to exactly 4 standard deviations wide, safely clamped between 0 and 1
    min_rate <- max(0, min(mean_A - (4 * sd_A), mean_B - (4 * sd_B)))
    max_rate <- min(1, max(mean_A + (4 * sd_A), mean_B + (4 * sd_B)))
    
    # High-resolution axis prevents Plotly from dropping sharp peaks
    x_axis <- seq(min_rate, max_rate, length.out = 1000)
    
    # Mathematically draw the Posterior Beta distributions
    y_A <- dbeta(x_axis, alpha_A, beta_A)
    y_B <- dbeta(x_axis, alpha_B, beta_B)
    
    # Package it for ggplot
    df_bayes <- data.frame(
      Rate = c(x_axis, x_axis),
      Likelihood = c(y_A, y_B),
      Variation = factor(rep(c("A (Control)", "B (Challenger)"), each = 1000))
    )
    
    # 3. Bulletproof Plotly rendering (geom_ribbon bypasses the geom_area bug)
    p_bayes <- ggplot(df_bayes, aes(x = Rate, color = Variation, fill = Variation, text = paste("Rate:", scales::percent(Rate, accuracy=0.1)))) +
      geom_ribbon(aes(ymin = 0, ymax = Likelihood), alpha = 0.45, color = NA) +
      geom_line(aes(y = Likelihood), linewidth = 1) +
      scale_fill_manual(values = c("A (Control)" = "#858796", "B (Challenger)" = "#4e73df")) +
      scale_color_manual(values = c("A (Control)" = "#858796", "B (Challenger)" = "#4e73df")) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_minimal() +
      labs(title = NULL, x = "True Conversion Rate", y = "Likelihood") +
      theme(axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 11, face = "bold"),
            legend.title = element_blank()) 
    
    # Make it interactive
    ggplotly(p_bayes, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3), 
             hovermode = "x unified",
             margin = list(b = 60)) 
  })
  
  # Raw Tech Stats
  output$tech_stats <- renderPrint({
    res <- ab_math()
    req(res$prob_B_wins)
    
    cat("Method: Pure Bayesian A/B Testing\n")
    cat("Prior:  Empirical Bayes (Weighted to Baseline)\n")
    cat("-----------------------------------------------\n")
    cat("Prob. B is Better: ", round(res$prob_B_wins * 100, 2), "%\n")
    cat("Verdict:           ", if(res$prob_B_wins >= 0.95 || res$prob_B_wins <= 0.05) "Statistically Valid" else "Inconclusive", "\n\n")
    cat("Conv. Rate A:      ", round(res$rate_A * 100, 2), "%\n")
    cat("Conv. Rate B:      ", round(res$rate_B * 100, 2), "%\n")
  })
  
  output$trend_chart_ui <- renderUI({
    req(input$data_source == "csv", input$date_col)
    if(input$date_col == "None") return(NULL)
    
    div(style = "background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 0.5rem; padding: 20px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);",
        withSpinner(plotlyOutput("trend_plot", height = "350px"), type = 8, color = "#4e73df")
    )
  })
  
  # Interactive Time-Series Trend Math & Plot
  output$trend_plot <- renderPlotly({
    req(input$data_source == "csv", input$date_col, input$date_col != "None")
    df <- raw_data()
    v_col <- input$var_col
    c_col <- input$conv_col
    d_col <- input$date_col
    
    # --- THE SPINNER BYPASS ----
    error_chart <- function(message) {
      # Dynamically wrap long text so it doesn't spill off the canvas
      wrapped_text <- paste(strwrap(message, width = 75), collapse = "<br>")
      
      plot_ly(x = c(1), y = c(1), type = "scatter", mode = "markers", opacity = 0) %>%
        layout(
          xaxis = list(visible = FALSE, range = c(0, 2)),
          yaxis = list(visible = FALSE, range = c(0, 2)),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          annotations = list(
            list(
              x = 0.5, y = 0.5,
              text = paste0("<b>", wrapped_text, "</b>"),
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              font = list(color = "#e74c3c", size = 15, family = "Inter")
            )
          )
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    if (v_col != "" && c_col != "" && v_col == c_col) {
      return(error_chart("🚨 Consultant Alert: You selected the exact same column for both Variation and Conversions. Please select two distinct columns."))
    }
    
    # SHIELD 1: Race Condition (Groups)
    v_unique <- unique(df[[v_col]])
    v_unique <- v_unique[!is.na(v_unique)]
    if (length(v_unique) > 2) {
      return(error_chart("Waiting for valid variation groups to render timeline..."))
    }
    
    # --- OMNIVOROUS DATE PARSER ---
    raw_dates <- trimws(as.character(df[[d_col]]))
    
    # Strip fractional seconds (long decimals) which cause parsers to choke
    clean_dates <- gsub("\\.[0-9]+", "", raw_dates)
    
    # This engine tests against standard AND AM/PM configurations
    parsed_time <- lubridate::parse_date_time(
      clean_dates,
      orders = c(
        "ymd_HMS", "ymd_HM", "ymd",         # Standard ISO 
        "dmy_IMSp", "mdy_IMSp", "ymd_IMSp", # 12-hour clock (AM/PM)
        "dmy_HMS", "dmy_HM", "dmy",         
        "mdy_HMS", "mdy_HM", "mdy",   
        "Ymd_HMS", "Ymd_HM", "Ymd",   
        "bdY", "dby", "bY"            
      ),
      quiet = TRUE 
    )
    
    df$PlotDate <- as.Date(parsed_time)
    df <- df[!is.na(df$PlotDate) & !is.na(df[[v_col]]) & !is.na(df[[c_col]]), ]
    
    # SHIELD 2: Un-parsable Data
    if (nrow(df) == 0) {
      return(error_chart("🚨 Consultant Alert: No valid dates could be parsed from this column."))
    }
    
    # SHIELD 3: ID Column Trap
    unique_days <- length(unique(df$PlotDate))
    if (unique_days > 365) {
      return(error_chart("🚨 Consultant Alert: This looks like an ID column. Please select a valid Date."))
    }
    
    # SHIELD 4: ZERO CONVERSIONS TRAP FOR PLOT 
    # Replicates the math engine's check so the plot doesn't crash on empty data
    c_data_temp <- tolower(trimws(as.character(df[[c_col]])))
    valid_convs <- sum(c_data_temp %in% c("1", "yes", "true", "y", "t", "success"))
    
    if (valid_convs == 0) {
      return(error_chart("🚨 Consultant Alert: Zero valid conversions found. Ensure your conversion column contains recognizable success values like '1', 'yes', 'true', or 'success'."))
    }
    # ----------------------------------------------------
    
    # --- SEGMENTATION FILTER FOR PLOT ---
    if (!is.null(input$seg_col) && input$seg_col != "None" && input$seg_col %in% names(df) && !is.null(input$seg_val) && input$seg_val != "All") {
      df <- df[df[[input$seg_col]] == input$seg_val, ]
      if (nrow(df) == 0) {
        return(error_chart("Consultant Alert: No trend data available for this segment."))
      }
    }
    
    # Aggregate Daily Conversions
    conv_sum <- aggregate(
      as.formula(paste(c_col, "~ PlotDate +", v_col)), 
      data = df, 
      FUN = function(x) {
        clean_x <- trimws(tolower(as.character(x)))
        sum(clean_x %in% c("1", "yes", "true", "y", "t", "success"))
      }
    )
    # FORCE NAMES FOR MERGE AND GGPLOT
    names(conv_sum) <- c("Date", "Variation", "Conversions")
    
    # Aggregate Daily Visitors
    vis_sum <- aggregate(
      as.formula(paste(c_col, "~ PlotDate +", v_col)), 
      data = df, 
      FUN = length
    )
    # FORCE NAMES FOR MERGE AND GGPLOT
    names(vis_sum) <- c("Date", "Variation", "Visitors")
    
    # Merge and calculate Daily Rate
    daily_data <- merge(conv_sum, vis_sum, by = c("Date", "Variation"))
    daily_data$Rate <- daily_data$Conversions / daily_data$Visitors
    
    # Force Variation to be a discrete category (prevents continuous scale crashes)
    daily_data$Variation <- as.character(daily_data$Variation)
    
    # Dynamic Color Mapping (Locks baseline to gray, challenger to blue)
    req(input$baseline_var)
    base_v <- input$baseline_var
    chal_v <- setdiff(unique(daily_data$Variation), base_v)[1]
    color_map <- setNames(c("#858796", "#4e73df"), c(base_v, chal_v))
    
    # Create the base ggplot
    p <- ggplot(daily_data, aes(x = Date, y = Rate, color = Variation, group = Variation, text = paste("Date:", Date, "<br>Rate:", scales::percent(Rate, accuracy=0.1), "<br>Visitors:", Visitors))) +
      geom_line(linewidth = 0.5, alpha = 0.4) +
      geom_smooth(method = "loess", se = FALSE, size = 1) +
      scale_color_manual(values = color_map) + # NEW: Applies dynamic colors
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_minimal() +
      labs(title = "Conversion Rate Over Time (Novelty Effect Check)", x = NULL, y = NULL) +
      theme(plot.title = element_text(face = "bold", color = "#2c3e50"))
    
    # Wrap 'p' in ggplotly
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = -0.15),
             hovermode = "x unified")
  })
  
  # Power Analysis & Timeline Engine
  output$power_verdict_ui <- renderUI({
    res <- ab_math()
    req(res$rate_A, input$target_power, input$mde, input$daily_traffic)
    
    # Base parameters
    p1 <- res$rate_A
    if(p1 <= 0 || p1 >= 1) return(HTML("<p>Need valid baseline conversion rate.</p>"))
    
    # Calculate the exact target rate we want to detect
    p2 <- p1 * (1 + (input$mde / 100))
    if(p2 >= 1) p2 <- 0.99
    
    power_dec <- input$target_power / 100
    
    # Run the Power Test
    pwr_test <- tryCatch({
      power.prop.test(p1 = p1, p2 = p2, sig.level = 0.05, power = power_dec, alternative = "two.sided")
    }, error = function(e) NULL)
    
    if(is.null(pwr_test)) return(HTML("<p style='color:red;'>Error calculating power. Check inputs.</p>"))
    
    # Calculate required vs actual sample size
    n_per_group <- ceiling(pwr_test$n)
    total_required <- n_per_group * 2
    current_total <- res$v_A + res$v_B
    remaining <- total_required - current_total
    
    # Render the Narrative
    if(remaining <= 0) {
      HTML(paste0(
        "<h2 style='color: #1cc88a; font-weight: 800; text-align: center;'>Test is Fully Powered!</h2>",
        "<p style='font-size: 1.1em; color: #2c3e50; text-align: center;'>You have enough data to make a confident decision.</p>",
        "<ul style='font-size: 1.05em;'>",
        "<li><b>Data Check:</b> You have ", format(current_total, big.mark=","), " total visitors. You only needed ", format(total_required, big.mark=","), " to detect a ", input$mde, "% change.</li>",
        "<li><b>Next Step:</b> Check the Executive Dashboard. If the result is inconclusive, it means the variation truly failed to move the needle. Shut the test down.</li>",
        "</ul>"
      ))
    } else {
      days_left <- ceiling(remaining / input$daily_traffic)
      HTML(paste0(
        "<h2 style='color: #e74c3c; font-weight: 800; text-align: center;'>More Data Needed</h2>",
        "<p style='font-size: 1.1em; color: #2c3e50; text-align: center;'>Do not stop the test yet. The results are premature.</p>",
        "<ul style='font-size: 1.05em; margin-bottom: 20px;'>",
        "<li><b>Target:</b> To reliably detect a ", input$mde, "% change, you need <b>", format(total_required, big.mark=","), "</b> total visitors.</li>",
        "<li><b>Current:</b> You only have ", format(current_total, big.mark=","), " visitors right now.</li>",
        "<li><b style='color: #e74c3c;'>Deficit: You need ", format(remaining, big.mark=","), " more visitors.</b></li>",
        "</ul>",
        "<div style='background-color: #f8f9fc; border-left: 5px solid #4e73df; padding: 20px; border-radius: 4px;'>",
        "<h4 style='color: #4e73df; margin-top: 0;'><i class='fas fa-clock'></i> Timeline Estimate</h4>",
        "<p style='font-size: 1.1em; margin-bottom: 0;'>At your current pace of ", format(input$daily_traffic, big.mark=","), " visitors per day, you must leave this test running for approximately <b>", days_left, " more days</b> to reach statistical validity.</p>",
        "</div>"
      ))
    }
  })
  # --- PDF Report Generation ---
  output$download_report <- downloadHandler(
    filename = function() {
      "Executive_Report.pdf"
    },
    content = function(file) {
      # 1. Grab Core Math
      res <- ab_math()
      req(res$prob_B_wins)
      final_verdict <- if(res$prob_B_wins >= 0.95) "Variation B Wins" else if(res$prob_B_wins <= 0.05) "Variation A Wins" else "Inconclusive"
      
      # --- Background Power Analysis Calculation ---
      p1 <- res$rate_A
      p2 <- p1 * (1 + (input$mde / 100))
      if(p2 >= 1) p2 <- 0.99
      power_dec <- input$target_power / 100
      
      pwr_test <- tryCatch({
        power.prop.test(p1 = p1, p2 = p2, sig.level = 0.05, power = power_dec, alternative = "two.sided")
      }, error = function(e) NULL)
      
      req_visitors <- if(!is.null(pwr_test)) ceiling(pwr_test$n) * 2 else NA
      current_total <- res$v_A + res$v_B
      
      # Calculate days left if more traffic is needed
      days_left <- 0
      if(!is.na(req_visitors) && req_visitors > current_total) {
        days_left <- ceiling((req_visitors - current_total) / input$daily_traffic)
      }
      # ---------------------------------------------------
      
      # Build the real cumulative dataset safely
      daily_df <- NA
      if (input$data_source == "csv" && !is.null(input$date_col) && input$date_col != "None") {
        # ... (keep your existing daily_df date parsing code here) ...
        df <- raw_data()
        v_col <- input$var_col
        c_col <- input$conv_col
        d_col <- input$date_col
        
        clean_dates <- gsub("\\.[0-9]+", "", trimws(as.character(df[[d_col]])))
        parsed_time <- suppressWarnings(lubridate::parse_date_time(
          clean_dates,
          orders = c("ymd_HMS", "ymd_HM", "ymd", "dmy_IMSp", "mdy_IMSp", "ymd_IMSp", "dmy_HMS", "dmy_HM", "dmy", "mdy_HMS", "mdy_HM", "mdy", "Ymd_HMS", "Ymd_HM", "Ymd", "bdY", "dby", "bY"), quiet = TRUE
        ))
        df$Date <- as.Date(parsed_time)
        df <- df[!is.na(df$Date) & !is.na(df[[v_col]]) & !is.na(df[[c_col]]), ]
        
        if (nrow(df) > 0) {
          c_data_temp <- tolower(trimws(as.character(df[[c_col]])))
          df$Converted <- ifelse(c_data_temp %in% c("1", "yes", "true", "y", "t", "success"), 1, 0)
          
          daily_agg <- aggregate(list(Conversions = df$Converted, Visitors = rep(1, nrow(df))),
                                 by = list(Date = df$Date, Variation = df[[v_col]]), FUN = sum)
          
          daily_agg <- daily_agg[order(daily_agg$Variation, daily_agg$Date), ]
          daily_agg$Cum_Conv <- ave(daily_agg$Conversions, daily_agg$Variation, FUN = cumsum)
          daily_agg$Cum_Vis <- ave(daily_agg$Visitors, daily_agg$Variation, FUN = cumsum)
          daily_agg$Cumulative_Rate <- daily_agg$Cum_Conv / daily_agg$Cum_Vis
          
          daily_df <- daily_agg
        }
      }
      
      params <- list(
        prob_b = res$prob_B_wins, 
        verdict = final_verdict,
        rate_a = res$rate_A, 
        rate_b = res$rate_B,
        v_a = res$v_A, 
        v_b = res$v_B,
        c_a = res$c_A, 
        c_b = res$c_B, 
        uplift = res$uplift,
        guardrail_alert = res$guardrail_alert,
        simpsons_alert = res$simpsons_alert,
        simpsons_text = res$simpsons_text,
        daily_data = daily_df,
        req_visitors = req_visitors,   
        current_total = current_total, 
        mde = input$mde,               
        days_left = days_left,
        target_power = input$target_power,
        daily_traffic = input$daily_traffic
      )
      
      # 2. Create a fully isolated temp directory
      temp_dir <- tempfile()
      dir.create(temp_dir)
      
      temp_rmd <- file.path(temp_dir, "report.Rmd")
      file.copy("report.Rmd", temp_rmd, overwrite = TRUE)
      
      target_pdf <- file.path(temp_dir, "output.pdf")
      
      # 3. Render
      rmarkdown::render(
        input = temp_rmd,
        output_file = target_pdf,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      # 4. Hand the exact file to Shiny
      file.copy(target_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
