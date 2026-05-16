library(shiny)
library(shinybusy)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)

# Null-coalescing helper
`%||%` <- function(x, y) {
  # Safe null-coalescing for Shiny inputs that may be vectors
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}



# ==========================================
# 1. DATA PREPARATION & CATEGORIZATION
# ==========================================

categorize_disease <- function(df) {
  
  # Ensure columns exist so we can safely reference
  if (!("CONDITION" %in% names(df))) df$CONDITION <- NA_character_
  if (!("DISEASE_CATEGORY" %in% names(df))) df$DISEASE_CATEGORY <- NA_character_
  if (!("CONDITION_CD" %in% names(df))) df$CONDITION_CD <- NA_character_
  
  df <- df %>%
    mutate(
      # one unified condition label across labs + investigations
      CONDITION_STD = dplyr::coalesce(
        na_if(trimws(CONDITION), ""),
        na_if(trimws(DISEASE_CATEGORY), ""),
        na_if(trimws(CONDITION_CD), "")
      )
    )
  
  # Bucket OFF the unified field
  df %>%
    mutate(PROGRAM_BUCKET = case_when(
      # Respiratory
      CONDITION_STD %in% c(
        "Rhinovirus spp", "Coronavirus", "Influenza", "Metapneumovirus spp",
        "Respiratory virus DNA and RNA 12 panel", "Pertussis", "RSV",
        "Respiratory syncytial virus infection (disorder)", "Chlamydia pneumoniae",
        "Legionellosis", "Measles", "Tuberculosis", "Adenovirus", "Mumps",
        "Streptococcus pneumoniae, invasive disease (IPD)", "Diphtheria"
      ) ~ "Respiratory 🫁",
      
      # Enteric / Food-borne
      CONDITION_STD %in% c(
        "Enteropathogenic Escherichia coli (EPEC)", "Norovirus", "Campylobacteriosis",
        "Amebiasis", "Astrovirus", "Sapovirus", "Bacteria identified in Stool by Culture",
        "Adenovirus F40/41", "Enteroaggregative Escherichia coli (EAEC)", "Rotavirus",
        "Escherichia coli (STEC) gastroenteritis", "Cyclosporiasis", "Salmonellosis",
        "Yersiniosis (non-pestis)", "Plesiomonas shigelloides (Dirty Water Bacteria)",
        "Shigellosis", "Vibriosis", "Cryptosporidiosis", "Giardiasis", "Hepatitis A",
        "Listeriosis", "Brucellosis", "Escherichia coli"
      ) ~ "Enteric/Food-borne 🥗",
      
      # STI
      CONDITION_STD %in% c(
        "Human papillomavirus", "HPV", "Chlamydia Trachomatis Infection", "Chlamydia", "Chlamydia trachomatis infection",
        "HIV Infection", "Syphilis", "Gonorrhea", "Herpes Simplex Virus Infection",
        "Hepatitis B", "Hepatitis C"
      ) ~ "STI 🩹",
      
      # Vector-borne
      CONDITION_STD %in% c(
        "West Nile Virus Blood Donor", "Disease due to West Nile virus (disorder)", "Spotted Fever Rickettsiosis",
        "West Nile RNA", "West Nile virus disease, neuroinvasive", "Dengue", "West Nile virus disease, nonneuroinvasive",
        "Lyme disease", "Chikungunya virus diseases", "Zika virus (organism)",
        "Oropouche virus disease, non-congenital", "Tularemia", "Q Fever",
        "Spotted Fever Rickettsiosis", "Ehrlichiosis, chaffeensis"
      ) ~ "Vector-borne 🦟",
      
      # Environmental
      CONDITION_STD %in% c(
        "Lead Poisoning", "Toxic effect of mercury AND/OR its compounds", "Lead poisoning",
        "Toxic effect of carbon monoxide (disorder)",
        "Toxic effect of arsenic AND/OR its compounds"
      ) ~ "Environmental 🧪",
      
      # HAI
      CONDITION_STD %in% c(
        "Clostridium difficile", "Clostridium Difficile", "MRSA/VRSA",
        "S. aureus, vancomycin intermediate susc (VISA)",
        "Vancomycin-Resistant Enterococcus"
      ) ~ "Healthcare (HAI) 🏥",
      
      TRUE ~ "Other/Viral/Zoonotic 🧬"
    ))
}

# ---- helper: robust date parsing (character/factor/excel numeric) ----
parse_mixed_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  # Excel numeric dates often arrive as numeric/integer
  if (is.numeric(x)) {
    # 25569 is 1970-01-01 in Excel date system
    # This works when x is "days since 1899-12-30"
    out <- as.Date(x, origin = "1899-12-30")
    return(out)
  }
  
  x_chr <- as.character(x)
  x_chr <- na_if(trimws(x_chr), "")
  
  suppressWarnings(
    as.Date(parse_date_time(
      x_chr,
      orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS", "Ymd HMS", "Ymd"),
      tz = "UTC"
    ))
  )
}

clean_and_load <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename_with(toupper)
  
  # Parse all likely date columns (includes DOB/BIRTH)
  date_cols <- names(df)[
    stringr::str_detect(names(df), "(DATE|_DT|DTTM|DTTIME|DOB|BIRTH)$")
  ]
  
  if (length(date_cols) > 0) {
    df <- df %>% mutate(across(all_of(date_cols), parse_mixed_date))
  }
  
  # Ensure DOB exists + parsed
  if (!("PATIENT_DOB" %in% names(df))) df$PATIENT_DOB <- NA
  df <- df %>% mutate(PATIENT_DOB = parse_mixed_date(PATIENT_DOB))
  
  # HIPAA De-identification
  df <- df %>%
    select(-any_of(c(
      "PATIENT_FIRST_NAME", "PATIENT_MIDDLE_NAME", "PATIENT_LAST_NAME",
      "PATIENT_STREET_ADDRESS", "PATIENT_ADDRESS", "PATIENT_PHONE_HOME"
    )))
  
  # ----------------------------
  # Age Calculation (bulletproof across schemas)
  # ----------------------------
  
  # Ensure all possible reported-age columns exist (so we can reference safely)
  if (!("PATIENT_AGE_REPORTED" %in% names(df))) df$PATIENT_AGE_REPORTED <- NA
  if (!("PATIENT_REPORTED_AGE" %in% names(df))) df$PATIENT_REPORTED_AGE <- NA
  if (!("PATIENT_REPORTED_AGE_UNITS" %in% names(df))) df$PATIENT_REPORTED_AGE_UNITS <- NA
  
  df <- df %>%
    mutate(
      # DOB-based age in years
      CALC_AGE = if_else(
        !is.na(PATIENT_DOB),
        floor(time_length(interval(PATIENT_DOB, today()), "years")),
        NA_real_
      ),
      
      # Pick whichever reported age is populated
      REPORTED_AGE_RAW = dplyr::coalesce(
        suppressWarnings(as.numeric(PATIENT_AGE_REPORTED)),
        suppressWarnings(as.numeric(PATIENT_REPORTED_AGE))
      ),
      
      # Convert to years if units exist (otherwise treat as years)
      REPORTED_AGE_YEARS = {
        u <- stringr::str_to_lower(as.character(PATIENT_REPORTED_AGE_UNITS))
        dplyr::case_when(
          is.na(REPORTED_AGE_RAW) ~ NA_real_,
          u %in% c("day", "days") ~ REPORTED_AGE_RAW / 365.25,
          u %in% c("month", "months", "mos", "mo") ~ REPORTED_AGE_RAW / 12,
          TRUE ~ REPORTED_AGE_RAW
        )
      },
      
      # Final age preference: reported (if present) else DOB-derived
      FINAL_AGE = coalesce(REPORTED_AGE_YEARS, CALC_AGE),
      
      # Range check
      FINAL_AGE = if_else(FINAL_AGE < 0 | FINAL_AGE > 120, NA_real_, FINAL_AGE)
    ) %>%
    select(-any_of(c("REPORTED_AGE_RAW", "REPORTED_AGE_YEARS")))
  
  # ---- Add AGE_GROUP + standardize race/ethnicity fields (works for both files) ----
  df <- df %>%
    mutate(
      AGE_GROUP = case_when(
        is.na(FINAL_AGE) ~ NA_character_,
        FINAL_AGE < 5 ~ "0–4",
        FINAL_AGE < 18 ~ "5–17",
        FINAL_AGE < 25 ~ "18–24",
        FINAL_AGE < 45 ~ "25–44",
        FINAL_AGE < 65 ~ "45–64",
        TRUE ~ "65+"
      ),
      AGE_GROUP = factor(AGE_GROUP, levels = c("0–4","5–17","18–24","25–44","45–64","65+"))
    )
  
  # Ensure race/ethnicity columns exist for both schemas
  if (!("PATIENT_RACE_CALCULATED" %in% names(df))) df$PATIENT_RACE_CALCULATED <- NA_character_
  if (!("RACE" %in% names(df))) df$RACE <- NA_character_
  if (!("ETHNICITY" %in% names(df))) df$ETHNICITY <- NA_character_
  if (!("PATIENT_ETHNICITY_CALCULATED" %in% names(df))) df$PATIENT_ETHNICITY_CALCULATED <- NA_character_
  
  df <- df %>%
    mutate(
      RACE_STD = coalesce(
        na_if(trimws(PATIENT_RACE_CALCULATED), ""),
        na_if(trimws(RACE), "")
      ),
      ETHNICITY_STD = coalesce(
        na_if(trimws(PATIENT_ETHNICITY_CALCULATED), ""),
        na_if(trimws(ETHNICITY), "")
      )
    )
  
  # Ensure DISEASE_CATEGORY exists
  if (!("DISEASE_CATEGORY" %in% names(df))) df$DISEASE_CATEGORY <- NA_character_
  
  df %>% categorize_disease()
}

# ==========================================
# 2. SHINY UI
# ==========================================

ui <- page_navbar(
  header = tagList(
    shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right", margins = c(20,20), height = "40px", width = "40px")
  ),
  theme = bs_theme(version = 5, bootswatch = "minty", primary = "#FF69B4", secondary = "#00FFFF"),
  title = "🍭 Rural Health surveillance visual book",

  # -------------------------
  # SUMMARY TAB (Labs / Investigations / Both) - mirrors existing sidebars
  # -------------------------
  nav_panel(
    "📌 Summary",
    layout_sidebar(
      sidebar = sidebar(
        title = "Summary Controls",

        radioButtons(
          "sum_source", "Summarize:",
          choices = c("Labs" = "labs", "Investigations" = "inv", "Both (cases + labs)" = "both"),
          selected = "both",
          inline = TRUE
        ),
        checkboxInput("sum_show5y", "Show 5-year comparison (median + % change)", value = FALSE),
        hr(),

        # --- LAB-STYLE CONTROLS ---
        conditionalPanel(
          condition = "input.sum_source == 'labs' || input.sum_source == 'both'",
          h5("Labs settings"),

          selectInput(
            "sum_lab_x", "Choose X-Axis (Date):",
            choices = c("LAB_REPORT_DATE", "LAB_RPT_CREATED_DT", "SPECIMEN_COLLECTION_DT", "EVENT_DATE"),
            selected = "LAB_REPORT_DATE"
          ),
          selectInput(
            "sum_lab_group", "Group Data By:",
            choices = c("PROGRAM_BUCKET","CONDITION_STD","DISEASE_CATEGORY",
                        "PATIENT_COUNTY","PATIENT_CURRENT_SEX","AGE_GROUP","RACE_STD","ETHNICITY_STD"),
            selected = "PROGRAM_BUCKET"
          ),
          dateRangeInput("sum_lab_date_rng", "Date range:", start = NULL, end = NULL),

          pickerInput("sum_lab_bucket", "Condition group (bucket):",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_lab_condition", "Condition(s):",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),

          pickerInput("sum_lab_county", "County:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_lab_sex", "Sex:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_lab_agegrp", "Age group:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_lab_race", "Race:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_lab_eth", "Ethnicity:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          hr()
        ),

        # --- INVESTIGATION-STYLE CONTROLS ---
        conditionalPanel(
          condition = "input.sum_source == 'inv' || input.sum_source == 'both'",
          h5("Investigations settings"),

          selectInput(
            "sum_inv_x", "Choose X-Axis (Date):",
            choices = c("INV_REPORT_DT", "INV_START_DT", "RECORD_ADDED_TO_NBS_DTTIME", "EVENT_DATE", "REPORT_DT"),
            selected = "INV_REPORT_DT"
          ),
          selectInput(
            "sum_inv_group", "Group By:",
            choices = c("PROGRAM_BUCKET","CONDITION_STD","CONDITION","PROGRAM_AREA","JURISDICTION_NAME",
                        "INVESTIGATION_STATUS","INV_CASE_STATUS",
                        "PATIENT_COUNTY","PATIENT_CURRENT_SEX","AGE_GROUP","RACE_STD","ETHNICITY_STD"),
            selected = "PROGRAM_BUCKET"
          ),
          dateRangeInput("sum_inv_date_rng", "Date range:", start = NULL, end = NULL),

          pickerInput("sum_inv_bucket", "Program bucket:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_inv_condition", "Condition(s):",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),

          pickerInput("sum_inv_county", "County:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_inv_sex", "Sex:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_inv_agegrp", "Age group:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_inv_race", "Race:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("sum_inv_eth", "Ethnicity:",
                      choices = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        ),

        helpText("Tip: Use 'Both' to view lab-order volume alongside case volume and simple case-per-lab rates for situational awareness.")
      ),

      div(
        style = "padding: 10px;",
        uiOutput("sum_value_boxes"),
        br(),
        DT::dataTableOutput("sum_table"),
        br(),
        plotlyOutput("sum_plot", height = "520px")
      )
    )
  ),

  # -------------------------
  # DATA PREVIEW TAB (expanded)
  # -------------------------
  nav_panel(
    "🔎 Data Preview",
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Preview",
        helpText("Tables below stay synchronized with filters from their respective tabs."),
        tags$ul(
          tags$li(tags$strong("Labs preview"), " uses the Labs tab filters/date selection."),
          tags$li(tags$strong("Investigations preview"), " uses the Investigations tab filters/date selection.")
        )
      ),
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Labs: De-identified Data Preview (filtered)"),
          DTOutput("lab_table")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Investigations: De-identified Data Preview (filtered)"),
          DTOutput("inv_table")
        )
      )
    )
  ),

  # -------------------------
  # COMBINED TRENDS TAB (Labs + Investigations in one plot)
  # -------------------------
  nav_panel(
    "📈 Combined Trends",
    layout_sidebar(
      sidebar = sidebar(
        title = "Combined Trends",
        selectInput("combo_freq", "Time frequency:", choices = c("Day"="day","Week"="week","Month"="month","Year"="year"), selected = "month"),
        radioButtons("combo_plot_type", "Plot type:", choices = c("Line","Bar","Horizontal Bar","Bar + Line"), selected = "Line", inline = FALSE),
        helpText("This view uses your existing Labs tab filters and Investigations tab filters (synchronized).")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Labs and cases over time"),
        plotlyOutput("combo_plot", height = "650px")
      )
    )
  ),






  
  # -------------------------
  # LAB SURVEILLANCE TAB
  # -------------------------
  nav_panel(
    "🧪 Lab Surveillance",
    layout_sidebar(
      sidebar = sidebar(
        title = "Visual Controls",
        
        selectInput(
          "lab_x", "Choose X-Axis (Date):",
          choices = c("LAB_REPORT_DATE", "LAB_RPT_CREATED_DT", "SPECIMEN_COLLECTION_DT", "EVENT_DATE"),
          selected = "LAB_REPORT_DATE"
        ),
        selectInput(
          "lab_group", "Group Data By:",
          choices = c("PROGRAM_BUCKET","CONDITION_STD","DISEASE_CATEGORY",
                      "PATIENT_COUNTY","PATIENT_CURRENT_SEX","AGE_GROUP","RACE_STD","ETHNICITY_STD"),
          selected = "PROGRAM_BUCKET"
        ),
        radioButtons(
          "lab_plot_type", "Plot type:",
          choices = c("Line","Bar"),
          selected = "Line",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.lab_plot_type == 'Bar'",
          radioButtons("lab_bar_orient", "Bar orientation:", choices = c("Vertical","Horizontal"), selected = "Vertical", inline = TRUE)
        ),
        selectInput(
          "lab_time_unit", "Time frequency:",
          choices = c("Day"="day","Week"="week","Month"="month","Year"="year"),
          selected = "day"
        ),
        sliderInput("lab_plot_h", "Plot height (px):", min = 400, max = 1200, value = 700, step = 50),
        hr(),
        h5("Filters (multi-select)"),
        
        dateRangeInput("lab_date_rng", "Date range:", start = NULL, end = NULL),
        
        pickerInput("lab_bucket", "Condition group (bucket):",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_condition", "Condition(s):",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_county", "County:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_sex", "Sex:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_agegrp", "Age group:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_race", "Race:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("lab_eth", "Ethnicity:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        hr(),
        helpText("Plot + table update together based on filters.")
      ),
      
      # ✅ MAIN CONTENT (this is what was missing)
      card(
        full_screen = TRUE,
        card_header("Trends in Laboratory Volume"),
        plotlyOutput("lab_plot", height = "700px")
      ))
  ),
  
  # -------------------------
  # INVESTIGATIONS TAB
  # -------------------------
  nav_panel(
    "📋 Investigations",
    layout_sidebar(
      sidebar = sidebar(
        title = "Investigation Controls",
        
        selectInput(
          "inv_x", "Choose X-Axis (Date):",
          choices = c("INV_REPORT_DT", "INV_START_DT", "RECORD_ADDED_TO_NBS_DTTIME", "EVENT_DATE", "REPORT_DT"),
          selected = "INV_REPORT_DT"
        ),
        selectInput(
          "inv_group", "Group By:",
          choices = c("PROGRAM_BUCKET","CONDITION_STD","CONDITION","PROGRAM_AREA","JURISDICTION_NAME",
                      "INVESTIGATION_STATUS","INV_CASE_STATUS",
                      "PATIENT_COUNTY","PATIENT_CURRENT_SEX","AGE_GROUP","RACE_STD","ETHNICITY_STD"),
          selected = "PROGRAM_BUCKET"
        ),
        
        radioButtons(
          "inv_plot_type", "Plot type:",
          choices = c("Line","Bar"),
          selected = "Line",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.inv_plot_type == 'Bar'",
          radioButtons("inv_bar_orient", "Bar orientation:", choices = c("Vertical","Horizontal"), selected = "Vertical", inline = TRUE)
        ),
        selectInput(
          "inv_time_unit", "Time frequency:",
          choices = c("Day"="day","Week"="week","Month"="month","Year"="year"),
          selected = "day"
        ),
        sliderInput("inv_plot_h", "Plot height (px):", min = 400, max = 1200, value = 700, step = 50),
        
        hr(),
        h5("Filters (multi-select)"),
        
        dateRangeInput("inv_date_rng", "Date range:", start = NULL, end = NULL),
        
        pickerInput("inv_bucket", "Condition group (bucket):",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_condition", "Condition(s):",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_case_status", "Case status:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_inv_status", "Investigation status:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_county", "County:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_agegrp", "Age group:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_race", "Race:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        
        pickerInput("inv_eth", "Ethnicity:",
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        hr(),
        helpText("Plot + table update together based on filters.")
      ),
      
      # ✅ MAIN CONTENT
      card(
        full_screen = TRUE,
        card_header("Investigation Performance"),
        plotlyOutput("inv_plot", height = "700px")
      ))
  )
)
# ==========================================
# 3. SHINY SERVER
# ==========================================

server <- function(input, output, session) {

  # --- helper: time bucketing for plots ---
  periodize_date <- function(d, unit) {
    d <- as.Date(d)
    if (unit == "week") {
      return(lubridate::floor_date(d, unit = "week", week_start = 1))
    }
    lubridate::floor_date(d, unit = unit)
  }

  safe_update_picker <- function(session, input_id, choices, selected_current) {
    choices <- sort(unique(na.omit(choices)))
    keep <- intersect(selected_current %||% character(0), choices)
    updatePickerInput(session, input_id, choices = choices, selected = keep)
  }
  
  # Load data (replace filenames)
  lab_data <- reactive({
  withProgress(message = "Loading lab surveillance data…", value = 0, {
    incProgress(0.15, detail = "Reading file")
    df <- clean_and_load("data/lab_surveillance_data1.csv")
    incProgress(0.85, detail = "Finalizing")
    df
  })
})
  
  inv_data <- reactive({
  withProgress(message = "Loading investigations data…", value = 0, {
    incProgress(0.15, detail = "Reading file")
    df <- clean_and_load("data/investigations_data1.csv")
    incProgress(0.85, detail = "Finalizing")
    df
  })
})
  

  
  observeEvent(lab_data(), {
    df <- lab_data()
    
    # date range defaults based on selected x later; set a safe default now using a common date if present
    candidate_dates <- intersect(c("LAB_REPORT_DATE","LAB_RPT_CREATED_DT","SPECIMEN_COLLECTION_DT","EVENT_DATE"), names(df))
    if (length(candidate_dates) > 0) {
      dd <- df[[candidate_dates[1]]]
      dd <- dd[!is.na(dd)]
      if (length(dd) > 0) updateDateRangeInput(session, "lab_date_rng", start = min(dd), end = max(dd))
    }
    
    updatePickerInput(session, "lab_bucket",
                      choices = sort(unique(na.omit(df$PROGRAM_BUCKET))),
                      selected = character(0))
    updatePickerInput(session, "lab_condition",
                      choices = sort(unique(na.omit(df$CONDITION_STD))),
                      selected = character(0))
    
    updatePickerInput(session, "lab_agegrp",
                      choices = levels(df$AGE_GROUP) %||% sort(unique(na.omit(df$AGE_GROUP))),
                      selected = character(0))
    
    updatePickerInput(session, "lab_race",
                      choices = sort(unique(na.omit(df$RACE_STD))),
                      selected = character(0))
    
    updatePickerInput(session, "lab_eth",
                      choices = sort(unique(na.omit(df$ETHNICITY_STD))),
                      selected = character(0))
    
    if ("PATIENT_COUNTY" %in% names(df)) {
      updatePickerInput(session, "lab_county",
                        choices = sort(unique(na.omit(df$PATIENT_COUNTY))),
                        selected = character(0))
    }
    if ("PATIENT_CURRENT_SEX" %in% names(df)) {
      updatePickerInput(session, "lab_sex",
                        choices = sort(unique(na.omit(df$PATIENT_CURRENT_SEX))),
                        selected = character(0))
    }
  })
  
  observeEvent(inv_data(), {
    df <- inv_data()
    
    candidate_dates <- intersect(c("INV_REPORT_DT","INV_START_DT","RECORD_ADDED_TO_NBS_DTTIME","EVENT_DATE","REPORT_DT"), names(df))
    if (length(candidate_dates) > 0) {
      dd <- df[[candidate_dates[1]]]
      dd <- dd[!is.na(dd)]
      if (length(dd) > 0) updateDateRangeInput(session, "inv_date_rng", start = min(dd), end = max(dd))
    }
    
    updatePickerInput(session, "inv_bucket",
                      choices = sort(unique(na.omit(df$PROGRAM_BUCKET))),
                      selected = character(0))
    updatePickerInput(session, "inv_condition",
                      choices = sort(unique(na.omit(df$CONDITION_STD))),
                      selected = character(0))
    
    updatePickerInput(session, "inv_agegrp",
                      choices = levels(df$AGE_GROUP) %||% sort(unique(na.omit(df$AGE_GROUP))),
                      selected = character(0))
    
    updatePickerInput(session, "inv_race",
                      choices = sort(unique(na.omit(df$RACE_STD))),
                      selected = character(0))
    
    updatePickerInput(session, "inv_eth",
                      choices = sort(unique(na.omit(df$ETHNICITY_STD))),
                      selected = character(0))
    
    if ("INV_CASE_STATUS" %in% names(df)) {
      updatePickerInput(session, "inv_case_status",
                        choices = sort(unique(na.omit(df$INV_CASE_STATUS))),
                        selected = character(0))
    }
    if ("INVESTIGATION_STATUS" %in% names(df)) {
      updatePickerInput(session, "inv_inv_status",
                        choices = sort(unique(na.omit(df$INVESTIGATION_STATUS))),
                        selected = character(0))
    }
    if ("PATIENT_COUNTY" %in% names(df)) {
      updatePickerInput(session, "inv_county",
                        choices = sort(unique(na.omit(df$PATIENT_COUNTY))),
                        selected = character(0))
    }
  })
  
  labs_filt <- reactive({
    df <- lab_data()
    req(df)
    
    # selected date column
    validate(need(input$lab_x %in% names(df), paste("Column not found:", input$lab_x)))
    dcol <- input$lab_x
    
    df <- df %>%
      mutate(.DATE = as.Date(.data[[dcol]])) %>%
      filter(!is.na(.DATE))
    
    # date range filter
    if (!is.null(input$lab_date_rng) && all(!is.na(input$lab_date_rng))) {
      df <- df %>% filter(.DATE >= input$lab_date_rng[1], .DATE <= input$lab_date_rng[2])
    }
    
    # multi-filters (only apply if user selected values)
    if (!is.null(input$lab_bucket) && length(input$lab_bucket) > 0) {
      df <- df %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    }
    if (!is.null(input$lab_condition) && length(input$lab_condition) > 0) {
      df <- df %>% filter(CONDITION_STD %in% input$lab_condition)
    }
    if ("PATIENT_COUNTY" %in% names(df) && !is.null(input$lab_county) && length(input$lab_county) > 0) {
      df <- df %>% filter(PATIENT_COUNTY %in% input$lab_county)
    }
    if ("PATIENT_CURRENT_SEX" %in% names(df) && !is.null(input$lab_sex) && length(input$lab_sex) > 0) {
      df <- df %>% filter(PATIENT_CURRENT_SEX %in% input$lab_sex)
    }
    
    if (!is.null(input$lab_agegrp) && length(input$lab_agegrp) > 0) {
      df <- df %>% filter(as.character(AGE_GROUP) %in% input$lab_agegrp)
    }
    if (!is.null(input$lab_race) && length(input$lab_race) > 0) {
      df <- df %>% filter(RACE_STD %in% input$lab_race)
    }
    if (!is.null(input$lab_eth) && length(input$lab_eth) > 0) {
      df <- df %>% filter(ETHNICITY_STD %in% input$lab_eth)
    }
    
    df
  })
  
  inv_filt <- reactive({
    df <- inv_data()
    req(df)
    
    validate(need(input$inv_x %in% names(df), paste("Column not found:", input$inv_x)))
    dcol <- input$inv_x
    
    df <- df %>%
      mutate(.DATE = as.Date(.data[[dcol]])) %>%
      filter(!is.na(.DATE))
    
    if (!is.null(input$inv_date_rng) && all(!is.na(input$inv_date_rng))) {
      df <- df %>% filter(.DATE >= input$inv_date_rng[1], .DATE <= input$inv_date_rng[2])
    }
    
    if (!is.null(input$inv_bucket) && length(input$inv_bucket) > 0) {
      df <- df %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    }
    if (!is.null(input$inv_condition) && length(input$inv_condition) > 0) {
      df <- df %>% filter(CONDITION_STD %in% input$inv_condition)
    }
    if ("INV_CASE_STATUS" %in% names(df) && !is.null(input$inv_case_status) && length(input$inv_case_status) > 0) {
      df <- df %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    }
    if ("INVESTIGATION_STATUS" %in% names(df) && !is.null(input$inv_inv_status) && length(input$inv_inv_status) > 0) {
      df <- df %>% filter(INVESTIGATION_STATUS %in% input$inv_inv_status)
    }
    if ("PATIENT_COUNTY" %in% names(df) && !is.null(input$inv_county) && length(input$inv_county) > 0) {
      df <- df %>% filter(PATIENT_COUNTY %in% input$inv_county)
    }
    
    if (!is.null(input$inv_agegrp) && length(input$inv_agegrp) > 0) {
      df <- df %>% filter(as.character(AGE_GROUP) %in% input$inv_agegrp)
    }
    if (!is.null(input$inv_race) && length(input$inv_race) > 0) {
      df <- df %>% filter(RACE_STD %in% input$inv_race)
    }
    if (!is.null(input$inv_eth) && length(input$inv_eth) > 0) {
      df <- df %>% filter(ETHNICITY_STD %in% input$inv_eth)
    }
    
    df
  })
  


  # -------------------------
  # SUMMARY: populate picker choices (mirror Labs/Inv tabs)
  # -------------------------
  observeEvent(lab_data(), {
    df <- lab_data()
    req(df)

    # date range defaults
    candidate_dates <- intersect(c("LAB_REPORT_DATE","LAB_RPT_CREATED_DT","SPECIMEN_COLLECTION_DT","EVENT_DATE"), names(df))
    if (length(candidate_dates) > 0) {
      dd <- df[[candidate_dates[1]]]
      dd <- dd[!is.na(dd)]
      if (length(dd) > 0) updateDateRangeInput(session, "sum_lab_date_rng", start = min(dd), end = max(dd))
    }

    updatePickerInput(session, "sum_lab_bucket",
                      choices = sort(unique(na.omit(df$PROGRAM_BUCKET))),
                      selected = character(0))
    updatePickerInput(session, "sum_lab_condition",
                      choices = sort(unique(na.omit(df$CONDITION_STD))),
                      selected = character(0))

    updatePickerInput(session, "sum_lab_agegrp",
                      choices = levels(df$AGE_GROUP) %||% sort(unique(na.omit(df$AGE_GROUP))),
                      selected = character(0))
    updatePickerInput(session, "sum_lab_race",
                      choices = sort(unique(na.omit(df$RACE_STD))),
                      selected = character(0))
    updatePickerInput(session, "sum_lab_eth",
                      choices = sort(unique(na.omit(df$ETHNICITY_STD))),
                      selected = character(0))

    if ("PATIENT_COUNTY" %in% names(df)) {
      updatePickerInput(session, "sum_lab_county",
                        choices = sort(unique(na.omit(df$PATIENT_COUNTY))),
                        selected = character(0))
    }
    if ("PATIENT_CURRENT_SEX" %in% names(df)) {
      updatePickerInput(session, "sum_lab_sex",
                        choices = sort(unique(na.omit(df$PATIENT_CURRENT_SEX))),
                        selected = character(0))
    }
  })

  observeEvent(inv_data(), {
    df <- inv_data()
    req(df)

    candidate_dates <- intersect(c("INV_REPORT_DT","INV_START_DT","RECORD_ADDED_TO_NBS_DTTIME","EVENT_DATE","REPORT_DT"), names(df))
    if (length(candidate_dates) > 0) {
      dd <- df[[candidate_dates[1]]]
      dd <- dd[!is.na(dd)]
      if (length(dd) > 0) updateDateRangeInput(session, "sum_inv_date_rng", start = min(dd), end = max(dd))
    }

    if ("PROGRAM_BUCKET" %in% names(df)) {
      updatePickerInput(session, "sum_inv_bucket",
                        choices = sort(unique(na.omit(df$PROGRAM_BUCKET))),
                        selected = character(0))
    }
    if ("CONDITION" %in% names(df)) {
      updatePickerInput(session, "sum_inv_condition",
                        choices = sort(unique(na.omit(df$CONDITION))),
                        selected = character(0))
    }

    if ("AGE_GROUP" %in% names(df)) {
      updatePickerInput(session, "sum_inv_agegrp",
                        choices = levels(df$AGE_GROUP) %||% sort(unique(na.omit(df$AGE_GROUP))),
                        selected = character(0))
    }
    if ("RACE_STD" %in% names(df)) {
      updatePickerInput(session, "sum_inv_race",
                        choices = sort(unique(na.omit(df$RACE_STD))),
                        selected = character(0))
    }
    if ("ETHNICITY_STD" %in% names(df)) {
      updatePickerInput(session, "sum_inv_eth",
                        choices = sort(unique(na.omit(df$ETHNICITY_STD))),
                        selected = character(0))
    }
    if ("PATIENT_COUNTY" %in% names(df)) {
      updatePickerInput(session, "sum_inv_county",
                        choices = sort(unique(na.omit(df$PATIENT_COUNTY))),
                        selected = character(0))
    }
    if ("PATIENT_CURRENT_SEX" %in% names(df)) {
      updatePickerInput(session, "sum_inv_sex",
                        choices = sort(unique(na.omit(df$PATIENT_CURRENT_SEX))),
                        selected = character(0))
    }
  })

  # -------------------------
  # SUMMARY: filtered datasets (separate from Labs/Inv tabs)
  # -------------------------
  labs_sum_filt <- reactive({
    df <- lab_data()
    req(df)

    validate(need(input$sum_lab_x %in% names(df), paste("Column not found:", input$sum_lab_x)))
    dcol <- input$sum_lab_x

    df <- df %>%
      mutate(.DATE = as.Date(.data[[dcol]])) %>%
      filter(!is.na(.DATE))

    if (!is.null(input$sum_lab_date_rng[1]) && !is.null(input$sum_lab_date_rng[2])) {
      df <- df %>% filter(.DATE >= input$sum_lab_date_rng[1], .DATE <= input$sum_lab_date_rng[2])
    }

    if (!is.null(input$sum_lab_bucket) && length(input$sum_lab_bucket) > 0) {
      df <- df %>% filter(PROGRAM_BUCKET %in% input$sum_lab_bucket)
    }
    if (!is.null(input$sum_lab_condition) && length(input$sum_lab_condition) > 0) {
      df <- df %>% filter(CONDITION_STD %in% input$sum_lab_condition)
    }
    if (!is.null(input$sum_lab_county) && length(input$sum_lab_county) > 0 && "PATIENT_COUNTY" %in% names(df)) {
      df <- df %>% filter(PATIENT_COUNTY %in% input$sum_lab_county)
    }
    if (!is.null(input$sum_lab_sex) && length(input$sum_lab_sex) > 0 && "PATIENT_CURRENT_SEX" %in% names(df)) {
      df <- df %>% filter(PATIENT_CURRENT_SEX %in% input$sum_lab_sex)
    }
    if (!is.null(input$sum_lab_agegrp) && length(input$sum_lab_agegrp) > 0 && "AGE_GROUP" %in% names(df)) {
      df <- df %>% filter(AGE_GROUP %in% input$sum_lab_agegrp)
    }
    if (!is.null(input$sum_lab_race) && length(input$sum_lab_race) > 0 && "RACE_STD" %in% names(df)) {
      df <- df %>% filter(RACE_STD %in% input$sum_lab_race)
    }
    if (!is.null(input$sum_lab_eth) && length(input$sum_lab_eth) > 0 && "ETHNICITY_STD" %in% names(df)) {
      df <- df %>% filter(ETHNICITY_STD %in% input$sum_lab_eth)
    }

    df
  })

  inv_sum_filt <- reactive({
    df <- inv_data()
    req(df)

    validate(need(input$sum_inv_x %in% names(df), paste("Column not found:", input$sum_inv_x)))
    dcol <- input$sum_inv_x

    df <- df %>%
      mutate(.DATE = as.Date(.data[[dcol]])) %>%
      filter(!is.na(.DATE))

    if (!is.null(input$sum_inv_date_rng[1]) && !is.null(input$sum_inv_date_rng[2])) {
      df <- df %>% filter(.DATE >= input$sum_inv_date_rng[1], .DATE <= input$sum_inv_date_rng[2])
    }

    if (!is.null(input$sum_inv_bucket) && length(input$sum_inv_bucket) > 0 && "PROGRAM_BUCKET" %in% names(df)) {
      df <- df %>% filter(PROGRAM_BUCKET %in% input$sum_inv_bucket)
    }
    if (!is.null(input$sum_inv_condition) && length(input$sum_inv_condition) > 0 && "CONDITION" %in% names(df)) {
      df <- df %>% filter(CONDITION %in% input$sum_inv_condition)
    }
    if (!is.null(input$sum_inv_county) && length(input$sum_inv_county) > 0 && "PATIENT_COUNTY" %in% names(df)) {
      df <- df %>% filter(PATIENT_COUNTY %in% input$sum_inv_county)
    }
    if (!is.null(input$sum_inv_sex) && length(input$sum_inv_sex) > 0 && "PATIENT_CURRENT_SEX" %in% names(df)) {
      df <- df %>% filter(PATIENT_CURRENT_SEX %in% input$sum_inv_sex)
    }
    if (!is.null(input$sum_inv_agegrp) && length(input$sum_inv_agegrp) > 0 && "AGE_GROUP" %in% names(df)) {
      df <- df %>% filter(AGE_GROUP %in% input$sum_inv_agegrp)
    }
    if (!is.null(input$sum_inv_race) && length(input$sum_inv_race) > 0 && "RACE_STD" %in% names(df)) {
      df <- df %>% filter(RACE_STD %in% input$sum_inv_race)
    }
    if (!is.null(input$sum_inv_eth) && length(input$sum_inv_eth) > 0 && "ETHNICITY_STD" %in% names(df)) {
      df <- df %>% filter(ETHNICITY_STD %in% input$sum_inv_eth)
    }

    df
  })

  # -------------------------
  # SUMMARY: metrics table (labs / inv / both)
  # -------------------------
  summary_metrics <- reactive({
    src <- input$sum_source %||% "both"
    show5y <- isTRUE(input$sum_show5y)

    labs_apply_filters <- function(df) {
      req(df)
      if (!is.null(input$sum_lab_bucket) && length(input$sum_lab_bucket) > 0) df <- df %>% filter(PROGRAM_BUCKET %in% input$sum_lab_bucket)
      if (!is.null(input$sum_lab_condition) && length(input$sum_lab_condition) > 0) df <- df %>% filter(CONDITION_STD %in% input$sum_lab_condition)
      if (!is.null(input$sum_lab_county) && length(input$sum_lab_county) > 0 && "PATIENT_COUNTY" %in% names(df)) df <- df %>% filter(PATIENT_COUNTY %in% input$sum_lab_county)
      if (!is.null(input$sum_lab_sex) && length(input$sum_lab_sex) > 0 && "PATIENT_CURRENT_SEX" %in% names(df)) df <- df %>% filter(PATIENT_CURRENT_SEX %in% input$sum_lab_sex)
      if (!is.null(input$sum_lab_agegrp) && length(input$sum_lab_agegrp) > 0 && "AGE_GROUP" %in% names(df)) df <- df %>% filter(AGE_GROUP %in% input$sum_lab_agegrp)
      if (!is.null(input$sum_lab_race) && length(input$sum_lab_race) > 0 && "RACE_STD" %in% names(df)) df <- df %>% filter(RACE_STD %in% input$sum_lab_race)
      if (!is.null(input$sum_lab_eth) && length(input$sum_lab_eth) > 0 && "ETHNICITY_STD" %in% names(df)) df <- df %>% filter(ETHNICITY_STD %in% input$sum_lab_eth)
      df
    }

    inv_apply_filters <- function(df) {
      req(df)
      if (!is.null(input$sum_inv_bucket) && length(input$sum_inv_bucket) > 0 && "PROGRAM_BUCKET" %in% names(df)) df <- df %>% filter(PROGRAM_BUCKET %in% input$sum_inv_bucket)
      if (!is.null(input$sum_inv_condition) && length(input$sum_inv_condition) > 0 && "CONDITION" %in% names(df)) df <- df %>% filter(CONDITION %in% input$sum_inv_condition)
      if (!is.null(input$sum_inv_county) && length(input$sum_inv_county) > 0 && "PATIENT_COUNTY" %in% names(df)) df <- df %>% filter(PATIENT_COUNTY %in% input$sum_inv_county)
      if (!is.null(input$sum_inv_sex) && length(input$sum_inv_sex) > 0 && "PATIENT_CURRENT_SEX" %in% names(df)) df <- df %>% filter(PATIENT_CURRENT_SEX %in% input$sum_inv_sex)
      if (!is.null(input$sum_inv_agegrp) && length(input$sum_inv_agegrp) > 0 && "AGE_GROUP" %in% names(df)) df <- df %>% filter(AGE_GROUP %in% input$sum_inv_agegrp)
      if (!is.null(input$sum_inv_race) && length(input$sum_inv_race) > 0 && "RACE_STD" %in% names(df)) df <- df %>% filter(RACE_STD %in% input$sum_inv_race)
      if (!is.null(input$sum_inv_eth) && length(input$sum_inv_eth) > 0 && "ETHNICITY_STD" %in% names(df)) df <- df %>% filter(ETHNICITY_STD %in% input$sum_inv_eth)
      df
    }

    labs_df_all <- NULL
    inv_df_all  <- NULL

    if (src %in% c("labs","both")) {
      df0 <- lab_data()
      validate(need(input$sum_lab_x %in% names(df0), paste("Column not found:", input$sum_lab_x)))
      df0 <- df0 %>% mutate(.DATE = as.Date(.data[[input$sum_lab_x]])) %>% filter(!is.na(.DATE))
      labs_df_all <- labs_apply_filters(df0)
      if (!is.null(input$sum_lab_date_rng[1]) && !is.null(input$sum_lab_date_rng[2])) {
        labs_df_all <- labs_df_all %>% filter(.DATE >= input$sum_lab_date_rng[1], .DATE <= input$sum_lab_date_rng[2])
      }
    }

    if (src %in% c("inv","both")) {
      df0 <- inv_data()
      validate(need(input$sum_inv_x %in% names(df0), paste("Column not found:", input$sum_inv_x)))
      df0 <- df0 %>% mutate(.DATE = as.Date(.data[[input$sum_inv_x]])) %>% filter(!is.na(.DATE))
      inv_df_all <- inv_apply_filters(df0)
      if (!is.null(input$sum_inv_date_rng[1]) && !is.null(input$sum_inv_date_rng[2])) {
        inv_df_all <- inv_df_all %>% filter(.DATE >= input$sum_inv_date_rng[1], .DATE <= input$sum_inv_date_rng[2])
      }
    }

    g_lab <- input$sum_lab_group %||% "PROGRAM_BUCKET"
    g_inv <- input$sum_inv_group %||% "PROGRAM_BUCKET"

    labs_agg <- tibble::tibble(Group = character())
    inv_agg  <- tibble::tibble(Group = character())

    if (!is.null(labs_df_all) && nrow(labs_df_all) > 0 && g_lab %in% names(labs_df_all)) {
      validate(need("LAB_RPT_LOCAL_ID" %in% names(labs_df_all), "Labs file must contain LAB_RPT_LOCAL_ID"))
      labs_agg <- labs_df_all %>%
        group_by(Group = .data[[g_lab]]) %>%
        summarise(
          labs = n_distinct(LAB_RPT_LOCAL_ID),
          patients_labs = if ("PATIENT_LOCAL_ID" %in% names(labs_df_all)) n_distinct(PATIENT_LOCAL_ID) else NA_integer_,
          start_labs = min(.DATE, na.rm = TRUE),
          end_labs   = max(.DATE, na.rm = TRUE),
          .groups = "drop"
        )
    }

    if (!is.null(inv_df_all) && nrow(inv_df_all) > 0 && g_inv %in% names(inv_df_all)) {
      inv_agg <- inv_df_all %>%
        group_by(Group = .data[[g_inv]]) %>%
        summarise(
          cases = if ("INV_LOCAL_ID" %in% names(inv_df_all)) n_distinct(INV_LOCAL_ID) else n(),
          patients_cases = if ("PATIENT_LOCAL_ID" %in% names(inv_df_all)) n_distinct(PATIENT_LOCAL_ID) else NA_integer_,
          start_cases = min(.DATE, na.rm = TRUE),
          end_cases   = max(.DATE, na.rm = TRUE),
          .groups = "drop"
        )
    }

    if (src == "labs") {
      out <- labs_agg %>%
        mutate(days = as.integer(end_labs - start_labs) + 1L,
               labs_per_day = ifelse(days > 0, round(labs / days, 3), NA_real_)) %>%
        transmute(
          Group = as.character(Group),
          `Total labs` = labs,
          `Patients (labs)` = patients_labs,
          `Labs/day` = labs_per_day,
          `Period start` = start_labs,
          `Period end` = end_labs,
          `Days` = days
        )
    } else if (src == "inv") {
      out <- inv_agg %>%
        mutate(days = as.integer(end_cases - start_cases) + 1L,
               cases_per_day = ifelse(days > 0, round(cases / days, 3), NA_real_)) %>%
        transmute(
          Group = as.character(Group),
          `Total cases` = cases,
          `Patients (cases)` = patients_cases,
          `Cases/day` = cases_per_day,
          `Period start` = start_cases,
          `Period end` = end_cases,
          `Days` = days
        )
    } else {
      if (nrow(inv_agg) == 0 && nrow(labs_agg) == 0) return(tibble::tibble(Group = character()))
      out <- dplyr::full_join(inv_agg, labs_agg, by = "Group")
      out$cases <- dplyr::coalesce(out$cases, 0L)
      out$labs  <- dplyr::coalesce(out$labs, 0L)
      out$start <- dplyr::coalesce(out$start_cases, out$start_labs)
      out$end   <- dplyr::coalesce(out$end_cases, out$end_labs)
      out$days  <- as.integer(out$end - out$start) + 1L
      out$labs_per_day  <- ifelse(out$days > 0, round(out$labs / out$days, 3), NA_real_)
      out$cases_per_day <- ifelse(out$days > 0, round(out$cases / out$days, 3), NA_real_)
      out$cases_per_lab <- dplyr::if_else(out$labs > 0, round(out$cases / out$labs, 3), NA_real_)
      out <- out %>%
        transmute(
          Group = as.character(Group),
          `Total cases` = cases,
          `Total labs` = labs,
          `Cases/day` = cases_per_day,
          `Labs/day` = labs_per_day,
          `Cases per lab` = cases_per_lab,
          `Period start` = start,
          `Period end` = end,
          `Days` = days
        )
    }

    if (!show5y || is.null(out) || nrow(out) == 0) return(out)

    end_date <- suppressWarnings(max(as.Date(out$`Period end`), na.rm = TRUE))
    if (!is.finite(end_date)) return(out)

    this_year <- lubridate::year(end_date)
    ytd_start <- as.Date(paste0(this_year, "-01-01"))
    ytd_len <- as.integer(end_date - ytd_start)
    yrs <- (this_year - 1):(this_year - 5)

    labs_ytd_for_year <- function(year) {
      df0 <- lab_data() %>% mutate(.DATE = as.Date(.data[[input$sum_lab_x]])) %>% filter(!is.na(.DATE))
      df0 <- labs_apply_filters(df0)
      s <- as.Date(paste0(year, "-01-01"))
      e <- s + ytd_len
      df0 <- df0 %>% filter(.DATE >= s, .DATE <= e)
      if (nrow(df0) == 0 || !(g_lab %in% names(df0))) return(tibble::tibble(Group = character(), val = integer()))
      df0 %>% group_by(Group = .data[[g_lab]]) %>% summarise(val = n_distinct(LAB_RPT_LOCAL_ID), .groups="drop")
    }

    inv_ytd_for_year <- function(year) {
      df0 <- inv_data() %>% mutate(.DATE = as.Date(.data[[input$sum_inv_x]])) %>% filter(!is.na(.DATE))
      df0 <- inv_apply_filters(df0)
      s <- as.Date(paste0(year, "-01-01"))
      e <- s + ytd_len
      df0 <- df0 %>% filter(.DATE >= s, .DATE <= e)
      if (nrow(df0) == 0 || !(g_inv %in% names(df0))) return(tibble::tibble(Group = character(), val = integer()))
      df0 %>% group_by(Group = .data[[g_inv]]) %>% summarise(val = if ("INV_LOCAL_ID" %in% names(df0)) n_distinct(INV_LOCAL_ID) else n(), .groups="drop")
    }

    hist <- dplyr::bind_rows(lapply(yrs, function(y) {
      if (src == "labs") labs_ytd_for_year(y) %>% mutate(year = y) else inv_ytd_for_year(y) %>% mutate(year = y)
    }))

    med <- hist %>% group_by(Group) %>% summarise(median_5y = as.integer(stats::median(val, na.rm = TRUE)), .groups="drop")

    cur_val <- if (src == "labs") {
      out %>% transmute(Group, cur = `Total labs`)
    } else if (src == "inv") {
      out %>% transmute(Group, cur = `Total cases`)
    } else {
      out %>% transmute(Group, cur = `Total cases`)
    }

    out2 <- out %>%
      left_join(med, by = "Group") %>%
      left_join(cur_val, by = "Group") %>%
      mutate(
        `5-Year Median YTD` = median_5y,
        `Percent Change vs 5Y Median` = dplyr::if_else(!is.na(median_5y) & median_5y > 0, round((cur - median_5y) / median_5y * 100, 1), NA_real_),
        Status = dplyr::case_when(
          is.na(`Percent Change vs 5Y Median`) ~ "—",
          `Percent Change vs 5Y Median` >= 10 ~ "↑",
          `Percent Change vs 5Y Median` <= -10 ~ "↓",
          TRUE ~ "→"
        )
      ) %>%
      select(-median_5y, -cur)

    out2
  })
  output$sum_value_boxes <- renderUI({
    df <- summary_metrics()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    src <- input$sum_source %||% "both"
    if (src == "labs") {
      total <- sum(df$`Total labs`, na.rm=TRUE)
      rate <- round(mean(df$`Labs/day`, na.rm=TRUE), 3)
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box("Total lab orders", total),
        bslib::value_box("Average labs/day", rate),
        bslib::value_box("Groups shown", nrow(df))
      )
    } else if (src == "inv") {
      total <- sum(df$`Total cases`, na.rm=TRUE)
      rate <- round(mean(df$`Cases/day`, na.rm=TRUE), 3)
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box("Total cases", total),
        bslib::value_box("Average cases/day", rate),
        bslib::value_box("Groups shown", nrow(df))
      )
    } else {
      total_cases <- sum(df$`Total cases`, na.rm=TRUE)
      total_labs  <- sum(df$`Total labs`, na.rm=TRUE)
      cpl <- if (total_labs > 0) round(total_cases / total_labs, 3) else NA_real_
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box("Total cases", total_cases),
        bslib::value_box("Total lab orders", total_labs),
        bslib::value_box("Cases per lab", cpl)
      )
    }
  })

  output$sum_table <- DT::renderDataTable({
    df <- summary_metrics()
    req(df)

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 12, autoWidth = TRUE, scrollX = TRUE)
    )
  })

  output$sum_plot <- renderPlotly({
    df <- summary_metrics()
    req(df)

    src <- input$sum_source %||% "both"

    if (src == "labs") {
      p <- ggplot(df, aes(x = reorder(Group, `Total labs`), y = `Total labs`)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Total lab orders", title = "Lab order volume by group") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y"))
    } else if (src == "inv") {
      p <- ggplot(df, aes(x = reorder(Group, `Total cases`), y = `Total cases`)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Total cases", title = "Case volume by group") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y"))
    } else {
      p <- ggplot(df, aes(x = reorder(Group, `Total cases`), y = `Cases per lab`)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Cases per lab", title = "Case-per-lab ratio by group (screening signal)") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y"))
    }
  })



  # -------------------------
  # Combined trends plot (Labs + Investigations)
  # -------------------------
  combo_df <- reactive({
    freq <- input$combo_freq %||% "month"

    labs0 <- tryCatch(labs_filt(), error = function(e) NULL)
    inv0  <- tryCatch(inv_filt(),  error = function(e) NULL)

    out <- tibble::tibble()

    if (!is.null(labs0) && nrow(labs0) > 0 && ".DATE" %in% names(labs0)) {
      validate(need("LAB_RPT_LOCAL_ID" %in% names(labs0), "Labs file must contain LAB_RPT_LOCAL_ID"))
      ll <- labs0 %>%
        mutate(period = periodize_date(.DATE, freq)) %>%
        group_by(period) %>%
        summarise(value = n_distinct(LAB_RPT_LOCAL_ID), .groups = "drop") %>%
        mutate(Source = "Labs")
      out <- bind_rows(out, ll)
    }

    if (!is.null(inv0) && nrow(inv0) > 0 && ".DATE" %in% names(inv0)) {
      has_inv_id <- "INV_LOCAL_ID" %in% names(inv0)
      ii <- inv0 %>%
        mutate(period = periodize_date(.DATE, freq)) %>%
        group_by(period) %>%
        summarise(value = if (has_inv_id) n_distinct(INV_LOCAL_ID) else n(), .groups = "drop") %>%
        mutate(Source = "Cases")
      out <- bind_rows(out, ii)
    }

    out
  })

  output$combo_plot <- renderPlotly({
    df <- combo_df()
    req(df)

    ptype <- input$combo_plot_type %||% "Line"

    if (ptype == "Line") {
      p <- ggplot(df, aes(x = period, y = value, color = Source)) +
        geom_line(linewidth = 1) +
        labs(x = NULL, y = "Count", title = "Labs and cases over time") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y","color"))
    } else if (ptype == "Bar") {
      p <- ggplot(df, aes(x = period, y = value, fill = Source)) +
        geom_col(position = "dodge") +
        labs(x = NULL, y = "Count", title = "Labs and cases over time") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y","fill"))
    } else if (ptype == "Horizontal Bar") {
      p <- ggplot(df, aes(x = period, y = value, fill = Source)) +
        geom_col(position = "dodge") +
        coord_flip() +
        labs(x = NULL, y = "Count", title = "Labs and cases over time") +
        theme_minimal(base_size = 13)
      ggplotly(p, tooltip = c("x","y","fill"))
    } else {
      labs_df <- df %>% filter(Source == "Labs")
      case_df <- df %>% filter(Source == "Cases")

      p <- ggplot() +
        geom_col(data = labs_df, aes(x = period, y = value), alpha = 0.7) +
        geom_line(data = case_df, aes(x = period, y = value), linewidth = 1) +
        labs(x = NULL, y = "Count", title = "Labs (bars) and cases (line)") +
        theme_minimal(base_size = 13)

      ggplotly(p, tooltip = c("x","y"))
    }
  })
  observeEvent(list(input$lab_bucket, lab_data()), {
    df <- lab_data()
    req(df)
    
    # base for cascading
    d1 <- df
    if (!is.null(input$lab_bucket) && length(input$lab_bucket) > 0) {
      d1 <- d1 %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    }
    
    safe_update_picker(session, "lab_condition",
                       choices = d1$CONDITION_STD,
                       selected_current = input$lab_condition)
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$lab_bucket, input$lab_condition, lab_data()), {
    df <- lab_data()
    req(df)
    
    d2 <- df
    if (!is.null(input$lab_bucket) && length(input$lab_bucket) > 0) {
      d2 <- d2 %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    }
    if (!is.null(input$lab_condition) && length(input$lab_condition) > 0) {
      d2 <- d2 %>% filter(CONDITION_STD %in% input$lab_condition)
    }
    
    if ("PATIENT_COUNTY" %in% names(d2)) {
      safe_update_picker(session, "lab_county",
                         choices = d2$PATIENT_COUNTY,
                         selected_current = input$lab_county)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$lab_bucket, input$lab_condition, input$lab_county, lab_data()), {
    df <- lab_data()
    req(df)
    
    d3 <- df
    if (!is.null(input$lab_bucket) && length(input$lab_bucket) > 0) {
      d3 <- d3 %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    }
    if (!is.null(input$lab_condition) && length(input$lab_condition) > 0) {
      d3 <- d3 %>% filter(CONDITION_STD %in% input$lab_condition)
    }
    if ("PATIENT_COUNTY" %in% names(d3) && !is.null(input$lab_county) && length(input$lab_county) > 0) {
      d3 <- d3 %>% filter(PATIENT_COUNTY %in% input$lab_county)
    }
    
    if ("PATIENT_CURRENT_SEX" %in% names(d3)) {
      safe_update_picker(session, "lab_sex",
                         choices = d3$PATIENT_CURRENT_SEX,
                         selected_current = input$lab_sex)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$inv_bucket, inv_data()), {
    df <- inv_data()
    req(df)
    
    d1 <- df
    if (!is.null(input$inv_bucket) && length(input$inv_bucket) > 0) {
      d1 <- d1 %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    }
    
    safe_update_picker(session, "inv_condition",
                       choices = d1$CONDITION_STD,
                       selected_current = input$inv_condition)
  }, ignoreInit = TRUE)
  
  
  observeEvent(list(input$inv_bucket, input$inv_condition, inv_data()), {
    df <- inv_data()
    req(df)
    
    d2 <- df
    if (!is.null(input$inv_bucket) && length(input$inv_bucket) > 0) {
      d2 <- d2 %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    }
    if (!is.null(input$inv_condition) && length(input$inv_condition) > 0) {
      d2 <- d2 %>% filter(CONDITION_STD %in% input$inv_condition)
    }
    
    if ("INV_CASE_STATUS" %in% names(d2)) {
      safe_update_picker(session, "inv_case_status",
                         choices = d2$INV_CASE_STATUS,
                         selected_current = input$inv_case_status)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$inv_bucket, input$inv_condition, input$inv_case_status, inv_data()), {
    df <- inv_data()
    req(df)
    
    d3 <- df
    if (!is.null(input$inv_bucket) && length(input$inv_bucket) > 0) {
      d3 <- d3 %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    }
    if (!is.null(input$inv_condition) && length(input$inv_condition) > 0) {
      d3 <- d3 %>% filter(CONDITION_STD %in% input$inv_condition)
    }
    if ("INV_CASE_STATUS" %in% names(d3) && !is.null(input$inv_case_status) && length(input$inv_case_status) > 0) {
      d3 <- d3 %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    }
    
    if ("INVESTIGATION_STATUS" %in% names(d3)) {
      safe_update_picker(session, "inv_inv_status",
                         choices = d3$INVESTIGATION_STATUS,
                         selected_current = input$inv_inv_status)
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(list(input$inv_bucket, input$inv_condition, input$inv_case_status, input$inv_inv_status, inv_data()), {
    df <- inv_data()
    req(df)
    
    d4 <- df
    if (!is.null(input$inv_bucket) && length(input$inv_bucket) > 0) {
      d4 <- d4 %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    }
    if (!is.null(input$inv_condition) && length(input$inv_condition) > 0) {
      d4 <- d4 %>% filter(CONDITION_STD %in% input$inv_condition)
    }
    if ("INV_CASE_STATUS" %in% names(d4) && !is.null(input$inv_case_status) && length(input$inv_case_status) > 0) {
      d4 <- d4 %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    }
    if ("INVESTIGATION_STATUS" %in% names(d4) && !is.null(input$inv_inv_status) && length(input$inv_inv_status) > 0) {
      d4 <- d4 %>% filter(INVESTIGATION_STATUS %in% input$inv_inv_status)
    }
    
    if ("PATIENT_COUNTY" %in% names(d4)) {
      safe_update_picker(session, "inv_county",
                         choices = d4$PATIENT_COUNTY,
                         selected_current = input$inv_county)
    }
  }, ignoreInit = TRUE)
  
  # ---- LAB: upstream filters constrain AGE_GROUP ----
  observeEvent(list(input$lab_bucket, input$lab_condition, input$lab_county, input$lab_sex, lab_data()), {
    df <- lab_data(); req(df)
    
    d <- df
    if (length(input$lab_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    if (length(input$lab_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$lab_condition)
    if (length(input$lab_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$lab_county)
    if (length(input$lab_sex) > 0) d <- d %>% filter(PATIENT_CURRENT_SEX %in% input$lab_sex)
    
    safe_update_picker(session, "lab_agegrp",
                       choices = as.character(d$AGE_GROUP),
                       selected_current = input$lab_agegrp)
  }, ignoreInit = TRUE)
  
  # ---- LAB: upstream filters constrain RACE ----
  observeEvent(list(input$lab_bucket, input$lab_condition, input$lab_county, input$lab_sex, input$lab_agegrp, lab_data()), {
    df <- lab_data(); req(df)
    
    d <- df
    if (length(input$lab_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    if (length(input$lab_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$lab_condition)
    if (length(input$lab_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$lab_county)
    if (length(input$lab_sex) > 0) d <- d %>% filter(PATIENT_CURRENT_SEX %in% input$lab_sex)
    if (length(input$lab_agegrp) > 0) d <- d %>% filter(as.character(AGE_GROUP) %in% input$lab_agegrp)
    
    safe_update_picker(session, "lab_race",
                       choices = d$RACE_STD,
                       selected_current = input$lab_race)
  }, ignoreInit = TRUE)
  
  # ---- LAB: upstream filters constrain ETHNICITY ----
  observeEvent(list(input$lab_bucket, input$lab_condition, input$lab_county, input$lab_sex, input$lab_agegrp, input$lab_race, lab_data()), {
    df <- lab_data(); req(df)
    
    d <- df
    if (length(input$lab_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$lab_bucket)
    if (length(input$lab_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$lab_condition)
    if (length(input$lab_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$lab_county)
    if (length(input$lab_sex) > 0) d <- d %>% filter(PATIENT_CURRENT_SEX %in% input$lab_sex)
    if (length(input$lab_agegrp) > 0) d <- d %>% filter(as.character(AGE_GROUP) %in% input$lab_agegrp)
    if (length(input$lab_race) > 0) d <- d %>% filter(RACE_STD %in% input$lab_race)
    
    safe_update_picker(session, "lab_eth",
                       choices = d$ETHNICITY_STD,
                       selected_current = input$lab_eth)
  }, ignoreInit = TRUE)
  
  # ---- INV: upstream filters constrain AGE_GROUP ----
  observeEvent(list(input$inv_bucket, input$inv_condition, input$inv_case_status, input$inv_inv_status, input$inv_county, inv_data()), {
    df <- inv_data(); req(df)
    
    d <- df
    if (length(input$inv_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    if (length(input$inv_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$inv_condition)
    if (length(input$inv_case_status) > 0) d <- d %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    if (length(input$inv_inv_status) > 0) d <- d %>% filter(INVESTIGATION_STATUS %in% input$inv_inv_status)
    if (length(input$inv_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$inv_county)
    
    safe_update_picker(session, "inv_agegrp",
                       choices = as.character(d$AGE_GROUP),
                       selected_current = input$inv_agegrp)
  }, ignoreInit = TRUE)
  
  # ---- INV: upstream filters constrain RACE ----
  observeEvent(list(input$inv_bucket, input$inv_condition, input$inv_case_status, input$inv_inv_status, input$inv_county, input$inv_agegrp, inv_data()), {
    df <- inv_data(); req(df)
    
    d <- df
    if (length(input$inv_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    if (length(input$inv_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$inv_condition)
    if (length(input$inv_case_status) > 0) d <- d %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    if (length(input$inv_inv_status) > 0) d <- d %>% filter(INVESTIGATION_STATUS %in% input$inv_inv_status)
    if (length(input$inv_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$inv_county)
    if (length(input$inv_agegrp) > 0) d <- d %>% filter(as.character(AGE_GROUP) %in% input$inv_agegrp)
    
    safe_update_picker(session, "inv_race",
                       choices = d$RACE_STD,
                       selected_current = input$inv_race)
  }, ignoreInit = TRUE)
  
  # ---- INV: upstream filters constrain ETHNICITY ----
  observeEvent(list(input$inv_bucket, input$inv_condition, input$inv_case_status, input$inv_inv_status, input$inv_county, input$inv_agegrp, input$inv_race, inv_data()), {
    df <- inv_data(); req(df)
    
    d <- df
    if (length(input$inv_bucket) > 0) d <- d %>% filter(PROGRAM_BUCKET %in% input$inv_bucket)
    if (length(input$inv_condition) > 0) d <- d %>% filter(CONDITION_STD %in% input$inv_condition)
    if (length(input$inv_case_status) > 0) d <- d %>% filter(INV_CASE_STATUS %in% input$inv_case_status)
    if (length(input$inv_inv_status) > 0) d <- d %>% filter(INVESTIGATION_STATUS %in% input$inv_inv_status)
    if (length(input$inv_county) > 0) d <- d %>% filter(PATIENT_COUNTY %in% input$inv_county)
    if (length(input$inv_agegrp) > 0) d <- d %>% filter(as.character(AGE_GROUP) %in% input$inv_agegrp)
    if (length(input$inv_race) > 0) d <- d %>% filter(RACE_STD %in% input$inv_race)
    
    safe_update_picker(session, "inv_eth",
                       choices = d$ETHNICITY_STD,
                       selected_current = input$inv_eth)
  }, ignoreInit = TRUE)
  
  
  
  # ---- Lab Plot ----
  output$lab_plot <- renderPlotly({
    df0 <- labs_filt()
    req(df0)

    validate(
      need(input$lab_group %in% names(df0), paste("Column not found:", input$lab_group)),
      need("LAB_RPT_LOCAL_ID" %in% names(df0), "Labs file must contain LAB_RPT_LOCAL_ID")
    )

    unit <- input$lab_time_unit %||% "day"

    df <- df0 %>%
      mutate(.PERIOD = periodize_date(.DATE, unit)) %>%
      group_by(Date = .PERIOD, Group = .data[[input$lab_group]]) %>%
      summarise(n = n_distinct(LAB_RPT_LOCAL_ID), .groups = "drop")

    validate(need(nrow(df) > 0, "No rows to plot after filters."))

    if ((input$lab_plot_type %||% "Line") == "Bar") {
      p <- ggplot(df, aes(x = Date, y = n, fill = as.factor(Group))) +
        geom_col(alpha = 0.9) +
        labs(x = NULL, y = "Distinct Lab Report IDs", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      orient <- input$lab_bar_orient %||% "Vertical"
      if (identical(orient, "Horizontal")) {
        p <- p + coord_flip() + theme(axis.text.x = element_text(angle = 0, hjust = 1))
      }

      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          height = input$lab_plot_h,
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    } else {
      p <- ggplot(df, aes(x = Date, y = n, color = as.factor(Group))) +
        geom_line(alpha = 0.85, linewidth = 1) +
        geom_point(alpha = 0.9, size = 2) +
        labs(x = NULL, y = "Distinct Lab Report IDs", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(
          height = input$lab_plot_h,
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    }
  })

  # ---- Lab Table ----# ---- Lab Table ----
  output$lab_table <- renderDT({
    df0 <- labs_filt()
    req(df0)
    
    show_cols <- intersect(
      c("PROGRAM_BUCKET", "CONDITION_STD", "DISEASE_CATEGORY", "LAB_RPT_LOCAL_ID", input$lab_x,
        "SPECIMEN_COLLECTION_DT", "PATIENT_COUNTY", "PATIENT_CURRENT_SEX", "PATIENT_RACE_CALCULATED", "FINAL_AGE"),
      names(df0)
    )
    
    datatable(df0 %>% select(all_of(show_cols)) %>% head(1000),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$inv_table <- renderDT({
    df0 <- inv_filt()
    req(df0)
    
    show_cols <- intersect(
      c("PROGRAM_BUCKET","CONDITION_STD","CONDITION","INV_LOCAL_ID", input$inv_x,
        "INV_CASE_STATUS","INVESTIGATION_STATUS","PROGRAM_AREA","JURISDICTION_NAME",
        "PATIENT_COUNTY","PATIENT_CURRENT_SEX","AGE_GROUP","RACE_STD","ETHNICITY_STD","FINAL_AGE"),
      names(df0)
    )
    
    datatable(df0 %>% select(all_of(show_cols)) %>% head(1000),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ---- Investigations Plot ----
  output$inv_plot <- renderPlotly({
    df0 <- inv_filt()
    req(df0)

    validate(need(input$inv_group %in% names(df0), paste("Column not found:", input$inv_group)))

    has_inv_id <- "INV_LOCAL_ID" %in% names(df0)
    unit <- input$inv_time_unit %||% "day"

    df <- df0 %>%
      mutate(.PERIOD = periodize_date(.DATE, unit)) %>%
      group_by(Date = .PERIOD, Group = .data[[input$inv_group]]) %>%
      summarise(n = if (has_inv_id) n_distinct(INV_LOCAL_ID) else n(), .groups = "drop")

    validate(need(nrow(df) > 0, "No rows to plot after filters."))

    ylab <- if (has_inv_id) "Distinct Investigation IDs" else "Row count"

    if ((input$inv_plot_type %||% "Line") == "Bar") {
      p <- ggplot(df, aes(x = Date, y = n, fill = as.factor(Group))) +
        geom_col(alpha = 0.9) +
        labs(x = NULL, y = ylab, fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      orient <- input$inv_bar_orient %||% "Vertical"
      if (identical(orient, "Horizontal")) {
        p <- p + coord_flip() + theme(axis.text.x = element_text(angle = 0, hjust = 1))
      }

      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          height = input$inv_plot_h,
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    } else {
      p <- ggplot(df, aes(x = Date, y = n, color = as.factor(Group))) +
        geom_line(alpha = 0.85, linewidth = 1) +
        geom_point(alpha = 0.9, size = 2) +
        labs(x = NULL, y = ylab, color = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(
          height = input$inv_plot_h,
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    }
  })

  output$labs_conditions <- renderDT({
    df <- labs_filt()
    req(df)
    
    x <- df %>%
      distinct(CONDITION_STD, DISEASE_CATEGORY) %>%
      arrange(CONDITION_STD)
    
    datatable(x, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$inv_conditions <- renderDT({
    df <- inv_filt()
    req(df)
    
    x <- df %>%
      distinct(CONDITION_STD, CONDITION, CONDITION_CD, PROGRAM_AREA) %>%
      arrange(CONDITION_STD)
    
    datatable(x, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)
