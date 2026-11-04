#library(readxl)
library(nortest)
#library(tseries)
library(moments)
library(grid)


# 1️⃣ Type Detection Helper

detect_type <- function(x) {
  
  # Numeric
  if (is.numeric(x)) {
    
    return("Numerical")
    
  }
  
  # Date
  if (inherits(x, "Date") ||
      inherits(x, "POSIXct")) {
    
    return("Date")
    
  }
  
  # Character / factor
  
  unique_n <- length(
    unique(
      na.omit(x)
    )
  )
  
  # Binary or categorical
  if (unique_n <= 10) {
    
    return("Categorical")
    
  }
  
  # Character with many levels
  return("Categorical")
}


# ------------------------------------------------------------
# Numerical descriptive statistics
# ------------------------------------------------------------

numeric_summary <- function(x, variable_name) {
  
  x <- x[!is.na(x)]
  
  n <- length(x)
  
  missing <- sum(is.na(x))
  
  if (n == 0) {
    
    return(
      data.frame(
        Variable = variable_name,
  #      Type = "Numerical",
        N = 0,
        Missing = missing,
        Mean = NA,
        SD = NA,
        Median = NA,
        IQR = NA,
        Minimum = NA,
        Maximum = NA,
        Shapiro_p = NA,
        Normality = NA,  
        Reporting = NA
      )
    )
  }
  
  # Shapiro-Wilk only valid for n >= 3
  # and R's shapiro.test supports n <= 5000
  
  if (n >= 3 && n <= 5000) {
    
    shapiro_p <- tryCatch(
      shapiro.test(x)$p.value,
      error = function(e) NA
    )
    
  } else {
    
    shapiro_p <- NA
  }
  #Normality
  if (!is.na(shapiro_p) && shapiro_p >= 0.05) {
    
    normality <- "Normal"
    
  } else {
    
    normality <- " Non Normal"
  }
  
  
  
  # Decide reporting format
  
  if (!is.na(shapiro_p) && shapiro_p >= 0.05) {
    
    reporting <- "Mean ± SD"
    
  } else {
    
    reporting <- "Median (IQR)"
  }
  
  
  data.frame(
    
    Variable = variable_name,
    
   # Type = "Numerical",
    
    N = n,
    
    Missing = missing,
    
    Mean = round(mean(x), 2),
    
    SD = round(sd(x), 2),
    
    Median = round(median(x), 2),
    
    IQR = round(IQR(x), 2),
    
    Minimum = round(min(x), 2),
    
    Maximum = round(max(x), 2),
    
    Shapiro_p = round(shapiro_p, 4),
   
   Normality = normality,
    
    Reporting = reporting
  )
}


# ------------------------------------------------------------
# Categorical descriptive statistics
# ------------------------------------------------------------

categorical_summary <- function(x, variable_name) {
  
  x_original <- x
  
  x <- as.character(x)
  
  x[is.na(x) | x == ""] <- "Missing"
  
  tab <- table(x)
  
  result <- data.frame(
    
    Variable = variable_name,
    
    Category = names(tab),
    
    Frequency = as.integer(tab),
    
    Percentage = round(
      100 * as.integer(tab) / sum(tab),
      2
    ),
    
    stringsAsFactors = FALSE
  )
  
  result
}


# ------------------------------------------------------------
# Create overall variable classification
# ------------------------------------------------------------

variable_classification <- function(dat) {
  
  data.frame(
    
    Variable = names(dat),
    
    Type = sapply(
      dat,
      detect_type
    ),
    
    Missing = sapply(
      dat,
      function(x) sum(is.na(x))
    ),
    
    Unique = sapply(
      dat,
      function(x)
        length(unique(na.omit(x)))
    ),
    
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# Create numerical table
# ------------------------------------------------------------

create_numeric_table <- function(dat) {
  
  numeric_vars <- names(
    dat[
      sapply(
        dat,
        function(x) is.numeric(x)
      )
    ]
  )
  
  if (length(numeric_vars) == 0) {
    
    return(NULL)
  }
  
  results <- lapply(
    numeric_vars,
    function(v) {
      
      numeric_summary(
        dat[[v]],
        v
      )
    }
  )
  
  bind_rows(results)
}


# ------------------------------------------------------------
# Create categorical table
# ------------------------------------------------------------

create_categorical_table <- function(dat) {
  
  categorical_vars <- names(
    dat[
      sapply(
        dat,
        function(x)
          !is.numeric(x)
      )
    ]
  )
  
  if (length(categorical_vars) == 0) {
    
    return(NULL)
  }
  
  results <- lapply(
    categorical_vars,
    function(v) {
      
      categorical_summary(
        dat[[v]],
        v
      )
    }
  )
  
  bind_rows(results)
}



server <- function(input, output, session) {
  
  editable_data <- reactiveVal(NULL)
  
  output$sheet_selector <- renderUI({
    req(input$file_global)
    if (!isTruthy(input$file_global$name)) return(NULL)
    
    ext <- tools::file_ext(input$file_global$name)
    
    if (tolower(ext) %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(input$file_global$datapath)
      selectInput("sheet_number", "Select Sheet:", choices = sheets)
    } else {
      NULL 
    }
  })
  
  # 2. Unified observer protected by isTruthy()
  observeEvent({
    input$file_global
    input$sheet_number
  }, {
    req(input$file_global)
    if (!isTruthy(input$file_global$name)) return()
    
    ext <- tools::file_ext(input$file_global$name)
    
    df <- if (tolower(ext) == "csv") {
      read.csv(input$file_global$datapath, stringsAsFactors = FALSE)
    } else {
      if (!isTruthy(input$sheet_number)) return()
      readxl::read_excel(input$file_global$datapath, sheet = input$sheet_number)
    }
    
    editable_data(as.data.frame(df))
  })
  
  observeEvent(input$file_global, {
    req(input$file_global)
    ext <- tools::file_ext(input$file_global$name)
    
    if (tolower(ext) == "csv") {
      df <- read.csv(input$file_global$datapath, stringsAsFactors = FALSE)
      editable_data(as.data.frame(df))
    } else {
      sheet_val <- if (isTruthy(input$sheet_number)) {
        input$sheet_number
      } else {
        readxl::excel_sheets(input$file_global$datapath)[1]
      }
      
      df <- readxl::read_excel(input$file_global$datapath, sheet = sheet_val)
      editable_data(as.data.frame(df))
    }
  })
  
  # ==========================================
  # TWO-STAGE SUBGROUP ANALYSIS LOGIC
  # ==========================================
  
  output$preview1 <- renderDT({
    
    req(global_data())
    
    datatable(global_data(), options = list(pageLength = 5, scrollX = TRUE))
    
  })
  
  
  output$subgroup_controls1 <- renderUI({
    req(input$subgroup_var1, editable_data())
    df <- editable_data()
    var <- input$subgroup_var1
    
    if (var == "") return(NULL)
    target_col <- df[[var]]
    
    if (is.numeric(target_col)) {
      min_val <- min(target_col, na.rm = TRUE)
      max_val <- max(target_col, na.rm = TRUE)
      tagList(
        numericInput("subgroup_min1", paste("Min for Var 1 (", var, "):"), value = min_val),
        numericInput("subgroup_max1", paste("Max for Var 1 (", var, "):"), value = max_val)
      )
    } else {
      unique_vals <- unique(as.character(target_col))
      unique_vals <- unique_vals[!is.na(unique_vals)]
      checkboxGroupInput("subgroup_categories1", paste("Categories for Variable 1 (", var, "):"),
                         choices = unique_vals, selected = unique_vals)
    }
  })
  
  output$subgroup_controls2 <- renderUI({
    req(input$subgroup_var2, editable_data())
    df <- editable_data()
    var <- input$subgroup_var2
    
    if (var == "") return(NULL)
    target_col <- df[[var]]
    
    if (is.numeric(target_col)) {
      min_val <- min(target_col, na.rm = TRUE)
      max_val <- max(target_col, na.rm = TRUE)
      tagList(
        numericInput("subgroup_min2", paste("Min for Var 2 (", var, "):"), value = min_val),
        numericInput("subgroup_max2", paste("Max for Var 2 (", var, "):"), value = max_val)
      )
    } else {
      unique_vals <- unique(as.character(target_col))
      unique_vals <- unique_vals[!is.na(unique_vals)]
      checkboxGroupInput("subgroup_categories2", paste("Categories for Variable 2 (", var, "):"),
                         choices = unique_vals, selected = unique_vals)
    }
  })
  
  subgroup_data <- reactive({
    df <- editable_data()
    req(df)
    
    if (!is.null(input$subgroup_var1) && input$subgroup_var1 != "") {
      var1 <- input$subgroup_var1
      col1 <- df[[var1]]
      if (is.numeric(col1)) {
        req(input$subgroup_min1, input$subgroup_max1)
        df <- df[is.na(col1) | (col1 >= input$subgroup_min1 & col1 <= input$subgroup_max1), , drop = FALSE]
      } else {
        req(input$subgroup_categories1)
        cats1 <- input$subgroup_categories1
        df <- df[is.na(col1) | as.character(col1) %in% cats1, , drop = FALSE]
      }
    }
    
    if (!is.null(input$subgroup_var2) && input$subgroup_var2 != "") {
      var2 <- input$subgroup_var2
      col2 <- df[[var2]]
      if (is.numeric(col2)) {
        req(input$subgroup_min2, input$subgroup_max2)
        df <- df[is.na(col2) | (col2 >= input$subgroup_min2 & col2 <= input$subgroup_max2), , drop = FALSE]
      } else {
        req(input$subgroup_categories2)
        cats2 <- input$subgroup_categories2
        df <- df[is.na(col2) | as.character(col2) %in% cats2, , drop = FALSE]
      }
    }
    
    df
  })
  
  global_data <- reactive({
    req(subgroup_data())
    subgroup_data()
  })
  
  # ==========================================
  # VARIABLE CODING LOGIC
  # ==========================================
  
  # Dynamic UI for variable coding based on type (Categorical vs Numerical)
  output$coding_ui <- renderUI({
    req(input$code_var, editable_data())
    df <- editable_data()
    var <- input$code_var
    if (var == "") return(NULL)
    
    target_col <- df[[var]]
    
    if (is.numeric(target_col)) {
      # Numerical variable -> Ask for number of groups
      tagList(
        numericInput("num_groups", "Number of Groups:", value = 2, min = 1, max = 10, step = 1),
        uiOutput("numeric_groups_ui")
      )
    } else {
      # Categorical variable -> Show categories and ask for code assignments
      unique_vals <- unique(as.character(target_col))
      unique_vals <- unique_vals[!is.na(unique_vals)]
      
      tagList(
        tags$h4("Assign Codes to Categories:"),
        lapply(unique_vals, function(cat_val) {
          numericInput(paste0("cat_code_", gsub("[^a-zA-Z0-9]", "_", cat_val)), 
                       label = paste("Code for category:", cat_val), 
                       value = 0)
        })
      )
    }
  })
  
  # Dynamic sub-UI for specifying range/bounds for numerical groups
  output$numeric_groups_ui <- renderUI({
    req(input$num_groups)
    k <- input$num_groups
    
    tagList(
      lapply(1:k, function(i) {
        fluidRow(
          column(4, numericInput(paste0("grp_min_", i), paste("Group", i, "Min:"), value = 0)),
          column(4, numericInput(paste0("grp_max_", i), paste("Group", i, "Max:"), value = 0)),
          column(4, numericInput(paste0("grp_val_", i), paste("Group", i, "Assign Code:"), value = i))
        )
      })
    )
  })
  
  
  # ==========================================
  # DATA VALIDATION & OUTLIER DETECTION PIPELINE
  # ==========================================
  
  # Dynamically update available numeric columns for outlier detection
  observeEvent(subgroup_data(), {
    df <- subgroup_data()
    if (!is.null(df)) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectizeInput(session, "outlier_cols", choices = num_cols)
    }
  })
  
  # Validated dataset reactive pipeline
  validated_data <- reactive({
    df <- subgroup_data()
    req(df)
    
    # 1. Handle Missing Values (NAs)
    if (isTRUE(input$handle_na)) {
      df <- na.omit(df)
    }
    
    # 2. Handle Outliers using IQR Method
    if (isTRUE(input$handle_outliers) && !is.null(input$outlier_cols) && length(input$outlier_cols) > 0) {
      for (col in input$outlier_cols) {
        if (col %in% colnames(df) && is.numeric(df[[col]])) {
          vals <- df[[col]]
          q1 <- quantile(vals, 0.25, na.rm = TRUE)
          q3 <- quantile(vals, 0.75, na.rm = TRUE)
          iqr_val <- q3 - q1
          
          lower_bound <- q1 - 1.5 * iqr_val
          upper_bound <- q3 + 1.5 * iqr_val
          
          # Keep rows within bounds (preserving NA values if present)
          df <- df[is.na(vals) | (vals >= lower_bound & vals <= upper_bound), , drop = FALSE]
        }
      }
    }
    
    df
  })
  
  # Update global data reference to point to the validated dataset
  global_data <- reactive({
    req(validated_data())
    validated_data()
  })
  
  # 3. Render the editable DataTable using the validated dataset
  output$preview <- renderDT({
    req(validated_data())
    datatable(
      validated_data(), 
      editable = 'cell', 
      options = list(pageLength = 5, scrollX = TRUE),
      selection = 'single'
    )
  })
  
  
  # Apply Coding and append the new column to editable_data()
  # Update inside your server code for apply_coding:
  observeEvent(input$apply_coding, {
    req(input$code_var, input$coded_col_name) # Changed from input$new_col_name
    df <- editable_data()
    var <- input$code_var
    target_col <- df[[var]]
    new_col <- rep(NA, nrow(df))
    
    if (is.numeric(target_col)) {
      req(input$num_groups)
      k <- input$num_groups
      for (i in 1:k) {
        g_min <- input[[paste0("grp_min_", i)]]
        g_max <- input[[paste0("grp_max_", i)]]
        g_val <- input[[paste0("grp_val_", i)]]
        if (!is.null(g_min) && !is.null(g_max) && !is.null(g_val)) {
          new_col[!is.na(target_col) & target_col >= g_min & target_col <= g_val] <- g_val
        }
      }
    } else {
      unique_vals <- unique(as.character(target_col))
      unique_vals <- unique_vals[!is.na(unique_vals)]
      for (cat_val in unique_vals) {
        input_id <- paste0("cat_code_", gsub("[^a-zA-Z0-9]", "_", cat_val))
        assigned_code <- input[[input_id]]
        if (!is.null(assigned_code)) {
          new_col[!is.na(target_col) & as.character(target_col) == cat_val] <- assigned_code
        }
      }
    }
    
    # Append new coded column using the unique coded_col_name input
    df[[input$coded_col_name]] <- new_col
    editable_data(df)
    showNotification("Coded variable successfully added as a new column!", type = "message")
  })
  
  # Dynamic UI for transformation inputs based on the operation chosen
  output$transform_ui <- renderUI({
    df <- editable_data()
    if (is.null(df)) return(NULL)
    cols <- colnames(df)
    
    req(input$transform_type)
    if (input$transform_type == "sum") {
      selectizeInput("sum_vars", "Select Variables to Add (Sum):", choices = cols, multiple = TRUE)
    } else if (input$transform_type == "div") {
      tagList(
        selectInput("num_var", "Numerator Variable:", choices = cols),
        selectInput("den_var", "Denominator Variable:", choices = cols)
      )
    } else if (input$transform_type == "custom") {
      textInput("custom_expr", "Enter R Expression (e.g., var1 + var2 * 2):", value = "")
    }
  })
  
  # Execute the transformation and append as a new column
  observeEvent(input$apply_transform, {
    req(input$transformed_col_name, input$transform_type)
    df <- editable_data()
    new_col_name <- input$transformed_col_name
    
    new_col <- switch(input$transform_type,
                      "sum" = {
                        req(input$sum_vars)
                        rowSums(df[, input$sum_vars, drop = FALSE], na.rm = TRUE)
                      },
                      "div" = {
                        req(input$num_var, input$den_var)
                        df[[input$num_var]] / df[[input$den_var]]
                      },
                      "custom" = {
                        req(input$custom_expr)
                        tryCatch({
                          with(df, eval(parse(text = input$custom_expr)))
                        }, error = function(e) {
                          showNotification(paste("Expression Error:", e$message), type = "error")
                          NULL
                        })
                      },
                      NULL
    )
    
    if (!is.null(new_col)) {
      df[[new_col_name]] <- new_col
      editable_data(df)
      showNotification("Variable transformation successfully applied and added as a new column!", type = "message")
    }
  })
  
  
  
  # ==========================================
  # RENDER & EDIT ACTIONS
  # ==========================================
  
  output$preview <- renderDT({
    req(subgroup_data())
    datatable(
      subgroup_data(), 
      editable = 'cell', 
      options = list(pageLength = 5, scrollX = TRUE),
      selection = 'single'
    )
  })
  
  observeEvent(input$preview_cell_edit, {
    info <- input$preview_cell_edit
    df_full <- editable_data()
    df_sub <- subgroup_data()
    
    row_name_full <- rownames(df_sub)[info$row]
    actual_row_idx <- which(rownames(df_full) == row_name_full)
    
    df_full[actual_row_idx, info$col] <- DT::coerceValue(info$value, df_full[actual_row_idx, info$col])
    editable_data(df_full)
  })
  
  observeEvent(input$add_row, {
    df <- editable_data()
    new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
    colnames(new_row) <- colnames(df)
    editable_data(rbind(df, new_row))
  })
  
  observeEvent(input$delete_row, {
    req(input$preview_rows_selected)
    df_full <- editable_data()
    df_sub <- subgroup_data()
    
    rows_to_delete <- rownames(df_sub)[input$preview_rows_selected]
    df_full <- df_full[!(rownames(df_full) %in% rows_to_delete), , drop = FALSE]
    editable_data(df_full)
  })
  
  observeEvent(input$add_col, {
    req(input$new_col_name) 
    df <- editable_data()
    df[[input$new_col_name]] <- NA 
    editable_data(df)
  })
  
  observeEvent(editable_data(), {
    df <- editable_data()
    if (!is.null(df)) {
      updateSelectInput(session, "col_to_delete", choices = colnames(df))
      cols <- c("None (Full Dataset)" = "", colnames(df))
      updateSelectInput(session, "subgroup_var1", choices = cols)
      updateSelectInput(session, "subgroup_var2", choices = cols)
      updateSelectInput(session, "code_var", choices = c("Select Variable" = "", colnames(df)))
    }
  })
  
  observeEvent(input$delete_col_btn, {
    req(input$col_to_delete)
    df <- editable_data()
    
    if (ncol(df) > 1 && input$col_to_delete %in% colnames(df)) {
      df[[input$col_to_delete]] <- NULL
      editable_data(df)
    } else {
      showNotification("Cannot delete the last remaining column!", type = "error")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coded_and_subgroup_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(subgroup_data(), file, row.names = FALSE)
    }
  )
  # ----------------------------------------------------------
  # QUICK ANALYSIS & TYPE DETECTION
  # ----------------------------------------------------------
  
  detect_type <- function(x) {
    if (is.numeric(x)) {
      return("Numerical")
    }
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      return("Date")
    }
    
    unique_n <- length(unique(na.omit(x)))
    if (unique_n <= 10) {
      return("Categorical")
    } else {
      return("Text / ID")
    }
  }
  
  classification_data <- eventReactive(input$analyse, {
    req(global_data())
    # Replace with your custom variable_classification function if defined elsewhere
    # variable_classification(global_data()) 
    
    df <- global_data()
    data.frame(
      Variable = names(df),
      Type = sapply(df, detect_type),
      stringsAsFactors = FALSE
    )
  })
  
  output$classification <- DT::renderDT({
    req(classification_data())
    DT::datatable(classification_data(), options = list(scrollX = TRUE))
  })
  
  # ----------------------------------------------------------
  # NUMERICAL ANALYSIS
  # ----------------------------------------------------------
  
  numeric_data <- eventReactive(input$analyse, {
    req(global_data())
    # Ensure create_numeric_table() is defined in your helper scripts, 
    # or implement basic numerical summary here:
    create_numeric_table(global_data())
  })
  
  output$numeric_table <- DT::renderDT({
    req(numeric_data())
    DT::datatable(numeric_data(), options = list(scrollX = TRUE))
  })
  
  # ----------------------------------------------------------
  # CATEGORICAL ANALYSIS
  # ----------------------------------------------------------
  
  categorical_data <- eventReactive(input$analyse, {
    req(global_data())
    df <- global_data()
    
    cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 10))]
    
    if (ncol(cat_cols) == 0) {
      return(data.frame(Message = "No categorical variables found."))
    }
    
    data.frame(
      Variable = names(cat_cols),
      Unique_Categories = sapply(cat_cols, function(x) length(unique(na.omit(x)))),
      Most_Frequent = sapply(cat_cols, function(x) {
        t <- sort(table(x, exclude = NULL), decreasing = TRUE)
        names(t)[1]
      }),
      stringsAsFactors = FALSE
    )
  })
  
  output$categorical_table <- renderDT({
    req(categorical_data())
    datatable(categorical_data(), options = list(scrollX = TRUE))
  })
  
  # ----------------------------------------------------------
  # WORD DOCUMENT DOWNLOAD HANDLER
  # ----------------------------------------------------------
  
  output$download_wordQ <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(classification_data())
      
      doc <- officer::read_docx()
      
      # Title Section
      doc <- officer::body_add_par(doc, "Descriptive Analysis Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Date:", Sys.Date()))
      doc <- officer::body_add_par(doc, "") 
      
      # 1. Variable Classification Section
      doc <- officer::body_add_par(doc, "Variable Classification", style = "heading 2")
      
      class_df <- as.data.frame(classification_data())
      
      if (nrow(class_df) > 0) {
        ft1 <- flextable::flextable(class_df)
        ft1 <- flextable::theme_booktabs(ft1)
        ft1 <- flextable::fontsize(ft1, size = 9, part = "body")
        ft1 <- flextable::fontsize(ft1, size = 10, part = "header")
        ft1 <- flextable::bold(ft1, part = "header")
        ft1 <- flextable::autofit(ft1)
        
        # FIX: Use flextable namespace instead of officer
        doc <- flextable::body_add_flextable(doc, ft1)
      } else {
        doc <- officer::body_add_par(doc, "No classification data available.")
      }
      
      doc <- officer::body_add_par(doc, "") 
      
      # 2. Numerical Analysis Section
      num_dat <- tryCatch({
        as.data.frame(numeric_data())
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(num_dat) && nrow(num_dat) > 0 && !"Message" %in% names(num_dat)) {
        doc <- officer::body_add_par(doc, "Numerical Variables Summary", style = "heading 2")
        ft2 <- flextable::flextable(num_dat)
        ft2 <- flextable::theme_booktabs(ft2)
        ft2 <- flextable::fontsize(ft2, size = 6, part = "body")
        ft2 <- flextable::fontsize(ft2, size = 7, part = "header")
        ft2 <- flextable::bold(ft2, part = "header")
        ft2 <- flextable::autofit(ft2)
        
        # FIX: Use flextable namespace here as well
        doc <- flextable::body_add_flextable(doc, ft2)
        doc <- officer::body_add_par(doc, "") 
      }
      
      print(doc, target = file)
    }
  )
  # Frequency table and advanced plots ---------------------------------------
  # Frequency table and advanced plots ---------------------------------------
  
  # 🔹 Select variable from global data
  
  # Frequency Table and Advanced Plots -------------------------------------------
  
  # 1. Select variable from global data
  output$freq_var <- renderUI({
    req(global_data())
    
    selectInput(
      inputId = "freq_variable",
      label = "Select Variable:",
      choices = names(global_data())
    )
  })
  
  # 2. Reactive Frequency Table Generator
  freq_table_data <- reactive({
    
    req(global_data(), input$freq_variable, input$freq_type)
    
    data <- global_data()
    
    validate(
      need(
        input$freq_variable %in% names(data),
        "Please select a valid variable."
      )
    )
    
    col_data <- data[[input$freq_variable]]
    
    # ---------------------------------------------------------------------------
    # A. DISCRETE / CATEGORICAL FREQUENCY TABLE
    # ---------------------------------------------------------------------------
    if (input$freq_type == "discrete") {
      
      x <- as.factor(col_data)
      x <- droplevels(x[!is.na(x)])
      
      validate(
        need(length(x) > 0, "Selected column contains no valid data.")
      )
      
      freq <- table(x)
      
      df <- data.frame(
        Category = names(freq),
        Frequency = as.integer(freq),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      names(df)[1] <- input$freq_variable
      
      df$`Relative_Freq (%)` <- round(
        df$Frequency / sum(df$Frequency) * 100, 1
      )
      
      df$Cumulative_Freq <- as.character(cumsum(df$Frequency))
      
      # Total row
      total_row <- data.frame(
        Category = "Total",
        Frequency = sum(df$Frequency),
        `Relative_Freq (%)` = 100.0,
        Cumulative_Freq = "-",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      names(total_row)[1] <- input$freq_variable
      
      df <- rbind(df, total_row)
      
      return(df)
    }
    
    # ---------------------------------------------------------------------------
    # B. NUMERIC GROUPED FREQUENCY TABLE
    # ---------------------------------------------------------------------------
    
    # Convert numeric columns safely, including factors
    if (is.factor(col_data)) {
      x <- suppressWarnings(as.numeric(as.character(col_data)))
    } else {
      x <- suppressWarnings(as.numeric(col_data))
    }
    
    x <- x[!is.na(x) & is.finite(x)]
    
    validate(
      need(
        length(x) >= 2,
        "At least 2 valid numeric values are required."
      )
    )
    
    validate(
      need(
        isTRUE(input$freq_type %in% c("inclusive", "exclusive")),
        "Please select a valid interval type."
      )
    )
    
    # TRUE: right-closed intervals; FALSE: left-closed intervals
    right_closed <- identical(input$freq_type, "inclusive")
    
    # ---------------------------------------------------------------------------
    # C. DETERMINE CLASS INTERVAL BREAKS
    # ---------------------------------------------------------------------------
    
    if (is.null(input$width_type) ||
        identical(input$width_type, "equal")) {
      
      w <- input$class_width
      
      validate(
        need(
          length(w) == 1 && is.finite(w) && w > 0,
          "Enter a valid positive class width."
        )
      )
      
      lower <- floor(min(x) / w) * w
      upper <- ceiling(max(x) / w) * w
      
      # Handle constant values or data at a class boundary
      if (lower >= upper) {
        upper <- lower + w
      }
      
      breaks <- seq(lower, upper, by = w)
      
      validate(
        need(
          length(breaks) >= 2 && all(diff(breaks) > 0),
          "Unable to construct class intervals. Check the class width."
        )
      )
      
    } else {
      
      # Custom / unequal class intervals
      req(input$custom_breaks)
      
      breaks_text <- trimws(input$custom_breaks)
      
      validate(
        need(
          nzchar(breaks_text),
          "Enter custom break points separated by commas."
        )
      )
      
      breaks <- suppressWarnings(
        as.numeric(trimws(strsplit(breaks_text, ",", fixed = TRUE)[[1]]))
      )
      
      validate(
        need(
          length(breaks) >= 2,
          "At least 2 break points are required (e.g., 0, 10, 20)."
        ),
        need(
          all(is.finite(breaks)),
          "Invalid break points. Enter valid finite numbers."
        ),
        need(
          all(diff(breaks) > 0),
          "Custom break points must be in strictly increasing order."
        )
      )
      
      validate(
        need(
          min(x) >= min(breaks) && max(x) <= max(breaks),
          paste0(
            "Dataset range [", min(x), ", ", max(x),
            "] exceeds custom breaks range [",
            min(breaks), ", ", max(breaks), "]."
          )
        )
      )
    }
    
    # ---------------------------------------------------------------------------
    # D. CALCULATE GROUPED FREQUENCIES
    # ---------------------------------------------------------------------------
    
    h <- hist(
      x,
      breaks = breaks,
      right = right_closed,
      include.lowest = TRUE,
      plot = FALSE
    )
    
    freq <- as.integer(h$counts)
    
    # ---------------------------------------------------------------------------
    # E. CREATE CLASS INTERVAL LABELS
    # ---------------------------------------------------------------------------
    
    intervals <- character(length(freq))
    
    for (i in seq_along(freq)) {
      
      lower_i <- breaks[i]
      upper_i <- breaks[i + 1]
      
      if (right_closed) {
        
        # Right-closed intervals: (a, b], first interval includes its minimum
        if (i == 1) {
          intervals[i] <- paste0("[", lower_i, ", ", upper_i, "]")
        } else {
          intervals[i] <- paste0("(", lower_i, ", ", upper_i, "]")
        }
        
      } else {
        
        # Left-closed intervals: [a, b), last interval includes its maximum
        if (i == length(freq)) {
          intervals[i] <- paste0("[", lower_i, ", ", upper_i, "]")
        } else {
          intervals[i] <- paste0("[", lower_i, ", ", upper_i, ")")
        }
      }
    }
    
    df <- data.frame(
      Class_Interval = intervals,
      Frequency = freq,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Relative frequency and cumulative frequency
    total_frequency <- sum(df$Frequency)
    
    df$`Relative_Freq (%)` <- round(
      df$Frequency / total_frequency * 100, 1
    )
    
    df$Cumulative_Freq <- as.character(cumsum(df$Frequency))
    
    # ---------------------------------------------------------------------------
    # F. APPEND TOTAL ROW
    # ---------------------------------------------------------------------------
    
    total_row <- data.frame(
      Class_Interval = "Total",
      Frequency = total_frequency,
      `Relative_Freq (%)` = 100.0,
      Cumulative_Freq = "-",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    df <- rbind(df, total_row)
    
    return(df)
  })
  
  # 3. Render Frequency Table
  output$grouped_freq <- renderTable({
    
    freq_table_data()
    
  }, digits = 1, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Save Frequency Table (CSV Download)
  output$download_freq_table <- downloadHandler(
    filename = function() {
      paste0("frequency_table_", input$freq_variable, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(freq_table_data(), file, row.names = FALSE)
    }
  )
  
  # ==============================================================================
  # 🔹 2. REACTIVE PLOT DRAWING FUNCTION & DOWNLOAD
  # ==============================================================================
  # ----------------------------------------------------
  # 1. DRAWING FUNCTION
  # ----------------------------------------------------
  draw_advanced_plot <- function(color, alpha, border_color, lwd, point_size, point_shape, palette) {
    req(input$show_chart)
    req(isTRUE(input$show_chart))
    req(input$graph_type, input$freq_display)
    req(global_data(), input$freq_variable)
    req(input$freq_variable %in% names(global_data()))
    
    raw_data <- global_data()[[input$freq_variable]]
  
    plot_title <- if (!is.null(input$custom_univariate_title) && nzchar(trimws(input$custom_univariate_title))) {
      input$custom_univariate_title
    } else {
      # Default auto-generated title based on graph type
      switch(input$graph_type,
             "bar"          = paste("Bar Chart of", input$freq_variable),
             "pie"          = paste("Pie Chart of", input$freq_variable),
             "line"         = paste("Line Chart of", input$freq_variable),
             "hist"         = paste("Histogram of", input$freq_variable),
             "polygon"      = paste("Frequency Polygon of", input$freq_variable),
             "ogive_less"   = paste("Ogive (Less Than) of", input$freq_variable),
             "ogive_more"   = paste("Ogive (More Than) of", input$freq_variable),
             paste("Chart of", input$freq_variable))
    }
    
      
    # Adjust margins temporarily to prevent long x-axis labels from being cut off
    # Adjust margins: c(bottom, left, top, right) in lines of text
    old_par <- par(mar = c(8, 6, 4, 2) + 0.1)
    on.exit(par(old_par), add = TRUE)
    
    # Helper function for pie chart colors
    get_pie_colors <- function(n, pal_type) {
      cols <- switch(pal_type,
                     "rainbow" = rainbow(n),
                     "terrain" = terrain.colors(n),
                     "heat" = heat.colors(n),
                     "blues" = colorRampPalette(blues9)(n), # Safely interpolates any number of colors
                     rainbow(n)
      )
      adjustcolor(cols, alpha.f = alpha)
    } # Closes get_pie_colors()
    
    # ----------------------------------------------------
    # CATEGORICAL / NON-NUMERIC DATA
    # ----------------------------------------------------
    if (!is.numeric(raw_data) || (input$freq_type == "discrete")) {
      
      validate(
        need(
          input$graph_type %in% c("bar", "bar_horizontal","line","pie"),
          "Histograms, Polygons, and Ogives require continuous numeric grouped data."
        )
      )
      
      x <- as.factor(raw_data)
      x <- na.omit(x)
      validate(need(length(x) > 0, "No valid data available for the selected variable."))
      
      freq <- table(x)
      labels <- names(freq)
      
      if (input$freq_display == "percent") {
        values <- round((freq / sum(freq)) * 100, 1)
        ylab_text <- "Percentage (%)"
      } else {
        values <- as.integer(freq)
        ylab_text <- "Frequency"
      }
      
      
      if (input$graph_type %in% c("bar", "bar_horizontal")) {
        
        label_texts <- if (input$freq_display == "percent") {
          paste0(values, "%")
        } else {
          as.character(values)
        }
        
        if (input$graph_type == "bar") {
          
          # Vertical bar chart
          b <- barplot(
            values,
            names.arg = labels,
            col = adjustcolor(color, alpha.f = alpha),
            main = plot_title,
            border = border_color,
            ylab = ylab_text,
            ylim = c(0, max(values, 0) * 1.15),
            las = 1
          )
          
          text(
            x = b,
            y = values,
            labels = label_texts,
            pos = 3,
            xpd = TRUE,
            cex = 0.9,
            font = 2
          )
          
        } else {
          
          # Horizontal bar chart
          b <- barplot(
            values,
            names.arg = labels,
            horiz = TRUE,
            col = adjustcolor(color, alpha.f = alpha),
            main = plot_title,
            border = border_color,
            xlab = ylab_text,
            xlim = c(0, max(values, 0) * 1.18),
            las = 1
          )
          
          text(
            x = values,
            y = b,
            labels = label_texts,
            pos = 4,
            xpd = TRUE,
            cex = 0.9,
            font = 2
          )
        }
      }
      # Closes graph_type == "bar"
      # Line chart for discrete / categorical data
      if (input$graph_type == "line") {
        
        # Use numeric positions for category labels
        x_pos <- seq_along(values)
        
        # Set suitable Y-axis limits
        y_max <- max(values, na.rm = TRUE)
        if (!is.finite(y_max) || y_max <= 0) {
          y_max <- 1
        }
        
        plot(
          x_pos,
          values,
          type = "o",
          col = color,
          lwd = lwd,
          pch = as.numeric(point_shape),
          cex = point_size,
          xaxt = "n",
          xlim = c(0.5, length(values) + 0.5),
          ylim = c(0, y_max * 1.2),
          main = plot_title,
          xlab = input$freq_variable,
          ylab = ylab_text
        )
        
        axis(
          side = 1,
          at = x_pos,
          labels = labels,
          las = 1
        )
        
        # Add value labels
        text(
          x_pos,
          values,
          labels = if (input$freq_display == "percent") {
            paste0(values, "%")
          } else {
            as.character(values)
          },
          pos = 3,
          cex = 0.85,
          font = 2,
          xpd = TRUE
        )
      }
      
      if (input$graph_type == "pie") {
        valid_idx <- values > 0
        plot_values <- values[valid_idx]
        plot_labels <- labels[valid_idx]
        
        validate(need(length(plot_values) > 0, "No non-zero data available for the pie chart."))
        
        pie(plot_values,
            labels = paste0(plot_labels, " (", plot_values,
                            ifelse(input$freq_display == "percent", "%", ""), ")"),
            col = get_pie_colors(length(plot_values), palette),
            main = paste("Pie Chart of", input$freq_variable))
      } # Closes graph_type == "pie"
      
    } # Closes categorical data `if` block
    
    
    
    # ----------------------------------------------------
    # NUMERIC DATA (GROUPED)
    # ----------------------------------------------------
    else {
      # Convert factors through their displayed values, not their internal integer codes.
      if (is.factor(raw_data) || is.character(raw_data)) {
        x <- suppressWarnings(as.numeric(as.character(raw_data)))
      } else {
        x <- suppressWarnings(as.numeric(raw_data))
      }
      x <- x[!is.na(x) & is.finite(x)]
      
      validate(
        need(length(x) > 1, "At least 2 numeric values are required.")
      )
      
      right_closed <- (input$freq_type == "inclusive")
      
      if (is.null(input$width_type) || input$width_type == "equal") {
        w <- input$class_width
        validate(
          need(
            length(w) == 1 && is.numeric(w) && is.finite(w) && w > 0,
            "Enter a valid positive class width."
          )
        )
        
        lower <- floor(min(x) / w) * w
        upper <- ceiling(max(x) / w) * w
        if (lower == upper) upper <- lower + w
        
        breaks <- seq(lower, upper, by = w)
        validate(need(length(breaks) > 1, "Class width is too large."))
        
      } else {
        req(input$custom_breaks)
        breaks <- suppressWarnings(
          as.numeric(trimws(unlist(strsplit(input$custom_breaks, ","))))
        )
        validate(
          need(all(!is.na(breaks)), "Invalid format. Please enter valid numbers separated by commas."),
          need(length(breaks) >= 2, "At least 2 break points are required."),
          need(all(is.finite(breaks)), "Break points must be finite numbers."),
          need(all(diff(breaks) > 0), "Custom break points must be in strictly increasing order."),
          need(min(x) >= min(breaks) && max(x) <= max(breaks),
               "The custom breaks must cover the full range of the selected data.")
        )
      } # Closes width_type condition
      
      h <- hist(x, breaks = breaks, right = right_closed, include.lowest = TRUE, plot = FALSE)
      freq <- h$counts
      mids <- h$mids
      
      intervals <- character(length(freq))
      for (i in seq_along(freq)) {
        lower_i <- breaks[i]
        upper_i <- breaks[i + 1]
        if (right_closed) {
          intervals[i] <- if (i == 1) {
            paste0("[", lower_i, ", ", upper_i, "]")
          } else {
            paste0("(", lower_i, ", ", upper_i, "]")
          }
        } else {
          intervals[i] <- if (i == length(freq)) {
            paste0("[", lower_i, ", ", upper_i, "]")
          } else {
            paste0("[", lower_i, ", ", upper_i, ")")
          }
        }
      } # Closes for loop
      
      if (input$freq_display == "percent") {
        values <- round((freq / sum(freq)) * 100, 1)
        ylab_text <- "Percentage (%)"
      } else {
        values <- freq
        ylab_text <- "Frequency"
      }
      
      cum_less <- cumsum(freq)
      
    # Inside your univariate plotting reactive / renderPlot:
        
      
      if (input$graph_type %in% c("bar", "bar_horizontal"))  {
        label_texts <- if (input$freq_display == "percent") {
          paste0(values, "%")
        } else {
          as.character(values)
        }
        
        b <- barplot(values,
                     names.arg = intervals,
                     las = 1,
                     col = adjustcolor(color, alpha.f = alpha),
                     border = border_color,
                     main = plot_title,
                     ylab = ylab_text,
                     ylim = c(0, max(values) * 1.15))
        
        text(x = b, y = values, labels = label_texts, pos = 3, xpd = TRUE, cex = 0.9, font = 2)
      } # Closes graph_type == "bar"
      
      if (input$graph_type == "pie") {
        valid_idx <- values > 0
        plot_values <- values[valid_idx]
        plot_intervals <- intervals[valid_idx]
        
        validate(need(length(plot_values) > 0, "No non-zero data available for the pie chart."))
        
        pie(plot_values,
            labels = paste0(plot_intervals, " (", plot_values, 
                            ifelse(input$freq_display == "percent", "%", ""), ")"),
            col = get_pie_colors(length(plot_values), palette),
            main = paste("Grouped Pie Chart of", input$freq_variable))
      } # Closes graph_type == "pie"
      
      if (input$graph_type == "hist") {
        barplot(values,
                space = 0,
                names.arg = intervals,
                las = 1,
                col = adjustcolor(color, alpha.f = alpha),
                border = border_color,
                main = plot_title,
                xlab = input$freq_variable,
                ylab = ylab_text,
                ylim = c(0, max(values) * 1.15))
        box()
      } # Closes graph_type == "hist"
      
      if (input$graph_type == "polygon") {
        w_first <- breaks[2] - breaks[1]
        w_last <- tail(breaks, 1) - tail(breaks, 2)[1]
        poly_x <- c(mids[1] - w_first, mids, tail(mids, 1) + w_last)
        poly_y <- c(0, values, 0)
        
        plot(poly_x, poly_y,
             type = "o",
             col = color,
             pch = as.numeric(point_shape),
             lwd = lwd,
             cex = point_size,
             xlim = range(poly_x),
             ylim = c(0, max(poly_y) * 1.15),
             main = plot_title,
             xlab = "",
             ylab = ylab_text,
             xaxt = "n")
        axis(1, at = mids, labels = intervals, las = 2)
      } # Closes graph_type == "polygon"
      
      if (input$graph_type == "ogive_less") {
        x_less <- breaks
        y_less <- if (input$freq_display == "percent") c(0, round((cumsum(freq) / sum(freq)) * 100, 1)) 
        else c(0, cum_less)
        
        plot(x_less, y_less,
             type = "o",
             col = color,
             pch = as.numeric(point_shape),
             lwd = lwd,
             cex = point_size,
             main = plot_title,
             xlab = "",
             ylab = ifelse(input$freq_display == "percent", "Cumulative Percentage (%)", "Cumulative Frequency"),
             ylim = c(0, max(y_less) * 1.1))
      } # Closes graph_type == "ogive_less"
      
      if (input$graph_type == "ogive_more") {
        x_more <- breaks
        cum_more_raw <- rev(cumsum(rev(freq)))
        y_more <- if (input$freq_display == "percent") c(round((cum_more_raw / sum(freq)) * 100, 1), 0)
        else c(cum_more_raw, 0)
        
        plot(x_more, y_more,
             type = "o",
             col = color,
             pch = as.numeric(point_shape),
             lwd = lwd,
             cex = point_size,
             main = plot_title,
             xlab = "",
             ylab = ifelse(input$freq_display == "percent", "Cumulative Percentage (%)", "Cumulative Frequency"),
             ylim = c(0, max(y_more) * 1.1))
      } # Closes graph_type == "ogive_more"
      
    } # Closes numeric data `else` block
    
    
    
  } # <--- Closes draw_advanced_plot() function definition properly!
  
  
  # ----------------------------------------------------
  # 2. SERVER OBSERVERS & OUTPUTS (OUTSIDE THE FUNCTION)
  # ----------------------------------------------------
  observeEvent(input$reset_defaults, {
    # Plot Dimensions
    updateSliderInput(session, "pheight", value = 480)
    updateSliderInput(session, "pwidth", value = 350)
    
    # Colors & Shading (Global shared controls)
    updateColourInput(session, "plotColor", value = "#1f77b4")
    updateColourInput(session, "borderColor", value = "#000000")
    updateSliderInput(session, "plotAlpha", value = 0.7)
    
    # Pie Chart Palette
    updateSelectInput(session, "piePalette", selected = "rainbow")
    
    # Line & Marker Controls (Polygons & Ogives)
    updateSliderInput(session, "plotLwd", value = 2)
    updateSliderInput(session, "pointSize", value = 1.5)
    updateSelectInput(session, "pointShape", selected = 19)
    
    # If you added the custom title text input earlier:
    # updateTextInput(session, "customTitle", value = "")
  })
  # Closes observeEvent()
  
  # Render Plot Output
  output$advanced_plot <- renderPlot(
    {
      draw_advanced_plot(
        color = input$plotColor,
        alpha = input$plotAlpha,
        border_color = input$borderColor,
        lwd = input$plotLwd,
        point_size = input$pointSize,
        point_shape = input$pointShape,
        palette = input$piePalette
      )
    },
    width = function() input$pwidth,
    height = function() input$pheight,
    res = 96
  ) # Closes renderPlot()
  
  # Save Advanced Plot (PNG Download)
  
  output$download_advanced_plot <- downloadHandler(
    
    filename = function() {
      paste0(
        "plot_",
        input$graph_type, "_",
        input$freq_variable, "_",
        Sys.Date(), ".png"
      )
    },
    
    content = function(file) {
      
      # Validate required inputs
      req(
        input$show_chart,
        input$graph_type,
        input$freq_variable,
        input$pwidth,
        input$pheight,
        global_data()
      )
      
      # Open PNG graphics device
      grDevices::png(
        filename = file,
        width = as.integer(input$pwidth * 2),
        height = as.integer(input$pheight * 2),
        res = 120
      )
      
      # Always close the PNG device, even if plotting fails
      on.exit(grDevices::dev.off(), add = TRUE)
      
      # Draw the selected chart
      draw_advanced_plot(
        color = input$plotColor %||% "#1f77b4",
        alpha = input$plotAlpha %||% 0.7,
        border_color = input$borderColor %||% "#000000",
        lwd = input$plotLwd %||% 2,
        point_size = input$pointSize %||% 1.5,
        point_shape = input$pointShape %||% 19,
        palette = input$piePalette %||% "rainbow"
      )
    }
  )
  # Closes downloadHandler()  
  
  
  # ============================================================
  # BIVARIATE FREQUENCY TABLE
  # Supports categorical and numerical variables
  # ============================================================
  # Automatically update all variable selectors when data is uploaded
  # ------------------------------------------------------------------------------
  # 🔹 Helper Function for Cell Formatting (Place at the top of your server script)
  # ------------------------------------------------------------------------------
  format_cell <- function(cnt, r_tot, c_tot, g_tot, show_r, show_c, show_t) {
    if (is.na(cnt)) return("-")
    
    pcts <- character(0)
    if (isTRUE(show_r)) {
      r_pct <- if (r_tot > 0) (cnt / r_tot) * 100 else 0
      pcts <- c(pcts, sprintf("R: %.1f%%", r_pct))
    }
    if (isTRUE(show_c)) {
      c_pct <- if (c_tot > 0) (cnt / c_tot) * 100 else 0
      pcts <- c(pcts, sprintf("C: %.1f%%", c_pct))
    }
    if (isTRUE(show_t)) {
      t_pct <- if (g_tot > 0) (cnt / g_tot) * 100 else 0
      pcts <- c(pcts, sprintf("Cell: %.1f%%", t_pct))
    }
    
    if (length(pcts) > 0) {
      paste0(cnt, " (", paste(pcts, collapse = ", "), ")")
    } else {
      as.character(cnt)
    }
  }
  
  
   observeEvent(global_data(), {
    df <- global_data()
    req(df)
    
    all_vars <- names(df)
    validate(need(length(all_vars) > 0, "Uploaded dataset contains no columns."))
    
    # Update single frequency variable dropdown
    updateSelectInput(session, "freq_variable", choices = all_vars, selected = all_vars[1])
    
    # Update bivariate row and column variable dropdowns
    updateSelectInput(session, "row_var", choices = all_vars, selected = all_vars[1])
    updateSelectInput(session, "col_var", choices = all_vars, 
                      selected = if (length(all_vars) > 1) all_vars[2] else all_vars[1])
  })
  # 1. Dynamic grouping controls for the row variable
  output$row_group_ui <- renderUI({
    req(global_data(), input$row_var)
    
    df <- global_data()
    req(input$row_var %in% names(df))
    
    if (!is.numeric(df[[input$row_var]])) {
      return(NULL)
    }
    
    tagList(
      h5("Group Row Numerical Variable"),
      
      numericInput(
        "row_num_groups",
        "Number of Groups:",
        value = 5,
        min = 2,
        step = 1
      ),
      
      radioButtons(
        "row_width_type",
        "Class Width:",
        choices = c(
          "Equal Width" = "equal",
          "Unequal Width (Custom Classes)" = "unequal"
        ),
        selected = "equal",
        inline = FALSE
      ),
      
      conditionalPanel(
        condition = "input.row_width_type == 'unequal'",
        
        textInput(
          "row_custom_breaks",
          "Exclusive Class Boundaries:",
          value = "",
          placeholder = "e.g., 0, 10, 20, 30, 50"
        ),
        
        helpText(
          "Enter increasing boundaries separated by commas. ",
          "Classes are [lower, upper); the final boundary must exceed the maximum."
        )
      )
    )
  })
  
  
  # 2. Dynamic grouping controls for the column variable
  output$col_group_ui <- renderUI({
    req(global_data(), input$col_var)
    
    df <- global_data()
    req(input$col_var %in% names(df))
    
    if (!is.numeric(df[[input$col_var]])) {
      return(NULL)
    }
    
    tagList(
      h5("Group Column Numerical Variable"),
      
      numericInput(
        "col_num_groups",
        "Number of Groups:",
        value = 5,
        min = 2,
        step = 1
      ),
      
      radioButtons(
        "col_width_type",
        "Class Width:",
        choices = c(
          "Equal Width" = "equal",
          "Unequal Width (Custom Classes)" = "unequal"
        ),
        selected = "equal",
        inline = FALSE
      ),
      
      conditionalPanel(
        condition = "input.col_width_type == 'unequal'",
        
        textInput(
          "col_custom_breaks",
          "Exclusive Class Boundaries:",
          value = "",
          placeholder = "e.g., 0, 10, 20, 30, 50"
        ),
        
        helpText(
          "Enter increasing boundaries separated by commas. ",
          "Classes are [lower, upper); the final boundary must exceed the maximum."
        )
      )
    )
  })
  
  
  # ============================================================
  # 3. Helper: Convert numerical data into grouped classes
  # ============================================================
  
  # ============================================================
  # 3. Helper: Convert numerical data into grouped classes
  # ============================================================
  
  make_grouped_variable <- function(
    x,
    variable_name,
    width_type,
    n_groups,
    custom_breaks) {
    
    # Safely convert numeric-like data
    if (is.factor(x)) {
      x <- as.character(x)
    }
    
    x <- suppressWarnings(as.numeric(x))
    
    valid <- !is.na(x) & is.finite(x)
    
    validate(
      need(any(valid), paste(variable_name, "has no valid numeric data."))
    )
    
    # Use a numeric vector with NA retained for incomplete pairs
    grouped <- rep(NA_character_, length(x))
    
    xv <- x[valid]
    
    if (identical(width_type, "equal")) {
      
      validate(
        need(
          length(n_groups) == 1 &&
            is.finite(n_groups) &&
            n_groups >= 2 &&
            n_groups == as.integer(n_groups),
          "Number of groups must be an integer of at least 2."
        )
      )
      
      # Generate the requested number of equal-width intervals
      xmin <- min(xv)
      xmax <- max(xv)
      
      if (xmin == xmax) {
        breaks <- seq(
          xmin - 0.5,
          xmax + 0.5,
          length.out = as.integer(n_groups) + 1
        )
      } else {
        # FIXED: Calculate proper equal width step
        w <- (xmax - xmin) / as.integer(n_groups)
        breaks <- seq(xmin, xmax, by = w)
        
        # Ensure the maximum belongs to the last [lower, upper) class
        breaks[length(breaks)] <-
          breaks[length(breaks)] +
          max(abs(xmax), 1) * 1e-10
        
        # Protect against floating-point duplicate boundaries
        if (any(diff(breaks) <= 0)) {
          validate(
            need(
              FALSE,
              "Unable to construct distinct equal-width boundaries."
            )
          )
        }
      }
      
    } else if (identical(width_type, "unequal")) {
      
      validate(
        need(
          !is.null(custom_breaks) &&
            nzchar(trimws(custom_breaks)),
          paste("Enter custom boundaries for", variable_name, ".")
        )
      )
      
      breaks <- suppressWarnings(
        as.numeric(
          trimws(
            strsplit(custom_breaks, ",", fixed = TRUE)[[1]]
          )
        )
      )
      
      validate(
        need(
          length(breaks) >= 3,
          "Enter at least 3 boundaries to create 2 or more classes."
        ),
        need(
          all(is.finite(breaks)),
          "All custom boundaries must be finite numeric values."
        ),
        need(
          all(diff(breaks) > 0),
          "Custom boundaries must be strictly increasing."
        ),
        need(
          min(xv) >= min(breaks) &&
            max(xv) < max(breaks),
          paste(
            "Custom boundaries must cover the data range.",
            "The first boundary must be <= the minimum,",
            "and the last boundary must be > the maximum."
          )
        )
      )
      
    } else {
      validate(
        need(FALSE, "Select equal or unequal class width.")
      )
    }
    
    # Exclusive classes: [lower, upper)
    grouped_factor <- cut(
      xv,
      breaks = breaks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    )
    
    validate(
      need(
        !anyNA(grouped_factor),
        paste("Some observations could not be grouped for", variable_name, ".")
      )
    )
    
    interval_labels <- paste0(
      "[",
      format(head(breaks, -1), trim = TRUE, scientific = FALSE),
      ", ",
      format(tail(breaks, -1), trim = TRUE, scientific = FALSE),
      ")"
    )
    
    grouped[valid] <- interval_labels[grouped_factor]
    
    # Preserve interval order, including empty classes
    factor(
      grouped,
      levels = interval_labels,
      ordered = TRUE
    )
  }  
  # ============================================================
  # 4. Build the two-way frequency table
  # ============================================================
  
  twoway_table_df <- reactive({
    
    req(global_data(), input$row_var, input$col_var)
    
    df <- global_data()
    
    validate(
      need(input$row_var %in% names(df),
           "Selected row variable not found."),
      need(input$col_var %in% names(df),
           "Selected column variable not found.")
    )
    
    x <- df[[input$row_var]]
    y <- df[[input$col_var]]
    
    # Group the row variable if numeric
    if (is.numeric(x)) {
      
      req(
        input$row_width_type,
        input$row_num_groups
      )
      
      x <- make_grouped_variable(
        x = x,
        variable_name = input$row_var,
        width_type = input$row_width_type,
        n_groups = input$row_num_groups,
        custom_breaks = input$row_custom_breaks
      )
      
    } else {
      x <- as.factor(x)
    }
    
    # Group the column variable if numeric
    if (is.numeric(y)) {
      
      req(
        input$col_width_type,
        input$col_num_groups
      )
      
      y <- make_grouped_variable(
        x = y,
        variable_name = input$col_var,
        width_type = input$col_width_type,
        n_groups = input$col_num_groups,
        custom_breaks = input$col_custom_breaks
      )
      
    } else {
      y <- as.factor(y)
    }
    
    # Remove incomplete pairs
    complete <- !is.na(x) & !is.na(y)
    
    validate(
      need(any(complete), "No complete observations are available.")
    )
    
    x <- droplevels(x[complete])
    y <- droplevels(y[complete])
    
    # Create contingency table
    mat <- table(x, y)
    
    validate(
      need(
        nrow(mat) > 0 && ncol(mat) > 0,
        "Unable to create the two-way frequency table."
      )
    )
    
    r_names <- rownames(mat)
    c_names <- colnames(mat)
    
    row_sums <- rowSums(mat)
    col_sums <- colSums(mat)
    grand_total <- sum(mat)
    
    # Format individual cells
    res_mat <- matrix(
      "",
      nrow = nrow(mat),
      ncol = ncol(mat),
      dimnames = list(r_names, c_names)
    )
    
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        
        res_mat[i, j] <- format_cell(
          cnt = mat[i, j],
          r_tot = row_sums[i],
          c_tot = col_sums[j],
          g_tot = grand_total,
          show_r = input$show_row_pct,
          show_c = input$show_col_pct,
          show_t = input$show_cell_pct
        )
      }
    }
    
    res_df <- as.data.frame(
      res_mat,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    names(res_df) <- c_names
    rownames(res_df) <- r_names
    
    # Row totals
    if (isTRUE(input$show_row_total)) {
      
      row_tot_col <- vapply(
        seq_len(nrow(mat)),
        function(i) {
          format_cell(
            row_sums[i],
            row_sums[i],
            grand_total,
            grand_total,
            input$show_row_pct,
            input$show_col_pct,
            input$show_cell_pct
          )
        },
        character(1)
      )
      
      res_df[["Row Total"]] <- row_tot_col
    }
    
    # Column totals and grand total
    if (isTRUE(input$show_col_total)) {
      
      col_tot_row <- vapply(
        seq_len(ncol(mat)),
        function(j) {
          format_cell(
            col_sums[j],
            grand_total,
            col_sums[j],
            grand_total,
            input$show_row_pct,
            input$show_col_pct,
            input$show_cell_pct
          )
        },
        character(1)
      )
      
      if (isTRUE(input$show_row_total)) {
        
        grand_cell <- if (isTRUE(input$show_cell_total)) {
          format_cell(
            grand_total,
            grand_total,
            grand_total,
            grand_total,
            input$show_row_pct,
            input$show_col_pct,
            input$show_cell_pct
          )
        } else {
          "-"
        }
        
        col_tot_row <- c(col_tot_row, grand_cell)
      }
      
      res_df <- rbind(
        res_df,
        "Col Total" = col_tot_row
      )
      
    } else if (
      isTRUE(input$show_row_total) &&
      isTRUE(input$show_cell_total)
    ) {
      
      grand_row <- rep("-", ncol(res_df))
      
      grand_row[ncol(res_df)] <- format_cell(
        grand_total,
        grand_total,
        grand_total,
        grand_total,
        input$show_row_pct,
        input$show_col_pct,
        input$show_cell_pct
      )
      
      res_df <- rbind(
        res_df,
        "Grand Total" = grand_row
      )
    }
    
    # Add row-category labels
    res_df <- cbind(
      Category = rownames(res_df),
      res_df
    )
    
    names(res_df)[1] <- paste(
      input$row_var,
      "\\",
      input$col_var
    )
    
    rownames(res_df) <- NULL
    
    res_df
  })
  
  
  # ============================================================
  # 5. Display table
  # ============================================================
  
  output$twoway_table <- renderTable({
    twoway_table_df()
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  # ============================================================
  # 6. Download table
  # ============================================================
  
  output$download_table_csv <- downloadHandler(
    
    filename = function() {
      paste0(
        "twoway_table_",
        input$row_var, "_vs_",
        input$col_var, "_",
        Sys.Date(), ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        twoway_table_df(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  
  # ------------------------------------------------------------------------------
  # 🔹 REACTIVE DATA PREPARATION FOR BAR DIAGRAMS
  # ------------------------------------------------------------------------------
  plot_data <- reactive({
    req(global_data(), input$row_var, input$col_var, input$pct_type)
    
    df <- global_data()
    
    validate(
      need(input$row_var %in% names(df), "Selected row variable not found."),
      need(input$col_var %in% names(df), "Selected column variable not found.")
    )
    
    x_raw <- df[[input$row_var]]
    y_raw <- df[[input$col_var]]
    
    # Process Row Variable (Numeric Grouping or Factor)
    if (is.numeric(x_raw)) {
      req(input$row_width_type, input$row_num_groups)
      x <- make_grouped_variable(
        x = x_raw, variable_name = input$row_var,
        width_type = input$row_width_type,
        n_groups = input$row_num_groups,
        custom_breaks = input$row_custom_breaks
      )
    } else {
      x <- as.factor(x_raw)
    }
    
    # Process Column Variable (Numeric Grouping or Factor)
    if (is.numeric(y_raw)) {
      req(input$col_width_type, input$col_num_groups)
      y <- make_grouped_variable(
        x = y_raw, variable_name = input$col_var,
        width_type = input$col_width_type,
        n_groups = input$col_num_groups,
        custom_breaks = input$col_custom_breaks
      )
    } else {
      y <- as.factor(y_raw)
    }
    
    # Remove incomplete pairs
    complete <- !is.na(x) & !is.na(y)
    validate(need(any(complete), "No complete overlapping observations available for plotting."))
    
    x <- droplevels(x[complete])
    y <- droplevels(y[complete])
    
    mat <- table(x, y)
    
    if (input$pct_type == "row") {
      prop_mat <- prop.table(mat, margin = 1) * 100
      plot_df <- as.data.frame(prop_mat)
      colnames(plot_df) <- c("X_Group", "Fill_Group", "Percentage")
      x_title <- input$row_var
      fill_title <- input$col_var
      pct_label <- "Percentage (%)"
    } else {
      prop_mat <- prop.table(mat, margin = 2) * 100
      plot_df <- as.data.frame(prop_mat)
      colnames(plot_df) <- c("Fill_Group", "X_Group", "Percentage")
      x_title <- input$col_var
      fill_title <- input$row_var
      pct_label <- "Percentage (%)"
    }
    
    # Ensure X_Group respects table category order rather than alphabetical sorting
    plot_df$X_Group <- factor(plot_df$X_Group, levels = unique(plot_df$X_Group))
    plot_df$Fill_Group <- factor(plot_df$Fill_Group, levels = unique(plot_df$Fill_Group))
    
    list(df = plot_df, x_title = x_title, fill_title = fill_title, pct_label = pct_label)
  })
  
  # ------------------------------------------------------------------------------
  # 🔹 1. MULTIPLE BAR DIAGRAM (Clustered / Side-by-Side) with Custom Settings
  # ------------------------------------------------------------------------------
  mult_bar_plot_obj <- reactive({
    req(isTRUE(input$show_mult_bar))
    pdata <- plot_data()
    
    # Dynamic Palette selection
    pal <- switch(input$biv_palette,
                  "viridis" = scale_fill_viridis_d(option = "viridis"),
                  "blues"   = scale_fill_brewer(palette = "Blues"),
                  "set2"    = scale_fill_brewer(palette = "Set2"),
                  "accent"  = scale_fill_brewer(palette = "Accent"),
                  scale_fill_viridis_d())
    
    p <- ggplot(pdata$df, aes(x = X_Group, y = Percentage, fill = Fill_Group)) +
      geom_col(position = position_dodge(width = input$bar_dodge_width %||% 0.8), 
               width = input$bar_width %||% 0.7, color = "black")
    
    # Conditionally add data labels if checked
    if (isTRUE(input$show_bar_labels)) {
      p <- p + geom_text(aes(label = sprintf("%.1f%%", Percentage)),
                         position = position_dodge(width = input$bar_dodge_width %||% 0.8),
                         vjust = -0.5, size = input$bar_text_size %||% 3.5)
    }
    
    p + scale_y_continuous(limits = c(0, 115), expand = c(0, 0)) +
      pal +
      labs(
        title = if (nzchar(input$custom_plot_title)) input$custom_plot_title else paste("Multiple Bar Diagram -", pdata$pct_label),
        x = pdata$x_title, y = pdata$pct_label, fill = pdata$fill_title
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5), 
        legend.position = input$legend_pos %||% "top",
        axis.text.x = element_text(angle = 0, hjust = 1)
      )
  })
  
  output$mult_bar_plot <- renderPlot({ mult_bar_plot_obj() })
  
  output$download_mult_bar_png <- downloadHandler(
    filename = function() { paste0("multiple_bar_plot_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = mult_bar_plot_obj(), device = "png", width = 9, height = 6, dpi = 300) }
  )
  
  # ------------------------------------------------------------------------------
  # 🔹 2. COMPONENT BAR DIAGRAM (Stacked to 100%) with Custom Settings
  # ------------------------------------------------------------------------------
  comp_bar_plot_obj <- reactive({
    req(isTRUE(input$show_comp_bar))
    pdata <- plot_data()
    
    pal <- switch(input$biv_palette,
                  "viridis" = scale_fill_viridis_d(option = "viridis"),
                  "blues"   = scale_fill_brewer(palette = "Blues"),
                  "set2"    = scale_fill_brewer(palette = "Set2"),
                  "accent"  = scale_fill_brewer(palette = "Accent"),
                  scale_fill_viridis_d())
    
    p <- ggplot(pdata$df, aes(x = X_Group, y = Percentage, fill = Fill_Group)) +
      geom_col(position = "stack", width = input$comp_bar_width %||% 0.6, color = "black")
    
    if (isTRUE(input$show_comp_labels)) {
      p <- p + geom_text(aes(label = ifelse(Percentage > 4, sprintf("%.1f%%", Percentage), "")),
                         position = position_stack(vjust = 0.5),
                         size = input$comp_text_size %||% 3.5, color = "white", fontface = "bold")
    }
    
    p + scale_y_continuous(limits = c(0, 102), expand = c(0, 0)) +
      pal +
      labs(
        title = if (nzchar(input$custom_plot_title)) input$custom_plot_title else paste("Component Bar Diagram -", pdata$pct_label),
        x = pdata$x_title, y = pdata$pct_label, fill = pdata$fill_title
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5), 
        legend.position = input$legend_pos %||% "top",
        axis.text.x = element_text(angle = 0, hjust = 1)
      )
  })
  
  output$comp_bar_plot <- renderPlot({ comp_bar_plot_obj() })
  
  output$download_comp_bar_png <- downloadHandler(
    filename = function() { paste0("component_bar_plot_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = comp_bar_plot_obj(), device = "png", width = 9, height = 6, dpi = 300) }
  )  
  #-------------------------------------------------------------------
  

  # Summary Statistics -----------------------------------------------------------
  
  # Summary statistics helper function -----------------------------------------
  # --- Combined Summary Table for Selected Variable ---
  # ==============================================================================
  # 🔹 STATS FOR SELECTED VARIABLE (DATASET)
  # ==============================================================================
  
  # 1. Raw Stats Helper Function
  get_raw_stats <- function(x) {
    n <- length(x)
    mean_x <- mean(x)
    median_x <- median(x)
    sd_x <- sd(x)
    var_x <- var(x)
    min_x <- min(x)
    max_x <- max(x)
    range_x <- max_x - min_x
    
    q1 <- quantile(x, 0.25, names = FALSE)
    q3 <- quantile(x, 0.75, names = FALSE)
    iqr_x <- IQR(x)
    
    mode_x <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
    
    skew <- if (sd_x > 0 && n > 1) sum((x - mean_x)^3) / ((n - 1) * sd_x^3) else NA
    kurt <- if (sd_x > 0 && n > 1) sum((x - mean_x)^4) / ((n - 1) * sd_x^4) else NA
    cv <- if (mean_x != 0) (sd_x / mean_x) * 100 else NA
    
    list(
      n = n, mean = mean_x, median = median_x, mode = mode_x,
      sd = sd_x, var = var_x, range = range_x, min = min_x, max = max_x,
      q1 = q1, q3 = q3, iqr = iqr_x,
      skew = skew, kurt = kurt, cv = cv
    )
  }
  
  # 2. Update select input choices dynamically
  observe({
    req(global_data())
    updateSelectInput(session, "desc_var", choices = names(global_data()))
  })
  
  # 3. Reactive block that processes the selected variable (MUST BE BEFORE tbl_combined)
  parsed_var_data <- reactive({
    req(global_data(), input$desc_var)
    req(input$desc_var %in% names(global_data()))
    
    x <- suppressWarnings(as.numeric(global_data()[[input$desc_var]]))
    x <- na.omit(x)
    
    validate(
      need(length(x) > 0, "Selected variable must contain numeric data")
    )
    get_raw_stats(x)
  })
  
  # 4. Combined Summary Table Reactive
  tbl_combined <- reactive({
    s <- parsed_var_data() # This now safely finds parsed_var_data()
    
    data.frame(
      
      Statistic = c(
        "Count (N)", "Mean", "Median", "Mode",
        "Standard Deviation", "Variance", "Range", "Minimum", "Maximum",
        "Q1 (25th Percentile)", "Median (Q2)", "Q3 (75th Percentile)", "Interquartile Range (IQR)",
        "Skewness", "Kurtosis",
        "Coefficient of Variation (CV)"
      ),
      Value = c(
        as.character(s$n), 
        sprintf("%.3f", s$mean), 
        sprintf("%.3f", s$median), 
        sprintf("%.3f", s$mode),
        
        sprintf("%.3f", s$sd), 
        sprintf("%.3f", s$var), 
        sprintf("%.3f", s$range), 
        sprintf("%.3f", s$min), 
        sprintf("%.3f", s$max),
        
        sprintf("%.3f", s$q1), 
        sprintf("%.3f", s$median), 
        sprintf("%.3f", s$q3), 
        sprintf("%.3f", s$iqr),
        
        if (is.na(s$skew)) "-" else sprintf("%.3f", s$skew),
        if (is.na(s$kurt)) "-" else sprintf("%.3f", s$kurt),
        
        if (is.na(s$cv)) "-" else sprintf("%.2f%%", s$cv)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # 5. Render outputs
  output$desc_combined <- renderTable({ tbl_combined() }, striped = TRUE, hover = TRUE)
  
  output$dl_desc_combined <- downloadHandler(
    filename = function() { paste0("comprehensive_stats_", input$desc_var, ".csv") },
    content = function(file) { write.csv(tbl_combined(), file, row.names = FALSE) }
  )
  
  # 6. Word Document Download Handler
  # Word Document Download Handler for Comprehensive Variable Statistics
  output$dl_desc_word <- downloadHandler(
    filename = function() {
      paste0("Statistical_Summary_", input$desc_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(tbl_combined())
      
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Descriptive Statistics Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Selected Variable:", input$desc_var))
      doc <- officer::body_add_par(doc, paste("Date Generated:", Sys.Date()))
      doc <- officer::body_add_par(doc, "") 
      
      doc <- officer::body_add_par(doc, paste("Summary Statistics for", input$desc_var), style = "heading 2")
      
      ft <- flextable::flextable(tbl_combined())
      ft <- flextable::theme_booktabs(ft)
      ft <- flextable::fontsize(ft, size = 9, part = "body")
      ft <- flextable::fontsize(ft, size = 10, part = "header")
      ft <- flextable::bold(ft, part = "header")
      ft <- flextable::autofit(ft)
      
      # Corrected function call (using flextable namespace instead of officer)
      doc <- flextable::body_add_flextable(doc, ft)
      
      print(doc, target = file)
    }
  )
  
  #-----------------------------------------------------------
  
  # ==============================================================================
  # 🔹 GROUP-WISE DESCRIPTIVE STATISTICS LOGIC
  # ==============================================================================
  
  # Update Category Variable dropdown options based on global_data()
  observe({
    req(global_data())
    
    updateSelectInput(session, "desc_var1", choices = names(global_data()))
    updateSelectInput(session, "desc_group_var", choices = names(global_data()))
  })
  
  # Compute raw stats for each category within the selected grouping variable
  group_parsed_data <- reactive({
    req(global_data(), input$desc_var1, input$desc_group_var)
    req(input$desc_var1 %in% names(global_data()), input$desc_group_var %in% names(global_data()))
    
    validate(
      need(input$desc_var1 != input$desc_group_var, "Numerical and Categorical variables must be different.")
    )
    
    df <- global_data()
    y  <- suppressWarnings(as.numeric(df[[input$desc_var1]]))
    grp <- factor(df[[input$desc_group_var]])
    
    clean_df <- na.omit(data.frame(y = y, Group = grp))
    
    validate(
      need(nrow(clean_df) > 0, "No valid complete observations found for selected variables.")
    )
    
    # Split response vector by group and run get_raw_stats on each subset
    split_data <- split(clean_df$y, clean_df$Group)
    
    # Apply get_raw_stats to each category
    stats_list <- lapply(split_data, get_raw_stats)
    
    stats_list
  })
  
  # Generic helper function to generate formatted group comparison tables
  format_grouped_table <- function(stats_list, row_mapping) {
    groups <- names(stats_list)
    out_df <- data.frame(Statistic = names(row_mapping), stringsAsFactors = FALSE)
    
    for (grp in groups) {
      s <- stats_list[[grp]]
      col_vals <- sapply(names(row_mapping), function(label) {
        expr <- row_mapping[[label]]
        val <- eval(expr, envir = s)
        
        if (is.na(val)) {
          return("-")
        } else if (is.numeric(val)) {
          if (grepl("Count", label)) {
            return(as.character(val))
          } else if (grepl("CV", label)) {
            return(sprintf("%.2f%%", val))
          } else {
            return(sprintf("%.3f", val))
          }
        } else {
          return(as.character(val))
        }
      })
      out_df[[grp]] <- col_vals
    }
    
    out_df
  }
  
  # --- 1. Grouped Central Tendencies ---
  # --- Combined Grouped Summary Table ---
  tbl_group_combined <- reactive({
    stats_list <- group_parsed_data()
    groups <- names(stats_list)
    
    categories <- c(
      rep("Central Tendency", 4),
      rep("Dispersion", 5),
      rep("Quartiles & Spread", 4),
      rep("Distribution Shape", 2),
      rep("Relative Measure", 1)
    )
    
    statistics <- c(
      "Count (N)", "Mean", "Median", "Mode",
      "Standard Deviation", "Variance", "Range", "Minimum", "Maximum",
      "Q1 (25th Percentile)", "Median (Q2)", "Q3 (75th Percentile)", "Interquartile Range (IQR)",
      "Skewness", "Kurtosis",
      "Coefficient of Variation (CV)"
    )
    
    expressions <- list(
      quote(n), quote(mean), quote(median), quote(mode),
      quote(sd), quote(var), quote(range), quote(min), quote(max),
      quote(q1), quote(median), quote(q3), quote(iqr),
      quote(skew), quote(kurt), quote(cv)
    )
    
    out_df <- data.frame(
      Statistic = statistics,
      stringsAsFactors = FALSE
    )
    
    for (grp in groups) {
      s <- stats_list[[grp]]
      col_vals <- sapply(seq_along(expressions), function(i) {
        expr <- expressions[[i]]
        label <- statistics[i]
        val <- eval(expr, envir = s)
        
        if (is.na(val)) {
          return("-")
        } else if (is.numeric(val)) {
          if (grepl("Count", label)) {
            return(as.character(val))
          } else if (grepl("CV", label)) {
            return(sprintf("%.2f%%", val))
          } else {
            return(sprintf("%.3f", val))
          }
        } else {
          return(as.character(val))
        }
      })
      out_df[[grp]] <- col_vals
    }
    
    out_df
  })
  
  # Render the single combined table
  output$desc_group_combined <- renderTable({ tbl_group_combined() }, striped = TRUE, hover = TRUE)
  
  # CSV download for the combined grouped table
  output$dl_desc_group_combined <- downloadHandler(
    filename = function() { 
      paste0("grouped_comprehensive_stats_", input$desc_var1, "_by_", input$desc_group_var, ".csv") 
    },
    content = function(file) { 
      write.csv(tbl_group_combined(), file, row.names = FALSE) 
    }
  )
  
  # Word Document Download Handler for Combined Grouped Statistics
  output$dl_desc_group_word <- downloadHandler(
    filename = function() {
      paste0("Grouped_Statistical_Report_", input$desc_var1, "_by_", input$desc_group_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(tbl_group_combined())
      
      doc <- officer::read_docx()
      
      # Document Metadata & Headings
      doc <- officer::body_add_par(doc, "Grouped Descriptive Statistics Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Numerical Variable:", input$desc_var1))
      doc <- officer::body_add_par(doc, paste("Grouping Variable:", input$desc_group_var))
      doc <- officer::body_add_par(doc, paste("Date Generated:", Sys.Date()))
      doc <- officer::body_add_par(doc, "") 
      
      doc <- officer::body_add_par(doc, "Grouped Summary Table", style = "heading 2")
      
      # Flextable formatting (using smaller body font size since wide tables have more columns)
      ft <- flextable::flextable(tbl_group_combined())
      ft <- flextable::theme_booktabs(ft)
      ft <- flextable::fontsize(ft, size = 8, part = "body")
      ft <- flextable::fontsize(ft, size = 9, part = "header")
      ft <- flextable::bold(ft, part = "header")
      ft <- flextable::autofit(ft)
      
      doc <- flextable::body_add_flextable(doc, ft)
      
      print(doc, target = file)
    }
  )
  
  
  # ==============================================================================
  # 🔹 2. STATS FOR ENTERED DATA
  # ==============================================================================
  compute_stats_df <- function(x) {
    n <- length(x)
    mean_x <- mean(x)
    median_x <- median(x)
    sd_x <- sd(x)
    var_x <- var(x)
    min_x <- min(x)
    max_x <- max(x)
    range_x <- max_x - min_x
    
    q1 <- quantile(x, 0.25, names = FALSE)
    q3 <- quantile(x, 0.75, names = FALSE)
    iqr_x <- IQR(x)
    
    # Mode calculation
    tbl <- table(x)
    mode_x <- as.numeric(names(sort(tbl, decreasing = TRUE)[1]))
    
    # Skewness & Kurtosis
    skew <- if (sd_x > 0 && n > 1) sum((x - mean_x)^3) / ((n - 1) * sd_x^3) else NA
    kurt <- if (sd_x > 0 && n > 1) sum((x - mean_x)^4) / ((n - 1) * sd_x^4) else NA
    
    # Coefficient of Variation
    cv <- if (mean_x != 0) (sd_x / mean_x) * 100 else NA
    
    data.frame(
      Statistic = c(
        "Count (N)", "Mean", "Median", "Mode",
        "Standard Deviation", "Variance", "Range", "Minimum", "Maximum",
        "Q1 (25th Percentile)", "Median (Q2)", "Q3 (75th Percentile)", "Interquartile Range (IQR)",
        "Skewness", "Kurtosis",
        "Coefficient of Variation (CV %)"
      ),
      Value = c(
        as.character(n),
        sprintf("%.3f", mean_x),
        sprintf("%.3f", median_x),
        sprintf("%.3f", mode_x),
        sprintf("%.3f", sd_x),
        sprintf("%.3f", var_x),
        sprintf("%.3f", range_x),
        sprintf("%.3f", min_x),
        sprintf("%.3f", max_x),
        sprintf("%.3f", q1),
        sprintf("%.3f", median_x),
        sprintf("%.3f", q3),
        sprintf("%.3f", iqr_x),
        if (is.na(skew)) "-" else sprintf("%.3f", skew),
        if (is.na(kurt)) "-" else sprintf("%.3f", kurt),
        if (is.na(cv)) "-" else sprintf("%.2f%%", cv)
      ),
      stringsAsFactors = FALSE
    )
  }
  desc_entered_df <- reactive({
    req(input$data)
    
    x <- as.numeric(trimws(unlist(strsplit(input$data, ","))))
    x <- na.omit(x)
    
    validate(
      need(length(x) > 0, "Enter valid numeric data"),
      need(length(x) > 1, "At least 2 values required")
    )
    
    df <- compute_stats_df(x)
    
    # Calculate & append CV Interpretation row
    mean_x <- mean(x)
    sd_x <- sd(x)
    
    validate(
      need(mean_x != 0, "CV cannot be calculated when mean = 0")
    )
    
    cv <- (sd_x / mean_x) * 100
    interp <- if (cv < 20) {
      "Low variability (consistent data)"
    } else if (cv < 40) {
      "Moderate variability"
    } else {
      "High variability (less consistency)"
    }
    
    df <- rbind(df, data.frame(
     # Category = "Interpretation",
      Statistic = "Variability Assessment",
      Value = interp,
      stringsAsFactors = FALSE
    ))
    
    df
  })
  
  # Render Table
  output$desc_entered_table <- renderTable({
    desc_entered_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Download Handler (CSV)
  output$download_desc_entered <- downloadHandler(
    filename = function() {
      paste0("descriptive_stats_entered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(desc_entered_df(), file, row.names = FALSE)
    }
  )
  
  # =====================================================
  # 🔹 normility test (UPLOAD)
  # =====================================================
  # 📂 Read uploaded file
 
  # ==============================================================================
  # 🔹 NORMALITY TESTING MODULE
  # ==============================================================================
  # Make sure required packages are loaded
  
  
  
  # ------------------------------------------------------------------------------
  # 🔹 1. DYNAMIC DROPDOWN UPDATE
  # ------------------------------------------------------------------------------
  observeEvent(global_data(), {
    req(global_data())
    
    df <- as.data.frame(global_data())
    
    # Filter only columns that contain numeric data
    num_cols <- names(df)[sapply(df, function(col) {
      is.numeric(col) || !all(is.na(suppressWarnings(as.numeric(col))))
    })]
    
    # Inside your observeEvent:
    if (length(num_cols) == 0) {
      showNotification("Uploaded dataset contains no numeric variables", type = "error")
      return() # This stops the observeEvent from running further
    }
    
    updateSelectInput(
      session,
      inputId = "norm_var",
      choices = num_cols,
      selected = num_cols[1]
    )
  })
  
  # ------------------------------------------------------------------------------
  # 🔹 2. EXTRACT DATA ON ANALYZE CLICK
  # ------------------------------------------------------------------------------
  data_values <- eventReactive(input$analyze, {
    req(global_data(), input$norm_var)
    
    df <- as.data.frame(global_data())
    req(input$norm_var %in% names(df))
    
    x <- suppressWarnings(as.numeric(df[[input$norm_var]]))
    x <- na.omit(x)
    
    validate(
      need(length(x) >= 3, "At least 3 numeric observations are required for normality tests.")
    )
    
    x
  })
  
  # ------------------------------------------------------------------------------
  # 🔹 3. PLOTS & DOWNLOAD HANDLERS
  # ------------------------------------------------------------------------------
  draw_hist_plot <- function() {
    req(data_values())
    x <- data_values()
    
    hist(x, probability = TRUE, col = "lightblue", border = "white",
         main = paste("Histogram with Normal Curve:", input$norm_var), 
         xlab = input$norm_var)
    
    if (sd(x) > 0) {
      curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "red", lwd = 2, add = TRUE)
    }
  }
  
  draw_qq_plot <- function() {
    req(data_values())
    x <- data_values()
    
    qqnorm(x, main = paste("Normal Q-Q Plot:", input$norm_var), pch = 19, col = "steelblue")
    qqline(x, col = "red", lwd = 2)
  }
  
  output$histPlot <- renderPlot({ draw_hist_plot() })
  output$qqPlot <- renderPlot({ draw_qq_plot() })
  
  
  # ------------------------------------------------------------------------------
  # 🔹 4. NORMALITY TESTS TABLE & CSV DOWNLOAD
  # ------------------------------------------------------------------------------
  normality_tests_df <- reactive({
    req(data_values())
    x <- data_values()
    n <- length(x)
    
    # Shapiro-Wilk Test (Valid N: 3 to 5000)
    sw_res  <- if (n >= 3 && n <= 5000) tryCatch(shapiro.test(x), error = function(e) NULL) else NULL
    sw_stat <- if (!is.null(sw_res)) sprintf("%.4f", sw_res$statistic) else "N/A (N > 5000)"
    sw_p    <- if (!is.null(sw_res)) sw_res$p.value else NA
    
    
    
    # Replace the K-S test calculation with Lilliefors correction:
    ks_res  <- tryCatch(lillie.test(x), error = function(e) NULL)
    ks_stat <- if (!is.null(ks_res)) sprintf("%.4f", ks_res$statistic) else "N/A"
    ks_p    <- if (!is.null(ks_res)) ks_res$p.value else NA
    # Anderson-Darling Test
    ad_res  <- tryCatch(ad.test(x), error = function(e) NULL)
    ad_stat <- if (!is.null(ad_res)) sprintf("%.4f", ad_res$statistic) else "N/A"
    ad_p    <- if (!is.null(ad_res)) ad_res$p.value else NA
    
    # Jarque-Bera Test
    jb_res  <- tryCatch(jarque.test(x), error = function(e) NULL)
    jb_stat <- if (!is.null(jb_res)) sprintf("%.4f", jb_res$statistic) else "N/A"
    jb_p    <- if (!is.null(jb_res)) jb_res$p.value else NA
    
    p_vals <- c(sw_p, ks_p, ad_p, jb_p)
    
    data.frame(
      `Test Name` = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Anderson-Darling", "Jarque-Bera"),
      `Test Statistic` = c(sw_stat, ks_stat, ad_stat, jb_stat),
      `p-value` = sapply(p_vals, function(p) if (is.na(p)) "N/A" else sprintf("%.4f", p)),
      `Conclusion (alpha = 0.05)` = sapply(p_vals, function(p) {
        if (is.na(p)) return("N/A")
        if (p > 0.05) "Normal (Fail to reject H0)" else "Non-Normal (Reject H0)"
      }),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  output$normality_tests_table <- renderTable({
    normality_tests_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------------------------
  # 🔹 5. SHAPE STATISTICS TABLE & CSV DOWNLOAD
  # ------------------------------------------------------------------------------
  shape_stats_df <- reactive({
    req(data_values())
    x <- data_values()
    
    skew_val <- moments::skewness(x)
    kurt_val <- moments::kurtosis(x)
    
    skew_interp <- if (abs(skew_val) < 0.5) {
      "Symmetric"
    } else if (skew_val >= 0.5) {
      "Positively Skewed (Right-tailed)"
    } else {
      "Negatively Skewed (Left-tailed)"
    }
    
    kurt_interp <- if (abs(kurt_val - 3) < 0.2) {
      "Mesokurtic (Normal-like peak)"
    } else if (kurt_val > 3) {
      "Leptokurtic (Heavy tails, higher peak)"
    } else {
      "Platykurtic (Light tails, flatter peak)"
    }
    
    data.frame(
      `Shape Metric` = c("Skewness", "Kurtosis"),
      `Calculated Value` = sprintf("%.4f", c(skew_val, kurt_val)),
      `Interpretation` = c(skew_interp, kurt_interp),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  output$shape_stats_table <- renderTable({
    shape_stats_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dl_normality_report_word <- downloadHandler(
    filename = function() {
      paste0("Normality_Analysis_Report_", input$norm_var, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Ensure all required reactives are available before generating
      req(input$norm_var)
      
      doc <- officer::read_docx()
      
      # 1. Document Title & Metadata
      doc <- officer::body_add_par(doc, "Normality Analysis Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Analyzed Variable:", input$norm_var))
      doc <- officer::body_add_par(doc, paste("Date Generated:", Sys.Date()))
      doc <- officer::body_add_par(doc, "") # Spacing
      
      # 2. Shape Statistics Section
      if (exists("shape_stats_df") && !is.null(shape_stats_df())) {
        doc <- officer::body_add_par(doc, "Shape & Distribution Statistics", style = "heading 2")
        ft_shape <- flextable::flextable(shape_stats_df())
        ft_shape <- flextable::theme_booktabs(ft_shape)
        ft_shape <- flextable::fontsize(ft_shape, size = 9, part = "body")
        ft_shape <- flextable::fontsize(ft_shape, size = 10, part = "header")
        ft_shape <- flextable::bold(ft_shape, part = "header")
        ft_shape <- flextable::autofit(ft_shape)
        doc <- flextable::body_add_flextable(doc, ft_shape)
        doc <- officer::body_add_par(doc, "") # Spacing
      }
      
      # 3. Normality Tests Section
      if (exists("normality_tests_df") && !is.null(normality_tests_df())) {
        doc <- officer::body_add_par(doc, "Formal Normality Tests", style = "heading 2")
        ft_tests <- flextable::flextable(normality_tests_df())
        ft_tests <- flextable::theme_booktabs(ft_tests)
        ft_tests <- flextable::fontsize(ft_tests, size = 9, part = "body")
        ft_tests <- flextable::fontsize(ft_tests, size = 10, part = "header")
        ft_tests <- flextable::bold(ft_tests, part = "header")
        ft_tests <- flextable::autofit(ft_tests)
        doc <- flextable::body_add_flextable(doc, ft_tests)
        doc <- officer::body_add_par(doc, "") # Spacing
      }
      
      # 4. Save Plots to Temporary Files & Embed into Word
      # Temporary file for Histogram
      hist_path <- tempfile(fileext = ".png")
      png(hist_path, width = 800, height = 600, res = 120)
      if (exists("draw_hist_plot")) { draw_hist_plot() }
      dev.off()
      
      doc <- officer::body_add_par(doc, "Histogram & Density Plot", style = "heading 2")
      doc <- officer::body_add_img(doc, src = hist_path, width = 5.5, height = 4.125)
      doc <- officer::body_add_par(doc, "") # Spacing
      
      # Temporary file for Q-Q Plot
      qq_path <- tempfile(fileext = ".png")
      png(qq_path, width = 800, height = 600, res = 120)
      if (exists("draw_qq_plot")) { draw_qq_plot() }
      dev.off()
      
      doc <- officer::body_add_par(doc, "Normal Q-Q Plot", style = "heading 2")
      doc <- officer::body_add_img(doc, src = qq_path, width = 5.5, height = 4.125)
      
      # Save target document
      print(doc, target = file)
    }
  )  
  # ------------------------------------------------------------------------------
  # 🔹 6. TEXT INTERPRETATION & CONSENSUS
  # ------------------------------------------------------------------------------
  
  output$final_decision <- renderText({
    req(data_values())
    df <- normality_tests_df()
    
    valid_conclusions <- df$`Conclusion (alpha = 0.05)`[df$`Conclusion (alpha = 0.05)` != "N/A"]
    normal_count <- sum(grepl("^Normal", valid_conclusions))
    total_valid <- length(valid_conclusions)
    
    if (total_valid > 0 && (normal_count / total_valid) >= 0.5) {
      "Majority of tests indicate a NORMAL distribution -> Use Parametric methods."
    } else {
      "Majority of tests indicate a NON-NORMAL distribution -> Use Non-Parametric methods."
    }
  })
  # =====================================================
  # 🔹 SINGLE MEAN TEST (UPLOAD)
  # =====================================================
  # 1. Dynamic Variable Dropdown
  observeEvent(global_data(), {
    req(global_data())
    
    df <- as.data.frame(global_data())
    
    num_cols <- names(df)[sapply(df, function(x) {
      is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(x))))
    })]
    
    if (length(num_cols) == 0) {
      showNotification("Uploaded dataset contains no numeric variables", type = "error")
      return() 
    }
    
    updateSelectInput(
      session,
      inputId = "single_mean_var",
      choices = num_cols,
      selected = num_cols[1]
    )
  })
  
  # 2. Reactive Data Extraction
  sample_data1 <- reactive({
    req(global_data(), input$single_mean_var)
    
    df <- as.data.frame(global_data())
    req(input$single_mean_var %in% names(df))
    
    x <- suppressWarnings(as.numeric(df[[input$single_mean_var]]))
    x <- na.omit(x)
    
    validate(
      need(length(x) >= 3, "Selected variable must contain at least 3 numeric observations."),
      need(sd(x) > 0, "Selected variable has zero variance (all values are identical). Cannot perform tests.")
    )
    x
  })
  
  # 3. Summary Statistics Data Frame
  summary1_df <- reactive({
    x <- sample_data1()
    n <- length(x)
    m <- mean(x)
    md <- median(x)
    s <- sd(x)
    se <- s / sqrt(n)
    
    data.frame(
      Statistic = c(
        "Sample Size (N)", 
        "Sample Mean",
        "Sample Median",
        "Standard Deviation (SD)", 
        "Standard Error (SE)", 
        "Minimum", 
        "Maximum"
      ),
      Value = c(
        as.character(n), 
        sprintf("%.4f", m),
        sprintf("%.4f", md),
        sprintf("%.4f", s), 
        sprintf("%.4f", se), 
        sprintf("%.4f", min(x)), 
        sprintf("%.4f", max(x))
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$summary1_table <- renderTable({
    summary1_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 4. Test Engine (Conditional on Sample Size & Normality)
  
  # =====================================================
  # 4. TEST ENGINE: NORMALITY-BASED TEST SELECTION
  # =====================================================
  
  test_engine_reactive <- reactive({
    
    x <- sample_data1()
    n <- length(x)
    
    mu_val <- as.numeric(input$mu1)
    
    validate(
      need(
        length(mu_val) == 1 && !is.na(mu_val) && is.finite(mu_val),
        "Please enter a valid population mean (μ₀)."
      ),
      need(
        !is.null(input$conf1) &&
          length(input$conf1) == 1 &&
          is.finite(input$conf1) &&
          input$conf1 > 0 &&
          input$conf1 < 1,
        "Confidence level must be between 0 and 1."
      )
    )
    
    # ---------------------------------------------------
    # STEP 1: Test normality for all sample sizes
    # ---------------------------------------------------
    
    # Shapiro-Wilk test requires 3 to 5000 observations.
    # For larger samples, normality testing needs a
    # different approach.
    
    if (n <= 5000) {
      
      norm_test <- tryCatch(
        shapiro.test(x),
        error = function(e) NULL
      )
      
      validate(
        need(
          !is.null(norm_test),
          "Normality testing failed. Please check the data."
        )
      )
      
      norm_p_val <- norm_test$p.value
      is_normal <- norm_p_val >= 0.05
      
    } else {
      
      # Do not silently assume normality when Shapiro-Wilk
      # cannot be applied.
      validate(
        need(
          FALSE,
          paste(
            "Shapiro-Wilk normality testing supports",
            "at most 5000 observations.",
            "Please use a suitable normality assessment",
            "for this larger dataset."
          )
        )
      )
    }
    
    # ---------------------------------------------------
    # STEP 2: If normal, perform only the t-test
    # ---------------------------------------------------
    
    if (is_normal) {
      
      t_res <- t.test(
        x,
        alternative = input$tail1,
        mu = mu_val,
        conf.level = input$conf1
      )
      
      return(
        list(
          mode = "single",
          test_type = "One-Sample Student's t-Test",
          norm_p = norm_p_val,
          is_normal = TRUE,
          res = t_res,
          n = n,
          mu = mu_val
        )
      )
    }
    
    # ---------------------------------------------------
    # STEP 3: If non-normal, perform BOTH tests
    # ---------------------------------------------------
    
    # Parametric test
    t_res <- t.test(
      x,
      alternative = input$tail1,
      mu = mu_val,
      conf.level = input$conf1
    )
    
    # Non-parametric test
    w_res <- tryCatch({
      
      wilcox.test(
        x,
        mu = mu_val,
        alternative = input$tail1,
        exact = FALSE,
        conf.int = TRUE,
        conf.level = input$conf1
      )
      
    }, error = function(e) {
      
      wilcox.test(
        x,
        mu = mu_val,
        alternative = input$tail1,
        exact = FALSE,
        conf.int = FALSE
      )
    })
    
    # ---------------------------------------------------
    # STEP 4: Return both test results
    # ---------------------------------------------------
    
    list(
      mode = "both",
      test_type = paste(
        "One-Sample Student's t-Test",
        "& Wilcoxon Signed-Rank Test"
      ),
      norm_p = norm_p_val,
      is_normal = FALSE,
      res_parametric = t_res,
      res_nonparametric = w_res,
      n = n,
      mu = mu_val
    )
    
  })
  
  # 5. Results Data Frame Builder
  result1_df <- reactive({
    eng <- test_engine_reactive()
    alpha <- 1 - input$conf1
    
    alt_txt <- switch(
      input$tail1,
      "two.sided" = paste("True parameter ≠", eng$mu),
      "less"      = paste("True parameter <", eng$mu),
      "greater"   = paste("True parameter >", eng$mu)
    )
    
    norm_txt <- if (is.na(eng$norm_p)) {
      " "
    } else {
      sprintf("%.4f (%s)", eng$norm_p, if(eng$is_normal) "Normal" else "Non-Normal (Normality Failed)")
    }
    
    if (eng$mode == "single") {
      t_res <- eng$res
      p_val <- t_res$p.value
      decision_txt <- if (p_val < alpha) "Significant" else "Not Significant"
      
      data.frame(
        Metric = c(
          "Variable Analyzed", "Selected Test", "Normality Test (Shapiro-Wilk p-value)",
          "Hypothesized Value (μ₀)", "Alternative Hypothesis (H₁)", "Test Statistic",
          "p-value", "Confidence Level", "CI Lower Bound", "CI Upper Bound",
          paste0("Decision (alpha = ", sprintf("%.2f", alpha), ")")
        ),
        Value = c(
          input$single_mean_var, eng$test_type, norm_txt,
          as.character(eng$mu), alt_txt,
          sprintf("%.4f", t_res$statistic),
          if (p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val),
          sprintf("%.2f%%", input$conf1 * 100),
          sprintf("%.4f", t_res$conf.int[1]), sprintf("%.4f", t_res$conf.int[2]),
          decision_txt
        ),
        stringsAsFactors = FALSE
      )
    } else {
      # Both Parametric and Non-Parametric results displayed when normality fails for n < 50
      tp <- eng$res_parametric
      wp <- eng$res_nonparametric
      
      p_val_t <- tp$p.value
      p_val_w <- if(!is.null(wp$p.value)) wp$p.value else NA
      
      dec_t <- if (p_val_t < alpha) "Significant" else "Not significant"
      dec_w <- if (!is.na(p_val_w) && p_val_w < alpha) "Significant" else "Not significant"
      
      data.frame(
        Metric = c(
          "Variable Analyzed", "Normality Status", "Hypothesized Value (μ₀)", "Alternative Hypothesis (H₁)",
          "--- Parametric Method ---", "t-Test", "p-value", "CI Lower", "CI Upper", "Decision",
          "--- Non-Parametric Method ---", "Wilcoxon signed-rank test", "p-value", " CI Lower", "CI Upper", "Decision"
        ),
        Value = c(
          input$single_mean_var, norm_txt, as.character(eng$mu), alt_txt,
          "", sprintf("%.4f", tp$statistic), if (p_val_t < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_t), sprintf("%.4f", tp$conf.int[1]), sprintf("%.4f", tp$conf.int[2]), dec_t,
          "", if(!is.null(wp$statistic)) sprintf("%.4f", wp$statistic) else "N/A", if(is.na(p_val_w)) "N/A" else if (p_val_w < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_w),
          if(!is.null(wp$conf.int)) sprintf("%.4f", wp$conf.int[1]) else "N/A", if(!is.null(wp$conf.int)) sprintf("%.4f", wp$conf.int[2]) else "N/A", dec_w
        ),
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$result1_table <- renderTable({
    result1_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dl_singlet_report <- downloadHandler(
    filename = function() {
      paste0("mean_test_report_", input$single_mean_var, "_", Sys.Date(), ".docx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      sum_df <- summary1_df()
      res_df <- result1_df()
      
      doc <- read_docx() %>%
        body_add_par("Statistical Analysis Report: Single Mean Test", style = "heading 1") %>%
        body_add_par(paste0("Generated on: ", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. Descriptive Summary Statistics", style = "heading 2")
      
      # Fixed: passed 'doc' to body_add_par
      for (i in 1:nrow(sum_df)) {
        doc <- body_add_par(doc, paste0("- ", sum_df$Statistic[i], ": ", sum_df$Value[i]), style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. Hypothesis Test Results", style = "heading 2")
      
      # Fixed: passed 'doc' to body_add_par
      for (i in 1:nrow(res_df)) {
        doc <- body_add_par(doc, paste0(res_df$Metric[i], ": ", res_df$Value[i]), style = "Normal")
      }
      
      print(doc, target = file)
    }
  )
  
  # ==============================================================================
  # 🔹 SINGLE MEAN t-TEST (ENTER DATA TAB)
  # ==============================================================================
  
  # 1. Manual Data Extraction Reactive
  sample_data_manual <- reactive({
    req(input$data)
    
    # Split by commas, spaces, tabs, or newlines
    raw_vals <- unlist(strsplit(input$data, "[\n\r,;\\s]+", perl = TRUE))
    raw_vals <- raw_vals[raw_vals != ""]
    
    num_vals <- suppressWarnings(as.numeric(raw_vals))
    num_vals <- na.omit(num_vals)
    
    validate(
      need(length(num_vals) >= 3, "Please enter at least 3 valid numeric values for normality testing."),
      need(sd(num_vals) > 0, "Entered values have zero variance (all values are identical). Cannot perform tests.")
    )
    
    num_vals
  })
  
  # 2. Summary Statistics Data Frame (Guaranteed Data Frame Return)
  summary_manual_df1 <- reactive({
    x <- sample_data_manual()
    n <- length(x)
    m <- mean(x)
    md <- median(x)
    s <- sd(x)
    se <- s / sqrt(n)
    
    as.data.frame(
      list(
        Statistic = c(
          "Sample Size (N)", 
          "Sample Mean",
          "Sample Median",
          "Standard Deviation (SD)", 
          "Standard Error (SE)", 
          "Minimum", 
          "Maximum"
        ),
        Value = c(
          as.character(n), 
          sprintf("%.4f", m),
          sprintf("%.4f", md),
          sprintf("%.4f", s), 
          sprintf("%.4f", se), 
          sprintf("%.4f", min(x)), 
          sprintf("%.4f", max(x))
        )
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$summary_tablesingle <- renderTable({
    summary_manual_df1()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 3. Test Engine (Conditional on Sample Size & Normality)
  test_engine_manual_reactive <- reactive({
    x <- sample_data_manual()
    n <- length(x)
    mu_val <- as.numeric(input$mu)
    
    validate(
      need(!is.null(mu_val) && !is.na(mu_val), "Please enter a valid population mean (μ₀)"),
      need(!is.null(input$conf) && input$conf > 0 && input$conf < 1, "Confidence level must be between 0 and 1")
    )
    
    if (n >= 50) {
      t_res <- t.test(x, alternative = input$tail, mu = mu_val, conf.level = input$conf)
      list(
        mode = "single",
        test_type = "Student's t-Test",
        norm_p = NA,
        is_normal = TRUE,
        res = t_res,
        n = n,
        mu = mu_val
      )
    } else {
      norm_test <- tryCatch({ shapiro.test(x) }, error = function(e) NULL)
      is_normal <- if (!is.null(norm_test)) norm_test$p.value >= 0.05 else TRUE
      norm_p_val <- if (!is.null(norm_test)) norm_test$p.value else NA
      
      if (is_normal) {
        t_res <- t.test(x, alternative = input$tail, mu = mu_val, conf.level = input$conf)
        list(
          mode = "single",
          test_type = "Student's t-Test",
          norm_p = norm_p_val,
          is_normal = TRUE,
          res = t_res,
          n = n,
          mu = mu_val
        )
      } else {
        t_res <- t.test(x, alternative = input$tail, mu = mu_val, conf.level = input$conf)
        
        w_res <- tryCatch({
          wilcox.test(x, mu = mu_val, alternative = input$tail, exact = FALSE, conf.int = TRUE, conf.level = input$conf)
        }, error = function(e) {
          wilcox.test(x, mu = mu_val, alternative = input$tail, exact = FALSE, conf.int = FALSE)
        })
        
        list(
          mode = "both",
          test_type = "Parametric (t-Test) & Non-Parametric (Wilcoxon)",
          norm_p = norm_p_val,
          is_normal = FALSE,
          res_parametric = t_res,
          res_nonparametric = w_res,
          n = n,
          mu = mu_val
        )
      }
    }
  })
  
  # 4. Results Data Frame Builder (Guaranteed Data Frame Return)
  result_manual_df <- reactive({
    eng <- test_engine_manual_reactive()
    alpha <- 1 - input$conf
    
    alt_txt <- switch(
      input$tail,
      "two.sided" = paste("True parameter ≠", eng$mu),
      "less"      = paste("True parameter <", eng$mu),
      "greater"   = paste("True parameter >", eng$mu)
    )
    
    norm_txt <- if (is.na(eng$norm_p)) {
      ""
    } else {
      sprintf("%.4f (%s)", eng$norm_p, if(eng$is_normal) "Normal" else "Non-Normal (Normality Failed)")
    }
    
    if (eng$mode == "single") {
      t_res <- eng$res
      p_val <- t_res$p.value
      decision_txt <- if (p_val < alpha) "Significant" else " Not Significant"
      
      as.data.frame(
        list(
          Metric = c(
            "Selected Test", "Normality Test (Shapiro-Wilk p-value)",
            "Hypothesized Value (μ₀)", "Alternative Hypothesis (H₁)", "Test Statistic",
            "p-value", "Confidence Level", "CI Lower Bound", "CI Upper Bound",
            paste0("Decision (alpha = ", sprintf("%.2f", alpha), ")")
          ),
          Value = c(
            eng$test_type, norm_txt,
            as.character(eng$mu), alt_txt,
            sprintf("%.4f", t_res$statistic),
            if (p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val),
            sprintf("%.2f%%", input$conf * 100),
            sprintf("%.4f", t_res$conf.int[1]), sprintf("%.4f", t_res$conf.int[2]),
            decision_txt
          )
        ),
        stringsAsFactors = FALSE
      )
    } else {
      tp <- eng$res_parametric
      wp <- eng$res_nonparametric
      
      p_val_t <- tp$p.value
      p_val_w <- if(!is.null(wp$p.value)) wp$p.value else NA
      
      dec_t <- if (p_val_t < alpha) "Significant" else "Not Significant"
      dec_w <- if (!is.na(p_val_w) && p_val_w < alpha) "Significant" else "Not Significant"
      
      as.data.frame(
        list(
          Metric = c(
            "Normality Status", "Hypothesized Value (μ₀)",
            "--- Parametric Method ---", "Parametric Test (t-Test) Statistic", " p-value", "CI Lower", "CI Upper", "Decision",
            "--- Non-Parametric Method ---", "Non-Parametric Test (Wilcoxon V) Statistic", "p-value", "CI Lower", "Upper", "Decision"
          ),
          Value = c(
            norm_txt, as.character(eng$mu),
            "", sprintf("%.4f", tp$statistic), if (p_val_t < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_t), sprintf("%.4f", tp$conf.int[1]), sprintf("%.4f", tp$conf.int[2]), dec_t,
            "", if(!is.null(wp$statistic)) sprintf("%.4f", wp$statistic) else "N/A", if(is.na(p_val_w)) "N/A" else if (p_val_w < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_w),
            if(!is.null(wp$conf.int)) sprintf("%.4f", wp$conf.int[1]) else "N/A", if(!is.null(wp$conf.int)) sprintf("%.4f", wp$conf.int[2]) else "N/A", dec_w
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$result_table <- renderTable({
    result_manual_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 5. Combined Word Document Download Handler
  output$dl_singlet_manuals <- downloadHandler(
    filename = function() {
      paste0("manual_mean_test_report_", Sys.Date(), ".docx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      sum_df <- summary_manual_df1()
      res_df <- result_manual_df()
      
      doc <- read_docx() %>%
        body_add_par("Statistical Analysis Report:  Single Mean Test", style = "heading 1") %>%
        body_add_par(paste0("Generated on: ", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("1. Descriptive Summary Statistics", style = "heading 2")
      
      # Fixed: Passed 'doc' as the first argument
      for (i in 1:nrow(sum_df)) {
        doc <- body_add_par(doc, paste0("- ", sum_df$Statistic[i], ": ", sum_df$Value[i]), style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("2. Hypothesis Test Results", style = "heading 2")
      
      # Fixed: Passed 'doc' as the first argument
      for (i in 1:nrow(res_df)) {
        doc <- body_add_par(doc, paste0(res_df$Metric[i], ": ", res_df$Value[i]), style = "Normal")
      }
      
      print(doc, target = file)
    }
  ) 
  
  
    #two sample independent t-test-------------------
  
    # ------------------------------------------------------------------
    # REACTION 1: LOAD UPLOADED DATA
    # ---------------------------------------------------------------- # Required for Word document generation
  # Dynamic Dropdown for Two Variables
  # Dynamic Dropdown for Two Variables
  observeEvent(global_data(), {
    req(global_data())
    
    df <- as.data.frame(global_data())
    
    num_cols <- names(df)[sapply(df, function(x) {
      is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(x))))
    })]
    
    if (length(num_cols) < 2) {
      showNotification(
        "Uploaded dataset must contain at least two numeric variables for comparison.", 
        type = "error"
      )
      return() 
    }
    
    # 1. Dynamic UI Dropdown for Group 1 Variable
    output$var1 <- renderUI({
      req(global_data())
      df <- as.data.frame(global_data())
      
      num_cols <- names(df)[sapply(df, function(x) {
        is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(x))))
      })]
      
      selectInput("group1_file", "Select Group 1 Variable:", choices = num_cols, selected = num_cols[1])
    })
    
    # 2. Dynamic UI Dropdown for Group 2 Variable
    output$var2 <- renderUI({
      req(global_data())
      df <- as.data.frame(global_data())
      
      num_cols <- names(df)[sapply(df, function(x) {
        is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(x))))
      })]
      
      default_val <- if (length(num_cols) > 1) num_cols[2] else num_cols[1]
      selectInput("group2_file", "Select Group 2 Variable:", choices = num_cols, selected = default_val)
    })
    
    updateSelectInput(
      session,
      inputId = "group1_var",
      choices = num_cols,
      selected = num_cols[1]
    )
    
    updateSelectInput(
      session,
      inputId = "group2_var",
      choices = num_cols,
      selected = ifelse(length(num_cols) > 1, num_cols[2], num_cols[1])
    )
  })
  
  # Process Uploaded Data
  group_data_upload <- reactive({
    req(global_data(), input$group1_file, input$group2_file, input$tail3)
    
    x <- suppressWarnings(as.numeric(global_data()[[input$group1_file]]))
    y <- suppressWarnings(as.numeric(global_data()[[input$group2_file]]))
    
    x <- na.omit(x)
    y <- na.omit(y)
    
    validate(
      need(length(x) >= 3, "Group 1 must contain at least 3 valid numeric values."),
      need(length(y) >= 3, "Group 2 must contain at least 3 valid numeric values.")
    )
    
    list(x = x, y = y)
  })
  
  # Summary Stats Data Frame
  summary_upload_df <- reactive({
    g <- group_data_upload()
    data.frame(
      Group = c(input$group1_file, input$group2_file),
      N = c(length(g[["x"]]), length(g[["y"]])),
      Mean = round(c(mean(g[["x"]]), mean(g[["y"]])), 4),
      SD = round(c(sd(g[["x"]]), sd(g[["y"]])), 4),
      Min = round(c(min(g[["x"]]), min(g[["y"]])), 4),
      Max = round(c(max(g[["x"]]), max(g[["y"]])), 4),
      stringsAsFactors = FALSE
    )
  })
  
  output$summary_table_upload <- renderDT({
    datatable(summary_upload_df(), options = list(dom = 't'), rownames = FALSE)
  })
  
  # Test Engine Reactive (Conditional Sample Size & Normality Rule)
  # Test Engine Reactive (CLT Rule + Normality Check for Small Samples)
  
  # =====================================================
  # TEST ENGINE: NORMALITY-BASED TEST SELECTION
  # Independent Two-Sample Comparison
  # =====================================================
  
  
  # =====================================================
  # TEST ENGINE: INDEPENDENT TWO-GROUP COMPARISON
  # =====================================================
  
  
  test_engine_upload_reactive <- reactive({
    
    g <- group_data_upload()
    x <- as.numeric(g[["x"]])
    y <- as.numeric(g[["y"]])
    
    # Remove missing and non-finite observations
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    
    var_eq <- isTRUE(as.logical(input$var_equal3))
    conf_level <- as.numeric(input$conf3)
    
    alt_val <- if (
      is.null(input$tail3) ||
      length(input$tail3) != 1 ||
      is.na(input$tail3) ||
      !input$tail3 %in% c("two.sided", "less", "greater")
    ) {
      "two.sided"
    } else {
      input$tail3
    }
    
    validate(
      need(length(x) >= 3 && length(y) >= 3,
           "Each group must contain at least 3 valid numeric observations."),
      need(length(x) <= 5000 && length(y) <= 5000,
           "Shapiro-Wilk supports at most 5000 observations per group."),
      need(length(conf_level) == 1 &&
             is.finite(conf_level) &&
             conf_level > 0 && conf_level < 1,
           "Confidence level must be between 0 and 1.")
    )
    
    # STEP 1: Test normality separately in each group
    shx <- tryCatch(shapiro.test(x), error = function(e) NULL)
    shy <- tryCatch(shapiro.test(y), error = function(e) NULL)
    
    validate(
      need(!is.null(shx) && !is.null(shy),
           "Normality testing failed. Check your uploaded data.")
    )
    
    norm_x_p <- shx$p.value
    norm_y_p <- shy$p.value
    
    # STEP 2: Determine whether either group fails normality
    normality_failed <- (norm_x_p < 0.05 || norm_y_p < 0.05)
    # STEP 3: Always calculate the independent t-test
    t_res <- t.test(
      x, y,
      alternative = alt_val,
      var.equal = var_eq,
      conf.level = conf_level
    )
    
    t_res$diff_val <- unname(t_res$estimate[1] - t_res$estimate[2])
    
    # STEP 4: If either group fails normality, calculate BOTH tests
    if (normality_failed) {
      
      w_res <- tryCatch(
        wilcox.test(
          x, y,
          alternative = alt_val,
          exact = FALSE,
          correct = TRUE
        ),
        error = function(e) e
      )
      
      validate(
        need(!inherits(w_res, "error"),
             paste("Wilcoxon test failed:",
                   if (inherits(w_res, "error")) w_res$message else ""))
      )
      
      return(list(
        mode = "both",
        normality_failed = TRUE,
        test_type = "Independent t-Test + Wilcoxon Rank-Sum Test",
        norm_x_p = norm_x_p,
        norm_y_p = norm_y_p,
        res_parametric = t_res,
        res_nonparametric = w_res
      ))
    }
    
    # STEP 5: Both groups pass normality -> t-test only
    list(
      mode = "single",
      normality_failed = FALSE,
      test_type = paste0(
        "Independent Two-Sample t-Test (",
        if (var_eq) "Student's" else "Welch's",
        ")"
      ),
      norm_x_p = norm_x_p,
      norm_y_p = norm_y_p,
      res = t_res
    )
  })
  # =====================================================
  # TEST RESULTS DATA FRAME BUILDER
  # =====================================================
  
  test_upload_df <- reactive({
    
    eng <- test_engine_upload_reactive()
    alpha <- 1 - input$conf3
    
    alt_val <- if (
      is.null(input$tail3) ||
      length(input$tail3) != 1 ||
      is.na(input$tail3) ||
      !input$tail3 %in% c("two.sided", "less", "greater")
    ) {
      "two.sided"
    } else {
      input$tail3
    }
    
    alt_txt <- switch(
      alt_val,
      "two.sided" = "True difference in means != 0",
      "less" = "True difference in means < 0",
      "greater" = "True difference in means > 0"
    )
    
    norm_txt <- sprintf(
      paste0(
        "Group 1 Shapiro-Wilk p = %.4f (%s); ",
        "Group 2 Shapiro-Wilk p = %.4f (%s)"
      ),
      eng[["norm_x_p"]],
      if (eng[["norm_x_p"]] >= 0.05)
        "Normality not rejected" else "Non-normal",
      eng[["norm_y_p"]],
      if (eng[["norm_y_p"]] >= 0.05)
        "Normality not rejected" else "Non-normal"
    )
    
    # ---------------------------------------------------
    # CASE 1: Both groups satisfy normality
    # ---------------------------------------------------
    
    if (!isTRUE(eng[["normality_failed"]]))  {
      
      tr <- eng[["res"]]
      p_val <- tr$p.value
      
      return(data.frame(
        Metric = c(
          "Selected Test",
          "Normality Check (Shapiro-Wilk p-values)",
          "Alternative Hypothesis (H1)",
          "Test Statistic",
          "Degrees of Freedom",
          "p-value",
          "Mean Difference (G1 - G2)",
          "CI Lower",
          "CI Upper",
          "Decision"
        ),
        Value = c(
          eng[["test_type"]],
          norm_txt,
          alt_txt,
          sprintf("%.4f", unname(tr$statistic)),
          sprintf("%.2f", unname(tr$parameter)),
          if (p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val),
          sprintf("%.4f", tr$diff_val),
          sprintf("%.4f", tr$conf.int[1]),
          sprintf("%.4f", tr$conf.int[2]),
          if (p_val < alpha) "Significant" else "Not significant"
        ),
        stringsAsFactors = FALSE
      ))
    }
    
    # ---------------------------------------------------
    # CASE 2: At least one group is non-normal
    # Display both tests
    # ---------------------------------------------------
    
    tp <- eng[["res_parametric"]]
    wp <- eng[["res_nonparametric"]]
    
    p_val_t <- tp$p.value
    p_val_w <- wp$p.value
    
    g <- group_data_upload()
    diff_t <- unname(tp$estimate[1] - tp$estimate[2])
    diff_w <- median(g$x) - median(g$y)
    
    data.frame(
      Metric = c(
        "Normality Status",
        "Alternative Hypothesis (H1)",
        "--- Parametric Method: Independent t-Test ---",
        "t-Test Statistic",
        "Degrees of Freedom",
        "p-value",
        "Mean Difference (G1 - G2)",
        "CI Lower",
        "CI Upper",
        "Decision",
        "--- Non-Parametric Method: Wilcoxon Rank-Sum ---",
        "Wilcoxon Rank-Sum Statistic (W)",
        "p-value",
        "Observed Median Difference (G1 - G2)",
        "Confidence Interval",
        "Decision"
      ),
      Value = c(
        norm_txt,
        alt_txt,
        "",
        sprintf("%.4f", unname(tp$statistic)),
        sprintf("%.2f", unname(tp$parameter)),
        if (p_val_t < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_t),
        sprintf("%.4f", diff_t),
        sprintf("%.4f", tp$conf.int[1]),
        sprintf("%.4f", tp$conf.int[2]),
        if (p_val_t < alpha) "Significant" else "Not significant",
        "",
        sprintf("%.4f", unname(wp$statistic)),
        if (p_val_w < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_w),
        sprintf("%.4f", diff_w),
        "Not calculated",
        if (p_val_w < alpha) "Significant" else "Not significant"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  
  # =====================================================
  # DISPLAY RESULTS
  # =====================================================
  
  output$result_table_upload <- DT::renderDT({
    df <- test_upload_df()
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        scrollY = "500px",
        autoWidth = TRUE
      )
    )
  }, server = FALSE)
  
  
  # Boxplot Rendering & Download
  draw_boxplot_upload <- function() {
    g <- group_data_upload()
    eng <- test_engine_upload_reactive()
    
    y_max <- max(c(g[["x"]], g[["y"]]), na.rm = TRUE)
    y_min <- min(c(g[["x"]], g[["y"]]), na.rm = TRUE)
    
    boxplot(g[["x"]], g[["y"]],
            names = c(input$group1_file, input$group2_file),
            main = "Comparison of Selected Variables",
            ylab = "Values",
            ylim = c(y_min, y_max * 1.15),
            col = c("#abd9e9", "#fdae61"),
            border = "#2b83ba")
    
    points(1, mean(g[["x"]], na.rm = TRUE), pch = 19, col = "red", cex = 1.5)
    points(2, mean(g[["y"]], na.rm = TRUE), pch = 19, col = "red", cex = 1.5)
  }
  
  output$boxplot_upload <- renderPlot({
    draw_boxplot_upload()
  })
  
  
  
  # Word Report Download (.docx)
  # Word Report Download (.docx)
  # Word Report Download (.docx) with Boxplot
  
  output$download_word_upload2 <- downloadHandler(
    
    filename = function() {
      paste0("two_sample_analysis_report_", Sys.Date(), ".docx")
    },
    
    contentType =
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    
    content = function(file) {
      
      sum_df <- summary_upload_df()
      res_df <- test_upload_df()
      g <- group_data_upload()
      
      # Create temporary PNG for the boxplot
      plot_file <- tempfile(fileext = ".png")
      
      png(
        filename = plot_file,
        width = 1800,
        height = 1200,
        res = 180
      )
      
      tryCatch({
        
        x <- g[["x"]]
        y <- g[["y"]]
        
        y_min <- min(c(x, y), na.rm = TRUE)
        y_max <- max(c(x, y), na.rm = TRUE)
        
        # Avoid identical limits when all values are equal
        if (y_min == y_max) {
          y_min <- y_min - 1
          y_max <- y_max + 1
        }
        
        boxplot(
          x, y,
          names = c(
            as.character(input$group1_file),
            as.character(input$group2_file)
          ),
          main = "Comparison of Selected Variables",
          ylab = "Values",
          ylim = c(y_min, y_max + 0.15 * (y_max - y_min)),
          col = c("#abd9e9", "#fdae61"),
          border = "#2b83ba",
          outline = TRUE
        )
        
        # Add group means as red points
        points(1, mean(x, na.rm = TRUE),
               pch = 19, col = "red", cex = 1.5)
        
        points(2, mean(y, na.rm = TRUE),
               pch = 19, col = "red", cex = 1.5)
        
        legend(
          "topright",
          legend = "Mean",
          pch = 19,
          col = "red",
          bty = "n"
        )
        
      }, finally = {
        dev.off()
      })
      
      # Build Word document
      doc <- officer::read_docx()
      
      doc <- doc %>%
        officer::body_add_par(
          "Two-Sample Statistical Analysis Report",
          style = "heading 1"
        ) %>%
        officer::body_add_par(
          paste0("Generated on: ", Sys.Date()),
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "1. Group Summary Statistics",
          style = "heading 2"
        )
      
      for (i in seq_len(nrow(sum_df))) {
        doc <- officer::body_add_par(
          doc,
          paste0(
            "Group: ", sum_df$Group[i],
            " | N: ", sum_df$N[i],
            " | Mean: ", sum_df$Mean[i],
            " | SD: ", sum_df$SD[i]
          ),
          style = "Normal"
        )
      }
      
      doc <- doc %>%
        officer::body_add_par(
          "2. Hypothesis Test Results",
          style = "heading 2"
        )
      
      for (i in seq_len(nrow(res_df))) {
        doc <- officer::body_add_par(
          doc,
          paste0(res_df$Metric[i], ": ", res_df$Value[i]),
          style = "Normal"
        )
      }
      
      # Insert boxplot in Word
      doc <- doc %>%
        officer::body_add_par(
          "3. Boxplot Comparison",
          style = "heading 2"
        ) %>%
        officer::body_add_img(
          src = plot_file,
          width = 6.2,
          height = 4.1,
          style = "centered"
        )
      
      print(doc, target = file)
      
      unlink(plot_file)
    }
  )
  
  
  # ------------------------------------------------------------------
  # TAB 2 Two independent t -test (ENTER DATA MANUALLY)
  # ------------------------------------------------------------------
  # REACTION 2: PROCESS MANUAL DATA
  
  # ------------------------------------------------------------------
  
  # Manual Data Processing Reactive
  # Manual Data Processing Reactive
  manual_data <- reactive({
    req(input$group1, input$group2)
    
    x <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$group1, ",")))))
    y <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$group2, ",")))))
    
    x <- na.omit(x)
    y <- na.omit(y)
    
    validate(
      need(length(x) >= 3, "Group 1 must contain at least 3 valid numeric entries for testing."),
      need(length(y) >= 3, "Group 2 must contain at least 3 valid numeric entries for testing.")
    )
    
    list(x = x, y = y)
  })
  
  # Summary Stats Data Frame (Guaranteed Data Frame Return)
  summary_manual_df <- reactive({
    g <- manual_data()
    as.data.frame(
      list(
        Group = c("Group 1", "Group 2"),
        N = c(length(g[["x"]]), length(g[["y"]])),
        Mean = round(c(mean(g[["x"]]), mean(g[["y"]])), 4),
        SD = round(c(sd(g[["x"]]), sd(g[["y"]])), 4),
        Min = round(c(min(g[["x"]]), min(g[["y"]])), 4),
        Max = round(c(max(g[["x"]]), max(g[["y"]])), 4)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$summary_table_manual <- renderDT({
    datatable(summary_manual_df(), options = list(dom = 't'), rownames = FALSE)
  })
  
  # Test Engine Reactive (Conditional Sample Size & Normality Rule)
  # Test Engine Reactive (CLT Rule + Normality Check for Small Samples)
  
  test_engine_manual2 <- reactive({
    
    g <- manual_data()
    x <- as.numeric(g[["x"]])
    y <- as.numeric(g[["y"]])
    
    # Remove missing and non-finite values
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    
    var_eq <- isTRUE(as.logical(input$var_equal2))
    conf_level <- as.numeric(input$conf2)
    
    alt_val <- if (
      is.null(input$tail2) ||
      length(input$tail2) != 1 ||
      is.na(input$tail2) ||
      !input$tail2 %in% c("two.sided", "less", "greater")
    ) {
      "two.sided"
    } else {
      input$tail2
    }
    
    validate(
      need(length(x) >= 3 && length(y) >= 3,
           "Each group must contain at least 3 valid observations."),
      need(length(x) <= 5000 && length(y) <= 5000,
           "Shapiro-Wilk supports at most 5000 observations per group."),
      need(length(conf_level) == 1 &&
             is.finite(conf_level) &&
             conf_level > 0 && conf_level < 1,
           "Confidence level must be between 0 and 1.")
    )
    
    # STEP 1: Normality tests for both groups
    shx <- tryCatch(shapiro.test(x), error = function(e) NULL)
    shy <- tryCatch(shapiro.test(y), error = function(e) NULL)
    
    validate(
      need(!is.null(shx) && !is.null(shy),
           "Normality testing failed. Please check the group data.")
    )
    
    norm_x_p <- shx$p.value
    norm_y_p <- shy$p.value
    
    # Both groups must pass normality
    is_normal <- norm_x_p >= 0.05 && norm_y_p >= 0.05
    
    # STEP 2: Always calculate the independent t-test
    t_base <- t.test(
      x, y,
      alternative = alt_val,
      var.equal = var_eq,
      conf.level = conf_level
    )
    
    # STEP 3: If either group fails normality, calculate both tests
    if (!is_normal) {
      
      w_base <- wilcox.test(
        x, y,
        alternative = alt_val,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = conf_level
      )
      
      return(list(
        mode = "both",
        test_type = "Independent t-Test + Wilcoxon Rank-Sum Test",
        norm_x_p = norm_x_p,
        norm_y_p = norm_y_p,
        is_normal = FALSE,
        res_parametric = t_base,
        res_nonparametric = w_base
      ))
    }
    
    # STEP 4: Both groups pass normality -> t-test only
    list(
      mode = "single",
      test_type = paste0(
        "Independent Two-Sample t-Test (",
        if (var_eq) "Student's" else "Welch's",
        ")"
      ),
      norm_x_p = norm_x_p,
      norm_y_p = norm_y_p,
      is_normal = TRUE,
      res = t_base
    )
    
  })
  
  # Test Results Data Frame Builder (Guaranteed Data Frame Return)
  
  test_manual_df <- reactive({
    
    eng <- test_engine_manual2()
    alpha <- 1 - as.numeric(input$conf2)
    
    alt_val <- if (
      is.null(input$tail2) ||
      length(input$tail2) != 1 ||
      is.na(input$tail2) ||
      !input$tail2 %in% c("two.sided", "less", "greater")
    ) {
      "two.sided"
    } else {
      input$tail2
    }
    
    alt_txt <- switch(
      alt_val,
      "two.sided" = "True difference in means != 0",
      "less" = "True difference in means < 0",
      "greater" = "True difference in means > 0"
    )
    
    # Normality results for both groups
    norm_txt <- sprintf(
      paste0(
        "Group 1 Shapiro-Wilk p = %.4f (%s); ",
        "Group 2 Shapiro-Wilk p = %.4f (%s)"
      ),
      eng[["norm_x_p"]],
      if (eng[["norm_x_p"]] >= 0.05)
        "Normality not rejected" else "Non-normal",
      eng[["norm_y_p"]],
      if (eng[["norm_y_p"]] >= 0.05)
        "Normality not rejected" else "Non-normal"
    )
    
    fmt_p <- function(p) {
      if (is.null(p) || length(p) == 0 || is.na(p)) {
        "N/A"
      } else if (p < 0.0001) {
        "< 0.0001"
      } else {
        sprintf("%.4f", p)
      }
    }
    
    fmt_num <- function(z, digits = 4) {
      if (is.null(z) || length(z) == 0 ||
          !is.finite(z[1])) {
        "N/A"
      } else {
        sprintf(paste0("%.", digits, "f"), z[1])
      }
    }
    
    # ===================================================
    # CASE 1: Both groups pass normality -> t-test only
    # ===================================================
    if (identical(eng[["mode"]], "single")) {
      
      tr <- eng[["res"]]
      p_val <- tr$p.value
      
      return(data.frame(
        Metric = c(
          "Selected Test",
          "Normality Check (Shapiro-Wilk)",
          "Alternative Hypothesis (H1)",
          "Test Statistic (t)",
          "Degrees of Freedom",
          "p-value",
          "Mean Difference (Group 1 - Group 2)",
          "CI Lower",
          "CI Upper",
          paste0("Decision (alpha = ", sprintf("%.3f", alpha), ")")
        ),
        Value = c(
          eng[["test_type"]],
          norm_txt,
          alt_txt,
          fmt_num(tr$statistic),
          fmt_num(tr$parameter, 2),
          fmt_p(p_val),
          fmt_num(tr$estimate[1] - tr$estimate[2]),
          fmt_num(tr$conf.int[1]),
          fmt_num(tr$conf.int[2]),
          if (p_val < alpha) "Significant" else "Not significant"
        ),
        stringsAsFactors = FALSE
      ))
    }
    
    # ===================================================
    # CASE 2: Either group fails normality -> both tests
    # ===================================================
    tp <- eng[["res_parametric"]]
    wp <- eng[["res_nonparametric"]]
    
    p_val_t <- tp$p.value
    p_val_w <- wp$p.value
    
    g <- manual_data()
    x <- as.numeric(g[["x"]])
    y <- as.numeric(g[["y"]])
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    
    mean_diff <- mean(x) - mean(y)
    median_diff <- median(x) - median(y)
    
    # Wilcoxon confidence interval, if returned by R
    w_ci <- wp$conf.int
    
    w_ci_lower <- if (!is.null(w_ci) && length(w_ci) >= 2) {
      fmt_num(w_ci[1])
    } else {
      "N/A"
    }
    
    w_ci_upper <- if (!is.null(w_ci) && length(w_ci) >= 2) {
      fmt_num(w_ci[2])
    } else {
      "N/A"
    }
    
    data.frame(
      Metric = c(
        "Normality Check (Shapiro-Wilk)",
        "Alternative Hypothesis (H1)",
        
        "--- PARAMETRIC: Independent t-Test ---",
        "t-Test Statistic",
        "Degrees of Freedom",
        "t-Test p-value",
        "Mean Difference (Group 1 - Group 2)",
        "t-Test CI Lower",
        "t-Test CI Upper",
        "t-Test Decision",
        
        "--- NON-PARAMETRIC: Wilcoxon Rank-Sum Test ---",
        "Wilcoxon Rank-Sum Statistic (W)",
        "Wilcoxon p-value",
        "Observed Median Difference (Group 1 - Group 2)",
        "Wilcoxon CI Lower",
        "Wilcoxon CI Upper",
        "Wilcoxon Decision"
      ),
      
      Value = c(
        norm_txt,
        alt_txt,
        
        "",
        fmt_num(tp$statistic),
        fmt_num(tp$parameter, 2),
        fmt_p(p_val_t),
        fmt_num(mean_diff),
        fmt_num(tp$conf.int[1]),
        fmt_num(tp$conf.int[2]),
        if (p_val_t < alpha) "Significant" else "Not significant",
        
        "",
        fmt_num(wp$statistic),
        fmt_p(p_val_w),
        fmt_num(median_diff),
        w_ci_lower,
        w_ci_upper,
        if (p_val_w < alpha) "Significant" else "Not significant"
      ),
      
      stringsAsFactors = FALSE
    )
  })
  
  output$result_table_manual <- DT::renderDT({
    
    DT::datatable(
      test_manual_df(),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  
  # Boxplot Rendering & Helper Function
  draw_boxplot <- function() {
    g <- manual_data()
    eng <- test_engine_manual_reactive()
    
    p_val_display <- if (eng[["mode"]] == "single") {
      eng[["res"]][["p.value"]]
    } else {
      eng[["res_parametric"]][["p.value"]]
    }
    
    y_max <- max(c(g[["x"]], g[["y"]]))
    
    boxplot(g[["x"]], g[["y"]],
            names = c("Group 1", "Group 2"),
            main = "Comparison of Two Groups",
            ylab = "Values",
            ylim = c(min(c(g[["x"]], g[["y"]])), y_max * 1.15),
            col = c("#abdda4", "#fdae61"),
            border = "#2b83ba")
    
    points(1, mean(g[["x"]]), pch = 19, col = "red", cex = 1.5)
    points(2, mean(g[["y"]]), pch = 19, col = "red", cex = 1.5)
    
    p_text <- ifelse(p_val_display < 0.001, "p < 0.001", paste0("p = ", round(p_val_display, 4)))
    
    segments(1, y_max * 1.05, 2, y_max * 1.05, lwd = 1.5)
    segments(1, y_max * 1.05, 1, y_max * 1.02, lwd = 1.5)
    segments(2, y_max * 1.05, 2, y_max * 1.02, lwd = 1.5)
    text(1.5, y_max * 1.08, labels = p_text, font = 2)
  }
  
  output$boxplot_manual <- renderPlot({
    draw_boxplot()
  })
  
  # Word Report Download Handler (.docx)
  
  # Word Report Download with Boxplot
  output$download_word_manual2 <- downloadHandler(
    
    filename = function() {
      paste0("two_sample_analysis_report_", Sys.Date(), ".docx")
    },
    
    contentType =
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    
    content = function(file) {
      
      sum_df <- summary_manual_df()
      res_df <- test_manual_df()
      g <- manual_data()
      
      x <- as.numeric(g[["x"]])
      y <- as.numeric(g[["y"]])
      
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
      
      # Get results from the updated test engine
      eng <- test_engine_manual2()
      
      p_val_display <- if (identical(eng[["mode"]], "single")) {
        eng[["res"]][["p.value"]]
      } else {
        eng[["res_parametric"]][["p.value"]]
      }
      
      # Create temporary boxplot image
      plot_file <- tempfile(fileext = ".png")
      
      grDevices::png(
        filename = plot_file,
        width = 1800,
        height = 1200,
        res = 180
      )
      
      tryCatch({
        
        all_values <- c(x, y)
        y_min <- min(all_values)
        y_max <- max(all_values)
        
        # Handle identical values and negative values safely
        value_range <- y_max - y_min
        if (value_range == 0) {
          value_range <- max(abs(y_max), 1)
        }
        
        plot_bottom <- y_min - 0.08 * value_range
        plot_top <- y_max + 0.30 * value_range
        bracket_y <- y_max + 0.12 * value_range
        
        graphics::boxplot(
          x, y,
          names = c("Group 1", "Group 2"),
          main = "Comparison of Two Groups",
          ylab = "Values",
          ylim = c(plot_bottom, plot_top),
          col = c("#abdda4", "#fdae61"),
          border = "#2b83ba",
          outline = TRUE
        )
        
        # Group means
        graphics::points(
          1, mean(x),
          pch = 19, col = "red", cex = 1.5
        )
        
        graphics::points(
          2, mean(y),
          pch = 19, col = "red", cex = 1.5
        )
        
        # Parametric t-test p-value annotation
        p_text <- if (p_val_display < 0.001) {
          "t-test: p < 0.001"
        } else {
          paste0("t-test: p = ", format.pval(
            p_val_display, digits = 4, eps = 0.0001
          ))
        }
        
        graphics::segments(
          1, bracket_y, 2, bracket_y,
          lwd = 1.5
        )
        
        graphics::segments(
          1, bracket_y, 1, bracket_y - 0.025 * value_range,
          lwd = 1.5
        )
        
        graphics::segments(
          2, bracket_y, 2, bracket_y - 0.025 * value_range,
          lwd = 1.5
        )
        
        graphics::text(
          1.5,
          bracket_y + 0.045 * value_range,
          labels = p_text,
          font = 2
        )
        
        graphics::legend(
          "topleft",
          legend = "Group mean",
          pch = 19,
          col = "red",
          bty = "n"
        )
        
      }, finally = {
        grDevices::dev.off()
      })
      
      # Create Word document
      doc <- officer::read_docx()
      
      doc <- doc %>%
        officer::body_add_par(
          "Two-Sample Statistical Analysis",
          style = "heading 1"
        ) %>%
        officer::body_add_par(
          paste0("Generated on: ", Sys.Date()),
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "1. Group Summary Statistics",
          style = "heading 2"
        )
      
      for (i in seq_len(nrow(sum_df))) {
        doc <- officer::body_add_par(
          doc,
          paste0(
            "Group: ", sum_df$Group[i],
            " | N: ", sum_df$N[i],
            " | Mean: ", sum_df$Mean[i],
            " | SD: ", sum_df$SD[i]
          ),
          style = "Normal"
        )
      }
      
      doc <- doc %>%
        officer::body_add_par(
          "2. Hypothesis Test Results",
          style = "heading 2"
        )
      
      for (i in seq_len(nrow(res_df))) {
        doc <- officer::body_add_par(
          doc,
          paste0(res_df$Metric[i], ": ", res_df$Value[i]),
          style = "Normal"
        )
      }
      
      # Add boxplot to Word report
      doc <- doc %>%
        officer::body_add_par(
          "3. Boxplot Comparison",
          style = "heading 2"
        ) %>%
        officer::body_add_img(
          src = plot_file,
          width = 6.2,
          height = 4.1,
          style = "centered"
        )
      
      print(doc, target = file)
      
      unlink(plot_file)
    }
  )
  
  
#paired t-test-----------------------------------  
  output$upvar1 <- renderUI({
    req(exists("global_data") && !is.null(global_data()))
    selectInput("before_var", "Before Variable:", choices = names(global_data()))
  })
  
  output$upvar2 <- renderUI({
    req(exists("global_data") && !is.null(global_data()))
    selectInput("after_var", "After Variable:", choices = names(global_data()))
  })
  
  paired_data <- reactive({
    req(exists("global_data"), global_data(), input$before_var, input$after_var)
    x <- suppressWarnings(as.numeric(global_data()[[input$before_var]]))
    y <- suppressWarnings(as.numeric(global_data()[[input$after_var]]))
    valid <- complete.cases(x, y)
    list(x = x[valid], y = y[valid])
  })
  
  upload_summary_df <- reactive({
    d <- paired_data()
    
    validate(
      need(length(d$x) > 1, "At least 2 complete paired observations are required.")
    )
    
    diff <- d$x - d$y
    
    data.frame(
      Parameter = c(
        "Before Variable",
        "After Variable",
        "Sample Size (n)",
        "Mean (Before)",
        "SD (Before)",
        "Mean (After)",
        "SD (After)",
        "Mean Difference",
        "SD Difference"
      ),
      Value = c(
        input$before_var,
        input$after_var,
        length(d$x),
        round(mean(d$x), 3),
        round(sd(d$x), 3),
        round(mean(d$y), 3),
        round(sd(d$y), 3),
        round(mean(diff), 3),
        round(sd(diff), 3)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # Paired Test Engine Reactive (Normality of differences for n < 50, CLT for n >= 50)
  # Paired Test Engine Reactive (Safe against NA and uninitialized inputs)
  
  paired_test_engine_reactive <- reactive({
    
    req(input$before_var, input$after_var, input$uptail, input$upconf)
    
    d <- paired_data()
    x <- as.numeric(d$x)
    y <- as.numeric(d$y)
    
    # Keep only complete, finite pairs
    valid <- is.finite(x) & is.finite(y)
    x <- x[valid]
    y <- y[valid]
    
    n <- length(x)
    
    alt_val <- if (
      length(input$uptail) == 1 &&
      !is.na(input$uptail) &&
      input$uptail %in% c("two.sided", "less", "greater")
    ) {
      input$uptail
    } else {
      "two.sided"
    }
    
    conf_level <- as.numeric(input$upconf)
    
    validate(
      need(n >= 2,
           "At least 2 complete paired observations are required."),
      need(length(conf_level) == 1 &&
             is.finite(conf_level) &&
             conf_level > 0 && conf_level < 1,
           "Confidence level must be between 0 and 1.")
    )
    
    # STEP 1: Normality of paired differences
    diff_vals <- x - y
    
    validate(
      need(any(diff_vals != 0),
           "All paired differences are zero. The tests cannot provide a useful comparison.")
    )
    
    if (n < 3) {
      validate(
        need(FALSE,
             "At least 3 paired observations are needed to assess normality. Add more complete pairs.")
      )
    }
    
    validate(
      need(n <= 5000,
           "Shapiro-Wilk supports at most 5000 paired differences.")
    )
    
    sh_diff <- tryCatch(
      shapiro.test(diff_vals),
      error = function(e) NULL
    )
    
    validate(
      need(!is.null(sh_diff),
           "Normality testing failed. Please check the paired differences.")
    )
    
    norm_p <- sh_diff$p.value
    is_normal <- norm_p >= 0.05
    
    # STEP 2: Always calculate paired t-test
    t_base <- t.test(
      x, y,
      paired = TRUE,
      alternative = alt_val,
      conf.level = conf_level
    )
    
    # STEP 3: If differences are non-normal, calculate both tests
    if (!is_normal) {
      
      w_base <- wilcox.test(
        x, y,
        paired = TRUE,
        alternative = alt_val,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = conf_level
      )
      
      return(list(
        mode = "both",
        test_type = "Paired t-Test + Wilcoxon Signed-Rank Test",
        norm_p = norm_p,
        is_normal = FALSE,
        res_parametric = t_base,
        res_nonparametric = w_base
      ))
    }
    
    # STEP 4: Normal differences -> paired t-test only
    list(
      mode = "single",
      test_type = "Paired t-Test (Normal Differences)",
      norm_p = norm_p,
      is_normal = TRUE,
      res = t_base
    )
    
  })
  
  upload_test_results_df <- reactive({
    req(input$upconf, input$uptail)
    eng <- paired_test_engine_reactive()
    alpha <- 1 - input$upconf
    
    alt_txt <- switch(
      input$uptail,
      "two.sided" = "True mean difference ≠ 0",
      "less"      = "True mean difference < 0",
      "greater"   = "True mean difference > 0"
    )
    
    norm_txt <- if (is.na(eng$norm_p)) {
      ""
    } else {
      sprintf("Shapiro-Wilk p-value on Difference: %.4f (%s)", eng$norm_p, if(eng$is_normal) "Normal" else "Non-Normal")
    }
    
    if (eng$mode == "single") {
      tp <- eng$res
      p_val <- tp$p.value
      dec <- if (!is.na(p_val) && !is.na(alpha) && p_val < alpha) "Significant Change" else "No Significant Change"
      
      data.frame(
        Metric = c(
          "Selected Test Engine", "Normality Check", "Alternative Hypothesis (H₁)",
          "t-Statistic", "Degrees of Freedom", "p-value",
          "Mean Difference", "CI Lower", "CI Upper", "Decision"
        ),
        Value = c(
          eng$test_type, norm_txt, alt_txt,
          sprintf("%.4f", tp$statistic), as.character(round(tp$parameter, 2)),
          if(!is.na(p_val) && p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val),
          sprintf("%.4f", mean(paired_data()$x - paired_data()$y)),
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[1]) else "N/A",
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[2]) else "N/A",
          dec
        ),
        stringsAsFactors = FALSE
      )
    } else {
      tp <- eng$res_parametric
      wp <- eng$res_nonparametric
      p_val_t <- tp$p.value
      p_val_w <- if(!is.null(wp$p.value)) wp$p.value else NA
      
      dec_t <- if (!is.na(p_val_t) && !is.na(alpha) && p_val_t < alpha) "Significant Change " else "No Significant Change"
      dec_w <- if (!is.na(p_val_w) && !is.na(alpha) && p_val_w < alpha) "Significant Change " else "No Significant Change"
      
      data.frame(
        Metric = c(
          "Normality Status", "Alternative Hypothesis (H₁)",
          "--- Parametric Method (Paired t-Test) ---",
          "t-Statistic", "DF", "p-value", "Mean Difference", "CI Lower", "CI Upper", "Decision",
          "--- Non-Parametric Method (Wilcoxon Signed-Rank) ---",
          "Wilcoxon Statistic (V)", "p-value", "Median Difference", "CI Lower", "CI Upper", "Decision"
        ),
        Value = c(
          norm_txt, alt_txt,
          "",
          sprintf("%.4f", tp$statistic), as.character(round(tp$parameter, 2)),
          if(!is.na(p_val_t) && p_val_t < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_t),
          sprintf("%.4f", mean(paired_data()$x - paired_data()$y)),
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[1]) else "N/A",
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[2]) else "N/A",
          dec_t,
          "",
          if(!is.null(wp$statistic)) sprintf("%.4f", wp$statistic) else "N/A",
          if(is.na(p_val_w)) "N/A" else if(p_val_w < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_w),
          sprintf("%.4f", median(paired_data()$x - paired_data()$y)),
          if(!is.null(wp$conf.int) && !is.na(wp$conf.int[1])) sprintf("%.4f", wp$conf.int[1]) else "N/A",
          if(!is.null(wp$conf.int) && !is.na(wp$conf.int[2])) sprintf("%.4f", wp$conf.int[2]) else "N/A",
          dec_w
        ),
        stringsAsFactors = FALSE
      )
    }
  })  
  output$upload_summary_table <- renderTable({
    upload_summary_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$upload_test_results_table <- renderTable({
    upload_test_results_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Reusable helper function to render the line chart
  draw_upload_line_chart <- function() {
    d <- paired_data()
    validate(need(length(d$x) > 1, "At least 2 pairs required"))
    
    n <- length(d$x)
    plot(1:n, d$x, type = "o", col = "blue", pch = 19, lwd = 2,
         xlab = "Observation Index", ylab = "Values", main = "Before vs After Observations",
         ylim = range(c(d$x, d$y), na.rm = TRUE))
    lines(1:n, d$y, type = "o", col = "red", pch = 17, lwd = 2, lty = 2)
    legend("topright", legend = c(input$before_var, input$after_var), col = c("blue", "red"),
           pch = c(19, 17), lty = c(1, 2), lwd = 2, bty = "n")
  }
  
  output$upload_line_chart <- renderPlot({
    draw_upload_line_chart()
  })
  
  
  # Word Report Download Handler (.docx) with Before vs After Line Chart
  output$download_word_paired <- downloadHandler(
    filename = function() {
      paste0("paired_analysis_report_", Sys.Date(), ".docx")
    },
    
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    
    content = function(file) {
      
      # Get summary and test results
      sum_df <- upload_summary_df()
      res_df <- upload_test_results_df()
      
      # Prepare paired data for the graph
      d <- paired_data()
      x <- as.numeric(d$x)
      y <- as.numeric(d$y)
      
      # Keep complete, finite pairs
      valid <- is.finite(x) & is.finite(y)
      x <- x[valid]
      y <- y[valid]
      
      validate(
        need(length(x) >= 2,
             "At least 2 complete pairs are required to create the Word report.")
      )
      
      n <- length(x)
      y_range <- range(c(x, y), na.rm = TRUE)
      
      # Add padding if all plotted values are identical
      if (diff(y_range) == 0) {
        padding <- max(abs(y_range[1]) * 0.10, 1)
        y_range <- c(y_range[1] - padding, y_range[2] + padding)
      } else {
        padding <- diff(y_range) * 0.10
        y_range <- c(y_range[1] - padding, y_range[2] + padding)
      }
      
      # Create a temporary PNG file for the line chart
      plot_file <- tempfile(fileext = ".png")
      
      # Automatically remove the temporary image after report generation
      on.exit(unlink(plot_file), add = TRUE)
      
      grDevices::png(
        filename = plot_file,
        width = 1800,
        height = 1100,
        res = 180
      )
      
      tryCatch({
        
        graphics::plot(
          1:n, x,
          type = "o",
          col = "blue",
          pch = 19,
          lwd = 2,
          xlab = "Observation Index",
          ylab = "Values",
          main = "Before vs After Observations",
          ylim = y_range,
          cex.main = 1.4,
          cex.lab = 1.2
        )
        
        graphics::lines(
          1:n, y,
          type = "o",
          col = "red",
          pch = 17,
          lwd = 2,
          lty = 2
        )
        
        graphics::legend(
          "topright",
          legend = c(
            as.character(input$before_var),
            as.character(input$after_var)
          ),
          col = c("blue", "red"),
          pch = c(19, 17),
          lty = c(1, 2),
          lwd = 2,
          bty = "n",
          cex = 1.0
        )
        
      }, finally = {
        grDevices::dev.off()
      })
      
      # Build the Word document
      doc <- officer::read_docx()
      
      doc <- doc %>%
        officer::body_add_par(
          "Paired Samples Statistical Analysis Report",
          style = "heading 1"
        ) %>%
        officer::body_add_par(
          paste0("Generated on: ", Sys.Date()),
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "",
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "1. Summary Statistics",
          style = "heading 2"
        )
      
      # Add summary statistics
      if (nrow(sum_df) > 0) {
        for (i in seq_len(nrow(sum_df))) {
          doc <- officer::body_add_par(
            doc,
            paste0("- ", sum_df$Parameter[i], ": ", sum_df$Value[i]),
            style = "Normal"
          )
        }
      }
      
      # Add hypothesis test results
      doc <- doc %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par(
          "2. Hypothesis Test Results",
          style = "heading 2"
        )
      
      if (nrow(res_df) > 0) {
        for (i in seq_len(nrow(res_df))) {
          doc <- officer::body_add_par(
            doc,
            paste0(res_df$Metric[i], ": ", res_df$Value[i]),
            style = "Normal"
          )
        }
      }
      
      # Add the line chart to the Word report
      doc <- doc %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par(
          "3. Before vs After Line Chart",
          style = "heading 2"
        ) %>%
        officer::body_add_img(
          src = plot_file,
          width = 6.2,
          height = 3.8
        )
      
      # Save the completed Word document
      print(doc, target = file)
    }
  )
  
    
    
      
    # paired t-test Enter data-------------------------------------------
  before_data <- reactive({
    req(input$before)
    na.omit(as.numeric(trimws(unlist(strsplit(input$before, ",")))))
  })
  
  after_data <- reactive({
    req(input$after)
    na.omit(as.numeric(trimws(unlist(strsplit(input$after, ",")))))
  })
  
  manual_summary_df <- reactive({
    x <- before_data()
    y <- after_data()
    
    validate(
      need(length(x) == length(y), "Both groups must have equal length."),
      need(length(x) > 1, "At least 2 paired observations are required.")
    )
    
    diff <- x - y
    
    data.frame(
      Parameter = c(
        "Sample Size (n)",
        "Mean (Before)",
        "SD (Before)",
        "Mean (After)",
        "SD (After)",
        "Mean Difference",
        "SD Difference"
      ),
      Value = c(
        length(x),
        round(mean(x), 3),
        round(sd(x), 3),
        round(mean(y), 3),
        round(sd(y), 3),
        round(mean(diff), 3),
        round(sd(diff), 3)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  
  # Manual Paired Test Engine (No CLT Restriction)
  manual_paired_test_engine_reactive <- reactive({
    
    req(input$before, input$after, input$ptail, input$pconf)
    
    x <- as.numeric(before_data())
    y <- as.numeric(after_data())
    
    # Keep complete, finite paired observations
    validate(
      need(length(x) == length(y),
           "Both groups must have equal length.")
    )
    
    valid <- is.finite(x) & is.finite(y)
    x <- x[valid]
    y <- y[valid]
    
    n <- length(x)
    
    validate(
      need(n >= 3,
           "At least 3 complete paired observations are required for the normality check."),
      need(n <= 5000,
           "Shapiro-Wilk supports at most 5000 paired differences."),
      need(length(input$pconf) == 1 &&
             is.finite(as.numeric(input$pconf)) &&
             as.numeric(input$pconf) > 0 &&
             as.numeric(input$pconf) < 1,
           "Confidence level must be between 0 and 1."),
      need(input$ptail %in% c("two.sided", "less", "greater"),
           "Please select a valid alternative hypothesis.")
    )
    
    conf_level <- as.numeric(input$pconf)
    diff_vals <- x - y
    
    validate(
      need(any(diff_vals != 0),
           "All paired differences are zero. A meaningful comparison cannot be calculated.")
    )
    
    # STEP 1: Test normality of paired differences
    sh_diff <- tryCatch(
      shapiro.test(diff_vals),
      error = function(e) NULL
    )
    
    validate(
      need(!is.null(sh_diff),
           "Normality testing failed. Please check the paired differences.")
    )
    
    norm_p <- sh_diff$p.value
    is_normal <- norm_p >= 0.05
    
    # STEP 2: Always calculate the paired t-test
    t_base <- t.test(
      x, y,
      paired = TRUE,
      alternative = input$ptail,
      conf.level = conf_level
    )
    
    # STEP 3: Non-normal differences -> calculate both tests
    if (!is_normal) {
      
      w_base <- tryCatch(
        suppressWarnings(
          wilcox.test(
            x, y,
            paired = TRUE,
            alternative = input$ptail,
            exact = FALSE,
            correct = TRUE,
            conf.int = TRUE,
            conf.level = conf_level
          )
        ),
        error = function(e) NULL
      )
      
      validate(
        need(!is.null(w_base),
             "Wilcoxon signed-rank testing failed. Please check the paired differences.")
      )
      
      return(list(
        mode = "both",
        test_type = paste(
          "Paired t-Test + Wilcoxon Signed-Rank Test"
        ),
        norm_p = norm_p,
        is_normal = FALSE,
        res_parametric = t_base,
        res_nonparametric = w_base
      ))
    }
    
    # STEP 4: Normal differences -> paired t-test only
    list(
      mode = "single",
      test_type = "Paired t-Test (Normal Differences)",
      norm_p = norm_p,
      is_normal = TRUE,
      res = t_base
    )
  })
  
  manual_test_results_df <- reactive({
    req(input$pconf, input$ptail)
    eng <- manual_paired_test_engine_reactive()
    alpha <- 1 - input$pconf
    
    alt_txt <- switch(
      input$ptail,
      "two.sided" = "True mean difference ≠ 0",
      "less"      = "True mean difference < 0",
      "greater"   = "True mean difference > 0"
    )
    
    norm_txt <- if (is.na(eng$norm_p)) {
      "Skipped (Large Sample / CLT, n >= 50)"
    } else {
      sprintf("Shapiro-Wilk p-value on Difference: %.4f (%s)", eng$norm_p, if(eng$is_normal) "Normal" else "Non-Normal")
    }
    
    if (eng$mode == "single") {
      tp <- eng$res
      p_val <- tp$p.value
      dec <- if (!is.na(p_val) && !is.na(alpha) && p_val < alpha) "Significant Change" else "No Significant Change"
      
      data.frame(
        Metric = c(
          "Selected Test Engine", "Normality Check", "Alternative Hypothesis (H₁)",
          "t-Statistic", "Degrees of Freedom", "p-value",
          "Mean Difference", "CI Lower", "CI Upper", "Decision"
        ),
        Value = c(
          eng$test_type, norm_txt, alt_txt,
          sprintf("%.4f", tp$statistic), as.character(round(tp$parameter, 2)),
          if(!is.na(p_val) && p_val < 0.0001) "< 0.0001" else sprintf("%.4f", p_val),
          sprintf("%.4f", mean(before_data() - after_data())),
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[1]) else "N/A",
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[2]) else "N/A",
          dec
        ),
        stringsAsFactors = FALSE
      )
    } else {
      tp <- eng$res_parametric
      wp <- eng$res_nonparametric
      p_val_t <- tp$p.value
      p_val_w <- if(!is.null(wp$p.value)) wp$p.value else NA
      
      dec_t <- if (!is.na(p_val_t) && !is.na(alpha) && p_val_t < alpha) "Significant Change" else "No Significant Change"
      dec_w <- if (!is.na(p_val_w) && !is.na(alpha) && p_val_w < alpha) "Significant Change" else "No Significant Change"
      
      data.frame(
        Metric = c(
          "Normality Status", "Alternative Hypothesis (H₁)",
          "--- Parametric Method (Paired t-Test) ---",
          "t-Statistic", "DF", "p-value", "Mean Difference", "CI Lower", "CI Upper", "Decision",
          "--- Non-Parametric Method (Wilcoxon Signed-Rank) ---",
          "Wilcoxon Statistic (V)", "p-value", "Median Difference", "CI Lower", "CI Upper", "Decision"
        ),
        Value = c(
          norm_txt, alt_txt,
          "",
          sprintf("%.4f", tp$statistic), as.character(round(tp$parameter, 2)),
          if(!is.na(p_val_t) && p_val_t < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_t),
          sprintf("%.4f", mean(before_data() - after_data())),
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[1]) else "N/A",
          if(!is.null(tp$conf.int)) sprintf("%.4f", tp$conf.int[2]) else "N/A",
          dec_t,
          "",
          if(!is.null(wp$statistic)) sprintf("%.4f", wp$statistic) else "N/A",
          if(is.na(p_val_w)) "N/A" else if(p_val_w < 0.0001) "< 0.0001" else sprintf("%.4f", p_val_w),
          sprintf("%.4f", median(before_data() - after_data())),
          if(!is.null(wp$conf.int) && !is.na(wp$conf.int[1])) sprintf("%.4f", wp$conf.int[1]) else "N/A",
          if(!is.null(wp$conf.int) && !is.na(wp$conf.int[2])) sprintf("%.4f", wp$conf.int[2]) else "N/A",
          dec_w
        ),
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$manual_summary_table <- renderTable({
    manual_summary_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$manual_test_results_table <- renderTable({
    manual_test_results_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Reusable helper function to render the line chart
  draw_line_chart <- function() {
    x <- before_data()
    y <- after_data()
    
    validate(
      need(length(x) == length(y), "Both groups must have equal length"),
      need(length(x) > 1, "At least 2 pairs required")
    )
    
    n <- length(x)
    plot(1:n, x, type = "o", col = "blue", pch = 19, lwd = 2,
         xlab = "Observation Index", ylab = "Values", main = "Before vs After Observations",
         ylim = range(c(x, y), na.rm = TRUE))
    lines(1:n, y, type = "o", col = "red", pch = 17, lwd = 2, lty = 2)
    legend("topright", legend = c("Before", "After"), col = c("blue", "red"),
           pch = c(19, 17), lty = c(1, 2), lwd = 2, bty = "n")
  }
  
  output$line_chart <- renderPlot({
    draw_line_chart()
  })
  
  # Word Report Download Handler (.docx) for Manual Input
  
  # Word Report Download Handler (.docx) with Line Chart
  output$download_word_manual_paired <- downloadHandler(
    
    filename = function() {
      paste0("paired_manual_analysis_report_", Sys.Date(), ".docx")
    },
    
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    
    content = function(file) {
      
      # Get summary statistics and test results
      sum_df <- manual_summary_df()
      res_df <- manual_test_results_df()
      
      # Get paired observations
      x <- as.numeric(before_data())
      y <- as.numeric(after_data())
      
      validate(
        need(length(x) == length(y),
             "Before and After groups must have equal length.")
      )
      
      # Keep complete, finite pairs
      valid <- is.finite(x) & is.finite(y)
      x <- x[valid]
      y <- y[valid]
      
      validate(
        need(length(x) >= 2,
             "At least 2 complete paired observations are required to create the graph.")
      )
      
      n <- length(x)
      
      # Set a safe Y-axis range
      y_range <- range(c(x, y), na.rm = TRUE)
      
      if (diff(y_range) == 0) {
        padding <- max(abs(y_range[1]) * 0.10, 1)
        y_range <- c(y_range[1] - padding, y_range[2] + padding)
      } else {
        padding <- diff(y_range) * 0.10
        y_range <- c(y_range[1] - padding, y_range[2] + padding)
      }
      
      # Create temporary PNG image
      plot_file <- tempfile(fileext = ".png")
      on.exit(unlink(plot_file), add = TRUE)
      
      grDevices::png(
        filename = plot_file,
        width = 1800,
        height = 1100,
        res = 180
      )
      
      tryCatch({
        
        graphics::plot(
          1:n, x,
          type = "o",
          col = "blue",
          pch = 19,
          lwd = 2,
          xlab = "Observation Index",
          ylab = "Values",
          main = "Before vs After Observations",
          ylim = y_range,
          cex.main = 1.4,
          cex.lab = 1.2
        )
        
        graphics::lines(
          1:n, y,
          type = "o",
          col = "red",
          pch = 17,
          lwd = 2,
          lty = 2
        )
        
        graphics::legend(
          "topright",
          legend = c("Before", "After"),
          col = c("blue", "red"),
          pch = c(19, 17),
          lty = c(1, 2),
          lwd = 2,
          bty = "n"
        )
        
      }, finally = {
        grDevices::dev.off()
      })
      
      # Create Word document
      doc <- officer::read_docx()
      
      doc <- doc %>%
        officer::body_add_par(
          "Paired Samples Statistical Analysis Report (Manual Input)",
          style = "heading 1"
        ) %>%
        officer::body_add_par(
          paste0("Generated on: ", Sys.Date()),
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "",
          style = "Normal"
        ) %>%
        officer::body_add_par(
          "1. Summary Statistics",
          style = "heading 2"
        )
      
      # Add summary statistics
      if (nrow(sum_df) > 0) {
        for (i in seq_len(nrow(sum_df))) {
          doc <- officer::body_add_par(
            doc,
            paste0("- ", sum_df$Parameter[i], ": ", sum_df$Value[i]),
            style = "Normal"
          )
        }
      }
      
      # Add hypothesis test results
      doc <- doc %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par(
          "2. Hypothesis Test Results",
          style = "heading 2"
        )
      
      if (nrow(res_df) > 0) {
        for (i in seq_len(nrow(res_df))) {
          doc <- officer::body_add_par(
            doc,
            paste0(res_df$Metric[i], ": ", res_df$Value[i]),
            style = "Normal"
          )
        }
      }
      
      # Insert line chart into Word report
      doc <- doc %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par(
          "3. Before vs After Line Chart",
          style = "heading 2"
        ) %>%
        officer::body_add_img(
          src = plot_file,
          width = 6.2,
          height = 3.8
        )
      
      # Save Word document
      print(doc, target = file)
    }
  )
  
  
    #One way ANOVA----------------------------------------------    
      
  # ==============================================================================
  # ONE-WAY ANOVA MODULE upload data
  # ==============================================================================
  
  # ------------------------------------------------------------------------------
  # 1. UI SELECTORS
  # ------------------------------------------------------------------------------
  
  output$anova_y <- renderUI({
    req(global_data())
    
    selectInput(
      "anova_y_var",
      "Select Response Variable (Numeric):",
      choices = names(global_data())
    )
  })
  
  output$anova_group <- renderUI({
    req(global_data())
    
    selectInput(
      "anova_group_var",
      "Select Group Variable (Categorical):",
      choices = names(global_data())
    )
  })
  
  
  # ------------------------------------------------------------------------------
  # 2. CENTRALIZED REACTIVE DATA & MODEL
  # ------------------------------------------------------------------------------
  
  anova_data <- reactive({
    
    req(
      global_data(),
      input$anova_y_var,
      input$anova_group_var
    )
    
    df <- global_data()
    
    y <- suppressWarnings(
      as.numeric(df[[input$anova_y_var]])
    )
    
    group <- as.factor(
      df[[input$anova_group_var]]
    )
    
    valid <- complete.cases(y, group)
    
    y <- y[valid]
    group <- droplevels(group[valid])
    
    validate(
      need(
        length(unique(group)) >= 2,
        "Need at least 2 unique groups."
      ),
      need(
        length(y) > 2,
        "Not enough observations."
      )
    )
    
    data.frame(
      y = y,
      group = group
    )
  })
  
  
  anova_model_fit <- reactive({
    
    d <- anova_data()
    
    d$group <- factor(d$group)
    
    validate(
      need(
        nlevels(d$group) >= 2,
        "Need at least 2 groups for ANOVA."
      )
    )
    
    aov(
      y ~ group,
      data = d
    )
  })
  # ------------------------------------------------------------------------------
  # 3. NORMALITY CHECK OF RESIDUALS
  # ------------------------------------------------------------------------------
  
  anova_normality_check <- reactive({
    
    req(input$anova_conf)
    
    d <- anova_data()
    model <- anova_model_fit()
    
    res <- residuals(model)
    
    alpha <- 1 - input$anova_conf
    
    n_res <- length(res)
    
    if (
      n_res < 3 ||
      n_res > 5000 ||
      length(unique(res)) <= 1 ||
      sd(res, na.rm = TRUE) == 0
    ) {
      
      return(
        list(
          p_val = NA_real_,
          is_normal = TRUE
        )
      )
    }
    
    shapiro <- tryCatch(
      shapiro.test(res),
      error = function(e) NULL
    )
    
    if (is.null(shapiro)) {
      
      return(
        list(
          p_val = NA_real_,
          is_normal = TRUE
        )
      )
    }
    
    list(
      p_val = shapiro$p.value,
      is_normal = shapiro$p.value >= alpha
    )
  })
  
  
  # ------------------------------------------------------------------------------
  # 4. GROUP-WISE SUMMARY STATISTICS
  # ------------------------------------------------------------------------------
  
  anova_summary_df <- reactive({
    
    d <- anova_data()
    
    means <- tapply(
      d$y,
      d$group,
      mean,
      na.rm = TRUE
    )
    
    sds <- tapply(
      d$y,
      d$group,
      sd,
      na.rm = TRUE
    )
    
    ns <- tapply(
      d$y,
      d$group,
      length
    )
    
    data.frame(
      Group = names(means),
      `Sample Size (n)` = as.vector(ns),
      Mean = round(as.vector(means), 4),
      `Std Dev (SD)` = round(as.vector(sds), 4),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  
  output$anova_means <- renderTable({
    
    anova_summary_df()
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE
  )
  
  
  
  
  # ------------------------------------------------------------------------------
  # 5. ANOVA TEST RESULTS
  # ------------------------------------------------------------------------------
  
  anova_result_df <- reactive({
    
    req(input$anova_conf)
    
    fit <- anova_model_fit()
    
    alpha <- 1 - input$anova_conf
    
    # Get ANOVA table
    anova_table <- summary(fit)[[1]]
    
    validate(
      need(
        !is.null(anova_table),
        "ANOVA table could not be calculated."
      )
    )
    
    # Convert to data frame
    df_out <- as.data.frame(anova_table)
    
    # Store row names before modifying them
    source_names <- rownames(df_out)
    
    # Add Source column
    df_out <- cbind(
      Source = source_names,
      df_out
    )
    
    rownames(df_out) <- NULL
    
    
    # ------------------------------------------------------------
    # Rename columns
    # ------------------------------------------------------------
    
    if ("Df" %in% names(df_out)) {
      names(df_out)[names(df_out) == "Df"] <- "Df"
    }
    
    if ("Sum Sq" %in% names(df_out)) {
      names(df_out)[names(df_out) == "Sum Sq"] <- "Sum Sq"
    }
    
    if ("Mean Sq" %in% names(df_out)) {
      names(df_out)[names(df_out) == "Mean Sq"] <- "Mean Sq"
    }
    
    if ("F value" %in% names(df_out)) {
      names(df_out)[names(df_out) == "F value"] <- "F Value"
    }
    
    if ("Pr(>F)" %in% names(df_out)) {
      names(df_out)[names(df_out) == "Pr(>F)"] <- "Pr(>F)"
    }
    
    
    # ------------------------------------------------------------
    # Replace model term with actual variable name
    # ------------------------------------------------------------
    
    df_out$Source[
      df_out$Source == "group"
    ] <- input$anova_group_var
    
    
    # ------------------------------------------------------------
    # ANOVA p-value
    # ------------------------------------------------------------
    
    p_raw <- NA_real_
    
    if ("Pr(>F)" %in% names(df_out)) {
      
      p_raw <- as.numeric(
        df_out$`Pr(>F)`[1]
      )
    }
    
    
    # ------------------------------------------------------------
    # Conclusion
    # ------------------------------------------------------------
    
    if (is.na(p_raw)) {
      
      conclusion <- "N/A"
      
    } else if (p_raw < alpha) {
      
      conclusion <- "Reject H0 (Statistically Significant)"
      
    } else {
      
      conclusion <- "Fail to Reject H0 (Not Significant)"
    }
    
    
    # ------------------------------------------------------------
    # Add conclusion
    # ------------------------------------------------------------
    
    df_out$Conclusion <- ""
    
    df_out$Conclusion[1] <- conclusion
    
    
    # ------------------------------------------------------------
    # Total row
    # ------------------------------------------------------------
    
    total_df <- sum(
      df_out$Df,
      na.rm = TRUE
    )
    
    total_ss <- sum(
      as.numeric(df_out$`Sum Sq`),
      na.rm = TRUE
    )
    
    total_row <- df_out[1, ]
    
    total_row[] <- NA
    
    total_row$Source <- "Total"
    
    total_row$Df <- total_df
    
    total_row$`Sum Sq` <- total_ss
    
    total_row$Conclusion <- ""
    
    
    df_out <- rbind(
      df_out,
      total_row
    )
    
    
    # ------------------------------------------------------------
    # Formatting
    # ------------------------------------------------------------
    
    df_out$Df <- as.character(
      df_out$Df
    )
    
    
    if ("Sum Sq" %in% names(df_out)) {
      
      df_out$`Sum Sq` <- ifelse(
        is.na(df_out$`Sum Sq`),
        "",
        format(
          round(
            as.numeric(df_out$`Sum Sq`),
            3
          ),
          trim = TRUE
        )
      )
    }
    
    
    if ("Mean Sq" %in% names(df_out)) {
      
      df_out$`Mean Sq` <- ifelse(
        is.na(df_out$`Mean Sq`),
        "",
        format(
          round(
            as.numeric(df_out$`Mean Sq`),
            3
          ),
          trim = TRUE
        )
      )
    }
    
    
    if ("F Value" %in% names(df_out)) {
      
      df_out$`F Value` <- ifelse(
        is.na(df_out$`F Value`),
        "",
        format(
          round(
            as.numeric(df_out$`F Value`),
            3
          ),
          trim = TRUE
        )
      )
    }
    
    
    if ("Pr(>F)" %in% names(df_out)) {
      
      df_out$`Pr(>F)` <- ifelse(
        
        is.na(df_out$`Pr(>F)`),
        
        "",
        
        ifelse(
          as.numeric(df_out$`Pr(>F)`) < 0.001,
          "< 0.001",
          format(
            round(
              as.numeric(df_out$`Pr(>F)`),
              4
            ),
            trim = TRUE
          )
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # KRUSKAL-WALLIS IF NORMALITY IS VIOLATED
    # ------------------------------------------------------------
    
    norm_info <- anova_normality_check()
    
    if (
      !is.null(norm_info) &&
      !is.na(norm_info$is_normal) &&
      !norm_info$is_normal
    ) {
      
      d <- anova_data()
      
      kw <- tryCatch(
        
        kruskal.test(
          y ~ group,
          data = d
        ),
        
        error = function(e) NULL
      )
      
      
      if (!is.null(kw)) {
        
        kw_p <- as.numeric(
          kw$p.value
        )
        
        kw_conclusion <- if (
          
          !is.na(kw_p) &&
          kw_p < alpha
          
        ) {
          
          "Statistically Significant"
          
        } else {
          
          "Not Significant"
        }
        
        
        sep_row <- df_out[1, ]
        
        sep_row[] <- ""
        
        sep_row$Source <-
          "=== Non-Parametric Alternative (Kruskal-Wallis Test) ==="
        
        
        kw_row <- df_out[1, ]
        
        kw_row[] <- ""
        
        kw_row$Source <-
          "Kruskal-Wallis"
        
        kw_row$Df <-
          as.character(
            as.numeric(kw$parameter)
          )
        
        kw_row$`F Value` <- paste0(
          "Chi-sq = ",
          round(
            as.numeric(kw$statistic),
            3
          )
        )
        
        kw_row$`Pr(>F)` <- if (
          
          is.na(kw_p)
          
        ) {
          
          ""
          
        } else if (
          
          kw_p < 0.001
          
        ) {
          
          "< 0.001"
          
        } else {
          
          as.character(
            round(
              kw_p,
              4
            )
          )
        }
        
        kw_row$Conclusion <-
          kw_conclusion
        
        
        df_out <- rbind(
          df_out,
          sep_row,
          kw_row
        )
      }
    }
    
    
    df_out
  })
  
  
  output$anova_result <- renderTable({
    
    anova_result_df()
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  na = ""
  )
  
  
  
  
  # ------------------------------------------------------------------------------
  # 6. ASSUMPTION CHECKS
  # ------------------------------------------------------------------------------
  
  assumption_result_df1 <- reactive({
    
    req(input$anova_conf)
    
    d <- anova_data()
    model <- anova_model_fit()
    
    res <- residuals(model)
    
    alpha <- 1 - input$anova_conf
    
    # ------------------------------------------------------------
    # SHAPIRO-WILK TEST
    # ------------------------------------------------------------
    
    n_res <- length(res)
    
    if (n_res < 3 || n_res > 5000) {
      
      w_stat <- "N/A"
      p_shap_fmt <- "N/A"
      status_shap <- "⚠ Sample size must be between 3 and 5000"
      
    } else {
      
      res_sd <- sd(res, na.rm = TRUE)
      
      if (
        is.na(res_sd) ||
        res_sd == 0 ||
        length(unique(res[!is.na(res)])) <= 1
      ) {
        
        w_stat <- "N/A"
        p_shap_fmt <- "N/A"
        status_shap <- "⚠ Residuals have zero/undefined variance"
        
      } else {
        
        shapiro <- tryCatch(
          shapiro.test(res),
          error = function(e) NULL
        )
        
        if (is.null(shapiro)) {
          
          w_stat <- "N/A"
          p_shap_fmt <- "N/A"
          status_shap <- "⚠ Shapiro-Wilk test could not be calculated"
          
        } else {
          
          w_stat <- round(
            as.numeric(shapiro$statistic),
            4
          )
          
          p_shap <- shapiro$p.value
          
          if (is.na(p_shap)) {
            
            p_shap_fmt <- "N/A"
            status_shap <- "⚠ p-value could not be calculated"
            
          } else {
            
            p_shap_fmt <- if (
              p_shap < 0.001
            ) {
              "< 0.001"
            } else {
              as.character(
                round(p_shap, 4)
              )
            }
            
            status_shap <- if (
              p_shap > alpha
            ) {
              
              "✔ Residuals are approximately NORMAL"
              
            } else {
              
              "❌ Residuals violate normality"
            }
          }
        }
      }
    }
    
    
    # ------------------------------------------------------------
    # LEVENE'S TEST
    # ------------------------------------------------------------
    
    y_sd <- sd(
      d$y,
      na.rm = TRUE
    )
    
    if (
      is.na(y_sd) ||
      y_sd == 0 ||
      length(unique(d$y[!is.na(d$y)])) <= 1
    ) {
      
      f_stat <- "N/A"
      p_lev_fmt <- "N/A"
      status_lev <- "⚠ Response variable has zero/undefined variance"
      
    } else {
      
      lev <- tryCatch(
        
        car::leveneTest(
          y ~ group,
          data = d
        ),
        
        error = function(e) NULL
      )
      
      
      if (is.null(lev)) {
        
        f_stat <- "N/A"
        p_lev_fmt <- "N/A"
        status_lev <- "⚠ Levene's test could not be calculated"
        
      } else {
        
        f_stat <- as.numeric(
          lev[1, "F value"]
        )
        
        p_lev <- as.numeric(
          lev[1, "Pr(>F)"]
        )
        
        
        if (
          is.na(f_stat) ||
          is.na(p_lev)
        ) {
          
          f_stat <- "N/A"
          p_lev_fmt <- "N/A"
          status_lev <- "⚠ Levene's test returned NA"
          
        } else {
          
          f_stat <- round(
            f_stat,
            4
          )
          
          p_lev_fmt <- if (
            p_lev < 0.001
          ) {
            
            "< 0.001"
            
          } else {
            
            as.character(
              round(p_lev, 4)
            )
          }
          
          
          status_lev <- if (
            p_lev > alpha
          ) {
            
            "✔ Variances are EQUAL (homogeneous)"
            
          } else {
            
            "❌ Variances are NOT equal"
          }
        }
      }
    }
    
    
    # ------------------------------------------------------------
    # FINAL TABLE
    # ------------------------------------------------------------
    
    data.frame(
      
      Assumption = c(
        "Normality of Residuals",
        "Homogeneity of Variance"
      ),
      
      `Test Used` = c(
        "Shapiro-Wilk Test",
        "Levene's Test"
      ),
      
      `Test Statistic` = c(
        paste0(
          "W = ",
          w_stat
        ),
        paste0(
          "F = ",
          f_stat
        )
      ),
      
      `p-value` = c(
        p_shap_fmt,
        p_lev_fmt
      ),
      
      `Alpha Threshold` = c(
        as.character(
          round(alpha, 4)
        ),
        as.character(
          round(alpha, 4)
        )
      ),
      
      Status = c(
        status_shap,
        status_lev
      ),
      
      check.names = FALSE,
      
      stringsAsFactors = FALSE
    )
  }) 
  
  output$assumption_result1 <- renderTable({
    
    assumption_result_df1()
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE
  )
  
  

  
  # ------------------------------------------------------------------------------
  # 7. POST-HOC COMPARISONS
  # ------------------------------------------------------------------------------
  
  posthoc_table_df <- reactive({
    
    req(
      input$posthoc_type,
      input$posthoc_type != "none",
      input$anova_conf
    )
    
    fit <- anova_model_fit()
    
    d <- anova_data()
    
    alpha <- 1 - input$anova_conf
    
    
    p_val <- summary(fit)[[1]][["Pr(>F)"]][1]
    
    
    validate(
      need(
        !is.na(p_val),
        "ANOVA p-value could not be calculated."
      )
    )
    
    
    validate(
      need(
        p_val < alpha,
        "Post-hoc test not required because ANOVA is not statistically significant."
      )
    )
    
    
    validate(
      need(
        length(unique(d$group)) > 2,
        "Post-hoc tests require at least 3 groups."
      )
    )
    
    
    # --------------------------------------------------------------------------
    # TUKEY HSD
    # --------------------------------------------------------------------------
    
    if (input$posthoc_type == "tukey") {
      
      tukey <- TukeyHSD(fit)
      
      res <- as.data.frame(
        tukey[[1]]
      )
      
      
      data.frame(
        
        `Comparison` =
          rownames(res),
        
        `Mean Diff` =
          round(
            res$diff,
            4
          ),
        
        `Lower CI` =
          round(
            res$lwr,
            4
          ),
        
        `Upper CI` =
          round(
            res$upr,
            4
          ),
        
        `p-value` = ifelse(
          
          res$`p adj` < 0.001,
          
          "< 0.001",
          
          as.character(
            round(
              res$`p adj`,
              4
            )
          )
        ),
        
        `Significance` = ifelse(
          
          res$`p adj` < alpha,
          
          "✔ Significant",
          
          "NS"
        ),
        
        check.names = FALSE,
        
        stringsAsFactors = FALSE
      )
      
      
    } else {
      
      # ------------------------------------------------------------------------
      # BONFERRONI / UNADJUSTED PAIRWISE T-TEST
      # ------------------------------------------------------------------------
      
      method <- if (
        input$posthoc_type == "bonf"
      ) {
        
        "bonferroni"
        
      } else {
        
        "none"
      }
      
      
      pair <- pairwise.t.test(
        d$y,
        d$group,
        p.adjust.method = method
      )
      
      
      mat <- pair$p.value
      
      
      res <- as.data.frame(
        as.table(mat)
      )
      
      
      res <- res[
        !is.na(res$Freq),
        ,
        drop = FALSE
      ]
      
      
      means <- tapply(
        d$y,
        d$group,
        mean,
        na.rm = TRUE
      )
      
      
      diffs <- round(
        
        means[
          as.character(res$Var1)
        ] -
          
          means[
            as.character(res$Var2)
          ],
        
        4
      )
      
      
      data.frame(
        
        `Comparison` =
          paste(
            res$Var1,
            "-",
            res$Var2
          ),
        
        `Mean Diff` =
          diffs,
        
        `Lower CI` =
          "N/A",
        
        `Upper CI` =
          "N/A",
        
        `p-value` = ifelse(
          
          res$Freq < 0.001,
          
          "< 0.001",
          
          as.character(
            round(
              res$Freq,
              4
            )
          )
        ),
        
        `Significance` = ifelse(
          
          res$Freq < alpha,
          
          "✔ Significant",
          
          "NS"
        ),
        
        check.names = FALSE,
        
        stringsAsFactors = FALSE
      )
    }
  })
  
  
  output$posthoc_result <- renderTable({
    
    posthoc_table_df()
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  na = "N/A"
  )
  
  

  # ------------------------------------------------------------------------------
  # 9. DIAGNOSTIC ASSUMPTION PLOTS
  # ------------------------------------------------------------------------------
  
  draw_assumption_plots <- function() {
    
    d <- anova_data()
    
    req(d)
    
    
    model <- aov(
      y ~ group,
      data = d
    )
    
    res <- residuals(model)
    
    
    old_par <- par(
      no.readonly = TRUE
    )
    
    on.exit(
      par(old_par),
      add = TRUE
    )
    
    
    par(
      mfrow = c(1, 2)
    )
    
    
    # QQ plot
    
    qqnorm(
      res,
      main = "QQ Plot (Normality)"
    )
    
    qqline(
      res,
      col = "red",
      lwd = 2
    )
    
    
    # Residuals vs fitted
    
    plot(
      
      fitted(model),
      
      res,
      
      main = "Residuals vs Fitted",
      
      xlab = "Fitted Values",
      
      ylab = "Residuals",
      
      pch = 19,
      
      col = "#2b83ba"
    )
    
    
    abline(
      h = 0,
      col = "red",
      lty = 2,
      lwd = 2
    )
  }
  
  
  output$assumption_plots <- renderPlot({
    
    draw_assumption_plots()
    
  })
  
  # ------------------------------------------------------------
  # COMPLETE WORD REPORT: ONE-WAY ANOVA
  # Includes summary, assumptions, ANOVA, post-hoc and plots
  # ------------------------------------------------------------
  
  
  # ------------------------------------------------------------
  # COMPLETE ONE-WAY ANOVA WORD REPORT
  # ------------------------------------------------------------
  
  
  # ------------------------------------------------------------
  # COMPLETE ONE-WAY ANOVA WORD REPORT
  # ------------------------------------------------------------
  
  output$download_word_anova <- shiny::downloadHandler(
    
    filename = function() {
      paste0("anova_analysis_report_", Sys.Date(), ".docx")
    },
    
    contentType =
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    
    content = function(file) {
      
      # ----------------------------------------------------------
      # 1. COLLECT RESULTS
      # ----------------------------------------------------------
      
      sum_df  <- anova_summary_df()
      res_df  <- anova_result_df()
      ass_df  <- assumption_result_df1()
      
      # Post-hoc results are optional. Do not stop the report
      # if the test is unavailable or not required.
      posthoc_df <- tryCatch(
        posthoc_table_df(),
        error = function(e) NULL
      )
      
      # ----------------------------------------------------------
      # 2. CREATE DOCUMENT
      # ----------------------------------------------------------
      
      doc <- officer::read_docx()
      
      # Add a section heading
      add_heading <- function(doc, heading, style = "heading 2") {
        officer::body_add_par(
          doc,
          heading,
          style = style
        )
      }
      
      # Add a table or a message when results are unavailable
      add_report_table <- function(doc, heading, df) {
        
        doc <- add_heading(doc, heading)
        
        if (
          is.null(df) ||
          !is.data.frame(df) ||
          nrow(df) == 0
        ) {
          return(
            officer::body_add_par(
              doc,
              "No results available for this section.",
              style = "Normal"
            )
          )
        }
        
        df <- as.data.frame(df, check.names = FALSE)
        
        # Convert factors and other unusual columns to safe
        # printable values for Word tables.
        df[] <- lapply(df, function(x) {
          if (is.factor(x)) {
            as.character(x)
          } else if (is.list(x)) {
            vapply(x, toString, character(1))
          } else {
            x
          }
        })
        
        # Convert non-finite numeric values to NA for display
        df[] <- lapply(df, function(x) {
          if (is.numeric(x)) {
            x[!is.finite(x)] <- NA_real_
          }
          x
        })
        
        doc <- officer::body_add_table(
          doc,
          value = df
        )
        
        officer::body_add_par(
          doc,
          "",
          style = "Normal"
        )
      }
      
      # ----------------------------------------------------------
      # 3. TITLE
      # ----------------------------------------------------------
      
      doc <- add_heading(
        doc,
        "One-Way ANOVA Analysis Report",
        "heading 1"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Generated on:", Sys.Date()),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Response variable:", input$anova_y_var),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Grouping variable:", input$anova_group_var),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        "",
        style = "Normal"
      )
      
      # ----------------------------------------------------------
      # 4. GROUP-WISE SUMMARY
      # ----------------------------------------------------------
      
      doc <- add_report_table(
        doc,
        "1. Group-wise Summary Statistics",
        sum_df
      )
      
      # ----------------------------------------------------------
      # 5. ASSUMPTION CHECKS
      # ----------------------------------------------------------
      
      doc <- add_report_table(
        doc,
        "2. Assumption Checks",
        ass_df
      )
      
      # ----------------------------------------------------------
      # 6. ANOVA AND NON-PARAMETRIC RESULTS
      # anova_result_df() already includes its Kruskal-Wallis
      # section when normality is violated.
      # ----------------------------------------------------------
      
      doc <- add_report_table(
        doc,
        "3. Hypothesis Test Results",
        res_df
      )
      
      # ----------------------------------------------------------
      # 7. POST-HOC RESULTS
      # ----------------------------------------------------------
      
      posthoc_method <- input$posthoc_type
      
      if (
        is.null(posthoc_method) ||
        length(posthoc_method) == 0 ||
        is.na(posthoc_method[1]) ||
        !nzchar(as.character(posthoc_method[1]))
      ) {
        posthoc_method <- "Not selected"
      } else {
        posthoc_method <- as.character(posthoc_method[1])
      }
      
      doc <- add_report_table(
        doc,
        paste("4. Post-Hoc Comparisons -", posthoc_method),
        posthoc_df
      )
      
      # ----------------------------------------------------------
      # 8. DIAGNOSTIC PLOTS
      # ----------------------------------------------------------
      
      doc <- add_heading(
        doc,
        "5. Assumption Diagnostic Plots"
      )
      
      plot_file <- tempfile(fileext = ".png")
      plot_ok <- FALSE
      
      tryCatch({
        
        grDevices::png(
          filename = plot_file,
          width = 1200,
          height = 700,
          res = 120
        )
        
        draw_assumption_plots()
        
        grDevices::dev.off()
        
        plot_ok <- file.exists(plot_file) &&
          file.info(plot_file)$size > 0
        
      }, error = function(e) {
        
        message(
          "Diagnostic plot generation failed: ",
          conditionMessage(e)
        )
        
        # Close the device if it is still open
        if (grDevices::dev.cur() > 1) {
          try(grDevices::dev.off(), silent = TRUE)
        }
      })
      
      if (isTRUE(plot_ok)) {
        
        doc <- officer::body_add_img(
          doc,
          src = plot_file,
          width = 6.2,
          height = 3.8
        )
        
      } else {
        
        doc <- officer::body_add_par(
          doc,
          "Diagnostic plots could not be generated.",
          style = "Normal"
        )
      }
      
      # ----------------------------------------------------------
      # 9. SAVE REPORT
      # ----------------------------------------------------------
      
      print(doc, target = file)
      
      if (file.exists(plot_file)) {
        unlink(plot_file)
      }
    }
  )
  
  # ============================================================
  # ONE-WAY ANOVA: COMPARE SEPARATE INDEPENDENT GROUP COLUMNS
  # Example: Group1_BMI, Group2_BMI, Group3_BMI
  # ============================================================
  
  # 1. Select number of groups and their columns
  output$anova_group_columns_ui <- renderUI({
    
    req(global_data(), input$anova_num_groups_upload)
    
    vars <- names(global_data())
    k <- max(2, as.integer(input$anova_num_groups_upload))
    
    tagList(
      lapply(seq_len(k), function(i) {
        
        selectInput(
          inputId = paste0("anova_column_", i),
          label = paste("Select variable for Group", i),
          choices = vars,
          selected = if (i <= length(vars)) vars[i] else NULL
        )
        
      })
    )
  })
  
  
  # 2. Prepare selected columns in long format
  anova_columns_data <- eventReactive(input$run_anova_columns, {
    
    req(global_data(), input$anova_num_groups_upload)
    
    df <- global_data()
    k <- as.integer(input$anova_num_groups_upload)
    
    selected <- vapply(seq_len(k), function(i) {
      input[[paste0("anova_column_", i)]] %||% ""
    }, character(1))
    
    validate(
      need(all(nzchar(selected)),
           "Please select a variable for every group."),
      need(length(unique(selected)) == k,
           "Please select a different column for each group.")
    )
    
    result <- lapply(seq_len(k), function(i) {
      
      v <- selected[i]
      
      y <- suppressWarnings(
        as.numeric(as.character(df[[v]]))
      )
      
      y <- y[is.finite(y)]
      
      data.frame(
        Group = paste0("Group ", i, ": ", v),
        Value = y
      )
    })
    
    long_data <- do.call(rbind, result)
    
    long_data$Group <- factor(long_data$Group)
    
    validate(
      need(nrow(long_data) > k,
           "Insufficient observations for ANOVA."),
      need(all(table(long_data$Group) >= 2),
           "Each group must have at least two valid observations.")
    )
    
    long_data
  })
  
  
  # 3. Descriptive statistics
  anova_columns_summary_df <- reactive({
    
    d <- anova_columns_data()
    
    groups <- split(d$Value, d$Group)
    
    data.frame(
      Group = names(groups),
      N = vapply(groups, length, integer(1)),
      Mean = round(vapply(groups, mean, numeric(1)), 3),
      SD = round(vapply(groups, sd, numeric(1)), 3),
      row.names = NULL
    )
  })
  
  
  output$anova_columns_summary <- renderTable({
    anova_columns_summary_df()
  }, striped = TRUE, bordered = TRUE)
  
  
  # 4. One-way ANOVA
  anova_columns_results_df <- reactive({
    
    d <- anova_columns_data()
    
    fit <- aov(Value ~ Group, data = d)
    tab <- summary(fit)[[1]]
    
    p <- tab[1, "Pr(>F)"]
    alpha <- if (!is.null(input$anova_conf)) {
      1 - input$anova_conf
    } else {
      0.05
    }
    
    data.frame(
      `Source of Variation` = c("Between Groups", "Within Groups"),
      Df = as.numeric(tab[, "Df"]),
      `Sum of Squares` = round(as.numeric(tab[, "Sum Sq"]), 4),
      `Mean Square` = round(as.numeric(tab[, "Mean Sq"]), 4),
      `F Value` = c(
        round(as.numeric(tab[1, "F value"]), 4),
        NA
      ),
      `p-value` = c(
        format.pval(p, digits = 4, eps = 0.001),
        ""
      ),
      Conclusion = c(
        if (is.na(p)) {
          "Could not calculate"
        } else if (p < alpha) {
          "Significant difference between at least two group means"
        } else {
          "No statistically significant difference between group means"
        },
        ""
      ),
      check.names = FALSE
    )
  })
  
  
  output$anova_columns_results <- renderTable({
    anova_columns_results_df()
  }, striped = TRUE, bordered = TRUE, na = "")
  
  
  
  # ------------------------------------------------------------
  # CONFIDENCE LEVEL AND MODEL
  # ------------------------------------------------------------
  
  anova_columns_model <- reactive({
    d <- anova_columns_data()
    req(d)
    
    d$Group <- droplevels(factor(d$Group))
    d$Value <- as.numeric(d$Value)
    
    validate(
      need(all(is.finite(d$Value)),
           "The selected group columns must contain valid numeric values."),
      need(nlevels(d$Group) >= 2,
           "Select at least two different group columns."),
      need(all(table(d$Group) >= 2),
           "Each group must contain at least two valid observations.")
    )
    
    aov(Value ~ Group, data = d)
  })
  
  
  # ------------------------------------------------------------
  # ASSUMPTION CHECKS
  # Normality of residuals + homogeneity of variance
  # ------------------------------------------------------------
  
  anova_columns_assumption_info <- reactive({
    d <- anova_columns_data()
    fit <- anova_columns_model()
    res <- residuals(fit)
    
    conf <- input$anova_columns_conf
    if (is.null(conf) || is.na(conf)) conf <- 95
    alpha <- 1 - conf / 100
    
    # Shapiro-Wilk test: R accepts 3 to 5000 observations
    normal_p <- NA_real_
    
    if (length(res) >= 3 &&
        length(res) <= 5000 &&
        length(unique(res)) > 2) {
      normal_p <- tryCatch(
        shapiro.test(res)$p.value,
        error = function(e) NA_real_
      )
    }
    
    # Levene-style test using deviations from group medians.
    # This is more robust to non-normality than Bartlett's test.
    group_medians <- ave(
      d$Value, d$Group,
      FUN = function(x) median(x, na.rm = TRUE)
    )
    abs_dev <- abs(d$Value - group_medians)
    
    variance_p <- tryCatch({
      if (length(unique(abs_dev)) < 2) {
        NA_real_
      } else {
        summary(aov(abs_dev ~ d$Group))[[1]][["Pr(>F)"]][1]
      }
    }, error = function(e) NA_real_)
    
    normal_ok <- !is.na(normal_p) && normal_p >= alpha
    variance_ok <- !is.na(variance_p) && variance_p >= alpha
    
    list(
      alpha = alpha,
      confidence = conf,
      normal_p = normal_p,
      variance_p = variance_p,
      normal_ok = normal_ok,
      variance_ok = variance_ok,
      assumptions_ok = normal_ok && variance_ok
    )
  })
  
  
  output$anova_columns_assumptions <- renderTable({
    a <- anova_columns_assumption_info()
    
    data.frame(
      Assumption = c(
        "Residual normality (Shapiro-Wilk)",
        "Homogeneity of variance (median-centred test)"
      ),
      P_Value = c(
        ifelse(is.na(a$normal_p), NA, signif(a$normal_p, 4)),
        ifelse(is.na(a$variance_p), NA, signif(a$variance_p, 4))
      ),
      Conclusion = c(
        if (is.na(a$normal_p)) {
          "Could not assess; inspect Q-Q plot"
        } else if (a$normal_ok) {
          "No significant evidence against normality"
        } else {
          "Normality assumption may be violated"
        },
        if (is.na(a$variance_p)) {
          "Could not assess"
        } else if (a$variance_ok) {
          "No significant evidence of unequal variances"
        } else {
          "Variance homogeneity may be violated"
        }
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------
  # CONFIDENCE INTERVALS FOR EACH GROUP MEAN
  # ------------------------------------------------------------
  
  anova_columns_mean_ci_df <- reactive({
    d <- anova_columns_data()
    a <- anova_columns_assumption_info()
    conf <- a$confidence / 100
    
    pieces <- lapply(split(d$Value, d$Group), function(x) {
      x <- x[is.finite(x)]
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      
      margin <- if (n > 1 && is.finite(s)) {
        qt(1 - (1 - conf) / 2, df = n - 1) * s / sqrt(n)
      } else {
        NA_real_
      }
      
      data.frame(
        N = n,
        Mean = m,
        SD = s,
        SE = s / sqrt(n),
        CI_Lower = m - margin,
        CI_Upper = m + margin
      )
    })
    
    out <- do.call(rbind, pieces)
    out$Group <- rownames(out)
    rownames(out) <- NULL
    out <- out[, c(
      "Group", "N", "Mean", "SD", "SE", "CI_Lower", "CI_Upper"
    )]
    
    out[, c("Mean", "SD", "SE", "CI_Lower", "CI_Upper")] <-
      lapply(out[, c("Mean", "SD", "SE", "CI_Lower", "CI_Upper")],
             round, 4)
    
    out
  })
  
  output$anova_columns_mean_ci <- renderTable({
    anova_columns_mean_ci_df()
  }, striped = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------
  # SELECT ANOVA OR KRUSKAL-WALLIS
  # ------------------------------------------------------------
  
  anova_columns_test_info <- reactive({
    d <- anova_columns_data()
    fit <- anova_columns_model()
    a <- anova_columns_assumption_info()
    
    if (a$assumptions_ok) {
      tab <- summary(fit)[[1]]
      p <- tab[1, "Pr(>F)"]
      
      list(
        method = "One-Way ANOVA",
        statistic_name = "F statistic",
        statistic = unname(tab[1, "F value"]),
        df1 = unname(tab[1, "Df"]),
        df2 = unname(tab[2, "Df"]),
        p_value = unname(p),
        assumptions_ok = TRUE
      )
    } else {
      kw <- kruskal.test(Value ~ Group, data = d)
      
      list(
        method = "Kruskal-Wallis test",
        statistic_name = "Kruskal-Wallis chi-squared",
        statistic = unname(kw$statistic),
        df1 = unname(kw$parameter),
        df2 = NA_real_,
        p_value = kw$p.value,
        assumptions_ok = FALSE
      )
    }
  })
  
  
  
  output$anova_columns_test_results <- renderTable({
    
    d <- anova_columns_data()
    z <- anova_columns_test_info()
    alpha <- 1 - (input$anova_columns_conf %||% 95) / 100
    
    if (z$method == "One-Way ANOVA") {
      
      fit <- anova_columns_model()
      tab <- summary(fit)[[1]]
      
      # Between-group values
      df_between <- tab[1, "Df"]
      ss_between <- tab[1, "Sum Sq"]
      ms_between <- tab[1, "Mean Sq"]
      f_value <- tab[1, "F value"]
      p_value <- tab[1, "Pr(>F)"]
      
      # Within-group (residual) values
      df_within <- tab[2, "Df"]
      ss_within <- tab[2, "Sum Sq"]
      ms_within <- tab[2, "Mean Sq"]
      
      # Total values
      df_total <- df_between + df_within
      ss_total <- ss_between + ss_within
      
      data.frame(
        Source = c(
          "Between Groups",
          "Within Groups (Error)",
          "Total"
        ),
        Df = c(
          df_between,
          df_within,
          df_total
        ),
        `Sum Sq` = round(c(
          ss_between,
          ss_within,
          ss_total
        ), 4),
        `Mean Sq` = round(c(
          ms_between,
          ms_within,
          NA_real_
        ), 4),
        `F Value` = round(c(
          f_value,
          NA_real_,
          NA_real_
        ), 4),
        `P Value` = c(
          signif(p_value, 4),
          NA,
          NA
        ),
        check.names = FALSE
      )
      
    } else {
      
      # Kruskal-Wallis does not use the ANOVA
      # sums-of-squares decomposition.
      data.frame(
        Test = "Kruskal-Wallis",
        Statistic = round(z$statistic, 4),
        Df = z$df1,
        `P Value` = signif(z$p_value, 4),
        Conclusion = if (z$p_value < alpha) {
          "Statistically significant"
        } else {
          "Not statistically significant"
        },
        check.names = FALSE
      )
    }
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # ------------------------------------------------------------
  # POST-HOC TESTS
  # Tukey HSD for ANOVA; pairwise Wilcoxon for Kruskal-Wallis
  # ------------------------------------------------------------
  
  anova_columns_posthoc_df <- reactive({
    d <- anova_columns_data()
    z <- anova_columns_test_info()
    conf <- (input$anova_columns_conf %||% 95) / 100
    alpha <- 1 - conf
    
    validate(
      need(!is.na(z$p_value) && z$p_value < alpha,
           "Post-hoc comparisons are shown only when the overall test is significant.")
    )
    
    if (z$method == "One-Way ANOVA") {
      fit <- anova_columns_model()
      tuk <- as.data.frame(
        TukeyHSD(fit, conf.level = conf)$Group
      )
      
      data.frame(
        Comparison = rownames(tuk),
        Mean_Difference = round(tuk$diff, 4),
        CI_Lower = round(tuk$lwr, 4),
        CI_Upper = round(tuk$upr, 4),
        Adjusted_P_Value = signif(tuk$`p adj`, 4),
        Method = "Tukey HSD",
        row.names = NULL
      )
    } else {
      pw <- pairwise.wilcox.test(
        d$Value, d$Group,
        p.adjust.method = "holm",
        exact = FALSE,
        conf.int = FALSE
      )
      
      mat <- pw$p.value
      idx <- which(!is.na(mat), arr.ind = TRUE)
      
      if (nrow(idx) == 0) {
        return(data.frame(
          Message = "No pairwise comparisons could be calculated."
        ))
      }
      
      data.frame(
        Comparison = paste(
          rownames(mat)[idx[, 1]],
          "vs",
          colnames(mat)[idx[, 2]]
        ),
        Adjusted_P_Value = signif(mat[idx], 4),
        Method = "Pairwise Wilcoxon rank-sum; Holm adjustment",
        row.names = NULL
      )
    }
  })
  
  
  output$anova_columns_posthoc_ui <- renderUI({
    z <- anova_columns_test_info()
    alpha <- 1 - (input$anova_columns_conf %||% 95) / 100
    
    if (!is.na(z$p_value) && z$p_value < alpha) {
      tagList(
        h4("Post-Hoc Pairwise Comparisons"),
        if (z$method == "One-Way ANOVA") {
          p("Tukey HSD comparisons with confidence intervals.")
        } else {
          p("Pairwise Wilcoxon rank-sum tests with Holm-adjusted p-values.")
        },
        tableOutput("anova_columns_posthoc_table")
      )
    } else {
      p("The overall test is not significant; post-hoc comparisons are not displayed.")
    }
  })
  
  output$anova_columns_posthoc_table <- renderTable({
    anova_columns_posthoc_df()
  }, striped = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------
  # NORMAL Q-Q PLOT
  # ------------------------------------------------------------
  
  output$anova_columns_qq_plot <- renderPlot({
    fit <- anova_columns_model()
    res <- residuals(fit)
    
    qqnorm(res, main = "Normal Q-Q Plot of ANOVA Residuals",
           pch = 19)
    qqline(res, col = "red", lwd = 2)
  })
  
  
  # ------------------------------------------------------------
  # RESIDUALS VS FITTED PLOT
  # ------------------------------------------------------------
  
  output$anova_columns_residual_plot <- renderPlot({
    fit <- anova_columns_model()
    
    plot(
      fitted(fit), residuals(fit),
      xlab = "Fitted Values",
      ylab = "Residuals",
      main = "Residuals vs Fitted Values",
      pch = 19
    )
    abline(h = 0, col = "red", lwd = 2)
    lines(
      lowess(fitted(fit), residuals(fit)),
      lwd = 2
    )
  })
  
  
  # ============================================================
  # DOWNLOAD COMPLETE ONE-WAY ANOVA REPORT AS WORD DOCUMENT
  # Includes all tables, conclusions, post-hoc results and plots
  # ============================================================
  
  output$download_anova_columns_word <- downloadHandler(
    
    filename = function() {
      paste0(
        "One_Way_ANOVA_Report_",
        format(Sys.Date(), "%Y%m%d"),
        ".docx"
      )
    },
    
    content = function(file) {
      
      req(requireNamespace("officer", quietly = TRUE))
      
      # --------------------------------------------------------
      # 1. COLLECT ALL EXISTING ANALYSIS OUTPUTS
      # --------------------------------------------------------
      
      summary_df <- anova_columns_summary_df()
      basic_anova_df <- anova_columns_results_df()
      assumptions_df <- anova_columns_assumptions_df <- {
        a <- anova_columns_assumption_info()
        
        data.frame(
          Assumption = c(
            "Residual normality (Shapiro-Wilk)",
            "Homogeneity of variance (median-centred test)"
          ),
          P_Value = c(
            a$normal_p,
            a$variance_p
          ),
          Conclusion = c(
            if (is.na(a$normal_p)) {
              "Could not assess"
            } else if (a$normal_ok) {
              "No significant evidence against normality"
            } else {
              "Possible violation of normality"
            },
            if (is.na(a$variance_p)) {
              "Could not assess"
            } else if (a$variance_ok) {
              "No significant evidence of unequal variances"
            } else {
              "Possible unequal variances"
            }
          ),
          check.names = FALSE
        )
      }
      
      ci_df <- anova_columns_mean_ci_df()
      test_info <- anova_columns_test_info()
      test_results_df <- anova_columns_test_results_df <- {
        # Reproduce the currently displayed overall test table.
        z <- test_info
        alpha <- 1 - (input$anova_columns_conf %||% 95) / 100
        
        if (z$method == "One-Way ANOVA") {
          
          fit <- anova_columns_model()
          tab <- summary(fit)[[1]]
          
          df_between <- tab[1, "Df"]
          df_within <- tab[2, "Df"]
          ss_between <- tab[1, "Sum Sq"]
          ss_within <- tab[2, "Sum Sq"]
          
          data.frame(
            Source = c(
              "Between Groups",
              "Within Groups (Error)",
              "Total"
            ),
            Df = c(
              df_between,
              df_within,
              df_between + df_within
            ),
            `Sum Sq` = round(
              c(ss_between, ss_within, ss_between + ss_within), 4
            ),
            `Mean Sq` = round(
              c(tab[1, "Mean Sq"], tab[2, "Mean Sq"], NA_real_), 4
            ),
            `F Value` = round(
              c(tab[1, "F value"], NA_real_, NA_real_), 4
            ),
            `P Value` = c(
              signif(tab[1, "Pr(>F)"], 4), NA, NA
            ),
            check.names = FALSE
          )
          
        } else {
          
          data.frame(
            Test = z$method,
            Statistic = round(z$statistic, 4),
            Df = z$df1,
            `P Value` = signif(z$p_value, 4),
            Conclusion = if (z$p_value < alpha) {
              "Statistically significant"
            } else {
              "Not statistically significant"
            },
            check.names = FALSE
          )
        }
      }
      
      # Post-hoc output may not exist when the overall test is
      # nonsignificant. Handle this without interrupting export.
      posthoc_df <- tryCatch(
        anova_columns_posthoc_df(),
        error = function(e) {
          data.frame(
            Information = paste(
              "Post-hoc comparisons unavailable:",
              conditionMessage(e)
            )
          )
        }
      )
      
      # --------------------------------------------------------
      # 2. CREATE DIAGNOSTIC PLOTS AS TEMPORARY PNG FILES
      # --------------------------------------------------------
      
      qq_file <- tempfile(fileext = ".png")
      residual_file <- tempfile(fileext = ".png")
      
      fit <- anova_columns_model()
      
      grDevices::png(
        qq_file, width = 1400, height = 1000, res = 150
      )
      
      qqnorm(
        residuals(fit),
        main = "Normal Q-Q Plot of ANOVA Residuals",
        pch = 19
      )
      qqline(residuals(fit), col = "red", lwd = 2)
      
      grDevices::dev.off()
      
      grDevices::png(
        residual_file, width = 1400, height = 1000, res = 150
      )
      
      plot(
        fitted(fit),
        residuals(fit),
        xlab = "Fitted Values",
        ylab = "Residuals",
        main = "Residuals vs Fitted Values",
        pch = 19
      )
      
      abline(h = 0, col = "red", lwd = 2)
      
      lines(
        lowess(fitted(fit), residuals(fit)),
        lwd = 2
      )
      
      grDevices::dev.off()
      
      # --------------------------------------------------------
      # 3. BUILD WORD DOCUMENT
      # --------------------------------------------------------
      
      doc <- officer::read_docx()
      
      add_heading <- function(doc, heading, level = 1) {
        officer::body_add_par(
          doc,
          heading,
          style = paste("heading", level)
        )
      }
      
      add_table <- function(doc, heading, dat) {
        
        doc <- add_heading(doc, heading, 2)
        
        if (is.null(dat) || !is.data.frame(dat) ||
            nrow(dat) == 0) {
          
          return(
            officer::body_add_par(
              doc,
              "No results available.",
              style = "Normal"
            )
          )
        }
        
        # Ensure the table can be written to Word.
        dat <- as.data.frame(dat)
        dat[] <- lapply(dat, function(x) {
          if (is.factor(x)) as.character(x) else x
        })
        
        officer::body_add_table(
          doc,
          value = dat
        )
      }
      
      doc <- add_heading(
        doc,
        "ONE-WAY ANOVA STATISTICAL ANALYSIS REPORT",
        1
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Report date:", Sys.Date()),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Selected statistical method:", test_info$method),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "Confidence level:",
          round(100 * (1 - anova_columns_assumption_info()$alpha)),
          "%"
        ),
        style = "Normal"
      )
      
      # --------------------------------------------------------
      # 4. ALL TABLES
      # --------------------------------------------------------
      
      doc <- add_table(
        doc,
        "1. Descriptive Statistics",
        summary_df
      )
      
      doc <- add_table(
        doc,
        "2. One-Way ANOVA Summary",
        basic_anova_df
      )
      
      doc <- add_table(
        doc,
        "3. Overall Test Results and ANOVA Table",
        test_results_df
      )
      
      doc <- add_table(
        doc,
        "4. Assumption Checks",
        assumptions_df
      )
      
      doc <- add_table(
        doc,
        "5. Confidence Intervals for Group Means",
        ci_df
      )
      
      doc <- add_table(
        doc,
        "6. Post-Hoc Pairwise Comparisons",
        posthoc_df
      )
      
      # --------------------------------------------------------
      # 5. DIAGNOSTIC PLOTS
      # --------------------------------------------------------
      
      doc <- add_heading(
        doc,
        "7. Normal Q-Q Plot",
        2
      )
      
      doc <- officer::body_add_img(
        doc,
        src = qq_file,
        width = 6,
        height = 4.3
      )
      
      doc <- add_heading(
        doc,
        "8. Residuals vs Fitted Plot",
        2
      )
      
      doc <- officer::body_add_img(
        doc,
        src = residual_file,
        width = 6,
        height = 4.3
      )
      
      # --------------------------------------------------------
      # 6. INTERPRETATION AND CONCLUSION
      # --------------------------------------------------------
      
      alpha <- anova_columns_assumption_info()$alpha
      p_value <- test_info$p_value
      
      doc <- add_heading(
        doc,
        "9. Statistical Interpretation",
        2
      )
      
      if (!is.na(p_value) && p_value < alpha) {
        
        conclusion <- paste(
          "The overall test was statistically significant at",
          "the selected significance level.",
          "There is evidence that at least one group differs",
          "from another. Refer to the post-hoc comparisons",
          "to identify the group differences."
        )
        
      } else if (!is.na(p_value)) {
        
        conclusion <- paste(
          "The overall test was not statistically significant",
          "at the selected significance level.",
          "There is insufficient evidence to conclude that",
          "the group means differ."
        )
        
      } else {
        
        conclusion <- "The overall test p-value could not be determined."
      }
      
      doc <- officer::body_add_par(
        doc,
        paste("Overall test:", test_info$method),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "Test statistic:",
          round(test_info$statistic, 4)
        ),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "P-value:",
          format.pval(p_value, digits = 4, eps = 0.001)
        ),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Conclusion:", conclusion),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "Note: Statistical significance does not necessarily",
          "indicate practical or clinical significance."
        ),
        style = "Normal"
      )
      
      # --------------------------------------------------------
      # 7. SAVE WORD FILE AND CLEAN TEMPORARY PLOTS
      # --------------------------------------------------------
      
      print(doc, target = file)
      
      unlink(c(qq_file, residual_file))
    }
  )
  
  
  
  
  
  #----------------------------------------------
  #One way ANOVA repeated measurements
  #--------------------------------------------
  
  
  # ============================================================
  # REPEATED-MEASURES ANALYSIS
  # Repeated-measures ANOVA / Friedman / Quade
  # Post-hoc tests, assumptions, plots, Word export
  # Requires: shiny, officer, PMCMRplus
  # Uses existing global_data()
  # ============================================================
  
  
  # ------------------------------------------------------------
  # 1. SELECT REPEATED-MEASUREMENT COLUMNS
  # ------------------------------------------------------------
  
  output$anova_rm_group_columns_ui <- renderUI({
    
    req(global_data(), input$anova_rm_num_groups)
    
    vars <- names(global_data())
    k <- as.integer(input$anova_rm_num_groups)
    
    validate(
      need(k >= 2, "Select at least two measurement columns."),
      need(length(vars) >= k,
           "The number of selected measurements exceeds the available columns.")
    )
    
    tagList(
      lapply(seq_len(k), function(i) {
        selectInput(
          inputId = paste0("anova_rm_column_", i),
          label = paste("Measurement", i, "- condition / time point"),
          choices = vars,
          selected = if (i <= length(vars)) vars[i] else vars[1]
        )
      })
    )
  })
  
  
  # ------------------------------------------------------------
  # 2. PREPARE PAIRED DATA
  # Each row is one subject; columns are repeated measurements.
  # Keep only subjects with complete observations in all selected
  # columns. Never independently remove values by condition.
  # ------------------------------------------------------------
  
  anova_rm_wide_data <- eventReactive(input$run_anova_rm, {
    
    req(global_data(), input$anova_rm_num_groups)
    
    df <- global_data()
    k <- as.integer(input$anova_rm_num_groups)
    
    selected <- vapply(seq_len(k), function(i) {
      x <- input[[paste0("anova_rm_column_", i)]]
      if (is.null(x)) "" else as.character(x)
    }, character(1))
    
    validate(
      need(length(selected) == k && all(nzchar(selected)),
           "Select a column for every measurement."),
      need(length(unique(selected)) == k,
           "Select a different column for each measurement.")
    )
    
    # Convert the selected measurement columns to numeric.
    wide <- as.data.frame(lapply(selected, function(v) {
      suppressWarnings(as.numeric(as.character(df[[v]])))
    }))
    
    names(wide) <- selected
    
    # Preserve original row number as subject identifier.
    wide$Subject <- seq_len(nrow(wide))
    
    # Exclude subjects with any missing or non-finite measurement.
    complete <- complete.cases(wide[, selected, drop = FALSE])
    
    complete <- complete &
      apply(wide[, selected, drop = FALSE], 1, function(x) {
        all(is.finite(x))
      })
    
    n_excluded <- sum(!complete)
    wide <- wide[complete, , drop = FALSE]
    
    validate(
      need(nrow(wide) >= 3,
           "At least three complete subjects are recommended."),
      need(all(vapply(wide[, selected, drop = FALSE],
                      function(x) length(unique(x)) > 1,
                      logical(1))),
           "At least one selected measurement has no variation.")
    )
    
    attr(wide, "selected_columns") <- selected
    attr(wide, "excluded_subjects") <- n_excluded
    
    wide
  })
  
  
  # ------------------------------------------------------------
  # 3. LONG FORMAT
  # ------------------------------------------------------------
  
  anova_rm_long_data <- reactive({
    
    wide <- anova_rm_wide_data()
    selected <- attr(wide, "selected_columns")
    
    n <- nrow(wide)
    
    long <- data.frame(
      Subject = rep(wide$Subject, times = length(selected)),
      Condition = factor(
        rep(selected, each = n),
        levels = selected
      ),
      Value = unlist(
        wide[, selected, drop = FALSE],
        use.names = FALSE
      )
    )
    
    long$Subject <- factor(long$Subject)
    
    long
  })
  
  
  # ------------------------------------------------------------
  # 4. CONFIDENCE LEVEL AND ALPHA
  # ------------------------------------------------------------
  
  anova_rm_alpha <- reactive({
    
    conf <- suppressWarnings(as.numeric(input$anova_rm_conf))
    
    if (length(conf) != 1 || is.na(conf) ||
        conf <= 0 || conf >= 100) {
      conf <- 95
    }
    
    1 - conf / 100
  })
  
  
  # ------------------------------------------------------------
  # 5. DESCRIPTIVE STATISTICS
  # ------------------------------------------------------------
  
  anova_rm_summary_df <- reactive({
    
    d <- anova_rm_long_data()
    
    groups <- split(d$Value, d$Condition)
    
    out <- data.frame(
      Condition = names(groups),
      N = vapply(groups, length, integer(1)),
      Mean = vapply(groups, mean, numeric(1)),
      SD = vapply(groups, sd, numeric(1)),
      Median = vapply(groups, median, numeric(1)),
      Min = vapply(groups, min, numeric(1)),
      Max = vapply(groups, max, numeric(1)),
      row.names = NULL,
      check.names = FALSE
    )
    
    num <- setdiff(names(out), c("Condition", "N"))
    out[num] <- lapply(out[num], round, 4)
    
    out
  })
  
  output$anova_rm_summary <- renderTable({
    anova_rm_summary_df()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # ------------------------------------------------------------
  # 6. REPEATED-MEASURES ANOVA MODEL
  # Subject is the blocking factor.
  # ------------------------------------------------------------
  
  anova_rm_model <- reactive({
    
    d <- anova_rm_long_data()
    
    validate(
      need(nlevels(d$Condition) >= 2,
           "At least two measurement conditions are required."),
      need(nlevels(d$Subject) >= 3,
           "Insufficient complete subjects.")
    )
    
    aov(Value ~ Condition + Error(Subject / Condition), data = d)
  })
  
  
  # ------------------------------------------------------------
  # 7. RESIDUAL NORMALITY AND DIAGNOSTICS
  # ------------------------------------------------------------
  
  # A fixed-effects model provides residuals for diagnostic plots.
  # This is not a substitute for checking sphericity.
  
  anova_rm_diagnostic_model <- reactive({
    
    d <- anova_rm_long_data()
    
    lm(Value ~ Subject + Condition, data = d)
  })
  
  
  anova_rm_assumptions_info <- reactive({
    
    d <- anova_rm_long_data()
    wide <- anova_rm_wide_data()
    
    fit <- anova_rm_diagnostic_model()
    res <- residuals(fit)
    
    alpha <- anova_rm_alpha()
    
    normal_p <- NA_real_
    
    if (length(res) >= 3 &&
        length(res) <= 5000 &&
        length(unique(res)) > 2) {
      
      normal_p <- tryCatch(
        shapiro.test(res)$p.value,
        error = function(e) NA_real_
      )
    }
    
    # Check normality of within-subject pairwise differences.
    # This is useful for repeated-measures comparisons.
    selected <- attr(wide, "selected_columns")
    
    difference_results <- lapply(
      combn(selected, 2, simplify = FALSE),
      function(pair) {
        
        dif <- wide[[pair[1]]] - wide[[pair[2]]]
        p <- NA_real_
        
        if (length(dif) >= 3 &&
            length(dif) <= 5000 &&
            length(unique(dif)) > 2) {
          
          p <- tryCatch(
            shapiro.test(dif)$p.value,
            error = function(e) NA_real_
          )
        }
        
        data.frame(
          Comparison = paste(pair, collapse = " - "),
          Shapiro_P = p,
          Conclusion = if (is.na(p)) {
            "Could not assess"
          } else if (p >= alpha) {
            "No significant evidence against normality"
          } else {
            "Possible non-normal differences"
          }
        )
      }
    )
    
    list(
      alpha = alpha,
      normal_p = normal_p,
      residual_normal = !is.na(normal_p) &&
        normal_p >= alpha,
      differences = do.call(rbind, difference_results),
      excluded = attr(wide, "excluded_subjects"),
      n = nrow(wide)
    )
  })
  
  
  output$anova_rm_assumptions <- renderTable({
    
    a <- anova_rm_assumptions_info()
    
    data.frame(
      Check = c(
        "Residual normality (Shapiro-Wilk)",
        "Subjects excluded for missing / invalid measurements",
        "Sphericity"
      ),
      Result = c(
        if (is.na(a$normal_p)) {
          "Could not assess"
        } else {
          paste0(
            "p = ", signif(a$normal_p, 4), "; ",
            if (a$residual_normal) {
              "no significant evidence against normality"
            } else {
              "possible departure from normality"
            }
          )
        },
        a$excluded,
        paste(
          "Not tested by this module;",
          "consider Mauchly's test and Greenhouse-Geisser correction"
        )
      ),
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------
  # 8. SELECT TEST
  # Repeated-measures ANOVA by default.
  # If explicitly requested, use Friedman or Quade.
  # ------------------------------------------------------------
  
  anova_rm_test_info <- reactive({
    
    wide <- anova_rm_wide_data()
    d <- anova_rm_long_data()
    
    method <- "Repeated-Measures ANOVA"
    selected_nonparam <- input$anova_rm_nonparam
    
    force_nonparam <- isTRUE(input$anova_rm_force_nonparam)
    
    if (force_nonparam) {
      
      validate(
        need(length(selected_nonparam) == 1,
             "Select exactly one nonparametric test: Friedman OR Quade.")
      )
      
      if (selected_nonparam == "friedman") {
        
        result <- friedman.test(Value ~ Condition | Subject, data = d)
        
        return(list(
          method = "Friedman test",
          statistic_name = "Friedman chi-squared",
          statistic = unname(result$statistic),
          df = unname(result$parameter),
          p_value = result$p.value
        ))
        
      } else if (selected_nonparam == "quade") {
        
        
        result <- stats::quade.test(
          y = d$Value,
          groups = d$Condition,
          blocks = d$Subject
        )
        
        return(list(
          method = "Quade test",
          statistic_name = "Quade F statistic",
          statistic = unname(result$statistic),
          df = unname(result$parameter),
          p_value = result$p.value
        ))
        
      }
    }
    
    # Base R repeated-measures ANOVA
    fit <- aov(Value ~ Condition + Error(Subject / Condition), data = d)
    
    tab <- summary(fit)
    
    # Extract the within-subject condition effect.
    condition_table <- NULL
    
    for (i in seq_along(tab)) {
      current <- tab[[i]][[1]]
      
      if (!is.null(current) &&
          "Condition" %in% rownames(current) &&
          "Pr(>F)" %in% colnames(current)) {
        
        condition_table <- current
        break
      }
    }
    
    validate(
      need(!is.null(condition_table),
           "Could not extract the repeated-measures ANOVA result.")
    )
    
    list(
      method = method,
      statistic_name = "F statistic",
      statistic = unname(condition_table["Condition", "F value"]),
      df = unname(condition_table["Condition", "Df"]),
      p_value = unname(condition_table["Condition", "Pr(>F)"])
    )
  })
  
  
  # ------------------------------------------------------------
  # 9. MAIN TEST RESULTS
  # ------------------------------------------------------------
  
  anova_rm_results_df <- reactive({
    
    z <- anova_rm_test_info()
    alpha <- anova_rm_alpha()
    
    conclusion <- if (is.na(z$p_value)) {
      "Could not determine statistical significance"
    } else if (z$p_value < alpha) {
      "Statistically significant difference between conditions"
    } else {
      "No statistically significant difference detected"
    }
    
    data.frame(
      Method = z$method,
      Statistic = z$statistic_name,
      Value = round(z$statistic, 4),
      `Degrees of Freedom` = if (is.null(z$df)) {
        NA
      } else {
        paste(z$df, collapse = ", ")
      },
      `P-value` = format.pval(z$p_value, digits = 4, eps = 0.001),
      Alpha = round(alpha, 4),
      Conclusion = conclusion,
      check.names = FALSE
    )
  })
  
  
  output$anova_rm_results <- renderTable({
    anova_rm_results_df()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # ------------------------------------------------------------
  # 10. CONFIDENCE INTERVALS FOR CONDITION MEANS
  # Marginal means with paired-subject standard errors.
  # These are descriptive intervals, not multiplicity-adjusted.
  # ------------------------------------------------------------
  
  anova_rm_ci_df <- reactive({
    
    wide <- anova_rm_wide_data()
    selected <- attr(wide, "selected_columns")
    
    conf <- as.numeric(input$anova_rm_conf) / 100
    n <- nrow(wide)
    alpha <- 1 - conf
    
    pieces <- lapply(selected, function(v) {
      
      x <- wide[[v]]
      m <- mean(x)
      s <- sd(x)
      se <- s / sqrt(n)
      
      margin <- qt(1 - alpha / 2, df = n - 1) * se
      
      data.frame(
        Condition = v,
        N = n,
        Mean = m,
        SD = s,
        SE = se,
        CI_Lower = m - margin,
        CI_Upper = m + margin
      )
    })
    
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    
    num <- setdiff(names(out), c("Condition", "N"))
    out[num] <- lapply(out[num], round, 4)
    
    out
  })
  
  
  output$anova_rm_ci <- renderTable({
    anova_rm_ci_df()
  }, striped = TRUE, bordered = TRUE)
  
  
  # ------------------------------------------------------------
  # 11. POST-HOC PAIRWISE COMPARISONS
  # Significant overall test required.
  # ANOVA: paired t-tests with Holm adjustment.
  # Friedman / Quade: Wilcoxon signed-rank with Holm adjustment.
  # ------------------------------------------------------------
  
  anova_rm_posthoc_df <- reactive({
    
    wide <- anova_rm_wide_data()
    selected <- attr(wide, "selected_columns")
    z <- anova_rm_test_info()
    alpha <- anova_rm_alpha()
    
    validate(
      need(!is.na(z$p_value) && z$p_value < alpha,
           "Post-hoc tests are available only after a significant overall test.")
    )
    
    pairs <- combn(selected, 2, simplify = FALSE)
    
    results <- lapply(pairs, function(pair) {
      
      x <- wide[[pair[1]]]
      y <- wide[[pair[2]]]
      
      if (z$method == "Repeated-Measures ANOVA") {
        
        test <- tryCatch(
          t.test(x, y, paired = TRUE,
                 conf.level = 1 - alpha),
          error = function(e) NULL
        )
        
        if (is.null(test)) {
          return(data.frame(
            Comparison = paste(pair, collapse = " vs "),
            Mean_Difference = mean(x - y),
            CI_Lower = NA_real_,
            CI_Upper = NA_real_,
            P_Value = NA_real_,
            Method = "Paired t-test"
          ))
        }
        
        data.frame(
          Comparison = paste(pair, collapse = " vs "),
          Mean_Difference = mean(x - y),
          CI_Lower = unname(test$conf.int[1]),
          CI_Upper = unname(test$conf.int[2]),
          P_Value = test$p.value,
          Method = "Paired t-test"
        )
        
      } else {
        
        test <- tryCatch(
          wilcox.test(
            x, y,
            paired = TRUE,
            exact = FALSE,
            conf.int = FALSE
          ),
          error = function(e) NULL
        )
        
        data.frame(
          Comparison = paste(pair, collapse = " vs "),
          Mean_Difference = mean(x - y),
          CI_Lower = NA_real_,
          CI_Upper = NA_real_,
          P_Value = if (is.null(test)) NA_real_ else test$p.value,
          Method = "Wilcoxon signed-rank test"
        )
      }
    })
    
    out <- do.call(rbind, results)
    
    out$Adjusted_P_Value <- p.adjust(out$P_Value, method = "holm")
    
    out$Conclusion <- ifelse(
      !is.na(out$Adjusted_P_Value) &
        out$Adjusted_P_Value < alpha,
      "Significant after Holm adjustment",
      "Not significant after Holm adjustment"
    )
    
    num <- intersect(
      c("Mean_Difference", "CI_Lower", "CI_Upper",
        "P_Value", "Adjusted_P_Value"),
      names(out)
    )
    
    out[num] <- lapply(out[num], round, 4)
    
    out
  })
  
  
  output$anova_rm_posthoc_ui <- renderUI({
    
    z <- anova_rm_test_info()
    alpha <- anova_rm_alpha()
    
    if (!is.na(z$p_value) && z$p_value < alpha) {
      
      tagList(
        p(paste(
          "Overall test is significant.",
          "Pairwise tests use Holm-adjusted p-values."
        )),
        tableOutput("anova_rm_posthoc_table")
      )
      
    } else {
      
      p("No post-hoc comparisons: the overall test is not significant.")
    }
  })
  
  
  output$anova_rm_posthoc_table <- renderTable({
    
    anova_rm_posthoc_df()
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # ------------------------------------------------------------
  # 12. NORMAL Q-Q PLOT
  # ------------------------------------------------------------
  
  output$anova_rm_qq_plot <- renderPlot({
    
    fit <- anova_rm_diagnostic_model()
    res <- residuals(fit)
    
    qqnorm(
      res,
      main = "Normal Q-Q Plot of Repeated-Measures Residuals",
      pch = 19
    )
    
    qqline(res, col = "red", lwd = 2)
    
  })
  
  
  # ------------------------------------------------------------
  # 13. RESIDUALS VS FITTED PLOT
  # ------------------------------------------------------------
  
  output$anova_rm_residual_plot <- renderPlot({
    
    fit <- anova_rm_diagnostic_model()
    
    plot(
      fitted(fit),
      residuals(fit),
      xlab = "Fitted Values",
      ylab = "Residuals",
      main = "Residuals vs Fitted Values",
      pch = 19
    )
    
    abline(h = 0, col = "red", lwd = 2)
    
    lines(
      lowess(fitted(fit), residuals(fit)),
      lwd = 2
    )
    
  })
  
  
  # ------------------------------------------------------------
  # 14. DOWNLOAD COMPLETE WORD REPORT
  # Tables + conclusions + assumptions + plots + post-hoc results
  # ------------------------------------------------------------
  
  output$download_anova_rm_word <- downloadHandler(
    
    filename = function() {
      paste0(
        "Repeated_Measures_Analysis_",
        format(Sys.Date(), "%Y%m%d"),
        ".docx"
      )
    },
    
    content = function(file) {
      
      req(requireNamespace("officer", quietly = TRUE))
      
      # Force evaluation and collect results.
      wide <- anova_rm_wide_data()
      summary_df <- anova_rm_summary_df()
      results_df <- anova_rm_results_df()
      ci_df <- anova_rm_ci_df()
      assumptions_df <- output_data <- {
        a <- anova_rm_assumptions_info()
        
        data.frame(
          Check = c(
            "Residual normality p-value",
            "Excluded incomplete subjects",
            "Sphericity"
          ),
          Result = c(
            if (is.na(a$normal_p)) {
              "Could not assess"
            } else {
              as.character(signif(a$normal_p, 4))
            },
            as.character(a$excluded),
            "Not tested; consider Mauchly's test and correction"
          ),
          check.names = FALSE
        )
      }
      
      z <- anova_rm_test_info()
      
      posthoc_df <- tryCatch(
        anova_rm_posthoc_df(),
        error = function(e) NULL
      )
      
      # Create temporary plot files.
      qq_file <- tempfile(fileext = ".png")
      resid_file <- tempfile(fileext = ".png")
      
      png(qq_file, width = 1400, height = 1000, res = 150)
      fit <- anova_rm_diagnostic_model()
      qqnorm(
        residuals(fit),
        main = "Normal Q-Q Plot of Residuals",
        pch = 19
      )
      qqline(residuals(fit), col = "red", lwd = 2)
      dev.off()
      
      png(resid_file, width = 1400, height = 1000, res = 150)
      plot(
        fitted(fit), residuals(fit),
        xlab = "Fitted Values",
        ylab = "Residuals",
        main = "Residuals vs Fitted",
        pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      lines(
        lowess(fitted(fit), residuals(fit)),
        lwd = 2
      )
      dev.off()
      
      doc <- officer::read_docx()
      
      doc <- officer::body_add_par(
        doc,
        "Repeated-Measures Statistical Analysis Report",
        style = "heading 1"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Report date:", Sys.Date()),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Selected test:", z$method),
        style = "heading 2"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste("Complete subjects:", nrow(wide)),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "Subjects excluded due to missing or invalid values:",
          attr(wide, "excluded_subjects")
        ),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        "Descriptive Statistics",
        style = "heading 2"
      )
      
      doc <- officer::body_add_table(
        doc,
        value = summary_df,
        style = "table_template"
      )
      
      doc <- officer::body_add_par(
        doc,
        "Overall Test Results",
        style = "heading 2"
      )
      
      doc <- officer::body_add_table(
        doc,
        value = results_df,
        style = "table_template"
      )
      
      doc <- officer::body_add_par(
        doc,
        "Confidence Intervals for Condition Means",
        style = "heading 2"
      )
      
      doc <- officer::body_add_table(
        doc,
        value = ci_df,
        style = "table_template"
      )
      
      doc <- officer::body_add_par(
        doc,
        "Assumption Checks and Diagnostics",
        style = "heading 2"
      )
      
      doc <- officer::body_add_table(
        doc,
        value = assumptions_df,
        style = "table_template"
      )
      
      # Include pairwise difference-normality checks.
      diff_checks <- anova_rm_assumptions_info()$differences
      
      doc <- officer::body_add_par(
        doc,
        "Normality of Pairwise Differences",
        style = "heading 3"
      )
      
      doc <- officer::body_add_table(
        doc,
        value = diff_checks,
        style = "table_template"
      )
      
      doc <- officer::body_add_par(
        doc,
        "Normal Q-Q Plot",
        style = "heading 2"
      )
      
      doc <- officer::body_add_img(
        doc,
        src = qq_file,
        width = 6,
        height = 4.3
      )
      
      doc <- officer::body_add_par(
        doc,
        "Residuals vs Fitted Plot",
        style = "heading 2"
      )
      
      doc <- officer::body_add_img(
        doc,
        src = resid_file,
        width = 6,
        height = 4.3
      )
      
      doc <- officer::body_add_par(
        doc,
        "Post-Hoc Pairwise Comparisons",
        style = "heading 2"
      )
      
      if (!is.null(posthoc_df)) {
        
        doc <- officer::body_add_table(
          doc,
          value = posthoc_df,
          style = "table_template"
        )
        
      } else {
        
        doc <- officer::body_add_par(
          doc,
          "No post-hoc table generated. The overall test may not be significant.",
          style = "Normal"
        )
      }
      
      doc <- officer::body_add_par(
        doc,
        "Interpretation",
        style = "heading 2"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "The overall method used was", z$method,
          "with significance level", round(anova_rm_alpha(), 4), "."
        ),
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        if (!is.na(z$p_value) && z$p_value < anova_rm_alpha()) {
          paste(
            "The overall test was statistically significant.",
            "At least one condition differs from another;",
            "refer to the adjusted post-hoc results."
          )
        } else {
          paste(
            "The overall test was not statistically significant.",
            "There is insufficient evidence of a difference between conditions."
          )
        },
        style = "Normal"
      )
      
      doc <- officer::body_add_par(
        doc,
        paste(
          "Note: the sphericity assumption was not tested by this module.",
          "For repeated-measures ANOVA with three or more conditions,",
          "consider Mauchly's test and Greenhouse-Geisser or",
          "Huynh-Feldt corrections. Nonparametric alternatives are",
          "available when selected explicitly."
        ),
        style = "Normal"
      )
      
      print(doc, target = file)
      
      unlink(c(qq_file, resid_file))
    }
  )
  
  
  
  
  
  
  
   #------------------------------------------
  #One way Anove (Enter data)
   #one way anova enter data----------------------------------------------
    # 1. Generate sample size inputs for each group
    # 1. Generate sample size inputs for each group
    output$sample_sizes_ui <- renderUI({
      req(input$num_groups)
      k <- input$num_groups
      
      lapply(1:k, function(i) {
        numericInput(
          inputId = paste0("n_", i), 
          label = paste("Observations in Group", i, ":"), 
          value = 5, 
          min = 2, 
          max = 100
        )
      })
    })
    
    # Helper function to safely get sample sizes
    get_n_vals <- reactive({
      k <- if(is.null(input$num_groups) || is.na(input$num_groups)) 2 else input$num_groups
      sapply(1:k, function(i) {
        val <- input[[paste0("n_", i)]]
        if (is.null(val) || is.na(val) || val < 2) 2 else val
      })
    })
    
    # 2. Generate the data entry table/grid
    output$data_table1_ui <- renderUI({
      k <- if(is.null(input$num_groups) || is.na(input$num_groups)) 2 else input$num_groups
      n_vals <- get_n_vals()
      
      fluidRow(
        lapply(1:k, function(j) {
          col_width <- max(2, floor(12 / k))
          column(
            width = col_width,
            tags$div(
              style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px; border: 1px solid #ddd;",
              h5(paste("Group", j), style = "margin-top: 0; font-weight: bold; text-align: center;"),
              lapply(1:n_vals[j], function(i) {
                numericInput(
                  inputId = paste0("val_", j, "_", i),
                  label = NULL,
                  value = 0
                )
              })
            )
          )
        })
      )
    })
    
    # 3. Compiled Data locked safely to the Action Button click
    compiled_data1 <- eventReactive(input$run_anovaone, {
      k <- if(is.null(input$num_groups) || is.na(input$num_groups)) 2 else input$num_groups
      n_vals <- get_n_vals()
      
      group_list <- list()
      for (j in 1:k) {
        vals <- sapply(1:n_vals[j], function(i) {
          val <- input[[paste0("val_", j, "_", i)]]
          if (is.null(val) || is.na(val)) 0 else val
        })
        vals <- unlist(vals)
        
        group_list[[j]] <- data.frame(
          Value = if(length(vals) > 0) vals else 0,
          Group = paste0("Group ", j)
        )
      }
      
      df <- do.call(rbind, group_list)
      
      validate(
        need(nrow(df) > 0, "Error: No data rows found. Please check your inputs."),
        need("Group" %in% names(df), "Error: Group column could not be generated."),
        need(length(unique(df$Group)) > 1, "Error: You must have at least 2 groups to run ANOVA.")
      )
      
      df$Group <- as.factor(df$Group)
      df
    }, ignoreNULL = FALSE)
    
    # Prevents errors on initial load before button click
    # 3. Descriptive Statistics Reactive
    desc_df1 <- reactive({
      df <- compiled_data1()
      req(df)
      
      agg_n <- aggregate(Value ~ Group, data = df, length)
      agg_mean <- aggregate(Value ~ Group, data = df, mean)
      agg_sd <- aggregate(Value ~ Group, data = df, sd)
      agg_min <- aggregate(Value ~ Group, data = df, min)
      agg_max <- aggregate(Value ~ Group, data = df, max)
      
      data.frame(
        Group = agg_n$Group,
        N = agg_n$Value,
        Mean = round(agg_mean$Value, 3),
        SD = round(agg_sd$Value, 3),
        Min = round(agg_min$Value, 3),
        Max = round(agg_max$Value, 3)
      )
    })
    
    output$desc_table1 <- renderTable({ desc_df1() })
    
    # 4. Assumption Checks Reactive (Safeguarded against zero-variance/identical values)
    assumptions_df1 <- reactive({
      df <- compiled_data1()
      req(df)
      
      fit <- aov(Value ~ Group, data = df)
      resids <- residuals(fit)
      
      # Check if all residual values are identical or variance is 0
      if (length(unique(resids)) <= 1 || sd(resids) == 0) {
        return(data.frame(
          Assumption_Test = c("Normality (Shapiro-Wilk on Residuals)", "Homogeneity of Variance (Bartlett's Test)"),
          Test_Statistic = c(NA, NA),
          P_Value = c(NA, NA),
          Conclusion = c("Cannot calculate (All data values/residuals are identical)", 
                         "Cannot calculate (All data values/residuals are identical)")
        ))
      }
      
      shap_res1 <- shapiro.test(resids)
      bart_res1 <- bartlett.test(Value ~ Group, data = df)
      
      data.frame(
        Assumption_Test = c("Normality (Shapiro-Wilk on Residuals)", "Homogeneity of Variance (Bartlett's Test)"),
        Test_Statistic = c(round(shap_res1$statistic, 3), round(bart_res1$statistic, 3)),
        P_Value = c(format.pval(shap_res1$p.value, digits = 4, eps = 0.001),
                    format.pval(bart_res1$p.value, digits = 4, eps = 0.001)),
        Conclusion = c(
          ifelse(shap_res1$p.value >= 0.05, "Assumption Met (p >= 0.05)", "Violation (p < 0.05)"),
          ifelse(bart_res1$p.value >= 0.05, "Assumption Met (p >= 0.05)", "Violation (p < 0.05)")
        )
      )
    })
    
    output$assumptions_table1 <- renderTable({ assumptions_df1() })
    
    # 4b. Non-Parametric (Kruskal-Wallis & Pairwise Wilcoxon) Logic
    kw_table_df1 <- reactive({
      df <- compiled_data1()
      req(df)
      
      # Safe check if all values in df are identical
      if (length(unique(df$Value)) <= 1) {
        return(data.frame(
          Test = "Kruskal-Wallis Rank Sum Test",
          Chi_Squared = NA,
          DF = NA,
          P_Value = NA,
          Result = "Cannot calculate (All data values are identical)"
        ))
      }
      
      kw_res <- kruskal.test(Value ~ Group, data = df)
      
      data.frame(
        Test = "Kruskal-Wallis Rank Sum Test",
        Chi_Squared = round(kw_res$statistic, 3),
        DF = as.numeric(kw_res$parameter),
        P_Value = format.pval(kw_res$p.value, digits = 4, eps = 0.001),
        Result = ifelse(kw_res$p.value < 0.05, "Significant", "Not Significant")
      )
    })
    
    output$kw_table1 <- renderTable({ kw_table_df1() })
    
    pw_wilcox_df <- reactive({
      df <- compiled_data1()
      req(df)
      pw <- pairwise.wilcox.test(df$Value, df$Group, p.adjust.method = "bonferroni")
      p_mat <- pw$p.value
      
      rows <- rownames(p_mat)
      cols <- colnames(p_mat)
      res_list <- list()
      idx <- 1
      for(i in seq_along(rows)) {
        for(j in seq_along(cols)) {
          val <- p_mat[i, j]
          if(!is.na(val)) {
            res_list[[idx]] <- data.frame(
              Comparison = paste(rows[i], "vs", cols[j]),
              Adjusted_P_Value = format.pval(val, digits = 4, eps = 0.001)
            )
            idx <- idx + 1
          }
        }
      }
      do.call(rbind, res_list)
    })
    
    output$pw_wilcox_table <- renderTable({ pw_wilcox_df() })
    
    output$nonparametric1_ui <- renderUI({
      df <- compiled_data1()
      req(df)
      
      fit <- aov(Value ~ Group, data = df)
      resids <- residuals(fit)
      
      # Ensure non-identical residuals before running Shapiro-Wilk test
      if (length(unique(resids)) > 1 && sd(resids) > 0) {
        shap_res1 <- shapiro.test(resids)
        
        if (!is.na(shap_res1$p.value) && shap_res1$p.value < 0.05) {
          kw_res <- kruskal.test(Value ~ Group, data = df)
          
          elements <- list(
            hr(),
            h3("2b. Non-Parametric Alternative (Kruskal-Wallis Test)"),
            p(tags$span(style = "color: #b94a48; font-weight: bold;", 
                        "Warning: Normality assumption violated (Shapiro-Wilk p < 0.05). Consider using the Kruskal-Wallis test below instead of standard ANOVA:")),
            tableOutput("kw_table1"), # Fixed ID mismatch
            downloadButton("dl_kw", "Download Kruskal-Wallis Table (.csv)", class = "btn-sm")
          )
          
          if (!is.na(kw_res$p.value) && kw_res$p.value < 0.05) {
            elements <- append(elements, list(
              br(), br(),
              h4("Pairwise Wilcoxon Rank-Sum Tests (Non-Parametric Post-Hoc)"),
              p("Since the Kruskal-Wallis test is significant, pairwise comparisons using Wilcoxon rank-sum tests (with Bonferroni adjustment) are shown below:"),
              tableOutput("pw_wilcox_table"),
              downloadButton("dl_pw_wilcox", "Download Pairwise Wilcoxon Table (.csv)", class = "btn-sm")
            ))
          }
          
          return(tagList(elements))
        }
      }
      return(NULL)
    })
    
    # 5. Boxplot Rendering and Download Handler
    output$anova_plot1 <- renderPlot({
      df <- compiled_data1()
      req(df)
      
      boxplot(Value ~ Group, data = df,
              col = rainbow(length(levels(df$Group)), alpha = 0.6),
              main = "Distribution of Values Across Groups",
              xlab = "Groups", ylab = "Values",
              las = 1, border = "darkblue", pch = 19)
      grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
    })
    
    
    # 6. Enhanced ANOVA Table Reactive
    anova_table_df1 <- reactive({
      df <- compiled_data1()
      req(df)
      
      fit <- aov(Value ~ Group, data = df)
      aov_summary <- summary(fit)[[1]]
      
      between_df <- aov_summary["Df"][1, ]
      between_ss <- aov_summary["Sum Sq"][1, ]
      between_ms <- aov_summary["Mean Sq"][1, ]
      f_val      <- aov_summary["F value"][1, ]
      p_val      <- aov_summary["Pr(>F)"][1, ]
      
      within_df  <- aov_summary["Df"][2, ]
      within_ss  <- aov_summary["Sum Sq"][2, ]
      within_ms  <- aov_summary["Mean Sq"][2, ]
      
      total_df   <- between_df + within_df
      total_ss   <- between_ss + within_ss
      
      result_str <- ifelse(p_val < 0.05, "Significant", "Not Significant")
      
      data.frame(
        Source = c("Between Groups", "Within Groups (Error)", "Total"),
        DF = c(between_df, within_df, total_df),
        Sum_of_Squares = c(round(between_ss, 3), round(within_ss, 3), round(total_ss, 3)),
        Mean_Square = c(round(between_ms, 3), round(within_ms, 3), NA),
        F_Value = c(round(f_val, 3), NA, NA),
        P_Value = c(format.pval(p_val, digits = 4, eps = 0.001), NA, NA),
        Result = c(result_str, "", "")
      )
    })
    
    output$anova_table1 <- renderTable({ anova_table_df1() })
    
    # 7. Post-Hoc (Tukey's HSD) Logic & Dynamic UI (Using numeric column indexes to prevent name bugs)
    tukey_table_df1 <- reactive({
      df <- compiled_data1()
      req(df)
      fit <- aov(Value ~ Group, data = df)
      tukey_res <- TukeyHSD(fit)
      res_mat <- as.data.frame(tukey_res$Group)
      
      data.frame(
        Comparison = rownames(res_mat),
        Mean_Difference = round(res_mat[, 1], 3),
        Lower_CI = round(res_mat[, 2], 3),
        Upper_CI = round(res_mat[, 3], 3),
        Adjusted_P_Value = format.pval(res_mat[, 4], digits = 4, eps = 0.001)
      )
    })
    
    output$tukey_table1 <- renderTable({ tukey_table_df1() })
    
    output$posthoc1_ui <- renderUI({
      df <- compiled_data1()
      req(df)
      fit <- aov(Value ~ Group, data = df)
      p_val <- summary(fit)[[1]]["Pr(>F)"][1, ]
      
      if (!is.na(p_val) && p_val < 0.05) {
        tagList(
          hr(),
          h3("5. Post-Hoc Test (Tukey's HSD)"),
          p("Since the ANOVA result is significant, pairwise group comparisons are shown below:"),
          tableOutput("tukey_table1"),
          downloadButton("dl_tukey1", "Download Post-Hoc Table (.csv)")
        )
      } else {
        tagList(
          hr(),
          h3("5. Post-Hoc Test (Tukey's HSD)"),
          p(em("ANOVA is not significant (p >= 0.05), so post-hoc pairwise comparisons are hidden."))
        )
      }
    })
    
    
    # ---------------------------------------------------------
    # COMPLETE ONE-WAY ANOVA WORD REPORT (.docx)
    # Requires: install.packages("officer")
    # ---------------------------------------------------------
    
    
    # ---------------------------------------------------------
    # COMPLETE ONE-WAY ANOVA WORD REPORT (.docx)
    # Requires: install.packages("officer")
    # ---------------------------------------------------------
    
    output$dl_anova_report <- downloadHandler(
      
      filename = function() {
        paste0("One_Way_ANOVA_Report_", Sys.Date(), ".docx")
      },
      
      content = function(file) {
        
        req(compiled_data1())
        
        # Create Word document
        doc <- officer::read_docx()
        
        # Helper to add headings
        add_heading <- function(doc, heading, level = 1) {
          officer::body_add_par(
            doc,
            value = heading,
            style = paste0("heading ", level)
          )
        }
        
        # Helper to add tables safely
        add_report_table <- function(doc, heading, data) {
          
          if (is.null(data) || !is.data.frame(data) ||
              nrow(data) == 0) {
            return(doc)
          }
          
          doc <- add_heading(doc, heading, 2)
          
          doc <- officer::body_add_table(
            doc,
            value = as.data.frame(data),
            style = "table_template"
          )
          
          officer::body_add_par(doc, "")
        }
        
        # -----------------------------------------------------
        # 1. Title and analysis information
        # -----------------------------------------------------
        
        doc <- officer::body_add_par(
          doc,
          "ONE-WAY ANOVA ANALYSIS REPORT",
          style = "centered"
        )
        
        doc <- officer::body_add_par(
          doc,
          paste("Report generated:", Sys.Date())
        )
        
        doc <- officer::body_add_par(
          doc,
          paste("Number of groups:", input$num_groups)
        )
        
        doc <- officer::body_add_par(doc, "")
        
        # -----------------------------------------------------
        # 2. Descriptive statistics
        # -----------------------------------------------------
        
        doc <- add_report_table(
          doc,
          "1. Descriptive Statistics",
          desc_df1()
        )
        
        # -----------------------------------------------------
        # 3. Assumption checks
        # -----------------------------------------------------
        
        doc <- add_report_table(
          doc,
          "2. ANOVA Assumption Checks",
          assumptions_df1()
        )
        
        # -----------------------------------------------------
        # 4. One-way ANOVA table
        # -----------------------------------------------------
        
        doc <- add_report_table(
          doc,
          "3. One-Way ANOVA Summary",
          anova_table_df1()
        )
        
        # -----------------------------------------------------
        # 5. Kruskal-Wallis and pairwise Wilcoxon, if applicable
        # -----------------------------------------------------
        
        assumption_results <- assumptions_df1()
        
        normality_row <- which(
          grepl("Normality", assumption_results$Assumption_Test)
        )
        
        normality_p <- NA_real_
        
        if (length(normality_row) > 0) {
          normality_p <- suppressWarnings(
            as.numeric(assumption_results$P_Value[normality_row[1]])
          )
        }
        
        # Include the non-parametric results when normality is violated
        if (!is.na(normality_p) && normality_p < 0.05) {
          
          doc <- add_report_table(
            doc,
            "4. Kruskal-Wallis Test",
            kw_table_df1()
          )
          
          kw_results <- kw_table_df1()
          
          if (nrow(kw_results) > 0 &&
              !is.na(kw_results$P_Value[1])) {
            
            kw_p <- suppressWarnings(
              as.numeric(kw_results$P_Value[1])
            )
            
            if (!is.na(kw_p) && kw_p < 0.05) {
              doc <- add_report_table(
                doc,
                "5. Pairwise Wilcoxon Rank-Sum Tests",
                pw_wilcox_df()
              )
            }
          }
        }
        
        # -----------------------------------------------------
        # 6. Tukey post-hoc results, if ANOVA is significant
        # -----------------------------------------------------
        
        anova_results <- anova_table_df1()
        anova_p <- suppressWarnings(
          as.numeric(anova_results$P_Value[1])
        )
        
        if (!is.na(anova_p) && anova_p < 0.05) {
          
          doc <- add_report_table(
            doc,
            "Post-Hoc Analysis: Tukey's HSD",
            tukey_table_df1()
          )
        }
        
        # -----------------------------------------------------
        # 7. Group distribution boxplot
        # -----------------------------------------------------
        
        doc <- add_heading(
          doc,
          "Group Distribution Boxplot",
          2
        )
        
        plot_file <- tempfile(fileext = ".png")
        
        grDevices::png(
          filename = plot_file,
          width = 1200,
          height = 800,
          res = 120
        )
        
        tryCatch({
          
          df <- compiled_data1()
          
          graphics::boxplot(
            Value ~ Group,
            data = df,
            col = grDevices::rainbow(
              length(levels(df$Group)),
              alpha = 0.6
            ),
            main = "Distribution of Values Across Groups",
            xlab = "Groups",
            ylab = "Values",
            las = 1,
            border = "darkblue",
            pch = 19
          )
          
          graphics::grid(
            nx = NA,
            ny = NULL,
            col = "lightgray",
            lty = "dotted"
          )
          
        }, finally = {
          grDevices::dev.off()
        })
        
        doc <- officer::body_add_img(
          doc,
          src = plot_file,
          width = 6.2,
          height = 4.2
        )
        
        # -----------------------------------------------------
        # 8. Save the complete Word document
        # -----------------------------------------------------
        
        print(doc, target = file)
        
        unlink(plot_file)
      }
    )
    
    
    #----------------------------------------
    #Two way ANOVA upload data
    #------------------------------------  
    # Dynamic UI Output Controls
    output$anova2_y <- renderUI({
      req(global_data())
      selectInput("anova2_y_var", "Select Response Variable (Numeric):",
                  choices = names(global_data()))
    })
    
    output$anova2_factor1 <- renderUI({
      req(global_data())
      selectInput("anova2_factor1_var", "Select Factor 1 Variable (Categorical):",
                  choices = names(global_data()))
    })
    
    output$anova2_factor2 <- renderUI({
      req(global_data())
      df_names <- names(global_data())
      default_sel <- if (length(df_names) > 1) df_names[2] else df_names[1]
      
      selectInput("anova2_factor2_var", "Select Factor 2 Variable (Categorical):",
                  choices = df_names,
                  selected = default_sel)
    })
    
    # ------------------------------------------------------------------------------
    # CALCULATIONS & MODELING
    # ------------------------------------------------------------------------------
    
    # 1. Primary Model Reactive
    anova_model <- reactive({
      req(global_data(), input$anova2_y_var, input$anova2_factor1_var, input$anova2_factor2_var)
      
      df <- global_data()
      
      req(input$anova2_y_var %in% names(df),
          input$anova2_factor1_var %in% names(df),
          input$anova2_factor2_var %in% names(df))
      
      validate(
        need(input$anova2_factor1_var != input$anova2_factor2_var, 
             "Factor 1 and Factor 2 must be different variables.")
      )
      
      y  <- suppressWarnings(as.numeric(df[[input$anova2_y_var]]))
      f1 <- factor(df[[input$anova2_factor1_var]])
      f2 <- factor(df[[input$anova2_factor2_var]])
      
      model_data <- na.omit(data.frame(Response = y, FactorA = f1, FactorB = f2))
      
      # Calculate unique factor combinations vs total observations
      n_obs <- nrow(model_data)
      n_cells <- length(unique(paste(model_data$FactorA, model_data$FactorB)))
      
      validate(
        need(n_obs > 0, "No complete non-NA observations found for selected variables.")
      )
      
      # If 1 observation per cell, interaction cannot be estimated
      if (isTRUE(input$interaction) && n_obs <= n_cells) {
        validate(
          need(FALSE, "Your dataset only has 1 observation per cell combination. Uncheck 'Include Interaction Term' to fit an additive model (Factor A + Factor B).")
        )
      }
      
      if (isTRUE(input$interaction)) {
        aov(Response ~ FactorA * FactorB, data = model_data)
      } else {
        aov(Response ~ FactorA + FactorB, data = model_data)
      }
    })
    # 2. Formatted ANOVA Output Dataframe (WITH TOTAL ROW & INTEGER DF)
    anova_df <- reactive({
      req(anova_model())
      fit <- anova_model()
      an_summary <- summary(fit)[[1]]
      
      term_names <- rownames(an_summary)
      term_names <- gsub("FactorA", input$anova2_factor1_var, term_names)
      term_names <- gsub("FactorB", input$anova2_factor2_var, term_names)
      
      # Base table values
      dfs <- as.integer(an_summary[["Df"]])
      sum_sqs <- an_summary[["Sum Sq"]]
      mean_sqs <- round(an_summary[["Mean Sq"]], 3)
      f_vals <- round(an_summary[["F value"]], 3)
      p_vals <- signif(an_summary[["Pr(>F)"]], 4)
      
      # Compute Totals
      total_df <- sum(dfs)
      total_ss <- sum(sum_sqs)
      
      # Build Data Frame (character formatted for Df to prevent decimal coercion)
      df_res <- data.frame(
        Source = c(trimws(term_names), "Total"),
        Df = c(as.character(dfs), as.character(total_df)),
        `Sum Sq` = c(round(sum_sqs, 3), round(total_ss, 3)),
        `Mean Sq` = c(as.character(mean_sqs), ""),
        `F Value` = c(as.character(f_vals), ""),
        `p-value` = c(as.character(p_vals), ""),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      df_res[is.na(df_res)] <- ""
      df_res
    })
    
    # 3. Summary Statistics Reactive
    summary_df <- reactive({
      req(global_data(), input$anova2_y_var, input$anova2_factor1_var, input$anova2_factor2_var)
      df <- global_data()
      req(input$anova2_y_var %in% names(df))
      
      y  <- suppressWarnings(as.numeric(df[[input$anova2_y_var]]))
      f1 <- factor(df[[input$anova2_factor1_var]])
      f2 <- factor(df[[input$anova2_factor2_var]])
      
      clean_df <- na.omit(data.frame(y = y, f1 = f1, f2 = f2))
      
      stats <- aggregate(y ~ f1 + f2, data = clean_df, FUN = function(x) {
        c(Count = length(x), Mean = round(mean(x), 3), SD = round(sd(x), 3))
      })
      
      res <- data.frame(
        Factor1 = stats$f1,
        Factor2 = stats$f2,
        Count = stats$y[, "Count"],
        Mean = stats$y[, "Mean"],
        SD = stats$y[, "SD"]
      )
      
      colnames(res)[1:2] <- c(input$anova2_factor1_var, input$anova2_factor2_var)
      res
    })
    
    #------------------------------------
    #Assumption check
    #-----------------------------------------
    
    # ------------------------------------------------------------------------------
    # CONSOLIDATED NUMERICAL ASSUMPTIONS TEST
    # ------------------------------------------------------------------------------
    
    assumption_result_df2 <- reactive({
      req(anova_model(), global_data(), input$anova2_y_var, input$anova2_factor1_var, input$anova2_factor2_var)
      
      alpha <- 0.05
      fit <- anova_model()
      res <- residuals(fit)
      
      # --- 1. Shapiro-Wilk Test (Normality) ---
      validate(
        need(length(res) >= 3, "At least 3 residuals are required for Shapiro-Wilk test."),
        need(length(res) <= 5000, "Shapiro-Wilk test requires sample size <= 5000.")
      )
      sw <- shapiro.test(res)
      w_stat     <- round(sw$statistic, 4)
      p_shap_fmt <- signif(sw$p.value, 4)
      status_shap <- ifelse(sw$p.value >= alpha, "Assumption Met", "Assumption Violated")
      
      # --- 2. Levene's Test (Homogeneity of Variance) ---
      df <- global_data()
      y  <- suppressWarnings(as.numeric(df[[input$anova2_y_var]]))
      f1 <- factor(df[[input$anova2_factor1_var]])
      f2 <- factor(df[[input$anova2_factor2_var]])
      
      clean_df <- na.omit(data.frame(y = y, f1 = f1, f2 = f2))
      clean_df$group <- interaction(clean_df$f1, clean_df$f2)
      
      lev <- car::leveneTest(y ~ group, data = clean_df)
      f_stat    <- round(lev[["F value"]][1], 4)
      p_lev_fmt <- signif(lev[["Pr(>F)"]][1], 4)
      status_lev <- ifelse(lev[["Pr(>F)"]][1] >= alpha, "Assumption Met", "Assumption Violated")
      
      # Construct table output
      data.frame(
        `Assumption`        = c("Normality of Residuals", "Homogeneity of Variance"),
        `Test Used`         = c("Shapiro-Wilk Test", "Levene's Test"),
        `Test Statistic`    = c(paste("W =", w_stat), paste("F =", f_stat)),
        `p-value`           = c(as.character(p_shap_fmt), as.character(p_lev_fmt)),
        `Alpha Threshold`   = c(as.character(alpha), as.character(alpha)),
        `Status`            = c(status_shap, status_lev),
        check.names         = FALSE,
        stringsAsFactors    = FALSE
      )
    })
    
    # Render Table Output
    output$assumption_result2 <- renderTable({
      assumption_result_df2()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Download Handler for Assumptions Table
    output$download_assumptions2 <- downloadHandler(
      filename = function() { paste0("anova_assumptions_checks_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(assumption_result_df(), file, row.names = FALSE) }
    )
    
    
    
    # ------------------------------------------------------------------------------
    # PLOT HELPER FUNCTIONS (REUSABLE FOR DISPLAY AND DOWNLOAD)
    # ------------------------------------------------------------------------------
    draw_interaction_plot <- function() {
      req(global_data(), input$anova2_y_var, input$anova2_factor1_var, input$anova2_factor2_var)
      df <- global_data()
      req(input$anova2_y_var %in% names(df))
      
      y  <- suppressWarnings(as.numeric(df[[input$anova2_y_var]]))
      f1 <- factor(df[[input$anova2_factor1_var]])
      f2 <- factor(df[[input$anova2_factor2_var]])
      
      interaction.plot(
        x.factor = f1,
        trace.factor = f2,
        response = y,
        fun = mean,
        type = "b",
        legend = TRUE,
        col = c("blue", "red", "green4", "purple", "orange"),
        pch = c(19, 17, 15, 18, 8),
        xlab = input$anova2_factor1_var,
        trace.label = input$anova2_factor2_var,
        ylab = paste("Mean of", input$anova2_y_var),
        main = paste("Interaction Plot:", input$anova2_factor1_var, "and", input$anova2_factor2_var)
      )
    }
    
    draw_residual_plot <- function() {
      req(anova_model())
      fit <- anova_model()
      
      # Check if residual degrees of freedom exist
      res_df <- df.residual(fit)
      validate(
        need(!is.null(res_df) && res_df > 0, 
             "Cannot plot residuals: Model has 0 residual degrees of freedom (e.g., 1 observation per group with interaction term). Uncheck 'Include Interaction Term' or select groups with replicated data.")
      )
      
      par(mfrow = c(1, 2))
      plot(fit, which = 1, main = "Residuals vs Fitted")
      plot(fit, which = 2, main = "Normal Q-Q Plot")
    }
    # ------------------------------------------------------------------------------
    # OUTPUT RENDERERS
    # ------------------------------------------------------------------------------
    output$summary_table2 <- renderTable({
      summary_df()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$anova_table2 <- renderTable({
      anova_df()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrrr")
    
    output$anova_summary2 <- renderPrint({
      req(anova_model())
      summary(anova_model())
    })
    
    output$interaction_plot <- renderPlot({
      draw_interaction_plot()
    })
    
    output$residual_plot <- renderPlot({
      draw_residual_plot()
    })
    
    # ------------------------------------------------------------------------------
    # DOWNLOAD HANDLERS
    # ------------------------------------------------------------------------------
    output$download_anova2 <- downloadHandler(
      filename = function() { paste0("two_way_anova_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(anova_df(), file, row.names = FALSE) }
    )
    
    output$download_summary2 <- downloadHandler(
      filename = function() { paste0("summary_statistics_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(summary_df(), file, row.names = FALSE) }
    )
    
    # Plot Download Handlers (PNG format)
    output$download_interaction_plot <- downloadHandler(
      filename = function() { paste0("interaction_plot_", Sys.Date(), ".png") },
      content = function(file) {
        png(file, width = 800, height = 600, res = 120)
        draw_interaction_plot()
        dev.off()
      }
    )
    
    output$download_residual_plot <- downloadHandler(
      filename = function() { paste0("residual_plots_", Sys.Date(), ".png") },
      content = function(file) {
        png(file, width = 900, height = 450, res = 120)
        draw_residual_plot()
        dev.off()
      }
    ) 
   
  #Two way one value cell--------------------------------  
    
    # Reactive values to hold grid data
    val <- reactiveValues(df = NULL)
    
    # Initialize or regenerate data matrix on dimension changes
    observe({
      req(input$num_rows, input$num_cols)
      r <- input$num_rows
      c <- input$num_cols
      
      mat <- matrix(round(rnorm(r * c, mean = 50, sd = 10), 1), nrow = r, ncol = c)
      df <- as.data.frame(mat)
      
      rownames(df) <- paste0("Row_Level_", 1:r)
      colnames(df) <- paste0("Col_Level_", 1:c)
      
      val$df <- df
    })
    
    # Render Editable Data Table
    output$data_grid <- renderDT({
      req(val$df)
      datatable(
        val$df,
        editable = "all",
        rownames = TRUE,
        options = list(dom = 't', pageLength = 50)
      )
    })
    
    # Observe Cell and Header Edits
    observeEvent(input$data_grid_cell_edit, {
      info <- input$data_grid_cell_edit
      r <- info$row
      c <- info$col
      v <- info$value
      
      if (r == 0 && c > 0) {
        colnames(val$df)[c] <- as.character(v)
      } else if (c == 0 && r > 0) {
        rownames(val$df)[r] <- as.character(v)
      } else if (r > 0 && c > 0) {
        num_v <- suppressWarnings(as.numeric(v))
        val$df[r, c] <- ifelse(is.na(num_v), 0, num_v)
      }
    })
    
    # Reshape wide grid into long format for ANOVA modeling
    long_data <- eventReactive(input$run_anova, {
      req(val$df)
      
      df_wide <- val$df
      row_names <- rownames(df_wide)
      col_names <- colnames(df_wide)
      
      long_df <- data.frame(
        Factor_A = factor(rep(row_names, times = ncol(df_wide)), levels = row_names),
        Factor_B = factor(rep(col_names, each = nrow(df_wide)), levels = col_names),
        Response = as.numeric(unlist(df_wide))
      )
      
      validate(
        need(nrow(long_df) > 0, "Error: Data table is empty."),
        need(sd(long_df$Response, na.rm = TRUE) > 0, "Error: All data values are identical.")
      )
      
      long_df
    }, ignoreNULL = FALSE)
    
    # Calculate Two-Way ANOVA Table
    anova_calc <- reactive({
      df <- long_data()
      req(df)
      
      fit <- aov(Response ~ Factor_A + Factor_B, data = df)
      res <- summary(fit)[[1]]
      
      f_a_name <- ifelse(nchar(trimws(input$factor_a_name)) > 0, input$factor_a_name, "Factor A")
      f_b_name <- ifelse(nchar(trimws(input$factor_b_name)) > 0, input$factor_b_name, "Factor B")
      
      data.frame(
        Source = c(f_a_name, f_b_name, "Residuals (Error)", "Total"),
        DF = c(res["Df"][1,], res["Df"][2,], res["Df"][3,], sum(res["Df"])),
        Sum_Sq = c(round(res["Sum Sq"][1,], 3), round(res["Sum Sq"][2,], 3), round(res["Sum Sq"][3,], 3), round(sum(res["Sum Sq"]), 3)),
        Mean_Sq = c(round(res["Mean Sq"][1,], 3), round(res["Mean Sq"][2,], 3), round(res["Mean Sq"][3,], 3), NA),
        F_Value = c(round(res["F value"][1,], 3), round(res["F value"][2,], 3), NA, NA),
        P_Value = c(format.pval(res["Pr(>F)"][1,], digits = 4, eps = 0.001), 
                    format.pval(res["Pr(>F)"][2,], digits = 4, eps = 0.001), NA, NA)
      )
    })
    
    output$anova_table <- renderTable({ anova_calc() }, na = "")
    
    # Plot Main Effects
    output$main_effects_plot <- renderPlot({
      df <- long_data()
      req(df)
      
      f_a_name <- ifelse(nchar(trimws(input$factor_a_name)) > 0, input$factor_a_name, "Factor A")
      f_b_name <- ifelse(nchar(trimws(input$factor_b_name)) > 0, input$factor_b_name, "Factor B")
      resp_name <- ifelse(nchar(trimws(input$response_name)) > 0, input$response_name, "Response")
      
      interaction.plot(
        x.factor = df$Factor_A,
        trace.factor = df$Factor_B,
        response = df$Response,
        fun = mean,
        type = "b",
        col = rainbow(length(levels(df$Factor_B))),
        pch = c(15, 16, 17, 18),
        xlab = f_a_name,
        ylab = paste("Mean of", resp_name),
        trace.label = f_b_name,
        main = "Main Effects / Profile Plot"
      )
    })
    
    # Post-Hoc Analysis (Tukey's HSD)
    tukey_res <- reactive({
      df <- long_data()
      req(df)
      fit <- aov(Response ~ Factor_A + Factor_B, data = df)
      TukeyHSD(fit)
    })
    
    # Helper Dataframes for Post-Hoc
    tukey_a_df <- reactive({
      res <- tukey_res()
      req(res)
      df_a <- as.data.frame(res$Factor_A)
      data.frame(
        Comparison = rownames(df_a),
        Diff = round(df_a[,1], 3),
        Lower_CI = round(df_a[,2], 3),
        Upper_CI = round(df_a[,3], 3),
        P_Adj = format.pval(df_a[,4], digits = 4, eps = 0.001)
      )
    })
    
    tukey_b_df <- reactive({
      res <- tukey_res()
      req(res)
      df_b <- as.data.frame(res$Factor_B)
      data.frame(
        Comparison = rownames(df_b),
        Diff = round(df_b[,1], 3),
        Lower_CI = round(df_b[,2], 3),
        Upper_CI = round(df_b[,3], 3),
        P_Adj = format.pval(df_b[,4], digits = 4, eps = 0.001)
      )
    })
    
    output$posthoc_ui <- renderUI({
      tagList(
        h3("Tukey's Honest Significant Difference (HSD)"),
        h4("Factor A Pairwise Comparisons"),
        renderTable({ tukey_a_df() }),
        hr(),
        h4("Factor B Pairwise Comparisons"),
        renderTable({ tukey_b_df() })
      )
    })
    
    # ============================================================================
    # WORD DOCUMENT DOWNLOAD HANDLER (.docx)
    # ============================================================================
    output$dl_word_report <- downloadHandler(
      filename = function() {
        paste0("Two_Way_ANOVA_Report_", Sys.Date(), ".docx")
      },
      content = function(file) {
        # 1. Save plot to a temporary image file
        temp_plot <- tempfile(fileext = ".png")
        png(temp_plot, width = 7, height = 5, units = "in", res = 300)
        df <- long_data()
        f_a_name <- ifelse(nchar(trimws(input$factor_a_name)) > 0, input$factor_a_name, "Factor A")
        f_b_name <- ifelse(nchar(trimws(input$factor_b_name)) > 0, input$factor_b_name, "Factor B")
        resp_name <- ifelse(nchar(trimws(input$response_name)) > 0, input$response_name, "Response")
        
        interaction.plot(
          x.factor = df$Factor_A,
          trace.factor = df$Factor_B,
          response = df$Response,
          fun = mean, type = "b",
          col = rainbow(length(levels(df$Factor_B))),
          pch = c(15, 16, 17, 18),
          xlab = f_a_name, ylab = paste("Mean of", resp_name),
          trace.label = f_b_name, main = "Main Effects Plot"
        )
        dev.off()
        
        # 2. Convert Data Frames to Flextables with basic styling
        ft_anova <- flextable(anova_calc()) %>% autofit()
        ft_tukey_a <- flextable(tukey_a_df()) %>% autofit()
        ft_tukey_b <- flextable(tukey_b_df()) %>% autofit()
        
        # 3. Build Word Document via officer
        doc <- read_docx() %>%
          body_add_par("Two-Way ANOVA Report", style = "heading 1") %>%
          body_add_par(paste("Generated on:", Sys.Date()), style = "Normal") %>%
          body_add_par("", style = "Normal") %>%
          
          # Section 1: ANOVA Table
          body_add_par("1. ANOVA Summary Table", style = "heading 2") %>%
          body_add_flextable(ft_anova) %>%
          body_add_par("", style = "Normal") %>%
          
          # Section 2: Main Effects Plot
          body_add_par("2. Main Effects Plot", style = "heading 2") %>%
          body_add_img(src = temp_plot, width = 6, height = 4.2) %>%
          body_add_par("", style = "Normal") %>%
          
          # Section 3: Post-Hoc Analysis
          body_add_par("3. Post-Hoc Pairwise Comparisons (Tukey's HSD)", style = "heading 2") %>%
          body_add_par("Factor A Comparisons:", style = "heading 3") %>%
          body_add_flextable(ft_tukey_a) %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("Factor B Comparisons:", style = "heading 3") %>%
          body_add_flextable(ft_tukey_b)
        
        # 4. Save file
        print(doc, target = file)
      }
    )
    
    
     
  #Two way anova enter data repeated measurement-------------------------------  
    
    safe_a <- reactive({
      val <- input$levels_a
      if (is.null(val) || is.na(val) || val < 2) 2 else val
    })
    
    safe_b <- reactive({
      val <- input$levels_b
      if (is.null(val) || is.na(val) || val < 2) 2 else val
    })
    
    safe_n <- reactive({
      val <- input$replicates2
      if (is.null(val) || is.na(val) || val < 2) 2 else val
    })
    
    # 1. Generate Two-Way Data Entry Grid safely
    output$grid_table_ui <- renderUI({
      a <- safe_a()
      b <- safe_b()
      n <- safe_n()
      
      table_cells <- lapply(1:a, function(i) {
        tags$tr(
          tags$td(tags$strong(paste("Var 1 - Level", i)), style = "padding: 10px; vertical-align: middle;"),
          lapply(1:b, function(j) {
            tags$td(
              tags$div(
                style = "background-color: #f9f9f9; padding: 8px; border-radius: 5px; border: 1px solid #ddd; margin: 2px;",
                tags$span(paste("Var 2 - Level", j), style = "font-size: 11px; font-weight: bold; display: block; text-align: center; margin-bottom: 4px;"),
                lapply(1:n, function(r) {
                  numericInput(
                    inputId = paste0("val_", i, "_", j, "_", r),
                    label = NULL,
                    value = 0,
                    width = "70px"
                  )
                })
              ),
              style = "padding: 5px;"
            )
          })
        )
      })
      
      header_cols <- lapply(1:b, function(j) {
        tags$th(paste("Var 2: Level", j), style = "text-align: center;")
      })
      
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(
            tags$th("Variable 1 / Variable 2"),
            header_cols
          )
        ),
        tags$tbody(table_cells)
      )
    })
    
    # Reactive helper to compile data frame with comprehensive error guards
    compiled_data <- reactive({
      req(input$run_anovatwo)
      a <- safe_a()
      b <- safe_b()
      n <- safe_n()
      
      df_list <- list()
      for (i in 1:a) {
        for (j in 1:b) {
          for (r in 1:n) {
            val <- input[[paste0("val_", i, "_", j, "_", r)]]
            if (is.null(val) || is.na(val)) val <- 0
            
            df_list[[length(df_list) + 1]] <- data.frame(
              Value = val,
              Var1 = paste0("Level ", i),
              Var2 = paste0("Level ", j)
            )
          }
        }
      }
      
      df <- do.call(rbind, df_list)
      validate(
        need(nrow(df) > 0, "Error: No data rows found. Please check your inputs."),
        need(length(unique(df$Var1)) > 1, "Error: Variable 1 must have at least 2 levels."),
        need(length(unique(df$Var2)) > 1, "Error: Variable 2 must have at least 2 levels.")
      )
      
      df$Var1 <- as.factor(df$Var1)
      df$Var2 <- as.factor(df$Var2)
      df
    })
    
    # 2. Descriptive Statistics Reactive
    desc_df <- reactive({
      df <- compiled_data()
      req(df)
      
      agg_n <- aggregate(Value ~ Var1 + Var2, data = df, length)
      agg_mean <- aggregate(Value ~ Var1 + Var2, data = df, mean)
      agg_sd <- aggregate(Value ~ Var1 + Var2, data = df, sd)
      
      data.frame(
        Var1 = agg_n$Var1,
        Var2 = agg_n$Var2,
        N = agg_n$Value,
        Mean = round(agg_mean$Value, 3),
        SD = round(agg_sd$Value, 3)
      )
    })
    
    output$desc_table2 <- renderTable({ desc_df() })
    
    # 3. Assumption Checks Reactive
    assumptions_df <- reactive({
      df <- compiled_data()
      req(df)
      
      fit <- aov(Value ~ Var1 * Var2, data = df)
      shap_res <- shapiro.test(residuals(fit))
      bart_res <- bartlett.test(Value ~ interaction(Var1, Var2), data = df)
      
      data.frame(
        Assumption_Test = c("Normality (Shapiro-Wilk on Residuals)", "Homogeneity of Variance (Bartlett's Test)"),
        Test_Statistic = c(round(shap_res$statistic, 3), round(bart_res$statistic, 3)),
        P_Value = c(format.pval(shap_res$p.value, digits = 4, eps = 0.001),
                    format.pval(bart_res$p.value, digits = 4, eps = 0.001)),
        Conclusion = c(
          ifelse(shap_res$p.value > 0.05, "Assumption Met (p >= 0.05)", "Violation (p < 0.05)"),
          ifelse(bart_res$p.value > 0.05, "Assumption Met (p >= 0.05)", "Violation (p < 0.05)")
        )
      )
    })
    
    output$assumptions_table2 <- renderTable({ assumptions_df() })
    
    # 4. Interaction Plot
    output$interaction_plot2 <- renderPlot({
      df <- compiled_data()
      req(df)
      
      interaction.plot(df$Var2, df$Var1, df$Value,
                       type = "b", col = c("blue", "red", "green", "purple"),
                       pch = 19, leg.bty = "o",
                       xlab = "Variable 2 Levels", ylab = "Mean Value",
                       trace.label = "Variable 1",
                       main = "Interaction Plot of Variable 1 and Variable 2")
      grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
    })
    
    output$dl_plot2 <- downloadHandler(
      filename = function() { paste0("twoway-interaction-plot-", Sys.Date(), ".png") },
      content = function(file) {
        png(file, width = 800, height = 600, res = 100)
        df <- compiled_data()
        interaction.plot(df$Var2, df$Var1, df$Value,
                         type = "b", col = c("blue", "red", "green", "purple"),
                         pch = 19, leg.bty = "o",
                         xlab = "Variable 2 Levels", ylab = "Mean Value",
                         trace.label = "Variable 1",
                         main = "Interaction Plot of Variable 1 and Variable 2")
        dev.off()
      }
    )
    
    # 5. Two-Way ANOVA Summary Table Reactive
    anova_table_df <- reactive({
      df <- compiled_data()
      req(df)
      
      fit <- aov(Value ~ Var1 * Var2, data = df)
      aov_summary <- summary(fit)[[1]]
      
      df_v1  <- aov_summary["Df"][1, ]
      ss_v1  <- aov_summary["Sum Sq"][1, ]
      ms_v1  <- aov_summary["Mean Sq"][1, ]
      f_v1   <- aov_summary["F value"][1, ]
      p_v1   <- aov_summary["Pr(>F)"][1, ]
      
      df_v2  <- aov_summary["Df"][2, ]
      ss_v2  <- aov_summary["Sum Sq"][2, ]
      ms_v2  <- aov_summary["Mean Sq"][2, ]
      f_v2   <- aov_summary["F value"][2, ]
      p_v2   <- aov_summary["Pr(>F)"][2, ]
      
      df_int <- aov_summary["Df"][3, ]
      ss_int <- aov_summary["Sum Sq"][3, ]
      ms_int <- aov_summary["Mean Sq"][3, ]
      f_int  <- aov_summary["F value"][3, ]
      p_int  <- aov_summary["Pr(>F)"][3, ]
      
      df_err <- aov_summary["Df"][4, ]
      ss_err <- aov_summary["Sum Sq"][4, ]
      ms_err <- aov_summary["Mean Sq"][4, ]
      
      total_df <- df_v1 + df_v2 + df_int + df_err
      total_ss <- ss_v1 + ss_v2 + ss_int + ss_err
      
      data.frame(
        Source = c("Variable 1", "Variable 2", "Interaction (Var1:Var2)", "Within (Error)", "Total"),
        DF = c(df_v1, df_v2, df_int, df_err, total_df),
        Sum_of_Squares = c(round(ss_v1, 3), round(ss_v2, 3), round(ss_int, 3), round(ss_err, 3), round(total_ss, 3)),
        Mean_Square = c(round(ms_v1, 3), round(ms_v2, 3), round(ms_int, 3), round(ms_err, 3), NA),
        F_Value = c(round(f_v1, 3), round(f_v2, 3), round(f_int, 3), NA, NA),
        P_Value = c(format.pval(p_v1, digits = 4, eps = 0.001),
                    format.pval(p_v2, digits = 4, eps = 0.001),
                    format.pval(p_int, digits = 4, eps = 0.001), NA, NA),
        Result = c(
          ifelse(p_v1 < 0.05, "Significant", "Not Significant"),
          ifelse(p_v2 < 0.05, "Significant", "Not Significant"),
          ifelse(p_int < 0.05, "Significant", "Not Significant"),
          "", ""
        )
      )
    })
    
    output$anova_table2 <- renderTable({ anova_table_df() })
    
    # 6. Post-Hoc Tukey HSD UI and Table
    tukey_table_df <- reactive({
      df <- compiled_data()
      req(df)
      fit <- aov(Value ~ Var1 * Var2, data = df)
      tukey_res <- TukeyHSD(fit)
      
      tukey_list <- list()
      for (nm in names(tukey_res)) {
        mat <- as.data.frame(tukey_res[[nm]])
        tukey_list[[nm]] <- data.frame(
          Type = nm,
          Comparison = rownames(mat),
          Mean_Difference = round(mat[, 1], 3),
          Lower_CI = round(mat[, 2], 3),
          Upper_CI = round(mat[, 3], 3),
          Adjusted_P_Value = format.pval(mat[, 4], digits = 4, eps = 0.001)
        )
      }
      do.call(rbind, tukey_list)
    })
    
    output$tukey_table2 <- renderTable({ tukey_table_df() })
    
    output$posthoc2_ui <- renderUI({
      df <- compiled_data()
      req(df)
      fit <- aov(Value ~ Var1 * Var2, data = df)
      aov_summary <- summary(fit)[[1]]
      
      p_v1  <- aov_summary["Pr(>F)"][1, ]
      p_v2  <- aov_summary["Pr(>F)"][2, ]
      p_int <- aov_summary["Pr(>F)"][3, ]
      
      if ((!is.na(p_v1) && p_v1 < 0.05) || (!is.na(p_v2) && p_v2 < 0.05) || (!is.na(p_int) && p_int < 0.05)) {
        tagList(
          hr(),
          h3("5. Post-Hoc Test (Tukey's HSD)"),
          p("Since at least one main effect or interaction is significant, pairwise comparisons are shown below:"),
          tableOutput("tukey_table2"),
          downloadButton("dl_tukey", "Download Post-Hoc Table (.csv)", class = "btn-sm")
        )
      } else {
        tagList(
          hr(),
          h3("5. Post-Hoc Test (Tukey's HSD)"),
          p(em("No significant main effects or interactions detected (p >= 0.05), so post-hoc comparisons are hidden."))
        )
      }
    })
    
    # 7. Download Handlers
    output$dl_desc2 <- downloadHandler(
      filename = function() { paste0("twoway-desc-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(desc_df(), file, row.names = FALSE) }
    )
    
    output$dl_assumptions2 <- downloadHandler(
      filename = function() { paste0("twoway-assumptions-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(assumptions_df(), file, row.names = FALSE) }
    )
    
    output$dl_anova2 <- downloadHandler(
      filename = function() { paste0("twoway-anova-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(anova_table_df(), file, row.names = FALSE) }
    )
    
    output$dl_tukey2 <- downloadHandler(
      filename = function() { paste0("twoway-tukey-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(tukey_table_df(), file, row.names = FALSE) }
    )
    
    
    
    
        # ==============================================================================
        # CHI-SQUARE TEST OF INDEPENDENCE (SERVER MODULE)
        # ==============================================================================
        
        # Populate dropdowns with ONLY valid categorical columns (<= 20 unique values)
    # ==============================================================================
    # 1. UI SELECT INPUT OBSERVERS & REACTIVE CONTINGENCY DATA
    # ==============================================================================
    observeEvent(global_data(), {
      req(global_data())
      df <- global_data()
      
      valid_cols <- names(df)[sapply(df, function(col) {
        is.factor(col) || is.character(col) || (length(unique(col)) <= 20)
      })]
      
      if (length(valid_cols) < 2) {
        showNotification("Uploaded data needs at least 2 categorical columns.", type = "warning")
      }
      
      updateSelectInput(session, "chi_x", choices = valid_cols, selected = valid_cols[1])
      updateSelectInput(session, "chi_y", choices = valid_cols, selected = valid_cols[2])
    })
    
    chi_data <- reactive({
      req(global_data(), input$chi_x, input$chi_y)
      
      validate(
        need(input$chi_x != input$chi_y, "Please select two DIFFERENT variables.")
      )
      
      df <- global_data()
      req(input$chi_x %in% names(df), input$chi_y %in% names(df))
      
      x_raw <- df[[input$chi_x]]
      y_raw <- df[[input$chi_y]]
      
      unique_x <- length(unique(na.omit(x_raw)))
      unique_y <- length(unique(na.omit(y_raw)))
      
      validate(
        need(unique_x >= 2 && unique_x <= 20, paste0("Variable '", input$chi_x, "' must have between 2 and 20 unique categories (found ", unique_x, ").")),
        need(unique_y >= 2 && unique_y <= 20, paste0("Variable '", input$chi_y, "' must have between 2 and 20 unique categories (found ", unique_y, ")."))
      )
      
      x <- as.factor(x_raw)
      y <- as.factor(y_raw)
      valid <- complete.cases(x, y)
      
      table(x[valid], y[valid])
    })
    
    # ==============================================================================
    # 2. OBSERVED FREQUENCIES TABLE
    # ==============================================================================
    # ==============================================================================
    # 2. OBSERVED FREQUENCIES TABLE (WITH TOTALS)
    # ==============================================================================
    # ==============================================================================
    # 2. OBSERVED FREQUENCIES TABLE (BASE)
    # ==============================================================================
    chi_observed_df <- reactive({
      req(chi_data())
      tbl_with_totals <- addmargins(chi_data(), FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(tbl_with_totals)
    })
    
    output$chi_table <- renderTable({
      chi_observed_df()
    }, rownames = TRUE)
    
    # ==============================================================================
    # 3. PERCENTAGE TABLES (REACTIVES)
    # ==============================================================================
    
    # Row Percentages Table
    chi_row_pct_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      pct_tbl <- prop.table(tbl, margin = 1) * 100
      # Add row/col margins safely rounded to 2 decimal places
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # Column Percentages Table
    chi_col_pct_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      pct_tbl <- prop.table(tbl, margin = 2) * 100
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # Cell Percentages Table
    chi_cell_pct_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      pct_tbl <- prop.table(tbl) * 100
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # ==============================================================================
    # 4. DYNAMIC UI RENDERING FOR PERCENTAGE TABLES
    # ==============================================================================
    output$chi_percentage_tables_ui <- renderUI({
      opts <- input$chi_percentages
      req(length(opts) > 0)
      
      tagList(
        lapply(opts, function(opt) {
          if (opt == "row") {
            tagList(
              hr(),
              h4("Row Percentages (%)"),
              tableOutput("chi_row_pct_table")
            )
          } else if (opt == "col") {
            tagList(
              hr(),
              h4("Column Percentages (%)"),
              tableOutput("chi_col_pct_table")
            )
          } else if (opt == "cell") {
            tagList(
              hr(),
              h4("Cell Percentages (%)"),
              tableOutput("chi_cell_pct_table")
            )
          }
        })
      )
    })
    
    # Render outputs for the dynamically generated tables
    output$chi_row_pct_table <- renderTable({ chi_row_pct_df() }, rownames = TRUE)
    output$chi_col_pct_table <- renderTable({ chi_col_pct_df() }, rownames = TRUE)
    output$chi_cell_pct_table <- renderTable({ chi_cell_pct_df() }, rownames = TRUE) 
    output$chi_table <- renderTable({
      chi_observed_df()
    }, rownames = TRUE)
    
    output$dl_chi_table <- downloadHandler(
      filename = function() { paste0("chi-observed-frequencies-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(chi_observed_df(), file, row.names = TRUE)
      }
    )
    
    # ==============================================================================
    # 4. EXPECTED FREQUENCIES TABLE (WITH TOTALS)
    # ==============================================================================
    chi_expected_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      req(all(dim(tbl) >= 2))
      
      chi_test <- suppressWarnings(chisq.test(tbl))
      
      # Calculate margins before rounding to preserve precision
      exp_with_totals <- addmargins(chi_test$expected, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(exp_with_totals, 2))
    })
    
    output$chi_expected <- renderTable({
      chi_expected_df()
    }, rownames = TRUE)
    
    output$dl_chi_expected <- downloadHandler(
      filename = function() { paste0("chi-expected-frequencies-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(chi_expected_df(), file, row.names = TRUE)
      }
    )
    
    
    # ==============================================================================
    # 3. CHI-SQUARE TEST RESULTS TABLE
    # ==============================================================================
    chi_result_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      
      validate(
        need(all(dim(tbl) >= 2), "Need at least 2 categories per variable."),
        need(sum(tbl) > 0, "Table has no data."),
        need(!any(rowSums(tbl) == 0), "Row with zero count detected."),
        need(!any(colSums(tbl) == 0), "Column with zero count detected.")
      )
      
      chi_nocorr <- tryCatch(
        suppressWarnings(chisq.test(tbl, correct = FALSE)),
        error = function(e) NULL
      )
      
      is_2x2 <- identical(as.integer(dim(tbl)), c(2L, 2L))
      chi_corr <- if (is_2x2) {
        tryCatch(suppressWarnings(chisq.test(tbl, correct = TRUE)), error = function(e) NULL)
      } else {
        NULL
      }
      
      fisher <- tryCatch({
        if (is_2x2) {
          fisher.test(tbl)
        } else if (sum(tbl) < 200) {
          fisher.test(tbl, workspace = 2e5)
        } else {
          fisher.test(tbl, simulate.p.value = TRUE, B = 2000)
        }
      }, error = function(e) NULL)
      
      conf_level <- if (!is.null(input$chi_conf)) input$chi_conf else 0.95
      alpha <- if (conf_level > 1) 1 - (conf_level / 100) else 1 - conf_level
      
      stat_nocorr <- if (!is.null(chi_nocorr)) as.character(round(unname(chi_nocorr$statistic), 4)) else "N/A"
      df_nocorr   <- if (!is.null(chi_nocorr)) as.character(unname(chi_nocorr$parameter)) else "N/A"
      p_nocorr    <- if (!is.null(chi_nocorr)) format.pval(chi_nocorr$p.value, digits = 4, eps = 0.0001) else "N/A"
      conc_nocorr <- if (!is.null(chi_nocorr)) ifelse(chi_nocorr$p.value < alpha, "Significant", "Not Significant") else "N/A"
      
      stat_corr <- if (!is.null(chi_corr)) as.character(round(unname(chi_corr$statistic), 4)) else "N/A (2x2 only)"
      df_corr   <- if (!is.null(chi_corr)) as.character(unname(chi_corr$parameter)) else "N/A"
      p_corr    <- if (!is.null(chi_corr)) format.pval(chi_corr$p.value, digits = 4, eps = 0.0001) else "N/A"
      conc_corr <- if (!is.null(chi_corr)) ifelse(chi_corr$p.value < alpha, "Significant", "Not Significant") else "N/A"
      
      p_fish    <- if (!is.null(fisher)) format.pval(fisher$p.value, digits = 4, eps = 0.0001) else "Skipped (Table too large)"
      conc_fish <- if (!is.null(fisher)) ifelse(fisher$p.value < alpha, "Significant", "Not Significant") else "N/A"
      
      res_df <- data.frame(
        "Test Method" = c(
          "Chi-Square (Without Correction)",
          "Chi-Square (With Yates' Correction)",
          "Fisher's Exact Test"
        ),
        "Statistic (χ²)" = c(stat_nocorr, stat_corr, "N/A"),
        "df" = c(df_nocorr, df_corr, "N/A"),
        "p-value" = c(p_nocorr, p_corr, p_fish),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      res_df[[paste0("Conclusion (α = ", round(alpha, 3), ")")]] <- c(conc_nocorr, conc_corr, conc_fish)
      res_df
    })
    
    output$chi_result_table <- renderTable({
      chi_result_df()
    }, bordered = TRUE, striped = TRUE, hover = TRUE, width = "100%", align = "c")
    
    output$dl_chi_result <- downloadHandler(
      filename = function() { paste0("chi-square-test-results-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(chi_result_df(), file, row.names = FALSE)
      }
    )
    
    
    # ==============================================================================
    # 5. CRAMÉR'S V EFFECT SIZE TABLE
    # ==============================================================================
    cramer_result_df <- reactive({
      req(chi_data())
      tbl <- chi_data()
      req(all(dim(tbl) >= 2), sum(tbl) > 0)
      
      chi <- suppressWarnings(chisq.test(tbl))
      n <- sum(tbl)
      chi_sq <- as.numeric(chi$statistic)
      k <- min(nrow(tbl), ncol(tbl))
      
      V <- if (k > 1 && n > 0) sqrt(chi_sq / (n * (k - 1))) else NA
      
      interp <- if (is.na(V)) {
        "Not defined"
      } else if (V < 0.1) {
        "Negligible / Very weak association"
      } else if (V < 0.3) {
        "Weak association"
      } else if (V < 0.5) {
        "Moderate association"
      } else {
        "Strong association"
      }
      
      data.frame(
        "Metric" = "Cramér's V",
        "Value" = ifelse(is.na(V), "N/A", as.character(round(V, 4))),
        "Interpretation" = interp,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    output$cramer_result_table <- renderTable({
      cramer_result_df()
    }, bordered = TRUE, striped = TRUE, hover = TRUE)
    
    output$dl_cramer_result <- downloadHandler(
      filename = function() { paste0("cramers-v-effect-size-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(cramer_result_df(), file, row.names = FALSE)
      }
    )
    # ------------------------------------------------------------------------------
    # SECTION 2: MANUAL CONTINGENCY MATRIX INPUT (SERVER LOGIC)
    # ------------------------------------------------------------------------------
    
    safe_chi <- function(mat) {
      tryCatch(
        suppressWarnings(chisq.test(mat)),
        error = function(e) NULL
      )
    }
    
    output$table_ui <- renderUI({
      req(input$create, input$rows, input$cols)
      
      rows <- as.integer(input$rows)
      cols <- as.integer(input$cols)
      
      tags$table(
        style = "border-collapse: collapse;",
        lapply(1:rows, function(i) {
          tags$tr(
            lapply(1:cols, function(j) {
              tags$td(
                style = "padding:5px;",
                numericInput(
                  inputId = paste0("cell_", i, "_", j),
                  label = NULL,
                  value = 0,
                  min = 0,
                  width = "80px"
                )
              )
            })
          )
        })
      )
    })
    
    data_matrix <- eventReactive(input$analyze, {
      req(input$create, input$rows, input$cols)
      
      rows <- as.integer(input$rows)
      cols <- as.integer(input$cols)
      
      mat <- matrix(0, nrow = rows, ncol = cols)
      
      for (i in 1:rows) {
        for (j in 1:cols) {
          val <- input[[paste0("cell_", i, "_", j)]]
          val <- suppressWarnings(as.numeric(val))
          mat[i, j] <- if (is.na(val) || val < 0) 0 else val
        }
      }
      
      rownames(mat) <- paste("Row", 1:rows)
      colnames(mat) <- paste("Col", 1:cols)
      storage.mode(mat) <- "double"
      mat
    })
    
    observeEvent(input$analyze, {
      if (is.null(input$create) || input$create == 0) {
        showNotification("⚠ Please click 'Create Table' first", type = "error")
      }
    })
    
    # ==============================================================================
    # 1. OBSERVED TABLE (WITH TOTALS & DOWNLOAD)
    # ==============================================================================
    manual_observed_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      tbl_totals <- addmargins(mat, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(tbl_totals)
    })
    
    output$observed <- renderTable({
      manual_observed_df()
    }, rownames = TRUE)
    
    output$dl_manual_observed <- downloadHandler(
      filename = function() { paste0("manual-observed-counts-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(manual_observed_df(), file, row.names = TRUE)
      }
    )
    
    # Row Percentages Table
    manual_row_pct_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      pct_tbl <- prop.table(mat, margin = 1) * 100
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # Column Percentages Table
    manual_col_pct_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      pct_tbl <- prop.table(mat, margin = 2) * 100
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # Cell Percentages Table
    manual_cell_pct_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      pct_tbl <- prop.table(mat) * 100
      tbl_with_totals <- addmargins(pct_tbl, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(tbl_with_totals, 2))
    })
    
    # ==============================================================================
    # 3. DYNAMIC UI RENDERING
    # ==============================================================================
    output$manual_percentage_tables_ui <- renderUI({
      opts <- input$manual_percentages
      req(length(opts) > 0)
      
      tagList(
        lapply(opts, function(opt) {
          if (opt == "row") {
            tagList(
              hr(),
              h4("Row Percentages (%)"),
              tableOutput("manual_row_pct_table")
            )
          } else if (opt == "col") {
            tagList(
              hr(),
              h4("Column Percentages (%)"),
              tableOutput("manual_col_pct_table")
            )
          } else if (opt == "cell") {
            tagList(
              hr(),
              h4("Cell Percentages (%)"),
              tableOutput("manual_cell_pct_table")
            )
          }
        })
      )
    })
    
    # Render outputs for the tables
    output$manual_row_pct_table <- renderTable({ manual_row_pct_df() }, rownames = TRUE)
    output$manual_col_pct_table <- renderTable({ manual_col_pct_df() }, rownames = TRUE)
    output$manual_cell_pct_table <- renderTable({ manual_cell_pct_df() }, rownames = TRUE)
    
    
    # ==============================================================================
    # 2. EXPECTED TABLE (WITH TOTALS & DOWNLOAD)
    # ==============================================================================
    manual_expected_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      test <- safe_chi(mat)
      req(!is.null(test))
      
      exp_totals <- addmargins(test$expected, FUN = list(Total = sum), quiet = TRUE)
      as.data.frame.matrix(round(exp_totals, 2))
    })
    
    output$expected <- renderTable({
      manual_expected_df()
    }, rownames = TRUE)
    
    output$dl_manual_expected <- downloadHandler(
      filename = function() { paste0("manual-expected-counts-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(manual_expected_df(), file, row.names = TRUE)
      }
    )
    
    # ==============================================================================
    # 3. TEST RESULTS TABLE (WITH DOWNLOAD)
    # ==============================================================================
    manual_results_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      
      validate(
        need(sum(mat) > 0, "Enter valid positive data."),
        need(!any(rowSums(mat) == 0), "Rows with all zeros are not allowed."),
        need(!any(colSums(mat) == 0), "Columns with all zeros are not allowed.")
      )
      
      chi_nocorr <- tryCatch(suppressWarnings(chisq.test(mat, correct = FALSE)), error = function(e) NULL)
      chi_corr   <- if (all(dim(mat) == 2)) tryCatch(suppressWarnings(chisq.test(mat, correct = TRUE)), error = function(e) NULL) else NULL
      fisher     <- tryCatch(
        fisher.test(mat),
        error = function(e) {
          tryCatch(fisher.test(mat, simulate.p.value = TRUE, B = 10000), error = function(e2) NULL)
        }
      )
      
      data.frame(
        "Test Method" = c(
          "Chi-Square (Without Correction)",
          "Chi-Square (With Yates' Correction)",
          "Fisher's Exact Test"
        ),
        "Statistic (χ²)" = c(
          if (!is.null(chi_nocorr)) as.character(round(chi_nocorr$statistic, 4)) else "N/A",
          if (!is.null(chi_corr)) as.character(round(chi_corr$statistic, 4)) else "N/A (2x2 only)",
          "N/A"
        ),
        "df" = c(
          if (!is.null(chi_nocorr)) as.character(chi_nocorr$parameter) else "N/A",
          if (!is.null(chi_corr)) as.character(chi_corr$parameter) else "N/A",
          "N/A"
        ),
        "p-value" = c(
          if (!is.null(chi_nocorr)) format.pval(chi_nocorr$p.value, digits = 4, eps = 0.0001) else "N/A",
          if (!is.null(chi_corr)) format.pval(chi_corr$p.value, digits = 4, eps = 0.0001) else "N/A",
          if (!is.null(fisher)) format.pval(fisher$p.value, digits = 4, eps = 0.0001) else "N/A"
        ),
        "Conclusion (α = 0.05)" = c(
          if (!is.null(chi_nocorr)) ifelse(chi_nocorr$p.value < 0.05, "Significant", "Not Significant") else "N/A",
          if (!is.null(chi_corr)) ifelse(chi_corr$p.value < 0.05, "Significant", "Not Significant") else "N/A",
          if (!is.null(fisher)) ifelse(fisher$p.value < 0.05, "Significant", " Not Significant") else "N/A"
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    output$test_result_table <- renderTable({
      manual_results_df()
    }, digits = 4, bordered = TRUE, striped = TRUE, hover = TRUE)
    
    output$dl_manual_results <- downloadHandler(
      filename = function() { paste0("manual-test-results-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(manual_results_df(), file, row.names = FALSE)
      }
    )
    
    # ==============================================================================
    # 4. CRAMÉR'S V EFFECT SIZE TABLE (WITH DOWNLOAD)
    # ==============================================================================
    manual_cramer_df <- reactive({
      req(data_matrix())
      mat <- data_matrix()
      
      if (sum(mat) == 0 || min(dim(mat)) < 2) {
        v_val <- "NA"
        interp <- "Invalid table dimension or zero sum"
      } else {
        test <- tryCatch(suppressWarnings(chisq.test(mat, correct = FALSE)), error = function(e) NULL)
        if (is.null(test)) {
          v_val <- "NA"
          interp <- "Test computation failed"
        } else {
          chi <- as.numeric(test$statistic)
          n <- sum(mat)
          k <- min(dim(mat))
          v <- sqrt(chi / (n * (k - 1)))
          
          if (is.nan(v) || is.na(v)) {
            v_val <- "NA"
            interp <- "Undefined"
          } else {
            v_val <- as.character(round(v, 4))
            interp <- if (v < 0.1) {
              "Negligible / Very weak association"
            } else if (v < 0.3) {
              "Weak association"
            } else if (v < 0.5) {
              "Moderate association"
            } else {
              "Strong association"
            }
          }
        }
      }
      
      data.frame(
        "Metric" = "Cramér's V",
        "Value" = v_val,
        "Interpretation" = interp,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    output$cramer_table <- renderTable({
      manual_cramer_df()
    }, bordered = TRUE, striped = TRUE, hover = TRUE)
    
    output$dl_manual_cramer <- downloadHandler(
      filename = function() { paste0("manual-cramers-v-", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(manual_cramer_df(), file, row.names = FALSE)
      }
    )    
    
    
    # ============================================================
    # CHARTS
    # ============================================================
    # CHARTS & GRAPHS — REORGANIZED SERVER CODE
    # Each graph's observers, reactive data, table, editing, plotting,
    # and download handler are kept together.
    # ============================================================
    
    # COMMON HELPER FUNCTIONS AND INITIAL DATA
    # ============================================================
    #                  CHARTS & GRAPHS
    # ============================================================
    
    
    # ============================================================
    # HELPER FUNCTIONS
    # ============================================================
    
    make_1d_df <- function(n, old = NULL) {
      
      n <- suppressWarnings(as.integer(n))
      
      if (is.na(n) || n < 1) {
        n <- 1
      }
      
      new_df <- data.frame(
        Category = paste0(
          "Category ",
          seq_len(n)
        ),
        Frequency = rep(10, n),
        stringsAsFactors = FALSE
      )
      
      # Preserve previous values
      if (!is.null(old) &&
          nrow(old) > 0) {
        
        nr <- min(
          n,
          nrow(old)
        )
        
        new_df$Category[seq_len(nr)] <-
          as.character(
            old$Category[seq_len(nr)]
          )
        
        new_df$Frequency[seq_len(nr)] <-
          old$Frequency[seq_len(nr)]
      }
      
      new_df
    }
    
    make_2d_df <- function(r, c, old = NULL) {
      
      r <- suppressWarnings(as.integer(r))
      c <- suppressWarnings(as.integer(c))
      
      if (is.na(r) || r < 1) {
        r <- 1
      }
      
      if (is.na(c) || c < 1) {
        c <- 1
      }
      
      new_df <- data.frame(
        Var1_Category =
          paste0(
            "Category ",
            seq_len(r)
          ),
        stringsAsFactors = FALSE
      )
      
      for (j in seq_len(c)) {
        
        new_df[[paste0(
          "Group ",
          j
        )]] <- rep(10, r)
      }
      
      # Preserve old values
      if (!is.null(old) &&
          nrow(old) > 0) {
        
        nr <- min(
          r,
          nrow(old)
        )
        
        old_groups <- setdiff(
          names(old),
          "Var1_Category"
        )
        
        new_groups <- setdiff(
          names(new_df),
          "Var1_Category"
        )
        
        nc <- min(
          length(old_groups),
          length(new_groups)
        )
        
        if (nr > 0 &&
            nc > 0) {
          
          new_df$Var1_Category[
            seq_len(nr)
          ] <-
            as.character(
              old$Var1_Category[
                seq_len(nr)
              ]
            )
          
          for (j in seq_len(nc)) {
            
            new_df[[new_groups[j]]][
              seq_len(nr)
            ] <-
              old[[old_groups[j]]][
                seq_len(nr)
              ]
          }
        }
      }
      
      new_df
    }
    
    # ============================================================
    # INITIAL DATA
    # ============================================================
    
    val <- reactiveValues(
      
      sb = make_1d_df(4),
      
      gb = make_2d_df(3, 2),
      
      st = make_2d_df(3, 2),
      
      pie = make_1d_df(4),
      
      line = make_1d_df(5),
      
      scat = data.frame(
        X = c(10, 20, 30, 40, 50, 60),
        Y = c(12, 28, 25, 42, 58, 65)
      ),
      
      circ = make_1d_df(8)
    )
    
    # ============================================================
    # CALCULATION FUNCTIONS
    # ============================================================
    
    calc_1d <- function(df) {
      
      freq <- suppressWarnings(
        as.numeric(df$Frequency)
      )
      
      freq[is.na(freq)] <- 0
      
      total <- sum(
        freq,
        na.rm = TRUE
      )
      
      percentage <- if (
        total > 0
      ) {
        
        freq / total * 100
        
      } else {
        
        rep(
          0,
          length(freq)
        )
      }
      
      data.frame(
        
        Category = df$Category,
        
        Frequency = freq,
        
        Percentage = round(percentage,2),
        
        Cum_Frequency =
          cumsum(freq),
        
        Cum_Percentage =round(
          cumsum(percentage),2),
        
        stringsAsFactors = FALSE
      )
    }
    
    
    calc_1d_pie <- function(df) {
      
      df <- df[
        !is.na(df$Category) &
          trimws(df$Category) != "" &
          df$Category != "Total",
        ,
        drop = FALSE
      ]
      
      freq <- suppressWarnings(
        as.numeric(df$Frequency)
      )
      
      freq[is.na(freq)] <- 0
      
      total <- sum(
        freq,
        na.rm = TRUE
      )
      
      percentage <- if (
        total > 0
      ) {
        
        freq / total * 100
        
      } else {
        
        rep(
          0,
          length(freq)
        )
      }
      
      data.frame(
        
        Category = df$Category,
        
        Frequency = freq,
        
        Percentage = round(percentage,2),
        
        stringsAsFactors = FALSE
      )
    }
    calc_2d <- function(df) {
      
      if (is.null(df) || !is.data.frame(df) || ncol(df) < 2) {
        return(data.frame())
      }
      
      out <- df
      
      for (j in 2:ncol(out)) {
        out[[j]] <- suppressWarnings(
          as.numeric(as.character(out[[j]]))
        )
        
        out[[j]][is.na(out[[j]])] <- 0
      }
      
      total_row <- data.frame(
        Var1_Category = "Total",
        matrix(
          colSums(out[, -1, drop = FALSE], na.rm = TRUE),
          nrow = 1,
          dimnames = list(NULL, names(out)[-1])
        ),
        check.names = FALSE
      )
      
      rbind(out, total_row)
    }
    
    calc_2d_with_totals <- function(df) {
      
      validate_data <- !is.null(df) &&
        is.data.frame(df) &&
        ncol(df) >= 2 &&
        "Var1_Category" %in% names(df)
      
      if (!validate_data) {
        return(data.frame())
      }
      
      out <- df
      
      # Convert group columns to numeric
      for (j in 2:ncol(out)) {
        out[[j]] <- suppressWarnings(
          as.numeric(as.character(out[[j]]))
        )
        
        out[[j]][is.na(out[[j]])] <- 0
      }
      
      # Remove any existing totals before recalculating
      out <- out[
        as.character(out$Var1_Category) != "Total",
        ,
        drop = FALSE
      ]
      
      out <- out[
        ,
        !tolower(trimws(names(out))) %in% "total",
        drop = FALSE
      ]
      
      # Calculate row totals
      out$Total <- rowSums(
        out[, -1, drop = FALSE],
        na.rm = TRUE
      )
      
      # Calculate the totals row
      total_row <- out[1, , drop = FALSE]
      total_row[1, ] <- NA
      total_row$Var1_Category <- "Total"
      
      for (j in 2:ncol(out)) {
        total_row[[j]] <- sum(out[[j]], na.rm = TRUE)
      }
      
      rbind(out, total_row)
    }    
     
    
    
    
    # ============================================================
    # SIMPLE BAR CHART
    # ============================================================
    # ============================================================
    # CHANGE NUMBER OF CATEGORIES / OBSERVATIONS
    # ============================================================
    
    # -------------------------
    # Simple Bar
    # -------------------------
    
    observeEvent(
      input$sb_num_cats,
      {
        
        val$sb <- make_1d_df(
          input$sb_num_cats,
          val$sb
        )
        
      },
      ignoreInit = FALSE
    )
    
    
    observeEvent(
      input$sb_update,
      {
        
        val$sb <- make_1d_df(
          input$sb_num_cats
        )
        
      }
    )
    
    sb_data <- reactive({
      calc_1d(val$sb)
    })
    
    # ============================================================
    # DATATABLES
    # ============================================================
    
    output$sb_table <- renderDT({
      
      datatable(
        sb_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
      
    })
    
    # ============================================================
    # CELL EDITING
    # ============================================================
    
    observeEvent(
      input$sb_table_cell_edit,
      {
        
        info <- input$sb_table_cell_edit
        
        row <- info$row
        col <- info$col + 1
        
        if (
          row <= nrow(val$sb) &&
          col <= 2
        ) {
          
          val$sb[row, col] <- info$value
        }
        
      }
    )
    
    # ============================================================
    # SIMPLE BAR CHART
    # ============================================================
    
    sb_gen <- reactive({
      
      req(sb_data())
      req(input$sb_val_type, input$sb_orient)
      
      df <- sb_data()
      
      # Remove Total row, if present
      if ("Category" %in% names(df)) {
        df <- df[df$Category != "Total" & !is.na(df$Category),
                 , drop = FALSE]
      }
      
      # Validate required columns
      validate(
        need(all(c("Category", "Frequency", "Percentage") %in% names(df)),
             "The data must contain Category, Frequency and Percentage columns."),
        need(nrow(df) > 0, "No category data available.")
      )
      
      # Select plotted variable
      yvar <- if (identical(input$sb_val_type, "Percentage")) {
        "Percentage"
      } else {
        "Frequency"
      }
      
      df[[yvar]] <- as.numeric(df[[yvar]])
      df <- df[!is.na(df[[yvar]]), , drop = FALSE]
      
      validate(
        need(nrow(df) > 0, "No valid numeric values available to plot.")
      )
      
      # Read appearance settings
      fill_col <- input$sb_fill
      border_col <- input$sb_border
      alpha_val <- input$sb_alpha
      base_size <- input$sb_font
      title_size <- input$sb_title_size
      
      # Select chart theme
      chart_theme <- switch(
        input$sb_theme,
        minimal = ggplot2::theme_minimal(base_size = base_size),
        classic = ggplot2::theme_classic(base_size = base_size),
        light   = ggplot2::theme_light(base_size = base_size),
        bw      = ggplot2::theme_bw(base_size = base_size),
        gray    = ggplot2::theme_gray(base_size = base_size),
        void    = ggplot2::theme_void(base_size = base_size),
        ggplot2::theme_minimal(base_size = base_size)
      )
      
      # Create plot
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = Category, y = .data[[yvar]])
      ) +
        ggplot2::geom_col(
          fill = fill_col,
          colour = border_col,
          alpha = alpha_val,
          width = 0.7,
          linewidth = 0.6
        ) +
        ggplot2::labs(
          title = input$sb_title,
          x = input$sb_xlab,
          y = if (identical(input$sb_val_type, "Percentage")) {
            paste0(input$sb_ylab, " (%)")
          } else {
            input$sb_ylab
          }
        ) +
        chart_theme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = title_size
          ),
          axis.text.x = ggplot2::element_text(
            angle = 0,
            hjust = 0.5,
            colour = "black"
          ),
          axis.text.y = ggplot2::element_text(
            colour = "black"
          ),
          axis.title = ggplot2::element_text(
            face = "bold"
          ),
          legend.position = "none",
          panel.grid.major = if (isTRUE(input$sb_grid)) {
            ggplot2::element_line(colour = "grey85", linewidth = 0.4)
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(12, 18, 12, 12)
        )
      
      # Add data labels
      if (isTRUE(input$sb_show_labels)) {
        
        label_values <- if (identical(yvar, "Percentage")) {
          sprintf("%.1f%%", df[[yvar]])
        } else {
          ifelse(
            df[[yvar]] == round(df[[yvar]]),
            as.character(round(df[[yvar]])),
            as.character(signif(df[[yvar]], 4))
          )
        }
        
        if (identical(input$sb_orient, "horizontal")) {
          
          p <- p +
            ggplot2::geom_text(
              data = df,
              mapping = ggplot2::aes(
                x = Category,
                y = .data[[yvar]],
                label = label_values
              ),
              inherit.aes = FALSE,
              hjust = -0.15,
              size = 3.5,
              colour = "black"
            )
          
        } else {
          
          p <- p +
            ggplot2::geom_text(
              data = df,
              mapping = ggplot2::aes(
                x = Category,
                y = .data[[yvar]],
                label = label_values
              ),
              inherit.aes = FALSE,
              vjust = -0.35,
              size = 3.5,
              colour = "black"
            )
        }
      }
      
      # Horizontal or vertical orientation
      if (identical(input$sb_orient, "horizontal")) {
        p <- p + ggplot2::coord_flip(clip = "off")
      } else {
        p <- p + ggplot2::coord_cartesian(clip = "off")
      }
      
      # Add space for labels above bars or beyond horizontal bars
      p <- p +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.15))
        )
      
      p
    })
    
    
    # ============================================================
    # DISPLAY SIMPLE BAR CHART
    # ============================================================
    
    output$sb_plot <- shiny::renderPlot({
      
      sb_gen()
      
    }, height = 480, res = 96)
    
    
    # ============================================================
    # DOWNLOAD SIMPLE BAR CHART AS PNG
    # ============================================================
    
    output$sb_download <- shiny::downloadHandler(
      
      filename = function() {
        paste0("Simple_Bar_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        
        p <- sb_gen()
        
        ggplot2::ggsave(
          filename = file,
          plot = p,
          device = "png",
          width = as.numeric(input$sb_out_width),
          height = as.numeric(input$sb_out_height),
          units = "in",
          dpi = as.numeric(input$sb_dpi),
          bg = "white",
          limitsize = TRUE
        )
      }
    )  
    
    # ============================================================
    # GROUPED BAR CHART
    # ============================================================
    # -------------------------
    # Grouped Bar
    # -------------------------
    
    observeEvent(
      list(
        input$gb_r,
        input$gb_c
      ),
      {
        
        val$gb <- make_2d_df(
          input$gb_r,
          input$gb_c,
          val$gb
        )
        
      },
      ignoreInit = FALSE
    )
    
    
    observeEvent(
      input$gb_update,
      {
        
        val$gb <- make_2d_df(
          input$gb_r,
          input$gb_c
        )
        
      }
    )
    
    gb_data <- reactive({
      calc_2d(val$gb)
    })
    
    output$gb_table <- renderDT({
      
      datatable(
        gb_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
      
    })
    
    observeEvent(
      input$gb_table_cell_edit,
      {
        
        info <- input$gb_table_cell_edit
        
        row <- info$row
        col <- info$col + 1
        
        if (
          row <= nrow(val$gb) &&
          col <= ncol(val$gb)
        ) {
          
          val$gb[row, col] <- info$value
        }
        
      }
    )
    
    # ============================================================
    # GROUPED BAR CHART
    # ============================================================
    
    
    # ============================================================
    # GROUPED BAR CHART - GENERATE PLOT
    # ============================================================
    
    
    # ============================================================
    # GROUPED BAR CHART - GENERATE PLOT
    # ============================================================
    
    gb_gen <- reactive({
      
      req(gb_data())
      req(input$gb_orient)
      
      df <- gb_data()
      
      validate(
        need("Var1_Category" %in% names(df),
             "The data must contain Var1_Category."),
        need(ncol(df) >= 2,
             "At least one group column is required.")
      )
      
      # Remove Total row
      df <- df[
        !is.na(df$Var1_Category) &
          df$Var1_Category != "Total",
        ,
        drop = FALSE
      ]
      
      validate(
        need(nrow(df) > 0, "No grouped bar chart data available.")
      )
      
      # Convert wide data to long format
      long <- tidyr::pivot_longer(
        df,
        cols = -Var1_Category,
        names_to = "Group",
        values_to = "Frequency"
      )
      
      long$Frequency <- suppressWarnings(
        as.numeric(as.character(long$Frequency))
      )
      
      long <- long[!is.na(long$Frequency), , drop = FALSE]
      
      validate(
        need(nrow(long) > 0, "No valid numeric frequencies available.")
      )
      
      long$Group <- factor(
        long$Group,
        levels = unique(long$Group)
      )
      
      # ----------------------------------------------------------
      # FREQUENCY / PERCENTAGE
      # ----------------------------------------------------------
      
      value_type <- input$gb_val_type %||% "Frequency"
      
      if (identical(value_type, "Percentage")) {
        
        long <- long |>
          dplyr::group_by(Group) |>
          dplyr::mutate(
            PlotValue = if (sum(Frequency) > 0) {
              100 * Frequency / sum(Frequency)
            } else {
              0
            }
          ) |>
          dplyr::ungroup()
        
        ylab <- paste0(
          input$gb_ylab %||% "Percentage",
          " (%)"
        )
        
      } else {
        
        long$PlotValue <- long$Frequency
        ylab <- input$gb_ylab %||% "Frequency"
      }
      
      # ----------------------------------------------------------
      # APPEARANCE SETTINGS
      # ----------------------------------------------------------
      
      border_col <- input$gb_border %||% "#2C3E50"
      alpha_val <- input$gb_alpha %||% 0.9
      base_size <- input$gb_font %||% 12
      title_size <- input$gb_title_size %||% 16
      
      valid_colour <- function(x, default) {
        tryCatch({
          grDevices::col2rgb(x)
          x
        }, error = function(e) default)
      }
      
      border_col <- valid_colour(border_col, "#2C3E50")
      
      alpha_val <- max(0.1, min(1, as.numeric(alpha_val)))
      
      # ----------------------------------------------------------
      # COLOUR PALETTE
      # ----------------------------------------------------------
      
      n_groups <- length(unique(as.character(long$Group)))
      
      palette_name <- input$gb_palette %||% "Set2"
      
      group_colours <- switch(
        
        palette_name,
        
        Set1 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 9), "Set1"
        ),
        
        Set2 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 8), "Set2"
        ),
        
        Set3 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 12), "Set3"
        ),
        
        Pastel1 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 9), "Pastel1"
        ),
        
        Pastel2 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 8), "Pastel2"
        ),
        
        Dark2 = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 8), "Dark2"
        ),
        
        Paired = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 12), "Paired"
        ),
        
        Accent = RColorBrewer::brewer.pal(
          min(max(n_groups, 3), 8), "Accent"
        ),
        
        tableau10 = grDevices::hcl.colors(
          n_groups, palette = "Tableau 10"
        ),
        
        viridis = viridisLite::viridis(n_groups),
        
        magma = viridisLite::magma(n_groups),
        
        plasma = viridisLite::plasma(n_groups),
        
        inferno = viridisLite::inferno(n_groups),
        
        rainbow = grDevices::rainbow(n_groups),
        
        grDevices::hcl.colors(n_groups, palette = "Dark 3")
      )
      
      # Ensure exactly one colour per group
      group_colours <- rep(
        group_colours,
        length.out = n_groups
      )
      
      names(group_colours) <- levels(long$Group)
      
      # ----------------------------------------------------------
      # CHART THEME
      # ----------------------------------------------------------
      
      chart_theme <- switch(
        input$gb_theme %||% "minimal",
        
        minimal = ggplot2::theme_minimal(base_size = base_size),
        classic = ggplot2::theme_classic(base_size = base_size),
        light = ggplot2::theme_light(base_size = base_size),
        bw = ggplot2::theme_bw(base_size = base_size),
        gray = ggplot2::theme_gray(base_size = base_size),
        void = ggplot2::theme_void(base_size = base_size),
        
        ggplot2::theme_minimal(base_size = base_size)
      )
      
      # ----------------------------------------------------------
      # CREATE GROUPED BAR CHART
      # ----------------------------------------------------------
      
      p <- ggplot2::ggplot(
        long,
        ggplot2::aes(
          x = Var1_Category,
          y = PlotValue,
          fill = Group
        )
      ) +
        
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.9),
          colour = border_col,
          alpha = alpha_val,
          width = 0.8,
          linewidth = 0.6
        ) +
        
        ggplot2::scale_fill_manual(
          values = group_colours,
          drop = FALSE
        ) +
        
        ggplot2::labs(
          title = input$gb_title %||% "Grouped Bar Chart",
          x = input$gb_xlab %||% "Category",
          y = ylab,
          fill = NULL
        ) +
        
        chart_theme +
        
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = title_size
          ),
          
          axis.text = ggplot2::element_text(
            colour = "black"
          ),
          
          axis.title = ggplot2::element_text(
            face = "bold"
          ),
          
          legend.position = input$gb_legend %||% "right",
          
          panel.grid.major = if (isTRUE(input$gb_grid)) {
            ggplot2::element_line(
              colour = "grey85",
              linewidth = 0.4
            )
          } else {
            ggplot2::element_blank()
          },
          
          panel.grid.minor = ggplot2::element_blank(),
          
          plot.margin = ggplot2::margin(12, 20, 12, 12)
        )
      
      # ----------------------------------------------------------
      # DATA LABELS
      # ----------------------------------------------------------
      
      if (isTRUE(input$gb_show_labels)) {
        
        label_values <- if (identical(value_type, "Percentage")) {
          sprintf("%.1f%%", long$PlotValue)
        } else {
          ifelse(
            long$Frequency == round(long$Frequency),
            as.character(round(long$Frequency)),
            as.character(signif(long$Frequency, 4))
          )
        }
        
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = label_values),
            position = ggplot2::position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5,
            show.legend = FALSE
          )
      }
      
      # ----------------------------------------------------------
      # ORIENTATION AND AXIS EXPANSION
      # ----------------------------------------------------------
      
      p <- p +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.15))
        )
      
      if (identical(input$gb_orient, "horizontal")) {
        p <- p + ggplot2::coord_flip(clip = "off")
      } else {
        p <- p + ggplot2::coord_cartesian(clip = "off")
      }
      
      p
    })
    
    
    # ============================================================
    # DISPLAY GROUPED BAR CHART
    # ============================================================
    
    output$gb_plot <- shiny::renderPlot({
      gb_gen()
    }, height = 480, res = 96)
    
    
    # ============================================================
    # DOWNLOAD GROUPED BAR CHART
    # ============================================================
    
    output$gb_download <- shiny::downloadHandler(
      
      filename = function() {
        paste0("Grouped_Bar_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        
        ggplot2::ggsave(
          filename = file,
          plot = gb_gen(),
          width = as.numeric(input$gb_out_width %||% 8),
          height = as.numeric(input$gb_out_height %||% 6),
          units = "in",
          dpi = as.numeric(input$gb_dpi %||% 300),
          bg = "white",
          limitsize = TRUE
        )
      }
    )
    
    
    
    
    # ============================================================
    # STACKED BAR CHART
    # ============================================================
    
    # ============================================================
    # STACKED / COMPONENT BAR CHART
    # ============================================================
  
    # ----------------------------------------------------------
    # STACKED BAR: INITIALIZE AND UPDATE DATA
    # ----------------------------------------------------------
    
    observeEvent(
      list(
        input$st_r,
        input$st_c
      ),
      {
        val$st <- make_2d_df(
          input$st_r,
          input$st_c,
          val$st
        )
      },
      ignoreInit = FALSE
    )
    
    
    observeEvent(
      input$st_update,
      {
        val$st <- make_2d_df(
          input$st_r,
          input$st_c
        )
      }
    )
    
    # ----------------------------------------------------------
    # STACKED BAR: REACTIVE DATA
    # ----------------------------------------------------------
    
    st_data <- reactive({
      req(val$st)
      calc_2d(val$st)
    })
    
    
    # ----------------------------------------------------------
    # STACKED BAR: DISPLAY TABLE
    # ----------------------------------------------------------
    
    output$st_table <- renderDT({
      
      req(st_data())
      
      datatable(
        st_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
      
    })
    
    
    # ----------------------------------------------------------
    # STACKED BAR: EDIT TABLE CELLS
    # ----------------------------------------------------------
    
    observeEvent(
      input$st_table_cell_edit,
      {
        
        info <- input$st_table_cell_edit
        
        row <- info$row
        col <- info$col + 1
        
        if (
          !is.null(val$st) &&
          row <= nrow(val$st) &&
          col <= ncol(val$st)
        ) {
          
          val$st[row, col] <- info$value
        }
        
      }
    )
    
    st_gen <- reactive({
      req(val$st)
      req(st_data())
      req(input$st_val_type)
      req(!is.null(input$st_swap_vars))
      
      df <- st_data()
      
      # ----------------------------------------------------------
      # 1. REMOVE TOTAL ROW AND TOTAL COLUMN
      # ----------------------------------------------------------
      
      validate(
        need("Var1_Category" %in% names(df),
             "The data must contain Var1_Category.")
      )
      
      df <- df[
        !is.na(df$Var1_Category) &
          as.character(df$Var1_Category) != "Total",
        ,
        drop = FALSE
      ]
      
      # Remove the Total column before pivoting
      df <- df[
        ,
        !tolower(trimws(names(df))) %in% c("total"),
        drop = FALSE
      ]
      
      validate(
        need(nrow(df) > 0, "No categories available for plotting."),
        need(ncol(df) >= 2, "At least one group column is required.")
      )
      
      # ----------------------------------------------------------
      # 2. CONVERT TO LONG FORMAT
      # ----------------------------------------------------------
      
      long <- tidyr::pivot_longer(
        df,
        cols = -Var1_Category,
        names_to = "Group",
        values_to = "Frequency"
      )
      
      long$Frequency <- suppressWarnings(
        as.numeric(as.character(long$Frequency))
      )
      
      long <- long[
        !is.na(long$Frequency),
        ,
        drop = FALSE
      ]
      
      validate(
        need(nrow(long) > 0, "No valid numeric frequencies available.")
      )
      
      # Preserve the original category order
      long$Var1_Category <- factor(
        long$Var1_Category,
        levels = unique(as.character(long$Var1_Category))
      )
      
      long$Group <- factor(
        long$Group,
        levels = unique(as.character(long$Group))
      )
      
      # ----------------------------------------------------------
      # 3. SWAP VARIABLES
      # ----------------------------------------------------------
      
      if (isTRUE(input$st_swap_vars)) {
        
        # Variable 2 on X-axis; Variable 1 defines the components
        xvar <- "Group"
        fillvar <- "Var1_Category"
        fill_name <- "Variable 1 Category"
        
      } else {
        
        # Variable 1 on X-axis; Variable 2 defines the components
        xvar <- "Var1_Category"
        fillvar <- "Group"
        fill_name <- "Variable 2 Group"
      }
      
      # ----------------------------------------------------------
      # 4. FREQUENCY OR PERCENTAGE
      # ----------------------------------------------------------
      
      value_type <- input$st_val_type %||% "frequency"
      
      if (identical(value_type, "percentage")) {
        
        # Calculate percentages within each displayed X-axis bar.
        # This also recalculates correctly when variables are swapped.
        long <- long |>
          dplyr::group_by(.data[[xvar]]) |>
          dplyr::mutate(
            Value = if (sum(Frequency, na.rm = TRUE) > 0) {
              100 * Frequency / sum(Frequency, na.rm = TRUE)
            } else {
              0
            }
          ) |>
          dplyr::ungroup()
        
        ylab <- paste0(
          input$st_ylab %||% "Percentage",
          " (%)"
        )
        
      } else {
        
        long$Value <- long$Frequency
        ylab <- input$st_ylab %||% "Frequency"
      }
      
      # ----------------------------------------------------------
      # 5. APPEARANCE SETTINGS
      # ----------------------------------------------------------
      
      border_col <- input$st_border %||% "#2C3E50"
      alpha_val <- input$st_alpha %||% 0.9
      base_size <- input$st_font %||% 12
      title_size <- input$st_title_size %||% 16
      
      valid_colour <- function(x, default) {
        tryCatch({
          grDevices::col2rgb(x)
          x
        }, error = function(e) default)
      }
      
      border_col <- valid_colour(border_col, "#2C3E50")
      
      alpha_val <- max(0.1, min(1, as.numeric(alpha_val)))
      
      # ----------------------------------------------------------
      # 6. GROUP COLOUR PALETTE
      # ----------------------------------------------------------
      
      palette_name <- input$st_palette %||% "Set2"
      
      group_levels <- unique(as.character(long[[fillvar]]))
      n_groups <- length(group_levels)
      
      # Generate requested palette
      palette_base <- switch(
        palette_name,
        
        Set1 = RColorBrewer::brewer.pal(3, "Set1"),
        Set2 = RColorBrewer::brewer.pal(3, "Set2"),
        Set3 = RColorBrewer::brewer.pal(3, "Set3"),
        Pastel1 = RColorBrewer::brewer.pal(3, "Pastel1"),
        Pastel2 = RColorBrewer::brewer.pal(3, "Pastel2"),
        Dark2 = RColorBrewer::brewer.pal(3, "Dark2"),
        Paired = RColorBrewer::brewer.pal(3, "Paired"),
        Accent = RColorBrewer::brewer.pal(3, "Accent"),
        
        tableau10 = grDevices::hcl.colors(
          max(n_groups, 1), "Tableau 10"
        ),
        
        viridis = viridisLite::viridis(max(n_groups, 1)),
        magma = viridisLite::magma(max(n_groups, 1)),
        plasma = viridisLite::plasma(max(n_groups, 1)),
        inferno = viridisLite::inferno(max(n_groups, 1)),
        rainbow = grDevices::rainbow(max(n_groups, 1)),
        
        RColorBrewer::brewer.pal(3, "Set2")
      )
      
      # Expand palettes safely for any number of categories
      if (palette_name %in% c(
        "Set1", "Set2", "Set3", "Pastel1",
        "Pastel2", "Dark2", "Paired", "Accent"
      )) {
        group_colours <- grDevices::colorRampPalette(
          palette_base
        )(max(n_groups, 1))
      } else {
        group_colours <- palette_base
      }
      
      group_colours <- group_colours[seq_len(max(n_groups, 1))]
      names(group_colours) <- group_levels
      
      # ----------------------------------------------------------
      # 7. CHART THEME
      # ----------------------------------------------------------
      
      chart_theme <- switch(
        input$st_theme %||% "minimal",
        
        minimal = ggplot2::theme_minimal(base_size = base_size),
        classic = ggplot2::theme_classic(base_size = base_size),
        light = ggplot2::theme_light(base_size = base_size),
        bw = ggplot2::theme_bw(base_size = base_size),
        gray = ggplot2::theme_gray(base_size = base_size),
        void = ggplot2::theme_void(base_size = base_size),
        
        ggplot2::theme_minimal(base_size = base_size)
      )
      
      # ----------------------------------------------------------
      # 8. CREATE STACKED BAR CHART
      # ----------------------------------------------------------
      
      p <- ggplot2::ggplot(
        long,
        ggplot2::aes(
          x = .data[[xvar]],
          y = Value,
          fill = .data[[fillvar]]
        )
      ) +
        
        ggplot2::geom_col(
          position = "stack",
          colour = border_col,
          alpha = alpha_val,
          width = 0.75,
          linewidth = 0.6
        ) +
        
        ggplot2::scale_fill_manual(
          values = group_colours,
          drop = FALSE
        ) +
        
        ggplot2::labs(
          title = input$st_title %||% "Stacked Bar Chart",
          x = input$st_xlab %||% "Variable 1",
          y = ylab,
          fill = fill_name
        ) +
        
        chart_theme +
        
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = title_size
          ),
          
          axis.text = ggplot2::element_text(colour = "black"),
          
          axis.title = ggplot2::element_text(face = "bold"),
          
          legend.position = input$st_legend %||% "right",
          
          panel.grid.major = if (isTRUE(input$st_grid)) {
            ggplot2::element_line(
              colour = "grey85",
              linewidth = 0.4
            )
          } else {
            ggplot2::element_blank()
          },
          
          panel.grid.minor = ggplot2::element_blank(),
          
          plot.margin = ggplot2::margin(12, 20, 12, 12)
        )
      
      # ----------------------------------------------------------
      # 9. Y-AXIS
      # ----------------------------------------------------------
      
      if (identical(value_type, "percentage")) {
        
        p <- p +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            limits = c(0, 100),
            breaks = seq(0, 100, 20),
            expand = c(0, 0)
          )
        
      } else {
        
        p <- p +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.12))
          )
      }
      
      # ----------------------------------------------------------
      # 10. DATA LABELS
      # ----------------------------------------------------------
      
      if (isTRUE(input$st_show_labels)) {
        
        label_values <- if (identical(value_type, "percentage")) {
          ifelse(
            long$Value > 0,
            sprintf("%.1f%%", long$Value),
            ""
          )
        } else {
          ifelse(
            long$Value == round(long$Value),
            as.character(round(long$Value)),
            as.character(signif(long$Value, 4))
          )
        }
        
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = label_values),
            position = ggplot2::position_stack(vjust = 0.5),
            size = 3.5,
            show.legend = FALSE
          )
      }
      
      
      p
    })
    
    # ============================================================
    # DISPLAY STACKED BAR CHART
    # ============================================================
    output$st_plot <- shiny::renderPlot({
      p <- st_gen()
      req(inherits(p, "ggplot"))
      print(p)
    }, height = 480, res = 96)
    
    # ============================================================
    # DOWNLOAD STACKED BAR CHART
    # ============================================================
    
    output$st_download <- shiny::downloadHandler(
      
      filename = function() {
        paste0("Stacked_Bar_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        
        ggplot2::ggsave(
          filename = file,
          plot = st_gen(),
          width = as.numeric(input$st_out_width %||% 8),
          height = as.numeric(input$st_out_height %||% 6),
          units = "in",
          dpi = as.numeric(input$st_dpi %||% 300),
          bg = "white",
          limitsize = TRUE
        )
      }
    )
    
    
    # ============================================================
    # PIE CHART
    # ============================================================
    # ============================================================
    # PIE CHART: INITIALIZE AND UPDATE DATA
    # ============================================================
    
    observeEvent(
      input$pie_num_cats,
      {
        val$pie <- make_1d_df(
          input$pie_num_cats,
          val$pie
        )
      },
      ignoreInit = FALSE
    )
    
    observeEvent(
      input$pie_update,
      {
        val$pie <- make_1d_df(
          input$pie_num_cats
        )
      }
    )
    
    # ============================================================
    # PIE CHART: REACTIVE DATA
    # ============================================================
    
    pie_data <- reactive({
      req(val$pie)
      calc_1d_pie(val$pie)
    })
    
    # ============================================================
    # PIE CHART: DISPLAY TABLE
    # ============================================================
    
    output$pie_table <- renderDT({
      req(pie_data())
      
      datatable(
        pie_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
    })
    
    # ============================================================
    # PIE CHART: EDIT TABLE CELLS
    # ============================================================
    
    observeEvent(
      input$pie_table_cell_edit,
      {
        info <- input$pie_table_cell_edit
        
        row <- info$row
        col <- info$col + 1
        
        if (
          !is.null(val$pie) &&
          row <= nrow(val$pie) &&
          col <= ncol(val$pie)
        ) {
          val$pie[row, col] <- info$value
        }
      }
    )
    
    # ============================================================
    # PIE CHART: GENERATE GRAPH
    # ============================================================
    
    pie_gen <- reactive({
      
      df <- pie_data()
      
      validate(
        need(
          is.data.frame(df) &&
            all(c("Category", "Frequency") %in% names(df)),
          "Pie chart data must contain Category and Frequency columns."
        )
      )
      
      df <- df[
        !is.na(df$Category) &
          as.character(df$Category) != "Total" &
          is.finite(df$Frequency) &
          df$Frequency > 0,
        ,
        drop = FALSE
      ]
      
      validate(
        need(nrow(df) > 0, "Enter at least one positive frequency.")
      )
      
      # ----------------------------------------------------------
      # PALETTE
      # ----------------------------------------------------------
      
      palette_name <- input$pie_palette %||% "Set2"
      n <- nrow(df)
      
      colours <- switch(
        palette_name,
        
        Set1 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Set1")
        )(n),
        
        Set2 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Set2")
        )(n),
        
        Set3 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Set3")
        )(n),
        
        Pastel1 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Pastel1")
        )(n),
        
        Pastel2 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Pastel2")
        )(n),
        
        Dark2 = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Dark2")
        )(n),
        
        Paired = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Paired")
        )(n),
        
        Accent = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Accent")
        )(n),
        
        tableau10 = grDevices::hcl.colors(n, "Tableau 10"),
        viridis = viridisLite::viridis(n),
        magma = viridisLite::magma(n),
        plasma = viridisLite::plasma(n),
        inferno = viridisLite::inferno(n),
        rainbow = grDevices::rainbow(n),
        
        grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(3, "Set2")
        )(n)
      )
      
      names(colours) <- as.character(df$Category)
      
      # ----------------------------------------------------------
      # APPEARANCE
      # ----------------------------------------------------------
      
      border_col <- input$pie_border %||% "#2C3E50"
      alpha_val <- input$pie_alpha %||% 0.9
      base_size <- input$pie_font %||% 12
      title_size <- input$pie_title_size %||% 16
      
      alpha_val <- max(0.1, min(1, as.numeric(alpha_val)))
      
      # ----------------------------------------------------------
      # THEME
      # ----------------------------------------------------------
      
      chart_theme <- switch(
        input$pie_theme %||% "minimal",
        
        minimal = ggplot2::theme_minimal(base_size = base_size),
        classic = ggplot2::theme_classic(base_size = base_size),
        light = ggplot2::theme_light(base_size = base_size),
        bw = ggplot2::theme_bw(base_size = base_size),
        gray = ggplot2::theme_gray(base_size = base_size),
        void = ggplot2::theme_void(base_size = base_size),
        
        ggplot2::theme_minimal(base_size = base_size)
      )
      
      # ----------------------------------------------------------
      # CREATE PIE CHART
      # ----------------------------------------------------------
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = "",
          y = Frequency,
          fill = Category
        )
      ) +
        ggplot2::geom_col(
          width = 1,
          colour = border_col,
          alpha = alpha_val,
          linewidth = 0.6
        ) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::scale_fill_manual(values = colours) +
        ggplot2::labs(
          title = input$pie_title %||% "Pie Chart Distribution",
          fill = "Category"
        ) +
        chart_theme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold",
            size = title_size
          ),
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = if (isTRUE(input$pie_grid)) {
            ggplot2::element_line(colour = "grey90")
          } else {
            ggplot2::element_blank()
          },
          legend.position = input$pie_legend %||% "right"
        )
      
      # ----------------------------------------------------------
      # SLICE LABELS
      # ----------------------------------------------------------
      
      if (isTRUE(input$pie_show_labels)) {
        
        df$Percentage <- 100 * df$Frequency / sum(df$Frequency)
        
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label = paste0(
                as.character(Category),
                "\n",
                sprintf("%.1f%%", Percentage)
              )
            ),
            position = ggplot2::position_stack(vjust = 0.5),
            size = max(2.5, base_size / 4)
          )
      }
      
      p
    })
    
    # ============================================================
    # DISPLAY PIE CHART
    # ============================================================
    
    output$pie_plot <- renderPlot({
      print(pie_gen())
    }, height = 480, res = 96)
    
    # ============================================================
    # DOWNLOAD PIE CHART
    # ============================================================
    
    output$pie_download <- downloadHandler(
      
      filename = function() {
        paste0("Pie_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        
        ggplot2::ggsave(
          filename = file,
          plot = pie_gen(),
          width = as.numeric(input$pie_out_width %||% 8),
          height = as.numeric(input$pie_out_height %||% 6),
          units = "in",
          dpi = as.numeric(input$pie_dpi %||% 300),
          bg = "white",
          limitsize = TRUE
        )
      }
    )
    
    # ============================================================
    # LINE CHART
    # ============================================================
    # -------------------------
    # Line Chart
    # -------------------------
    
    # ============================================================
    # HELPER: CREATE MULTIPLE-LINE FREQUENCY TABLE
    # Place inside server before the observeEvent blocks
    # ============================================================3
    
    
    make_line_input_df <- function(n_pts, n_lines, old = NULL) {
      
      # Validate number of categories
      n_pts <- suppressWarnings(as.integer(n_pts[1]))
      
      if (length(n_pts) == 0 || is.na(n_pts) || n_pts < 1) {
        n_pts <- 5L
      }
      
      # Validate number of lines
      n_lines <- suppressWarnings(as.integer(n_lines[1]))
      
      if (length(n_lines) == 0 || is.na(n_lines) || n_lines < 1) {
        n_lines <- 1L
      }
      
      # Limit values to UI settings
      n_pts <- max(2L, min(30L, n_pts))
      n_lines <- max(1L, min(4L, n_lines))
      
      categories <- paste("Category", seq_len(n_pts))
      
      # Preserve existing categories when possible
      if (
        !is.null(old) &&
        is.data.frame(old) &&
        "Category" %in% names(old) &&
        nrow(old) > 0
      ) {
        keep <- min(n_pts, nrow(old))
        categories[seq_len(keep)] <-
          as.character(old$Category[seq_len(keep)])
      }
      
      result <- data.frame(
        Category = categories,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Create one frequency column per selected line
      for (j in seq_len(n_lines)) {
        
        col_name <- paste("Line", j)
        
        result[[col_name]] <- rep(0, n_pts)
        
        if (
          !is.null(old) &&
          is.data.frame(old) &&
          col_name %in% names(old) &&
          nrow(old) > 0
        ) {
          keep <- min(n_pts, nrow(old))
          
          values <- suppressWarnings(
            as.numeric(as.character(old[[col_name]][seq_len(keep)]))
          )
          
          values[!is.finite(values) | is.na(values) | values < 0] <- 0
          
          result[[col_name]][seq_len(keep)] <- values
        }
      }
      
      result
    }
    
    
    # ============================================================
    # INITIALISE AND UPDATE TABLE
    # ============================================================
    
    observeEvent(
      list(input$line_num_pts, input$line_num_lines),
      {
        val$line <- make_line_input_df(
          input$line_num_pts,
          input$line_num_lines,
          val$line
        )
      },
      ignoreInit = FALSE
    )
    
    
    observeEvent(input$line_update, {
      val$line <- make_line_input_df(
        input$line_num_pts,
        input$line_num_lines
      )
    })
    
    
    # ============================================================
    # EDITABLE FREQUENCY TABLE
    # ============================================================
    
    line_data <- reactive({
      req(val$line)
      val$line
    })
    
    
    output$line_table <- renderDT({
      req(line_data())
      
      DT::datatable(
        line_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
    })
    
    
    observeEvent(input$line_table_cell_edit, {
      info <- input$line_table_cell_edit
      
      row <- info$row
      col <- info$col + 1
      
      # Category is column 1; frequencies begin in column 2
      if (
        !is.null(val$line) &&
        row >= 1 && row <= nrow(val$line) &&
        col >= 2 && col <= ncol(val$line)
      ) {
        
        new_value <- suppressWarnings(
          as.numeric(info$value)
        )
        
        if (is.finite(new_value) && new_value >= 0) {
          val$line[row, col] <- new_value
        }
      }
    })
    
    
    # ============================================================
    # CALCULATE ALL STATISTICS FOR EVERY LINE
    # These calculated statistics are NOT displayed in the table
    # ============================================================
    
    line_all_stats <- reactive({
      
      df <- line_data()
      
      validate(
        need(nrow(df) > 0, "Enter frequency values.")
      )
      
      line_names <- setdiff(names(df), "Category")
      
      result <- lapply(line_names, function(nm) {
        
        freq <- suppressWarnings(
          as.numeric(df[[nm]])
        )
        
        freq[!is.finite(freq) | is.na(freq)] <- 0
        
        total <- sum(freq)
        
        percentage <- if (total > 0) {
          freq / total * 100
        } else {
          rep(0, length(freq))
        }
        
        data.frame(
          Category = df$Category,
          Line = nm,
          Frequency = freq,
          Percentage = percentage,
          Cum_Frequency = cumsum(freq),
          Cum_Percentage = cumsum(percentage),
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, result)
    })
    
    
    # ============================================================
    # GENERATE MULTIPLE-LINE CHART
    # ============================================================
    
    line_gen <- reactive({
      
      stats <- line_all_stats()
      
      measure <- input$line_measure
      req(measure)
      
      validate(
        need(measure %in% names(stats),
             "Select a valid statistic.")
      )
      
      stats$Value <- stats[[measure]]
      
      stats$Category <- factor(
        stats$Category,
        levels = unique(stats$Category)
      )
      
      line_names <- unique(stats$Line)
      n_lines <- length(line_names)
      
      # Generate palette colours
      palette_name <- input$line_palette
      
      if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
        
        max_colours <- RColorBrewer::brewer.pal.info[
          palette_name, "maxcolors"
        ]
        
        base_colours <- RColorBrewer::brewer.pal(
          max(3, min(max_colours, n_lines)),
          palette_name
        )
        
        colours <- grDevices::colorRampPalette(
          base_colours
        )(n_lines)
        
      } else {
        
        colours <- switch(
          palette_name,
          viridis = viridisLite::viridis(n_lines),
          magma = viridisLite::magma(n_lines),
          plasma = viridisLite::plasma(n_lines),
          viridisLite::viridis(n_lines)
        )
      }
      
      names(colours) <- line_names
      
      p <- ggplot2::ggplot(
        stats,
        ggplot2::aes(
          x = Category,
          y = Value,
          colour = Line,
          group = Line
        )
      ) +
        ggplot2::geom_line(
          linewidth = input$line_width,
          linetype = input$line_type,
          alpha = input$line_alpha
        ) +
        ggplot2::geom_point(
          shape = as.numeric(input$line_shape),
          size = input$line_point_size,
          alpha = input$line_alpha
        ) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::labs(
          title = input$line_title,
          x = input$line_xlab,
          y = input$line_ylab,
          colour = "Line"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            size = input$line_title_size,
            face = "bold"
          ),
          text = ggplot2::element_text(
            size = input$line_font
          ),
          legend.position = input$line_legend,
          panel.grid.major = if (isTRUE(input$line_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = if (isTRUE(input$line_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          }
        )
      
      if (isTRUE(input$line_show_labels)) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label = round(Value, 2)
            ),
            vjust = -0.7,
            size = 3,
            show.legend = FALSE,
            check_overlap = TRUE
          )
      }
      
      p
    })
    
    
    # ============================================================
    # DISPLAY CHART
    # ============================================================
    
    output$line_plot <- renderPlot({
      print(line_gen())
    }, height = 480, res = 96)
    
    
    # ============================================================
    # EXPORT PNG
    # ============================================================
    
    output$line_download <- downloadHandler(
      
      filename = function() {
        paste0("Line_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = line_gen(),
          width = input$line_out_width,
          height = input$line_out_height,
          dpi = as.numeric(input$line_dpi),
          units = "in",
          bg = "white"
        )
      }
    )
    
    # line chart upload data
    
    
    
    # ============================================================
    # X-AXIS VARIABLE
    # ============================================================
    
    output$line_upload_x_selector <- renderUI({
      df <- global_data()
      req(is.data.frame(df), nrow(df) > 0)
      
      selectInput(
        "line_upload_xvar",
        "Select X-axis Variable:",
        choices = names(df),
        selected = names(df)[1]
      )
    })
    
    
    # ============================================================
    # DYNAMIC Y-AXIS VARIABLE SELECTORS
    # ============================================================
    
    output$line_upload_var_selectors <- renderUI({
      df <- global_data()
      req(is.data.frame(df))
      
      numeric_vars <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      if (!is.null(input$line_upload_xvar)) {
        numeric_vars <- setdiff(
          numeric_vars,
          input$line_upload_xvar
        )
      }
      
      n <- suppressWarnings(
        as.integer(input$line_upload_num_vars)
      )
      
      if (length(n) == 0 || is.na(n) || n < 1) n <- 1L
      n <- min(n, 10L)
      
      if (length(numeric_vars) == 0) {
        return(helpText(
          "No numeric Y variables found. Select numeric count/frequency columns."
        ))
      }
      
      tagList(
        lapply(seq_len(n), function(i) {
          selectInput(
            inputId = paste0("line_upload_var", i),
            label = paste("Select Y variable", i, ":"),
            choices = numeric_vars,
            selected = numeric_vars[
              min(i, length(numeric_vars))
            ]
          )
        })
      )
    })
    
    
    # ============================================================
    # BUILD FREQUENCY TABLE FROM UPLOADED DATA
    # Table shows only X and frequency/count columns
    # ============================================================
    
    line_upload_frequency <- reactive({
      
      df <- global_data()
      
      validate(
        need(
          is.data.frame(df) && nrow(df) > 0,
          "Please upload data first."
        )
      )
      
      xvar <- input$line_upload_xvar
      
      validate(
        need(
          !is.null(xvar) && xvar %in% names(df),
          "Select an X-axis variable."
        )
      )
      
      n <- suppressWarnings(
        as.integer(input$line_upload_num_vars)
      )
      
      if (length(n) == 0 || is.na(n) || n < 1) n <- 1L
      n <- min(n, 10L)
      
      yvars <- vapply(
        seq_len(n),
        function(i) {
          val <- input[[paste0("line_upload_var", i)]]
          if (is.null(val) || length(val) == 0) {
            NA_character_
          } else {
            as.character(val[1])
          }
        },
        character(1)
      )
      
      yvars <- unique(yvars[!is.na(yvars) & nzchar(yvars)])
      
      validate(
        need(length(yvars) > 0, "Select at least one Y variable."),
        need(all(yvars %in% names(df)), "A selected variable is unavailable."),
        need(!xvar %in% yvars, "X and Y variables must be different."),
        need(
          all(vapply(df[yvars], is.numeric, logical(1))),
          "All selected Y variables must be numeric counts or frequencies."
        )
      )
      
      # Remove rows with missing X values
      work <- df[!is.na(df[[xvar]]), c(xvar, yvars), drop = FALSE]
      
      validate(
        need(nrow(work) > 0, "The selected X variable has no valid values.")
      )
      
      # Group by X and sum each selected numeric Y variable
      grouped <- dplyr::group_by(
        work,
        .data[[xvar]],
        .drop = TRUE
      )
      
      result <- dplyr::summarise(
        grouped,
        dplyr::across(
          dplyr::all_of(yvars),
          ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      )
      
      names(result)[1] <- "X_Category"
      
      # Sort numeric or date-like X values where possible
      if (is.numeric(result$X_Category) ||
          inherits(result$X_Category, "Date")) {
        result <- result[
          order(result$X_Category),
          ,
          drop = FALSE
        ]
      }
      
      result
    })
    
    
    # ============================================================
    # DISPLAY FREQUENCY TABLE
    # No percentage or cumulative columns displayed
    # ============================================================
    
    output$line_upload_table <- DT::renderDT({
      
      df <- line_upload_frequency()
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
    })
    
    
    # ============================================================
    # CALCULATE STATISTICS INTERNALLY FOR EACH LINE
    # ============================================================
    
    line_upload_stats <- reactive({
      
      df <- line_upload_frequency()
      yvars <- setdiff(names(df), "X_Category")
      
      result <- lapply(yvars, function(v) {
        
        freq <- as.numeric(df[[v]])
        total <- sum(freq, na.rm = TRUE)
        
        pct <- if (total > 0) {
          freq / total * 100
        } else {
          rep(0, length(freq))
        }
        
        data.frame(
          X_Category = df$X_Category,
          Series = v,
          Frequency = freq,
          Percentage = pct,
          Cum_Frequency = cumsum(freq),
          Cum_Percentage = cumsum(pct),
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, result)
    })
    
    
    # ============================================================
    # GENERATE MULTIPLE-LINE CHART
    # ============================================================
    
    line_upload_gen <- reactive({
      
      plot_df <- line_upload_stats()
      measure <- input$line_upload_measure
      
      validate(
        need(
          measure %in% names(plot_df),
          "Select a valid statistic."
        )
      )
      
      plot_df$PlotValue <- plot_df[[measure]]
      
      # Treat X as categorical, preserving the table order
      x_levels <- unique(as.character(plot_df$X_Category))
      
      plot_df$X_Category <- factor(
        as.character(plot_df$X_Category),
        levels = x_levels
      )
      
      series_names <- unique(plot_df$Series)
      n_series <- length(series_names)
      palette_name <- input$line_upload_palette
      
      if (palette_name %in%
          rownames(RColorBrewer::brewer.pal.info)) {
        
        max_colours <- RColorBrewer::brewer.pal.info[
          palette_name, "maxcolors"
        ]
        
        base_colours <- RColorBrewer::brewer.pal(
          max(3, min(max_colours, n_series)),
          palette_name
        )
        
        line_colours <- grDevices::colorRampPalette(
          base_colours
        )(n_series)
        
      } else {
        
        line_colours <- switch(
          palette_name,
          viridis = viridisLite::viridis(n_series),
          magma = viridisLite::magma(n_series),
          plasma = viridisLite::plasma(n_series),
          viridisLite::viridis(n_series)
        )
      }
      
      names(line_colours) <- series_names
      
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = X_Category,
          y = PlotValue,
          colour = Series,
          group = Series
        )
      ) +
        ggplot2::geom_line(
          linewidth = input$line_upload_width
        ) +
        ggplot2::geom_point(
          shape = as.numeric(input$line_upload_shape),
          size = input$line_upload_point_size
        ) +
        ggplot2::scale_colour_manual(values = line_colours) +
        ggplot2::labs(
          title = input$line_upload_title,
          x = input$line_upload_xlab,
          y = input$line_upload_ylab,
          colour = "Variable"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold"
          ),
          legend.position = input$line_upload_legend,
          panel.grid.major = if (isTRUE(input$line_upload_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = if (isTRUE(input$line_upload_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          }
        )
      
      if (isTRUE(input$line_upload_labels)) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = round(PlotValue, 2)),
            vjust = -0.7,
            size = 3,
            show.legend = FALSE,
            check_overlap = TRUE
          )
      }
      
      p
    })
    
    
    # ============================================================
    # DISPLAY CHART
    # ============================================================
    
    output$line_upload_plot <- renderPlot({
      print(line_upload_gen())
    }, height = 480, res = 96)
    
    
    # ============================================================
    # EXPORT PNG
    # ============================================================
    
    output$line_upload_download <- downloadHandler(
      
      filename = function() {
        paste0("Uploaded_Line_Chart_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = line_upload_gen(),
          width = input$line_upload_out_width,
          height = input$line_upload_out_height,
          dpi = as.numeric(input$line_upload_dpi),
          units = "in",
          bg = "white"
        )
      }
    )
    
    
    
    
    # ============================================================
    # SCATTER DIAGRAM
    # ============================================================
    # -------------------------
    # Scatter Diagram
    # -------------------------
    
    observeEvent(
      input$scat_num_obs,
      {
        
        n <- suppressWarnings(
          as.integer(input$scat_num_obs)
        )
        
        if (is.na(n) || n < 2) {
          n <- 2
        }
        
        old <- val$scat
        
        new_df <- data.frame(
          X = seq(
            10,
            10 * n,
            by = 10
          ),
          Y = seq(
            10,
            10 * n,
            length.out = n
          )
        )
        
        if (!is.null(old) &&
            nrow(old) > 0) {
          
          nr <- min(
            n,
            nrow(old)
          )
          
          new_df$X[seq_len(nr)] <-
            old$X[seq_len(nr)]
          
          new_df$Y[seq_len(nr)] <-
            old$Y[seq_len(nr)]
        }
        
        val$scat <- new_df
        
      },
      ignoreInit = FALSE
    )
    
    observeEvent(
      input$scat_update,
      {
        
        n <- input$scat_num_obs
        
        val$scat <- data.frame(
          X = seq(
            10,
            10 * n,
            by = 10
          ),
          Y = seq(
            10,
            10 * n,
            length.out = n
          )
        )
        
      }
    )
    
    scat_data <- reactive({
      val$scat
    })
    
    output$scat_table <- DT::renderDT({
      DT::datatable(
        val$scat,
        editable = TRUE,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "tip",
          pageLength = 15,
          autoWidth = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(
              targets = 0:1,
              width = "20px",
              className = "dt-center"
            )
          )
        ),
        class = "compact"
      )
    })
    
    observeEvent(
      input$scat_table_cell_edit,
      {
        
        info <- input$scat_table_cell_edit
        
        row <- info$row
        col <- info$col + 1
        
        if (
          row <= nrow(val$scat) &&
          col <= 2
        ) {
          
          val$scat[row, col] <- info$value
        }
        
      }
    )
    
    # ============================================================
    # SCATTER DIAGRAM
    # ============================================================
    
    # ============================================================
    # SCATTER DIAGRAM
    # ============================================================
    
    scat_gen <- reactive({
      
      df <- scat_data()
      req(is.data.frame(df))
      
      validate(
        need(all(c("X", "Y") %in% names(df)),
             "The table must contain X and Y columns.")
      )
      
      df$X <- suppressWarnings(
        as.numeric(as.character(df$X))
      )
      
      df$Y <- suppressWarnings(
        as.numeric(as.character(df$Y))
      )
      
      df <- df[
        is.finite(df$X) & is.finite(df$Y),
        ,
        drop = FALSE
      ]
      
      validate(
        need(nrow(df) >= 2,
             "Enter at least two valid X and Y observations.")
      )
      
      # Select chart theme
      theme_function <- switch(
        input$scat_theme,
        classic = ggplot2::theme_classic(),
        light   = ggplot2::theme_light(),
        bw      = ggplot2::theme_bw(),
        gray    = ggplot2::theme_gray(),
        void    = ggplot2::theme_void(),
        ggplot2::theme_minimal()
      )
      
      # Create scatter plot
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = X, y = Y)
      ) +
        ggplot2::geom_point(
          colour = input$scat_point_colour,
          shape = as.numeric(input$scat_point_shape),
          size = input$scat_point_size,
          alpha = input$scat_alpha
        ) +
        ggplot2::labs(
          title = input$scat_title,
          x = input$scat_xlab,
          y = input$scat_ylab
        ) +
        theme_function +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            size = input$scat_title_size,
            face = "bold"
          ),
          text = ggplot2::element_text(
            size = input$scat_font
          ),
          panel.grid.major = if (isTRUE(input$scat_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = if (isTRUE(input$scat_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          }
        )
      
      # Optional trend line
      if (isTRUE(input$scat_show_line)) {
        method <- input$scat_line_method
        
        if (method == "loess" && length(unique(df$X)) < 3) {
          method <- "lm"
        }
        
        p <- p + ggplot2::geom_smooth(
          data = df,
          mapping = ggplot2::aes(x = X, y = Y),
          method = method,
          se = FALSE,
          colour = input$scat_line_colour,
          linetype = input$scat_line_type,
          linewidth = input$scat_line_width
        )
      }
      
      # Optional coordinate labels
      if (isTRUE(input$scat_show_labels)) {
        
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label = paste0(
                "(",
                format(X, trim = TRUE),
                ", ",
                format(Y, trim = TRUE),
                ")"
              )
            ),
            colour = input$scat_point_colour,
            vjust = -0.8,
            size = 3,
            check_overlap = TRUE
          )
      }
      
      p
    })
    
    
    # ============================================================
    # DISPLAY SCATTER DIAGRAM
    # ============================================================
    
    output$scat_plot <- ggplot2::ggplotGrob
    
    output$scat_plot <- renderPlot({
      print(scat_gen())
    }, height = 480, res = 96)
    
    
    # ============================================================
    # DOWNLOAD SCATTER DIAGRAM
    # ============================================================
    
    output$scat_download <- downloadHandler(
      
      filename = function() {
        paste0("Scatter_Diagram_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        
        ggplot2::ggsave(
          filename = file,
          plot = scat_gen(),
          width = input$scat_out_width,
          height = input$scat_out_height,
          dpi = as.numeric(input$scat_dpi),
          units = "in",
          bg = "white"
        )
      }
    )
    
  #------------------Upload scatter diagram data-----------------------
    
    # ============================================================
    # UPLOADED DATA SCATTER DIAGRAM
    # ============================================================
    
    # Dynamic X-variable selector
    output$scat_upload_x_ui <- renderUI({
      df <- global_data()
      req(is.data.frame(df), nrow(df) > 0)
      
      numeric_vars <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      selectInput(
        "scat_upload_x",
        "Select X Variable",
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    })
    
    # Dynamic Y-variable selector
    output$scat_upload_y_ui <- renderUI({
      df <- global_data()
      req(is.data.frame(df), nrow(df) > 0)
      
      numeric_vars <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      selectInput(
        "scat_upload_y",
        "Select Y Variable",
        choices = numeric_vars,
        selected = if (length(numeric_vars) >= 2) {
          numeric_vars[2]
        } else {
          numeric_vars[1]
        }
      )
    })
    
    
    # Temporary editable dataset
    scat_upload_values <- reactiveVal(NULL)
    
    
    # Load selected X and Y variables
    observeEvent(
      list(global_data(), input$scat_upload_x, input$scat_upload_y),
      {
        df <- global_data()
        
        req(
          is.data.frame(df),
          input$scat_upload_x,
          input$scat_upload_y,
          input$scat_upload_x %in% names(df),
          input$scat_upload_y %in% names(df)
        )
        
        validate(
          need(
            input$scat_upload_x != input$scat_upload_y,
            "Please select different X and Y variables."
          )
        )
        
        selected_data <- data.frame(
          X = df[[input$scat_upload_x]],
          Y = df[[input$scat_upload_y]]
        )
        
        selected_data$X <- suppressWarnings(
          as.numeric(as.character(selected_data$X))
        )
        
        selected_data$Y <- suppressWarnings(
          as.numeric(as.character(selected_data$Y))
        )
        
        scat_upload_values(selected_data)
      },
      ignoreInit = FALSE
    )
    
    
    # Editable X-Y table
    output$scat_upload_table <- DT::renderDT({
      df <- scat_upload_values()
      req(is.data.frame(df))
      
      DT::datatable(
        df,
        editable = TRUE,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "tip",
          pageLength = 10,
          autoWidth = FALSE,
          scrollX = FALSE,
          columnDefs = list(
            list(
              targets = 0:1,
              width = "70px",
              className = "dt-center"
            )
          )
        ),
        class = "compact"
      )
    })
    
    
    # Edit X or Y values
    observeEvent(input$scat_upload_table_cell_edit, {
      info <- input$scat_upload_table_cell_edit
      df <- scat_upload_values()
      
      req(is.data.frame(df))
      
      row <- info$row
      col <- info$col + 1
      
      if (row >= 1 && row <= nrow(df) &&
          col >= 1 && col <= 2) {
        
        new_value <- suppressWarnings(
          as.numeric(as.character(info$value))
        )
        
        if (length(new_value) == 1 &&
            (is.finite(new_value) || is.na(new_value))) {
          df[row, col] <- new_value
          scat_upload_values(df)
        }
      }
    })
    
    
    # Add observation
    observeEvent(input$scat_upload_add_row, {
      df <- scat_upload_values()
      req(is.data.frame(df))
      
      df[nrow(df) + 1, ] <- list(NA_real_, NA_real_)
      scat_upload_values(df)
    })
    
    
    # Delete last observation
    observeEvent(input$scat_upload_delete_row, {
      df <- scat_upload_values()
      req(is.data.frame(df))
      
      if (nrow(df) > 1) {
        scat_upload_values(
          df[-nrow(df), , drop = FALSE]
        )
      }
    })
    
    
    # Prepare valid data for plotting
    scat_upload_plot_data <- reactive({
      df <- scat_upload_values()
      req(is.data.frame(df))
      
      df <- df[
        is.finite(df$X) & is.finite(df$Y),
        ,
        drop = FALSE
      ]
      
      validate(
        need(
          nrow(df) >= 2,
          "Enter at least two valid pairs of X and Y values."
        )
      )
      
      df
    })
    
    
    # Generate scatter diagram
    scat_upload_gen <- reactive({
      df <- scat_upload_plot_data()
      
      theme_function <- switch(
        input$scat_upload_theme,
        classic = ggplot2::theme_classic(),
        light = ggplot2::theme_light(),
        bw = ggplot2::theme_bw(),
        gray = ggplot2::theme_gray(),
        ggplot2::theme_minimal()
      )
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = X, y = Y)
      ) +
        ggplot2::geom_point(
          colour = input$scat_upload_point_colour,
          shape = as.numeric(input$scat_upload_point_shape),
          size = input$scat_upload_point_size,
          alpha = input$scat_upload_alpha
        ) +
        ggplot2::labs(
          title = input$scat_upload_title,
          x = input$scat_upload_xlab,
          y = input$scat_upload_ylab
        ) +
        theme_function +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold"
          ),
          panel.grid.major = if (isTRUE(input$scat_upload_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = if (isTRUE(input$scat_upload_grid)) {
            ggplot2::element_line()
          } else {
            ggplot2::element_blank()
          }
        )
      
      # Regression / trend line
      if (isTRUE(input$scat_upload_show_line)) {
        
        method <- input$scat_upload_line_method
        
        if (method == "loess" &&
            length(unique(df$X)) < 3) {
          method <- "lm"
        }
        
        if (method == "loess" &&
            length(unique(df$X)) >= 3) {
          
          p <- p + suppressMessages(
            ggplot2::geom_smooth(
              data = df,
              mapping = ggplot2::aes(x = X, y = Y),
              method = "loess",
              se = FALSE,
              colour = input$scat_upload_line_colour,
              linetype = input$scat_upload_line_type,
              linewidth = input$scat_upload_line_width
            )
          )
          
        } else {
          
          p <- p + ggplot2::geom_smooth(
            data = df,
            mapping = ggplot2::aes(x = X, y = Y),
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            colour = input$scat_upload_line_colour,
            linetype = input$scat_upload_line_type,
            linewidth = input$scat_upload_line_width
          )
        }
      }
      
      # Coordinate labels
      if (isTRUE(input$scat_upload_show_labels)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(
            label = paste0("(", X, ", ", Y, ")")
          ),
          colour = input$scat_upload_point_colour,
          vjust = -0.8,
          size = 3,
          check_overlap = TRUE
        )
      }
      
      p
    })
    
    
    # Display plot
    output$scat_upload_plot <- renderPlot({
      scat_upload_gen()
    }, height = 500, res = 96)
    
    
    # Download PNG
    output$scat_upload_download <- downloadHandler(
      filename = function() {
        paste0("Uploaded_Scatter_Diagram_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = scat_upload_gen(),
          width = input$scat_upload_out_width,
          height = input$scat_upload_out_height,
          dpi = input$scat_upload_dpi,
          units = "in",
          bg = "white"
        )
      }
    )  
    
    
    
    
    # ============================================================
    # CIRCULAR COLUMN CHART
    # ============================================================
    # ============================================================
    # CIRCULAR COLUMN CHART
    # ============================================================
    
    # Initialize table when category count changes
    observeEvent(
      input$circ_num_cats,
      {
        val$circ <- make_1d_df(
          input$circ_num_cats,
          val$circ
        )
      },
      ignoreInit = FALSE
    )
    
    # Reset table
    observeEvent(
      input$circ_update,
      {
        val$circ <- make_1d_df(
          input$circ_num_cats
        )
      }
    )
    
    # Calculate frequency and percentage
    circ_data <- reactive({
      req(val$circ)
      calc_1d(val$circ)
    })
    
    # Editable table
    output$circ_table <- DT::renderDT({
      DT::datatable(
        val$circ,
        editable = TRUE,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(
              targets = 0:1,
              width = "30px",
              className = "dt-center"
            )
            
          )
        ),
        class = "compact"
      )
    })
    
    # Update table cells
    observeEvent(
      input$circ_table_cell_edit,
      {
        info <- input$circ_table_cell_edit
        df <- val$circ
        
        row <- info$row
        col <- info$col + 1
        
        if (row >= 1 && row <= nrow(df) &&
            col >= 1 && col <= ncol(df)) {
          
          if (col == 1) {
            df[row, col] <- as.character(info$value)
          } else if (col == 2) {
            new_value <- suppressWarnings(
              as.numeric(info$value)
            )
            
            if (is.finite(new_value) && new_value >= 0) {
              df[row, col] <- new_value
            }
          }
          
          val$circ <- df
        }
      }
    )
    
    # ============================================================
    # GENERATE CIRCULAR COLUMN CHART
    # ============================================================
    # ============================================================
    # CIRCULAR COLUMN CHART WITH ADJUSTABLE INNER HOLE
    # ============================================================
    
    
    circ_gen <- reactive({
      
      df <- circ_data()
      req(nrow(df) > 0)
      
      df <- df[
        !is.na(df$Category) &
          trimws(as.character(df$Category)) != "",
        ,
        drop = FALSE
      ]
      
      df$Category <- as.character(df$Category)
      
      validate(
        need(nrow(df) > 0, "Enter at least one category."),
        need(
          all(is.finite(df$Frequency)) &&
            all(df$Frequency >= 0),
          "Frequencies must be non-negative numbers."
        )
      )
      
      # Select plotting value
      yvar <- if (identical(input$circ_val_type, "Percentage")) {
        "Percentage"
      } else {
        "Frequency"
      }
      
      df$Value <- df[[yvar]]
      
      validate(
        need(
          all(is.finite(df$Value)) &&
            all(df$Value >= 0),
          "Plot values must be non-negative."
        )
      )
      
      # Order categories
      df$Category <- factor(
        df$Category,
        levels = unique(df$Category)
      )
      
      # Label positions
      max_value <- max(df$Value, na.rm = TRUE)
      if (max_value <= 0) max_value <- 1
      
      df$Label <- if (yvar == "Percentage") {
        sprintf("%.1f%%", df$Value)
      } else {
        format(round(df$Value, 2), trim = TRUE)
      }
      
      # Selected palette
      n_cat <- nlevels(df$Category)
      
      palette_base <- RColorBrewer::brewer.pal(
        min(8, max(3, min(n_cat, 8))),
        input$circ_palette
      )
      
      palette_colours <- grDevices::colorRampPalette(
        palette_base
      )(n_cat)
      
      names(palette_colours) <- levels(df$Category)
      
      # Theme
      theme_function <- switch(
        input$circ_theme,
        classic = ggplot2::theme_classic(),
        light = ggplot2::theme_light(),
        bw = ggplot2::theme_bw(),
        gray = ggplot2::theme_gray(),
        void = ggplot2::theme_void(),
        ggplot2::theme_minimal()
      )
      
      # Build chart
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = Category,
          y = Value,
          fill = Category
        )
      ) +
        ggplot2::geom_col(
          width = 0.82,
          colour = input$circ_border,
          linewidth = 0.8,
          alpha = input$circ_alpha
        ) +
        ggplot2::scale_fill_manual(
          values = palette_colours,
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, max_value * 1.35),
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::coord_polar(theta = "x") +
        ggplot2::labs(
          title = input$circ_title,
          x = input$circ_xlab,
          y = input$circ_ylab,
          fill = NULL
        ) +
        theme_function +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            size = input$circ_title_size,
            face = "bold"
          ),
          text = ggplot2::element_text(
            size = input$circ_font
          ),
          legend.position = if (input$circ_legend == "none") {
            "none"
          } else {
            input$circ_legend
          },
          plot.margin = ggplot2::margin(15, 25, 15, 25)
        )
      
      # Labels outside columns
      if (isTRUE(input$circ_show_labels)) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              y = Value + max_value * 0.07,
              label = Label
            ),
            size = input$circ_font / 4
          )
      }
      
      # Approximate central white hole
      # Note: this is a visual overlay, not a true donut coordinate scale.
      if (input$circ_hole > 0) {
        p <- p +
          ggplot2::annotate(
            "point",
            x = 1,
            y = 0,
            shape = 21,
            size = input$circ_hole * 0.35,
            colour = "white",
            fill = "white",
            stroke = 0
          )
      }
      
      p
    })
    
    
    # Display chart
    output$circ_plot <- shiny::renderPlot({
      print(circ_gen())
    }, height = 520, res = 96)
    
    # Export PNG using selected dimensions and resolution
    output$circ_download <- downloadHandler(
      
      filename = function() {
        paste0(
          "Circular_Column_Chart_",
          Sys.Date(),
          ".png"
        )
      },
      
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = circ_gen(),
          width = input$circ_out_width,
          height = input$circ_out_height,
          dpi = as.numeric(input$circ_dpi),
          units = "in",
          bg = "white"
        )
      }
    )
    
  #upload data circular chart---------------------------------------------------  
    
# ============================================================
# UPLOADED DATA: CIRCULAR COLUMN CHART
# ============================================================

# Dynamic category selector
output$circ_upload_category_ui <- renderUI({

  df <- global_data()

  req(is.data.frame(df), nrow(df) > 0, ncol(df) > 0)

  selectInput(
    "circ_upload_category",
    "Select Category Variable",
    choices = names(df),
    selected = names(df)[1]
  )
})


# Summarize uploaded data
circ_upload_summary <- reactive({

  df <- global_data()

  req(
    is.data.frame(df),
    nrow(df) > 0,
    input$circ_upload_category,
    input$circ_upload_category %in% names(df)
  )

  category <- as.character(
    df[[input$circ_upload_category]]
  )

  category[is.na(category) | trimws(category) == ""] <-
    "(Missing)"

  result <- as.data.frame(
    table(category, useNA = "no"),
    stringsAsFactors = FALSE
  )

  names(result) <- c("Category", "Frequency")

  result$Frequency <- as.numeric(result$Frequency)

  result$Percentage <- if (sum(result$Frequency) > 0) {
    result$Frequency / sum(result$Frequency) * 100
  } else {
    0
  }

  result
})


# Display frequency table
output$circ_upload_table <- DT::renderDT({

  df <- circ_upload_summary()

  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE
    )
  ) |>
    DT::formatRound(
      columns = "Percentage",
      digits = 2
    )
})


# ============================================================
# GENERATE CIRCULAR COLUMN CHART
# ============================================================

circ_upload_gen <- reactive({

  df <- circ_upload_summary()

  req(nrow(df) > 0)

  # Select frequency or percentage
  yvar <- if (
    identical(input$circ_upload_value_type, "Percentage")
  ) {
    "Percentage"
  } else {
    "Frequency"
  }

  df$Value <- df[[yvar]]

  df$Category <- factor(
    df$Category,
    levels = df$Category
  )

  max_value <- max(df$Value, na.rm = TRUE)

  if (!is.finite(max_value) || max_value <= 0) {
    max_value <- 1
  }

  df$Label <- if (yvar == "Percentage") {
    sprintf("%.1f%%", df$Value)
  } else {
    format(round(df$Value, 2), trim = TRUE)
  }

  # Generate distinct palette colours
  n_cat <- nlevels(df$Category)

  palette_info <- RColorBrewer::brewer.pal.info

  max_palette_colours <- palette_info[
    input$circ_upload_palette,
    "maxcolors"
  ]

  base_n <- min(
    max_palette_colours,
    max(3, n_cat)
  )

  base_colours <- RColorBrewer::brewer.pal(
    base_n,
    input$circ_upload_palette
  )

  palette_colours <- grDevices::colorRampPalette(
    base_colours
  )(n_cat)

  names(palette_colours) <- levels(df$Category)

  # Chart theme
  theme_function <- switch(
    input$circ_upload_theme,
    classic = ggplot2::theme_classic(),
    light = ggplot2::theme_light(),
    bw = ggplot2::theme_bw(),
    gray = ggplot2::theme_gray(),
    void = ggplot2::theme_void(),
    ggplot2::theme_minimal()
  )

  # Calculate inner-hole radius.
  # A negative lower radial limit creates a genuine central hole.
  hole_fraction <- input$circ_upload_hole / 100

  radial_min <- -max_value * hole_fraction /
    max(1 - hole_fraction, 0.01)

  radial_max <- max_value * 1.35

  # Build chart
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Category,
      y = Value,
      fill = Category
    )
  ) +
    ggplot2::geom_col(
      width = 0.82,
      colour = input$circ_upload_border,
      linewidth = 0.8,
      alpha = input$circ_upload_alpha
    ) +
    ggplot2::scale_fill_manual(
      values = palette_colours,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(radial_min, radial_max),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_polar(
      theta = "x",
      clip = "off"
    ) +
    ggplot2::labs(
      title = input$circ_upload_title,
      x = input$circ_upload_xlab,
      y = input$circ_upload_ylab,
      fill = NULL
    ) +
    theme_function +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = input$circ_upload_title_size,
        face = "bold"
      ),
      text = ggplot2::element_text(
        size = input$circ_upload_font
      ),
      legend.position = input$circ_upload_legend,
      plot.margin = ggplot2::margin(20, 30, 20, 30)
    )

  # Add labels outside the columns
  if (isTRUE(input$circ_upload_show_labels)) {

    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          y = Value + max_value * 0.07,
          label = Label
        ),
        size = input$circ_upload_font / 4
      )
  }

  p
})


# Display chart
output$circ_upload_plot <- renderPlot({

  print(circ_upload_gen())

}, height = 520, res = 96)


# Export PNG
output$circ_upload_download <- downloadHandler(

  filename = function() {
    paste0(
      "Uploaded_Circular_Column_Chart_",
      Sys.Date(),
      ".png"
    )
  },

  content = function(file) {

    ggplot2::ggsave(
      filename = file,
      plot = circ_upload_gen(),
      width = input$circ_upload_out_width,
      height = input$circ_upload_out_height,
      dpi = as.numeric(input$circ_upload_dpi),
      units = "in",
      bg = "white"
    )
  }
)

    
    
    
    
#correlation--------------------------------------------------------    
    observe({
      df <- global_data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      updateSelectInput(session, "var_xcor", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "var_ycor", choices = num_cols, selected = num_cols[2])
      updateSelectInput(session, "dep_var", choices = num_cols, selected = num_cols[1])
      updateCheckboxGroupInput(session, "ind_vars", choices = num_cols, selected = num_cols[2:3])
    })
    
    # Data table preview tab
    output$table_preview <- renderDT({
      datatable(global_data(), options = list(pageLength = 10))
    })
    
    # Statistical computation rendered as a structured Table
    output$summary_table <- renderTable({
      df <- global_data()
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor, input$var_ycor)
        x <- df[[input$var_xcor]]
        y <- df[[input$var_ycor]]
        
        cor_res <- cor.test(x, y, method = "pearson")
        
        data.frame(
          Metric = c("Independent Variable (X)", 
                     "Dependent Variable (Y)", 
                     "Pearson Correlation Coefficient (r)", 
                     "t-Statistic", 
                     "Degrees of Freedom (df)", 
                     "P-Value"),
          Value = c(input$var_xcor, 
                    input$var_ycor, 
                    round(cor_res$estimate, 4), 
                    round(cor_res$statistic, 4), 
                    as.character(cor_res$parameter), 
                    format.pval(cor_res$p.value, digits = 4))
        )
        
      } else {
        req(input$dep_var, input$ind_vars)
        all_vars <- c(input$dep_var, input$ind_vars)
        sub_df <- df[, all_vars, drop = FALSE]
        
        # FIX: Wrap variable names in backticks to prevent name collision/function lookup errors
        dep_safe <- paste0("`", input$dep_var, "`")
        ind_safe <- paste0("`", input$ind_vars, "`")
        formula_str <- paste(dep_safe, "~", paste(ind_safe, collapse = " + "))
        
        fit <- lm(as.formula(formula_str), data = df)
        r_sq <- summary(fit)$r.squared
        
        data.frame(
          Metric = c("Dependent Variable (Y)", 
                     "Independent Variables (X)", 
                     "Multiple R-Squared (R²)", 
                     "Multiple Correlation Coefficient (R)"),
          Value = c(input$dep_var, 
                    paste(input$ind_vars, collapse = ", "), 
                    round(r_sq, 4), 
                    round(sqrt(r_sq), 4))
        )
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Plotting output
    output$main_plot <- renderPlot({
      df <- global_data()
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor, input$var_ycor)
        ggplot(df, aes(x = .data[[input$var_xcor]], y = .data[[input$var_ycor]])) +
          geom_point(color = "steelblue", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", formula = y ~ x, color = "darkred", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", input$var_ycor, "vs", input$var_xcor),
               x = input$var_xcor, y = input$var_ycor)
        
      } else {
        req(input$dep_var, input$ind_vars)
        all_vars <- c(input$dep_var, input$ind_vars)
        sub_df <- df[, all_vars, drop = FALSE]
        cor_mat <- cor(sub_df, use = "complete.obs", method = "pearson")
        
        corrplot(cor_mat, method = "color", type = "upper", 
                 addCoef.col = "black", tl.col = "black", tl.srt = 45,
                 diag = FALSE, title = "Pearson Correlation Matrix Heatmap", mar = c(0,0,1,0))
      }
    })
    
    generate_plot_file <- function(filepath) {
      df <- global_data()
      png(filepath, width = 800, height = 600, res = 100)
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor, input$var_ycor)
        p <- ggplot(df, aes(x = .data[[input$var_xcor]], y = .data[[input$var_ycor]])) +
          geom_point(color = "steelblue", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", formula = y ~ x, color = "darkred", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", input$var_ycor, "vs", input$var_xcor),
               x = input$var_xcor, y = input$var_ycor)
        print(p)
      } else {
        req(input$dep_var, input$ind_vars)
        all_vars <- c(input$dep_var, input$ind_vars)
        sub_df <- df[, all_vars, drop = FALSE]
        cor_mat <- cor(sub_df, use = "complete.obs", method = "pearson")
        
        corrplot(cor_mat, method = "color", type = "upper", 
                 addCoef.col = "black", tl.col = "black", tl.srt = 45,
                 diag = FALSE, title = "Pearson Correlation Matrix Heatmap", mar = c(0,0,1,0))
      }
      
      dev.off()
    }
    
    # Download handler for Word document containing both table and plot
    output$download_word <- downloadHandler(
      filename = function() {
        paste("correlation_report_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        # 1. Create temporary file for the plot image
        tmp_img <- tempfile(fileext = ".png")
        generate_plot_file(tmp_img)
        
        # 2. Build Word document
        doc <- read_docx()
        doc <- body_add_par(doc, "Global Data: Pearson Correlation Analysis Report", style = "heading 1")
        doc <- body_add_par(doc, paste("Analysis Type:", ifelse(input$dataset_choice == "simple", "Simple Correlation", "Multiple Correlation Matrix")))
        doc <- body_add_par(doc, paste("Generated Date:", Sys.Date()))
        doc <- body_add_par(doc, "") 
        
        # Add Section: Statistical Results
        doc <- body_add_par(doc, "1. Statistical Summary", style = "heading 2")
        doc <- body_add_table(doc, value = summary_df(), style = "table default")
        doc <- body_add_par(doc, "") 
        
        # Add Section: Visualization Plot
        doc <- body_add_par(doc, "2. Data Visualization", style = "heading 2")
        doc <- body_add_img(doc, src = tmp_img, width = 6.0, height = 4.5)
        
        # 3. Save file
        print(doc, target = file)
      }
    )  
    
#correlation enter data-----------------------------------------
    
#--------------------------------------------------------------    
    # Reactive values to hold the editable data frame state
    # Reactive values to hold the editable data frame state
    values <- reactiveValues()
    
    # Initialize or reset the table based on user dimensions
    observeEvent(input$update_table, {
      vars <- input$cnum_vars
      obs <- input$cnum_obs
      
      col_names <- paste0("Var_", 1:vars)
      df <- as.data.frame(matrix(round(rnorm(vars * obs, mean = 50, sd = 15), 2), nrow = obs))
      names(df) <- col_names
      values$df <- df
    }, ignoreNULL = FALSE)
    
    # Capture user edits from the rhandsontable widget
    current_data <- reactive({
      if (!is.null(input$editable_table)) {
        df <- hot_to_r(input$editable_table)
        df[] <- lapply(df, function(col) as.numeric(as.character(col)))
        return(df)
      }
      if (!is.null(values$df)) {
        return(values$df)
      }
      return(data.frame())
    })
    
    # Render the editable grid UI component
    output$editable_table <- renderRHandsontable({
      df <- values$df
      req(df)
      rhandsontable(df, stretchH = "all") %>%
        hot_cols(type = "numeric")
    })
    
    # Dynamically update selection inputs based on current table columns
    observe({
      df <- current_data()
      req(ncol(df) > 0)
      num_cols <- names(df)
      
      updateSelectInput(session, "var_xcor1", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "var_ycor1", choices = num_cols, selected = num_cols[min(2, length(num_cols))])
      updateSelectInput(session, "dep_var1", choices = num_cols, selected = num_cols[1])
      updateCheckboxGroupInput(session, "ind_vars1", choices = num_cols, selected = num_cols[min(2, length(num_cols)):min(3, length(num_cols))])
    })
    
    # Reactive summary table generator
    summary_df <- reactive({
      df <- current_data()
      validate(need(nrow(df) > 2, "Please provide at least 3 rows of data."))
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor1, input$var_ycor1)
        x <- df[[input$var_xcor1]]
        y <- df[[input$var_ycor1]]
        
        validate(need(!any(is.na(x)) && !any(is.na(y)), "Please fill in all table cells with valid numbers."))
        cor_res <- cor.test(x, y, method = "pearson")
        
        data.frame(
          Metric = c("Variable 1 (X)", 
                     "Variable 2 (Y)", 
                     "Pearson Correlation Coefficient (r)", 
                     "t-Statistic", 
                     "Degrees of Freedom (df)", 
                     "P-Value"),
          Value = c(input$var_xcor1, 
                    input$var_ycor1, 
                    round(cor_res$estimate, 4), 
                    round(cor_res$statistic, 4), 
                    as.character(cor_res$parameter), 
                    format.pval(cor_res$p.value, digits = 4))
        )
        
      } else {
        req(input$dep_var1, input$ind_vars1)
        all_vars <- unique(c(input$dep_var1, input$ind_vars1))
        validate(need(length(all_vars) >= 2, "Please select at least two variables for multiple correlation analysis."))
        
        sub_df <- df[, all_vars, drop = FALSE]
        validate(need(!any(is.na(sub_df)), "Please fill in all table cells with valid numbers."))
        
        dep_safe <- paste0("`", input$dep_var1, "`")
        ind_safe <- paste0("`", input$ind_vars1, "`")
        formula_str <- paste(dep_safe, "~", paste(ind_safe, collapse = " + "))
        
        fit <- lm(as.formula(formula_str), data = df)
        r_sq <- summary(fit)$r.squared
        
        data.frame(
          Metric = c("Target Dependent Variable (Y)", 
                     "Variables Among Which Correlation is Analyzed", 
                     "Multiple R-Squared (R²)", 
                     "Multiple Correlation Coefficient (R)"),
          Value = c(input$dep_var, 
                    paste(all_vars, collapse = ", "), 
                    round(r_sq, 4), 
                    round(sqrt(r_sq), 4))
        )
      }
    })
    
    # Render statistical results in UI
    output$csummary_table <- renderTable({
      summary_df()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Helper function to generate the plot and save it to a temp file
    generate_plot_file <- function(filepath) {
      df <- current_data()
      png(filepath, width = 800, height = 600, res = 100)
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor1, input$var_ycor1)
        p <- ggplot(df, aes(x = .data[[input$var_xcor1]], y = .data[[input$var_ycor1]])) +
          geom_point(color = "steelblue", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", formula = y ~ x, color = "darkred", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Correlation Between", input$var_ycor1, "and", input$var_xcor1),
               x = input$var_xcor1, y = input$var_ycor1)
        print(p)
      } else {
        req(input$dep_var1, input$ind_vars1)
        all_vars <- unique(c(input$dep_var1, input$ind_vars1))
        sub_df <- df[, all_vars, drop = FALSE]
        cor_mat <- cor(sub_df, use = "complete.obs", method = "pearson")
        
        corrplot(cor_mat, method = "color", type = "upper", 
                 addCoef.col = "black", tl.col = "black", tl.srt = 45,
                 diag = FALSE, title = "Correlation Matrix Among Variables", mar = c(0,0,1,0))
      }
      
      dev.off()
    }
    
    # Download handler for Word document containing both table and plot
    output$download_word <- downloadHandler(
      filename = function() {
        paste("correlation_report_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        tmp_img <- tempfile(fileext = ".png")
        generate_plot_file(tmp_img)
        
        doc <- read_docx()
        doc <- body_add_par(doc, "Pearson Correlation Analysis Report", style = "heading 1")
        doc <- body_add_par(doc, paste("Analysis Type:", ifelse(input$dataset_choice == "simple", "Simple Correlation (Between Variables)", "Multiple Correlation (Among Variables)")))
        doc <- body_add_par(doc, paste("Generated Date:", Sys.Date()))
        doc <- body_add_par(doc, "") 
        
        doc <- body_add_par(doc, "1. Statistical Summary", style = "heading 2")
        doc <- body_add_table(doc, value = summary_df(), style = "table default")
        doc <- body_add_par(doc, "") 
        
        doc <- body_add_par(doc, "2. Data Visualization", style = "heading 2")
        doc <- body_add_img(doc, src = tmp_img, width = 6.0, height = 4.5)
        
        print(doc, target = file)
      }
    )
    
    # Plotting output in UI
    output$cmain_plot <- renderPlot({
      df <- current_data()
      req(nrow(df) > 0)
      
      if (input$dataset_choice == "simple") {
        req(input$var_xcor1, input$var_ycor1)
        ggplot(df, aes(x = .data[[input$var_xcor1]], y = .data[[input$var_ycor1]])) +
          geom_point(color = "steelblue", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", formula = y ~ x, color = "darkred", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Correlation Between", input$var_ycor1, "and", input$var_xcor1),
               x = input$var_xcor1, y = input$var_xcor1)
        
      } else {
        req(input$dep_var, input$ind_vars)
        all_vars <- unique(c(input$dep_var1, input$ind_vars1))
        sub_df <- df[, all_vars, drop = FALSE]
        cor_mat <- cor(sub_df, use = "complete.obs", method = "pearson")
        
        corrplot(cor_mat, method = "color", type = "upper", 
                 addCoef.col = "black", tl.col = "black", tl.srt = 45,
                 diag = FALSE, title = "Correlation Matrix Among Variables", mar = c(0,0,1,0))
      }
    })
    
    
#Binary logistic--------------------------------------------    
    
#---------------------------------------------------
    observe({
      
      df <- global_data()
      
      all_cols <- names(df)
      
      predictor_cols <- setdiff(
        all_cols,
        "Country"
      )
      
      updateSelectInput(
        session,
        "bin_y_simple",
        choices = all_cols,
        selected = "Default_Status"
      )
      
      updateSelectInput(
        session,
        "bin_x_simple",
        choices = predictor_cols,
        selected = "Region"
      )
      
      updateSelectInput(
        session,
        "bin_y_multi",
        choices = all_cols,
        selected = "Default_Status"
      )
      
      updateCheckboxGroupInput(
        session,
        "bin_x_multi",
        choices = predictor_cols,
        selected = c(
          "Region",
          "GDP_Per_Capita",
          "Inflation_Rate"
        )
      )
    })
    
    
    # ==========================================================
    # SIMPLE MODEL REFERENCE CATEGORY
    # ==========================================================
    
    output$ref_cat_simple_ui <- renderUI({
      
      df <- global_data()
      
      req(input$bin_x_simple)
      
      x_var <- input$bin_x_simple
      
      if (
        is.factor(df[[x_var]]) ||
        is.character(df[[x_var]])
      ) {
        
        cats <- unique(
          as.character(df[[x_var]])
        )
        
        selectInput(
          "ref_cat_simple",
          paste(
            "Reference Category for",
            x_var,
            ":"
          ),
          choices = cats,
          selected = cats[1]
        )
        
      } else {
        
        NULL
      }
    })
    
    
    # ==========================================================
    # MULTIPLE MODEL REFERENCE CATEGORIES
    # ==========================================================
    
    output$ref_cat_multi_ui <- renderUI({
      
      df <- global_data()
      
      req(input$bin_x_multi)
      
      if (length(input$bin_x_multi) == 0) {
        return(NULL)
      }
      
      cat_vars <- input$bin_x_multi[
        sapply(
          input$bin_x_multi,
          function(v) {
            is.factor(df[[v]]) ||
              is.character(df[[v]])
          }
        )
      ]
      
      if (length(cat_vars) == 0) {
        return(NULL)
      }
      
      tagList(
        
        lapply(
          cat_vars,
          function(v) {
            
            cats <- unique(
              as.character(df[[v]])
            )
            
            selectInput(
              paste0("ref_cat_", v),
              paste(
                "Reference Category for",
                v,
                ":"
              ),
              choices = cats,
              selected = cats[1]
            )
          }
        )
      )
    })
    

    # ==========================================================
    # PROCESS DATA
    # APPLY REFERENCE CATEGORIES
    # ==========================================================
    
    processed_data <- reactive({
      
      df <- global_data()
      
      if (input$model_choice == "simple") {
        
        req(input$bin_x_simple)
        
        x_var <- input$bin_x_simple
        
        if (
          is.factor(df[[x_var]]) ||
          is.character(df[[x_var]])
        ) {
          
          req(input$ref_cat_simple)
          
          df[[x_var]] <- factor(
            df[[x_var]]
          )
          
          df[[x_var]] <- relevel(
            df[[x_var]],
            ref = input$ref_cat_simple
          )
        }
        
      } else {
        
        req(input$bin_x_multi)
        
        if (length(input$bin_x_multi) > 0) {
          
          for (v in input$bin_x_multi) {
            
            if (
              is.factor(df[[v]]) ||
              is.character(df[[v]])
            ) {
              
              ref_id <- paste0(
                "ref_cat_",
                v
              )
              
              ref_value <- input[[ref_id]]
              
              if (!is.null(ref_value)) {
                
                df[[v]] <- factor(
                  df[[v]]
                )
                
                df[[v]] <- relevel(
                  df[[v]],
                  ref = ref_value
                )
              }
            }
          }
        }
      }
      
      df
    })
    
    
    # ==========================================================
    # CHECK BINARY OUTCOME
    # ==========================================================
    
    validate_binary_outcome <- function(df, y_var) {
      
      req(y_var)
      
      y <- df[[y_var]]
      
      if (is.factor(y)) {
        
        if (nlevels(y) != 2) {
          
          validate(
            need(
              FALSE,
              paste(
                "The selected outcome variable must have exactly",
                "two categories."
              )
            )
          )
        }
        
      } else {
        
        unique_values <- unique(
          na.omit(y)
        )
        
        if (
          length(unique_values) != 2 ||
          !all(unique_values %in% c(0, 1))
        ) {
          
          validate(
            need(
              FALSE,
              "The outcome variable must contain exactly two values coded as 0 and 1."
            )
          )
        }
      }
    }
    
    
    # ==========================================================
    # FIT LOGISTIC REGRESSION MODEL
    # ==========================================================
    
    fitted_model <- reactive({
      
      df <- processed_data()
      
      if (input$model_choice == "simple") {
        
        req(
          input$bin_y_simple,
          input$bin_x_simple
        )
        
        y_var <- input$bin_y_simple
        x_var <- input$bin_x_simple
        
      } else {
        
        req(
          input$bin_y_multi,
          input$bin_x_multi
        )
        
        validate(
          need(
            length(input$bin_x_multi) > 0,
            "Please select at least one predictor variable."
          )
        )
        
        y_var <- input$bin_y_multi
      }
      
      
      # Validate outcome
      validate_binary_outcome(
        df,
        y_var
      )
      
      
      # Remove rows with missing values
      if (input$model_choice == "simple") {
        
        model_vars <- c(
          y_var,
          x_var
        )
        
      } else {
        
        model_vars <- c(
          y_var,
          input$bin_x_multi
        )
      }
      
      model_df <- df[
        complete.cases(
          df[, model_vars, drop = FALSE]
        ),
        ,
        drop = FALSE
      ]
      
      
      validate(
        need(
          nrow(model_df) > 10,
          "Not enough complete observations for the selected model."
        )
      )
      
      
      # Build formula safely
      if (input$model_choice == "simple") {
        
        formula_str <- paste0(
          "`",
          y_var,
          "` ~ `",
          x_var,
          "`"
        )
        
      } else {
        
        predictors <- paste0(
          "`",
          input$bin_x_multi,
          "`"
        )
        
        formula_str <- paste0(
          "`",
          y_var,
          "` ~ ",
          paste(
            predictors,
            collapse = " + "
          )
        )
      }
      
      
      fit <- tryCatch(
        
        glm(
          as.formula(formula_str),
          data = model_df,
          family = binomial
        ),
        
        error = function(e) {
          
          validate(
            need(
              FALSE,
              paste(
                "Model could not be fitted:",
                e$message
              )
            )
          )
        }
      )
      
      fit
    })
    
    
    # ==========================================================
    # COEFFICIENT TABLE
    # ==========================================================
    
    coef_df <- reactive({
      
      fit <- fitted_model()
      
      s <- summary(fit)
      
      coefs <- as.data.frame(s$coefficients)
      
      terms <- rownames(coefs)
      
      # Wald 95% CI on coefficient scale
      lower <- coefs$Estimate -
        qnorm(0.975) * coefs$`Std. Error`
      
      upper <- coefs$Estimate +
        qnorm(0.975) * coefs$`Std. Error`
      
      # Convert coefficients to Odds Ratios
      OR <- exp(coefs$Estimate)
      
      OR_lower <- exp(lower)
      
      OR_upper <- exp(upper)
      
      # Determine categorical variables
      df <- global_data()
      
      if (input$model_choice == "simple") {
        
        categorical_vars <- character(0)
        
        if (!is.null(input$bin_x_simple) &&
            input$bin_x_simple %in% names(df)) {
          
          if (is.factor(df[[input$bin_x_simple]]) ||
              is.character(df[[input$bin_x_simple]])) {
            
            categorical_vars <- input$bin_x_simple
          }
        }
        
      } else {
        
        categorical_vars <- character(0)
        
        if (!is.null(input$bin_x_multi) &&
            length(input$bin_x_multi) > 0) {
          
          categorical_vars <- input$bin_x_multi[
            sapply(
              input$bin_x_multi,
              function(v) {
                
                if (!v %in% names(df)) {
                  return(FALSE)
                }
                
                is.factor(df[[v]]) ||
                  is.character(df[[v]])
              }
            )
          ]
        }
      }
      
      
      # ----------------------------------------------------------
      # Create Metric Type safely
      # ----------------------------------------------------------
      
      Metric_Type <- rep(
        "Estimate (Numeric)",
        length(terms)
      )
      
      # Intercept
      Metric_Type[terms == "(Intercept)"] <-
        "Coefficient"
      
      
      # Categorical predictor terms
      if (length(categorical_vars) > 0) {
        
        for (v in categorical_vars) {
          
          # Terms generated by factor variables
          ind <- grepl(
            paste0("^", v),
            terms
          )
          
          Metric_Type[ind] <-
            "Odds Ratio (Categorical)"
        }
      }
      
      
      # ----------------------------------------------------------
      # Final table
      # ----------------------------------------------------------
      
      data.frame(
        
        Term = terms,
        
        Estimate = round(
          coefs$Estimate,
          4
        ),
        
        Odds_Ratio = round(
          OR,
          4
        ),
        
        CI_Lower = round(
          OR_lower,
          4
        ),
        
        CI_Upper = round(
          OR_upper,
          4
        ),
        
        P_Value = format.pval(
          coefs$`Pr(>|z|)`,
          digits = 4
        ),
        
        Metric_Type = Metric_Type,
        
        stringsAsFactors = FALSE,
        
        check.names = FALSE,
        
        row.names = NULL
      )
    })
    # ==========================================================
    # DISPLAY COEFFICIENT TABLE
    # ==========================================================
    
    output$model_summary_table <- renderTable({
      
      coef_df()
      
    }, striped = TRUE,
    hover = TRUE,
    bordered = TRUE)
    
    
    # ==========================================================
    # CLASSIFICATION PERFORMANCE
    # ==========================================================
    
    class_perf_metrics <- reactive({
      
      fit <- fitted_model()
      
      df <- model.frame(fit)
      
      actuals <- model.response(df)
      
      preds <- fitted(fit)
      
      
      # Convert outcome to 0/1
      if (is.factor(actuals)) {
        
        actuals <- as.numeric(
          actuals
        ) - 1
        
      } else {
        
        actuals <- as.numeric(
          actuals
        )
      }
      
      
      # ROC
      roc_obj <- tryCatch(
        
        pROC::roc(
          response = actuals,
          predictor = preds,
          quiet = TRUE,
          direction = "<"
        ),
        
        error = function(e) NULL
      )
      
      
      validate(
        need(
          !is.null(roc_obj),
          "ROC analysis could not be calculated."
        )
      )
      
      
      auc_val <- as.numeric(
        pROC::auc(
          roc_obj
        )
      )
      
      
      # 0.5 classification threshold
      pred_classes <- ifelse(
        preds >= 0.5,
        1,
        0
      )
      
      
      cm <- caret::confusionMatrix(
        
        factor(
          pred_classes,
          levels = c(0, 1)
        ),
        
        factor(
          actuals,
          levels = c(0, 1)
        ),
        
        positive = "1"
      )
      
      
      sens_val <- as.numeric(
        cm$byClass["Sensitivity"]
      )
      
      spec_val <- as.numeric(
        cm$byClass["Specificity"]
      )
      
      acc_val <- as.numeric(
        cm$overall["Accuracy"]
      )
      
      
      data.frame(
        
        Metric = c(
          "Area Under Curve (AUC)",
          "Sensitivity (Recall)",
          "Specificity",
          "Overall Accuracy"
        ),
        
        Value = round(
          c(
            auc_val,
            sens_val,
            spec_val,
            acc_val
          ),
          4
        )
      )
    })
    
    
    # ==========================================================
    # DISPLAY CLASSIFICATION PERFORMANCE
    # ==========================================================
    
    output$class_perf_table <- renderTable({
      
      class_perf_metrics()
      
    }, striped = TRUE,
    hover = TRUE,
    bordered = TRUE)
    
    
    # ==========================================================
    # CONFUSION MATRIX
    # ==========================================================
    
    output$conf_matrix_text <- renderPrint({
      
      fit <- fitted_model()
      
      df <- model.frame(fit)
      
      actuals <- model.response(df)
      
      preds <- fitted(fit)
      
      
      if (is.factor(actuals)) {
        
        actuals <- as.numeric(
          actuals
        ) - 1
        
      } else {
        
        actuals <- as.numeric(
          actuals
        )
      }
      
      
      pred_classes <- ifelse(
        preds >= 0.5,
        1,
        0
      )
      
      
      cm <- caret::confusionMatrix(
        
        factor(
          pred_classes,
          levels = c(0, 1)
        ),
        
        factor(
          actuals,
          levels = c(0, 1)
        ),
        
        positive = "1"
      )
      
      
      cm
    })
    
    
    # ==========================================================
    # MODEL FIT METRICS
    # ==========================================================
    
    perf_df <- reactive({
      
      fit <- fitted_model()
      
      
      data.frame(
        
        Metric = c(
          "Null Deviance",
          "Residual Deviance",
          "AIC",
          "Number of Observations"
        ),
        
        Value = c(
          
          round(
            fit$null.deviance,
            2
          ),
          
          round(
            fit$deviance,
            2
          ),
          
          round(
            AIC(fit),
            2
          ),
          
          nobs(fit)
        )
      )
    })
    
    
    output$perf_table <- renderTable({
      
      perf_df()
      
    }, striped = TRUE,
    hover = TRUE,
    bordered = TRUE)
    
    
    # ==========================================================
    # ROC CURVE
    # ==========================================================
    
    output$roc_plot <- renderPlot({
      
      fit <- fitted_model()
      
      df <- model.frame(fit)
      
      actuals <- model.response(df)
      
      preds <- fitted(fit)
      
      
      if (is.factor(actuals)) {
        
        actuals <- as.numeric(
          actuals
        ) - 1
        
      } else {
        
        actuals <- as.numeric(
          actuals
        )
      }
      
      
      roc_obj <- pROC::roc(
        
        response = actuals,
        
        predictor = preds,
        
        quiet = TRUE,
        
        direction = "<"
      )
      
      
      plot(
        
        roc_obj,
        
        col = "steelblue",
        
        lwd = 3,
        
        main = "ROC Curve for Logistic Regression Model"
      )
      
      
      abline(
        
        a = 1,
        
        b = -1,
        
        lty = 2,
        
        col = "red"
      )
      
      
      legend(
        
        "bottomright",
        
        legend = paste(
          "AUC =",
          round(
            as.numeric(
              pROC::auc(
                roc_obj
              )
            ),
            4
          )
        ),
        
        col = "steelblue",
        
        lwd = 3,
        
        bty = "n"
      )
    })
    
    
    # ==========================================================
    # GENERATE ROC IMAGE FOR WORD
    # ==========================================================
    
    generate_plot_file <- function(filepath) {
      
      fit <- fitted_model()
      
      df <- model.frame(fit)
      
      actuals <- model.response(df)
      
      preds <- fitted(fit)
      
      
      if (is.factor(actuals)) {
        
        actuals <- as.numeric(
          actuals
        ) - 1
        
      } else {
        
        actuals <- as.numeric(
          actuals
        )
      }
      
      
      roc_obj <- pROC::roc(
        
        response = actuals,
        
        predictor = preds,
        
        quiet = TRUE,
        
        direction = "<"
      )
      
      
      png(
        
        filepath,
        
        width = 1000,
        
        height = 700,
        
        res = 120
      )
      
      
      plot(
        
        roc_obj,
        
        col = "steelblue",
        
        lwd = 3,
        
        main = "ROC Curve for Logistic Regression Model"
      )
      
      
      abline(
        
        a = 1,
        
        b = -1,
        
        lty = 2,
        
        col = "red"
      )
      
      
      legend(
        
        "bottomright",
        
        legend = paste(
          
          "AUC =",
          
          round(
            as.numeric(
              pROC::auc(
                roc_obj
              )
            ),
            4
          )
        ),
        
        col = "steelblue",
        
        lwd = 3,
        
        bty = "n"
      )
      
      
      dev.off()
    }
    
    
    # ==========================================================
    # WORD REPORT
    # ==========================================================
    
    output$download_word <- downloadHandler(
      
      filename = function() {
        
        paste0(
          "logistic_regression_report_",
          Sys.Date(),
          ".docx"
        )
      },
      
      
      content = function(file) {
        
        tmp_img <- tempfile(
          fileext = ".png"
        )
        
        
        generate_plot_file(
          tmp_img
        )
        
        
        doc <- read_docx()
        
        
        # -----------------------------------------
        # TITLE
        # -----------------------------------------
        
        doc <- body_add_par(
          
          doc,
          
          "Binary Logistic Regression Report",
          
          style = "heading 1"
        )
        
        
        # -----------------------------------------
        # MODEL INFORMATION
        # -----------------------------------------
        
        model_name <- ifelse(
          
          input$model_choice == "simple",
          
          "Simple Binary Logistic Regression",
          
          "Multiple Logistic Regression"
        )
        
        
        doc <- body_add_par(
          
          doc,
          
          paste(
            "Model Type:",
            model_name
          )
        )
        
        
        if (
          input$model_choice == "simple"
        ) {
          
          doc <- body_add_par(
            
            doc,
            
            paste(
              "Outcome Variable:",
              input$bin_y_simple
            )
          )
          
          doc <- body_add_par(
            
            doc,
            
            paste(
              "Predictor Variable:",
              input$bin_x_simple
            )
          )
          
        } else {
          
          doc <- body_add_par(
            
            doc,
            
            paste(
              "Outcome Variable:",
              input$bin_y_multi
            )
          )
          
          doc <- body_add_par(
            
            doc,
            
            paste(
              "Predictor Variables:",
              paste(
                input$bin_x_multi,
                collapse = ", "
              )
            )
          )
        }
        
        
        doc <- body_add_par(
          
          doc,
          
          paste(
            "Generated Date:",
            Sys.Date()
          )
        )
        
        
        doc <- body_add_par(
          doc,
          ""
        )
        
        
        # -----------------------------------------
        # COEFFICIENTS
        # -----------------------------------------
        
        doc <- body_add_par(
          
          doc,
          
          "1. Detailed Regression Coefficients, Odds Ratios & Confidence Intervals",
          
          style = "heading 2"
        )
        
        
        doc <- body_add_table(
          
          doc,
          
          value = coef_df(),
          
          style = "table default"
        )
        
        
        doc <- body_add_par(
          doc,
          ""
        )
        
        
        # -----------------------------------------
        # CLASSIFICATION
        # -----------------------------------------
        
        doc <- body_add_par(
          
          doc,
          
          "2. Classification Performance Metrics",
          
          style = "heading 2"
        )
        
        
        doc <- body_add_table(
          
          doc,
          
          value = class_perf_metrics(),
          
          style = "table default"
        )
        
        
        doc <- body_add_par(
          doc,
          ""
        )
        
        
        # -----------------------------------------
        # MODEL FIT
        # -----------------------------------------
        
        doc <- body_add_par(
          
          doc,
          
          "3. Model Fit Statistics",
          
          style = "heading 2"
        )
        
        
        doc <- body_add_table(
          
          doc,
          
          value = perf_df(),
          
          style = "table default"
        )
        
        
        doc <- body_add_par(
          doc,
          ""
        )
        
        
        # -----------------------------------------
        # ROC
        # -----------------------------------------
        
        doc <- body_add_par(
          
          doc,
          
          "4. ROC Curve",
          
          style = "heading 2"
        )
        
        
        doc <- body_add_img(
          
          doc,
          
          src = tmp_img,
          
          width = 6.5,
          
          height = 4.5
        )
        
        
        # -----------------------------------------
        # SAVE DOCUMENT
        # -----------------------------------------
        
        print(
          doc,
          target = file
        )
      }
    )
    
    
#-----------------------------------    
#Sample size
#----------------------------    
    
    observeEvent(input$calc2, {
      
      # UI text outputs
      output$wr <- renderText("Where")
      
      output$N <- renderText(
        paste("N : Population size =", input$N)
      )
      
      output$Ref2 <- renderText({
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$Ref2))
      })
      
      output$d2 <- renderText(
        paste("d : Margin of error =", input$d2, "%")
      )
      
      # --- Sample size calculation ---
      N      <- input$N
      d      <- input$d2 / 100
      f      <- input$f_2 / 100
      
      # base sample size
      n_base <- N / (1 + N * d^2)
      
      if (f == 0) {
        sample_text <- ""
        final_n <- n_base
      } else {
        sample_text <- paste("With Nonresponse rate:", input$f_2, "%")
        final_n <- n_base / (1 - f)
      }
      
      output$sample_size2 <- renderText({
        paste(
          sample_text,
          paste("n = Required sample size:", ceiling(final_n)),
          sep = "\n"
        )
      })
    })
    
    # Go to next tab
    observeEvent(input$next_tab, {
      updateNavbarPage(session, "tabs", selected = "Help")
    })
    
    # Screenshot button
    observeEvent(input$save_screenshotd, {
      screenshot(
        filename = "sample_Size",
        id = "main2",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Mean estimation(with mean n SD)-----------------------------------------------------------------------------------  
    observeEvent(input$calc1, {
      
      output$wr1 <- renderText("Where")
      
      # ---- Population size text ----
      if (input$Popu == "UnKnown") {
        pop_text <- "Population size = Unknown"
        N1 <- NA  # no finite population correction
      } else {
        pop_text <- paste("Population size =", input$Popu, "and N =", input$N1)
        N1 <- input$N1
      }
      
      output$po_1 <- renderText(pop_text)
      
      
      # ---- Z value based on CI ----
      z_values <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      z1 <- z_values[as.character(input$CI1)]
      
      output$z1 <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", z1, " (For ", input$CI1, " % Confidence Interval) "))
      })
      
      
      # ---- Standard deviation & Mean text ----
      output$SD <- renderText(
        paste("σ : Estimated standard deviation =", input$SD)
      )
      
      output$Mean <- renderText(
        paste("μ : Estimated Mean =", input$mu1)
      )
      
      output$Ref <- renderText(
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$Ref))
      )
      
      
      # ---- Margin of error ----
      if (input$error1 == "Absolute_error") {
        d1 <- input$d1
      } else {
        d1 <- input$mu1 * input$d1 / 100   # relative error
      }
      
      output$d_1 <- renderText(
        paste("d : Margin of error =", input$d1, "(", input$error1, ")")
      )
      
      
      # ---- Base sample size (n1) ----
      SD1 <- input$SD
      n1 <- (z1^2 * SD1^2) / (d1^2)
      
      output$n1 <- renderText(
        paste("n = Required sample size:", ceiling(n1))
      )
      
      
      # ---- Finite population correction ----
      if (is.na(N1)) {
        n_fpc <- n1
      } else {
        n_fpc <- (n1 * N1) / (n1 + (N1 - 1))
      }
      
      
      # ---- Non-response adjustment ----
      f_rate <- input$f1 / 100
      
      if (f_rate == 0) {
        sample_msg <- ""
        final_n <- n_fpc
      } else {
        final_n <- n_fpc / (1 - f_rate)
        sample_msg <- paste("With Nonresponse/Attrition rate =", input$f1, "%")
      }
      
      output$sample_size1 <- renderText(
        paste(
          sample_msg,
          paste("n = Required sample size:", ceiling(final_n)),
          sep = "\n"
        )
      )
    })
    
    
    
    # ---- Navigation & Screenshot ----
    observeEvent(input$next_tab1, {
      updateNavbarPage(session, "tabs", selected = "Help")
    })
    
    observeEvent(input$save_screenshot1, {
      screenshot(
        filename = "sample_Size",
        id = "main1",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Mean estimation(with median n IQR)-----------------------------------------------------------------------------------  
    observeEvent(input$qrcalc1, {
      
      output$wr2 <- renderText("Where")
      
      # ---- Population size handling ----
      if (input$qrPopu == "UnKnown") {
        pop_text <- "Population size = Unknown"
        Nq <- NA
      } else {
        pop_text <- paste("Population size =", input$qrPopu, "and N =", input$qrN1)
        Nq <- input$qrN1
      }
      output$qrpo_1 <- renderText(pop_text)
      
      
      # ---- Z value based on CI ----
      z_lookup <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      zq <- z_lookup[as.character(input$qrCI1)]
      
      output$qrz1 <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", zq, " (For ", input$qrCI1, " % confidence Interval) "))
      })
      
      
      # ---- Reference text ----
      output$qrRef <- renderText(
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$qrRef))
      )
      
      
      # ---- Nonresponse rate text ----
      output$qrf1 <- renderText(
        paste("With Non response/Attrition rate =", input$qrf1, "%")
      )
      
      
      # ---- Margin of error ----
      if (input$qrerror1 == "Absolute_error") {
        dq <- input$qrd1
      } else {
        dq <- input$Q2 * (input$qrd1 / 100)  # relative error
      }
      
      output$qrd_1 <- renderText(
        paste("d : Margin of error =", input$qrd1, "(", input$qrerror1, ")")
      )
      
      
      # ---- Median and SIQR ----
      output$qrQ <- renderText(
        paste("Median =", input$Q2, "and Semi-interquartile range =", input$Q3)
      )
      
      
      # ---- SD estimation from SIQR ----
      SDq <- (2 * input$Q3) / 1.348
      
      output$qrSD <- renderText(
        paste("σ : Estimated Standard deviation =", round(SDq, 1))
      )
      
      output$qrMean <- renderText(
        paste("μ : Estimated Mean =", input$Q2)
      )
      
      
      # ---- Base sample size ----
      nq_base <- (zq^2 * SDq^2) / dq^2
      
      
      # ---- Finite population correction ----
      if (is.na(Nq)) {
        nq_adj <- nq_base
      } else {
        nq_adj <- (nq_base * Nq) / (nq_base + (Nq - 1))
      }
      
      
      # ---- Nonresponse adjustment ----
      f_rate <- input$qrf1 / 100
      
      if (f_rate == 0) {
        final_nq <- nq_adj
        msg_q <- ""
      } else {
        final_nq <- nq_adj / (1 - f_rate)
        msg_q <- paste("With Non response/Attrition rate =", input$qrf1, "%")
      }
      
      
      # ---- Final sample size output ----
      output$qrsample_size1 <- renderText(
        paste(
          msg_q,
          paste("n = Required sample size:", ceiling(final_nq)),
          sep = "\n"
        )
      )
    })
    
    
    
    # ---- Navigation & Screenshot ----
    observeEvent(input$qrnext_tab1, {
      updateNavbarPage(session, "tabs", selected = "Help")
    })
    
    observeEvent(input$qrsave_screenshot1, {
      screenshot(
        filename = "sample_Size",
        id = "qrmain1",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    
    # =========================
    # Two Sample Mean Estimation (Median & SIQR)
    # =========================
    observeEvent(input$qrcalc2, {
      
      output$wr2_2 <- renderText("Where")
      
      # ---- Population size handling ----
      if (input$qrPopu2 == "UnKnown") {
        pop_text <- "Population size = Unknown"
        Nq <- NA
      } else {
        pop_text <- paste("Population size =", input$qrPopu2, "and N =", input$qrN2)
        Nq <- input$qrN2
      }
      output$qrpo_2 <- renderText(pop_text)
      
      
      # ---- Z value ----
      z_lookup <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      zq <- z_lookup[as.character(input$qrCI2)]
      
      output$qrz2 <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> = ", zq,
                    " (For ", input$qrCI2, "% confidence Interval)"))
      })
      
      
      # ---- Reference ----
      output$qrRef2 <- renderText(
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$qrRef2))
      )
      
      
      # ---- Margin of error ----
      if (input$qrerror2 == "Absolute_error") {
        dq <- input$qrd2
      } else {
        # use difference in medians for relative error
        diff_med <- abs(input$Q2_1 - input$Q2_2)
        dq <- diff_med * (input$qrd2 / 100)
      }
      
      output$qrd_2 <- renderText(
        paste("d : Margin of error =", input$qrd2, "(", input$qrerror2, ")")
      )
      
      
      # ---- Median display ----
      output$qrMean_2 <- renderText(
        paste("Group 1 Median =", input$Q2_1,
              " | Group 2 Median =", input$Q2_2)
      )
      
      
      # ---- SD estimation from SIQR ----
      SD1 <- (2 * input$SIQR1) / 1.348
      SD2 <- (2 * input$SIQR2) / 1.348
      
      output$qrSD_2 <- renderText(
        paste("σ1 =", round(SD1, 2), "and σ2 =", round(SD2, 2))
      )
      
      
      # ---- Base sample size (per group) ----
      nq_base <- (2 * (zq^2) * (SD1^2 + SD2^2)) / (dq^2)
      
      
      # ---- Finite population correction ----
      if (is.na(Nq)) {
        nq_adj <- nq_base
      } else {
        nq_adj <- (nq_base * Nq) / (nq_base + (Nq - 1))
      }
      
      
      # ---- Nonresponse adjustment ----
      f_rate <- input$qrf2 / 100
      
      if (f_rate == 0) {
        final_nq <- nq_adj
        msg_q <- ""
      } else {
        final_nq <- nq_adj / (1 - f_rate)
        msg_q <- paste("With Non response/Attrition rate =", input$qrf2, "%")
      }
      
      
      # ---- Final output ----
      output$qrsample_size2 <- renderText(
        paste(
          msg_q,
          paste("n (per group) =", ceiling(final_nq)),
          paste("Total sample size =", ceiling(final_nq) * 2),
          sep = "\n"
        )
      )
    })
    
    observeEvent(input$qrnext_tab2, {
      updateNavbarPage(session, "tabs", selected = "Help")
    })
    
    observeEvent(input$qrsave_screenshot2, {
      screenshot(
        filename = "two_sample_size",
        id = "qrmain2",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    
    
    
    
    #proportion estimation-----------------------------------------------------------------------------------------
    observeEvent(input$calc, {
      
      output$wr3 <- renderText("Where")
      
      # ------------------- Population size -------------------
      if (input$qPopu == "UnKnown") {
        pop_text <- "Population size = Unknown"
        Np <- NA
      } else {
        pop_text <- paste("Population size =", input$qPopu, "and N =", input$N2)
        Np <- input$N2
      }
      output$ppo_1 <- renderText(pop_text)
      
      
      # ------------------- Z value lookup -------------------
      z_lookup <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      z <- z_lookup[as.character(input$CI)]
      
      output$z <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", z, " (For ", input$CI, " % Confidence Interval) "))
      })
      
      
      # ------------------- p and q -------------------
      p <- input$p / 100
      q <- 1 - p
      
      output$p <- renderText(
        paste("p : Estimated Proportion =", input$p, "%  and q =", round(q * 100, 1), "%")
      )
      
      
      # ------------------- Reference -------------------
      output$Ref1 <- renderText(
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$Ref1))
      )
      
      
      # ------------------- Nonresponse rate -------------------
      output$f <- renderText(
        paste("With Non response/Attrition rate =", input$f, "%")
      )
      
      
      # ------------------- Margin of error -------------------
      if (input$error == "Absolute_error") {
        d <- input$d / 100
      } else {
        d <- (input$p * input$d / 10000)   # relative error
      }
      
      output$qd_1 <- renderText(
        paste("d : Margin of error =", input$d, "% (", input$error, ")")
      )
      
      
      # ------------------- Base sample size -------------------
      n_base <- (z^2 * p * q) / (d^2)
      
      
      # ------------------- Finite population correction -------------------
      if (is.na(Np)) {
        n_adj <- n_base
      } else {
        n_adj <- (n_base * Np) / (n_base + (Np - 1))
      }
      
      
      # ------------------- Nonresponse adjustment -------------------
      f_rate <- input$f / 100
      
      if (f_rate == 0) {
        final_n <- n_adj
        msg <- ""
      } else {
        final_n <- n_adj / (1 - f_rate)
        msg <- paste("With Non response/Attrition rate =", input$f, "%")
      }
      
      
      # ------------------- Final output -------------------
      output$sample_size <- renderText(
        paste(
          msg,
          paste("n = Required sample size:", ceiling(final_n)),
          sep = "\n"
        )
      )
    })
    
    
    
    # ------------------- Tab navigation -------------------
    observeEvent(input$next_tab2, {
      updateNavbarPage(session, "tabs", selected = "Help")
    })
    
    
    # ------------------- Screenshot -------------------
    observeEvent(input$qsave_screenshot, {
      screenshot(
        filename = "sample_Size",
        id = "qmain",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #proportion estimation with unknown p-----------------------------------------------------------------------------------------
    observeEvent(input$upcalc, {
      
      # --- WHERE ---
      output$wr4 <- renderText("Where")
      
      # --- Population Size ---
      if (input$upPopu == "UnKnown") {
        pop_text <- ""
        N <- NA
      } else {
        N <- input$upN2
        pop_text <- paste("and N =", N)
      }
      
      output$uppo_1 <- renderText({
        paste("Population size =", input$upPopu, pop_text)
      })
      
      # --- Z value ---
      z_values <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      upz <- z_values[as.character(input$upCI)]
      
      output$upz <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", upz, " (For ", input$upCI, " % Confidence Interval) "))
      })
      
      # --- Non-response rate ---
      output$upf <- renderText({
        paste("Non response / Attrition rate =", input$upf, "%")
      })
      
      # --- Margin of Error ---
      d <- input$upd
      output$upd_1 <- renderText({
        paste("d : Margin of error =", d, "%")
      })
      
      # --- Sample Size (unadjusted) ---
      n <- (upz^2) / (4 * (d/100)^2)
      
      # --- Finite Population Correction ---
      if (input$upPopu == "UnKnown") {
        n_adj <- n
      } else {
        n_adj <- (n * N) / (n + (N - 1))
      }
      
      # --- Adjust for Non-response ---
      if (input$upf == 0) {
        final_n <- n_adj
        note <- ""
      } else {
        final_n <- n_adj / (1 - input$upf / 100)
        note <- paste("With Non response/Attrition rate =", input$upf, "%")
      }
      
      # --- Output final sample size ---
      output$upsample_size <- renderText({
        paste(note, paste("n = Required sample size:", ceiling(final_n)), sep="\n")
      })
    })
    
    # --- NEXT TAB ---
    observeEvent(input$upnext_tab2, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- SAVE SCREENSHOT ---
    observeEvent(input$upsave_screenshot, {
      screenshot(
        filename = "sample_Size",
        id = "upmain",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Correlation estimation-----------------------------------------------------------------------------------  
    observeEvent(input$crcalc1, {
      
      # --- WHERE ---
      output$wr5 <- renderText("Where")
      
      # --- Population Size ---
      if (input$crPopu == "UnKnown") {
        pop_text <- ""
        N <- NA
      } else {
        N <- input$crN1
        pop_text <- paste("and N =", N)
      }
      
      output$crpo_1 <- renderText({
        paste("Population size =", input$crPopu, pop_text)
      })
      
      # --- Z value ---
      z_values <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      crz1 <- z_values[as.character(input$crCI)]
      
      output$crz1 <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", crz1, " (For ", input$crCI, " % Confidence Interval) "))
      })
      
      # --- Anticipated Correlation ---
      output$Ancr <- renderText({
        paste("r : Anticipated correlation =", input$cr)
      })
      
      # --- Reference ---
      output$crRef <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$crRef)
      })
      
      # --- Margin of Error ---
      d <- input$crd1
      output$crd_1 <- renderText({
        paste("d : Margin of error =", d, "%")
      })
      
      # --- Base Sample Size ---
      crn1 <- (crz1^2 * (1 - input$cr^2)^2) / ((d / 100)^2) + 1
      
      # --- Finite Population Correction ---
      if (input$crPopu == "UnKnown") {
        n_adj <- crn1
      } else {
        n_adj <- (crn1 * N) / (crn1 + (N - 1))
      }
      
      # --- Adjust for Non-response ---
      if (input$crf1 == 0) {
        final_n <- n_adj
        note <- ""
      } else {
        final_n <- n_adj / (1 - input$crf1 / 100)
        note <- paste("With Non response/Attrition rate =", input$crf1, "%")
      }
      
      # --- Output Final Sample Size ---
      output$crsample_size1 <- renderText({
        paste(note,
              paste("n = Required sample size:", ceiling(final_n)),
              sep = "\n")
      })
      
    })
    
    
    # --- NEXT TAB ---
    observeEvent(input$crnext_tab1, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- SAVE SCREENSHOT ---
    observeEvent(input$crsave_screenshot1, {
      screenshot(
        filename = "sample_Size",
        id = "crmain1",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Sensitivity and Specificity estimation-----------------------------------------------------------------------------------------
    observeEvent(input$sscalc, {
      
      # --- WHERE ---
      output$wr6 <- renderText("Where")
      
      # --- Z value ---
      z_values <- c("90" = 1.645, "95" = 1.96, "99" = 2.58)
      ssz <- z_values[as.character(input$ssCI)]
      
      output$ssz <- renderUI({
        HTML(paste0("Z<sub>(1-α/2)</sub> =", ssz, " (For ", input$ssCI, " % Confidence Interval) "))
      })
      
      # --- Estimated Sensitivity & Specificity ---
      output$sn <- renderUI({HTML(paste0("S<sub>N</sub> : Estimated Sensitivity =", input$SN, "%"))})
      output$sp <- renderUI({HTML(paste0("S<sub>P</sub> : Estimated Specificity =", input$SP, "%"))})
      output$ssp <- renderText(paste("p : Estimated Proportion =", input$ssp, "%"))
      
      # --- Reference ---
      output$ssRef <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$ssRef)
      })
      
      # --- Margin of Error ---
      d <- input$ssd
      output$ssd_1 <- renderText({
        paste("d : Margin of error =", d, "%")
      })
      
      # Convert percentage inputs to proportions
      SN  <- input$SN  / 100
      SP  <- input$SP  / 100
      p   <- input$ssp / 100
      dd  <- d / 100
      
      # --- Sample Size for SN and SP ---
      n_SN <- (ssz^2 * SN * (1 - SN)) / (dd^2 * p)
      n_SP <- (ssz^2 * SP * (1 - SP)) / (dd^2 * (1 - p))
      
      output$snsample_size1 <- renderText({
        paste("n(SN) = Sample size for Sensitivity:", ceiling(n_SN))
      })
      
      output$spsample_size2 <- renderText({
        paste("n(SP) = Sample size for Specificity:", ceiling(n_SP))
      })
      
      # --- Final Required Sample Size ---
      output$snpsample_size <- renderText({
        paste("n = Required Sample size:", max(ceiling(n_SN), ceiling(n_SP)))
      })
      
    })
    
    
    # --- NEXT TAB ---
    observeEvent(input$ssnext_tab, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- SAVE SCREENSHOT ---
    observeEvent(input$sssave_screenshot, {
      screenshot(
        filename = "sample_Size",
        id = "ssmain",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #One sample mean test(Large Sample)--------------------------------------------------------------------------------------------  
    
    observeEvent(input$calc3, {
      
      output$wr7 <- renderText("Where")
      
      # --- Alternative hypothesis ---
      output$tail3 <- renderText(paste("Alternative hypothesis:", input$tail3))
      
      # --- Z values for Confidence Interval ---
      z_two  <- c("90" = 1.645, "95" = 1.96,  "99" = 2.576)
      z_one  <- c("90" = 1.28,  "95" = 1.645, "99" = 2.33)
      
      if (input$tail3 == "Two_sided") {
        z3 <- z_two[as.character(input$CI3)]
      } else {
        z3 <- z_one[as.character(input$CI3)]
      }
      
      output$z3 <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub> =", z3, " (For ", input$CI3, " % Confidence Interval) "))
      })
      
      # --- Power (Z_1-β) ---
      power_z <- c(
        "80" = 0.842,
        "85" = 0.036,
        "90" = 1.28,
        "95" = 1.645,
        "99" = 2.33
      )
      
      b_3 <- power_z[as.character(input$b3)]
      
      output$b_3 <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub> =", b_3, " (For ", input$b3, " % power) "))
      })
      
      # --- Means & SD ---
      output$ad3 <- renderText(paste("μ₀ : Assumed mean =", input$ad3))
      output$SD3 <- renderText(paste("σ : Estimated Standard deviation =", input$SD3))
      output$d3  <- renderText(paste("μ : Estimated mean =", input$d3))
      
      # --- Reference ---
      output$Ref3 <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$Ref3)
      })
      
      # --- Difference between means ---
      md3 <- abs(input$ad3 - input$d3)
      
      output$md3 <- renderText({
        paste("δ : Difference between means =", round(md3, 1))
      })
      
      # --- Nonresponse Rate ---
      output$f3 <- renderText({
        paste("With Non-response / Attrition rate =", input$f3, "%")
      })
      
      # --- Sample Size Formula ---
      n3 <- ((z3 + b_3)^2 * (input$SD3)^2) / (md3)^2
      
      # Adjust for nonresponse
      if (input$f3 == 0) {
        final_n <- n3
        line1 <- ""
      } else {
        final_n <- n3 / (1 - input$f3 / 100)
        line1 <- paste("With Non-response / Attrition rate =", input$f3, "%")
      }
      
      output$sample_size3 <- renderText({
        paste(line1, paste("n = Required sample size:", ceiling(final_n)), sep = "\n")
      })
      
    })
    
    # --- Change Tab ---
    observeEvent(input$next_tab3, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- Save Screenshot ---
    observeEvent(input$save_screenshot3, {
      screenshot(
        filename = "sample_Size",
        id = "main3",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    
    #Two sample Mean test (Large sample)---------------------------------------------------------------------------------------------------------------  
    observeEvent(input$calc4, {
      
      output$wr9 <- renderText("Where")
      
      # --- Alternative hypothesis ---
      output$tail4 <- renderText(paste("Alternative hypothesis:", input$tail4))
      
      # --- Z-values for CI ---
      z_two  <- c("90" = 1.645, "95" = 1.96,  "99" = 2.576)
      z_one  <- c("90" = 1.28,  "95" = 1.645, "99" = 2.33)
      
      if (input$tail4 == "Two_sided") {
        z4 <- z_two[as.character(input$CI4)]
      } else {
        z4 <- z_one[as.character(input$CI4)]
      }
      
      output$z4 <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub> =", z4, " (For ", input$CI4, " % Confidence Interval) "))
      })
      
      # --- Z-values for power ---
      power_z <- c(
        "80" = 0.842,
        "90" = 1.28,
        "95" = 1.645,
        "99" = 2.33
      )
      
      b4 <- power_z[as.character(input$b4)]
      
      output$b_4 <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub> =", b4, " (For ", input$b4, " % Power) "))
      })
      
      # --- Means & SDs ---
      output$m4_1 <- renderUI({
        HTML(paste0("Group 1 mean & SD:  μ<sub>1</sub> =", input$m4_1, " & σ<sub>1</sub> =", input$SD4_1))
      })
      output$m4_2 <- renderUI({
        HTML(paste0("Group 2 mean & SD:  μ<sub>2</sub> =", input$m4_2, " & σ<sub>2</sub> =", input$SD4_2))
      })
      
      # --- Reference ---
      output$Ref4 <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$Ref4)
      })
      
      # --- Mean difference ---
      d_4 <- abs(input$m4_1 - input$m4_2)
      
      output$d_4 <- renderText(
        paste("δ : Difference between means =", round(d_4, 2))
      )
      
      # --- Allocation Ratio ---
      output$R_4 <- renderText(paste("Allocation ratio = 1 : k = 1 :", input$R4))
      
      k <- input$R4
      
      # --- Nonresponse rate ---
      wrr <- input$f4
      
      # --- Correct Group Sample Size Formula ---
      # n1 = sample size in group 1
      # n2 = k * n1
      # Variance = σ1^2 + σ2^2/k
      n4 <- ((z4 + b4)^2 * ((input$SD4_1)^2 + (input$SD4_2)^2 / k)) / (d_4^2)
      
      if (wrr == 0) {
        n1 <- ceiling(n4)
        n2 <- ceiling(n4)* k
        total_n <- n1 + n2
        
        msg1 <- ""
        out1 <- paste("n1 = Required sample size for Group 1:", n1)
        out2 <- paste("n2 = Required sample size for Group 2:", n2)
        out3 <- paste("Total Required Sample Size:", total_n)
        
      } else {
        adj <- n4 / (1 - wrr/100)
        
        n1 <- ceiling(adj)
        n2 <- ceiling(adj) * k
        total_n <- n1 + n2
        
        msg1 <- paste("With Non-response / Attrition rate =", wrr, "%")
        out1 <- paste("n1 = Required sample size for Group 1:", n1)
        out2 <- paste("n2 = Required sample size for Group 2:", n2)
        out3 <- paste("Total Required Sample Size:", total_n)
      }
      
      output$sample_size4 <- renderText({
        paste(msg1, out1, out2, out3, sep = "\n")
      })
      
    })
    
    
    # --- Change Tab ---
    observeEvent(input$next_tab4, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- Save Screenshot ---
    observeEvent(input$save_screenshot4, {
      screenshot(
        filename = "sample_Size",
        id = "main4",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #equal variance(two sample mean)---------------------------------
    observeEvent(input$ecalc4, {
      
      output$wr10 <- renderText("Where")
      
      # --- Alternative hypothesis ---
      output$etail4 <- renderText(
        paste("Alternative hypothesis:", input$etail4)
      )
      
      # --- Lookup tables for Z-values ---
      z_two  <- c("90" = 1.645, "95" = 1.96,  "99" = 2.576)
      z_one  <- c("90" = 1.28,  "95" = 1.645, "99" = 2.33)
      
      # Select correct Zα based on one/two sided
      if (input$etail4 == "Two_sided") {
        ez4 <- z_two[as.character(input$eCI4)]
      } else {
        ez4 <- z_one[as.character(input$eCI4)]
      }
      
      output$ez4 <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub> =", ez4, " (For ", input$eCI4, " % Confidence Interval) "))
      })
      
      # --- Lookup for Power Zβ ---
      power_z <- c(
        "80" = 0.842,
        "90" = 1.28,
        "95" = 1.645,
        "99" = 2.33
      )
      
      eb4 <- power_z[as.character(input$eb4)]
      
      output$eb_4 <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub> =", eb4, " (For ", input$eb4, " % Power) "))
      })
      
      # --- Means & SD ---
      output$eSD4_1 <- renderText(
        paste("σ : Common SD =", input$eSD4_1)
      )
      
      output$em4_1 <- renderText(
        paste("μ₁ : Group 1 estimated mean =", input$em4_1)
      )
      
      output$em4_2 <- renderText(
        paste("μ₂ : Group 2 estimated mean =", input$em4_2)
      )
      
      # --- Reference ---
      output$eRef4 <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$eRef4)
      })
      
      # --- Mean difference ---
      ed_4 <- abs(input$em4_1 - input$em4_2)
      
      output$ed_4 <- renderText(
        paste("δ : Difference between means =", round(ed_4, 2))
      )
      
      # --- Allocation ratio k ---
      k <- input$eR4
      output$eR_4 <- renderText(paste("Allocation ratio = 1 : k = 1 :", k))
      
      
      # ======== Sample Size Formula (Equal Variance) =========
      # n1 = ((1 + k)/k) * (Zα + Zβ)² * σ² / δ²
      n_calc <- ((1 + k) / k) * ((ez4 + eb4)^2) * (input$eSD4_1)^2 / (ed_4^2)
      
      
      # --- Nonresponse rate ---
      nr <- input$ef4
      
      if (nr == 0) {
        n1 <- ceiling(n_calc)
        n2 <- ceiling(n_calc) * k
        total_n <- n1 + n2
        
        msg <- ""
      } else {
        adj <- n_calc / (1 - nr/100)
        
        n1 <- ceiling(adj)
        n2 <- ceiling(adj) * k
        total_n <- n1 + n2
        
        msg <- paste("With Non-response / Attrition rate =", nr, "%")
      }
      
      # --- Output sample size ---
      output$esample_size4 <- renderText({
        paste(
          msg,
          paste("n1 = Required sample size for Group 1:", n1),
          paste("n2 = Required sample size for Group 2:", n2),
          paste("Total Required Sample Size:", total_n),
          sep = "\n"
        )
      })
      
    })
    
    
    # ========== Change Tab ==========
    observeEvent(input$enext_tab4, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # ========== Save Screenshot ==========
    observeEvent(input$esave_screenshot4, {
      screenshot(
        filename = "sample_Size",
        id = "emain4",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #single proportion test--------------------------------------------------
    observeEvent(input$calc5, {
      
      output$wr12 <- renderText("Where")
      
      # --- Alternative hypothesis ---
      output$tail5 <- renderText(
        paste("Alternative hypothesis:", input$tail5)
      )
      
      # --- Z-value lookup tables ---
      z_two  <- c("90" = 1.645, "95" = 1.96,  "99" = 2.576)
      z_one  <- c("90" = 1.28,  "95" = 1.645, "99" = 2.33)
      
      # Select correct Zα
      if (input$tail5 == "Two_sided") {
        z5 <- z_two[as.character(input$CI5)]
      } else {
        z5 <- z_one[as.character(input$CI5)]
      }
      
      output$z5 <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub> =", z5, " (For ", input$CI5, " % Confidence Interval) "))
      })
      
      
      # --- Z-value for Power (Zβ) ---
      power_z <- c(
        "80" = 0.842,
        "90" = 1.28,
        "95" = 1.645,
        "99" = 2.33
      )
      
      b_5 <- power_z[as.character(input$b5)]
      
      output$b_5 <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub> =", b_5, " (For ", input$b5, " % Power) "))
      })
      
      
      # --- Proportions ---
      P0 <- input$ad5 / 100
      Q0 <- 1 - P0
      
      P1 <- input$p5 / 100
      Q1 <- 1 - P1
      
      output$ad5 <- renderText(
        paste("P₀ : Assumed proportion =", input$ad5, "%,   Q₀ =", round(Q0*100, 1), "%")
      )
      
      output$p5 <- renderText(
        paste("p₁ : Estimated proportion =", input$p5, "%,   q₁ =", round(Q1*100, 1), "%")
      )
      
      
      # --- Reference ---
      output$Ref5 <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$Ref5)
      })
      
      
      # --- Difference between proportions ---
      md5 <- abs(P0 - P1)   # delta
      
      
      # --- Sample size formula for 2-proportion comparison ---
      # n = [ Zα √(P₀Q₀) + Zβ √(P₁Q₁) ]² / δ²
      n_calc <- (z5 * sqrt(P0 * Q0) + b_5 * sqrt(P1 * Q1))^2 / (md5^2)
      
      
      # --- Non-response adjustment ---
      nr <- input$f5
      
      if (nr == 0) {
        n_final <- ceiling(n_calc)
        msg <- ""
      } else {
        n_final <- ceiling(n_calc / (1 - nr/100))
        msg <- paste("With Non-response / Attrition rate =", nr, "%")
      }
      
      
      # --- Output sample size ---
      output$sample_size5 <- renderText({
        paste(
          msg,
          paste("n = Required sample size:", n_final),
          sep = "\n"
        )
      })
      
    })
    
    
    # ========== Change Tab ==========
    observeEvent(input$next_tab5, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # ========== Save Screenshot ==========
    observeEvent(input$save_screenshot5, {
      screenshot(
        filename = "sample_Size",
        id = "main5",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    
    #Two sample proportion test (equal proportion)--------------------------------------------------------------------------------------------
    observeEvent(input$calc6, {
      
      output$wr14 <- renderText("Where")
      
      output$tail6 <- renderText(
        paste("Alternative hypothesis:", input$tail6)
      )
      
      # --- Z value lookup for 1-α* ---
      z_two  <- c("90" = 1.645, "95" = 1.96,  "99" = 2.576)
      z_one  <- c("90" = 1.28,  "95" = 1.645, "99" = 2.33)
      
      if (input$tail6 == "Two_sided") {
        z6 <- z_two[as.character(input$CI6)]
      } else {
        z6 <- z_one[as.character(input$CI6)]
      }
      
      output$z6 <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub> =", z6, " (For ", input$CI6, " % Confidence Interval) "))
      })
      
      # --- Z for power: Zβ ---
      power_z <- c(
        "80" = 0.842,
        "90" = 1.28,
        "95" = 1.645,
        "99" = 2.33
      )
      
      b6 <- power_z[as.character(input$b6)]
      
      output$b_6 <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub> =", b6, " (For ", input$b6, " % Power) "))
      })
      
      # --- Input proportions ---
      p1 <- input$p6_1 / 100
      q1 <- 1 - p1
      
      p2 <- input$p6_2 / 100
      q2 <- 1 - p2
      
      output$p6_1 <- renderText(
        paste("p₁ : Group 1 proportion =", input$p6_1, "%,   q₁ =", round(q1*100, 1), "%")
      )
      
      output$p6_2 <- renderText(
        paste("p₂ : Group 2 proportion =", input$p6_2, "%,   q₂ =", round(q2*100, 1), "%")
      )
      
      # --- Reference text ---
      output$Ref6 <- renderText({
        HTML("<b><i>Reference/Note:</i></b>", input$Ref6)
      })
      
      # --- Allocation ratio k ---
      k <- input$R6
      
      output$R_6 <- renderText(
        paste("Allocation ratio = 1 : k = 1 :", k)
      )
      
      # --- Pooled proportion ---
      P  <- (p1 + k*p2) / (1 + k)
      Q  <- 1 - P
      
      output$P <- renderText(
        paste("P : Pooled proportion =", round(P*100, 2), "%")
      )
      
      # --- Difference (δ) ---
      d6 <- abs(p1 - p2)
      
      # --- Core sample size formula ---
      eq1 <- sqrt((1 + k) * P * Q)
      eq2 <- sqrt(k * p1*q1 + p2*q2)
      
      n6 <- (z6*eq1 + b6*eq2)^2 / (k * d6^2)
      
      # --- Non-response / Attrition adjustment ---
      nr <- input$f6
      
      if (nr == 0) {
        n1 <- ceiling(n6)          # group 1
        n2 <- ceiling(n6) * k     # group 2
        nt <- n1 + n2
        msg <- ""
      } else {
        adj <- n6 / (1 - nr/100)
        n1 <- ceiling(adj)
        n2 <- ceiling(adj) * k
        nt <- n1 + n2
        msg <- paste("With Non-response / Attrition rate =", nr, "%")
      }
      
      # --- Output ---
      output$sample_size6 <- renderText({
        paste(
          msg,
          paste("n₁ = Required sample size for Group 1:", n1),
          paste("n₂ = Required sample size for Group 2:", n2),
          paste("Total Sample Size n =", nt),
          sep = "\n"
        )
      })
      
    })
    
    # --- Change Tab ---
    observeEvent(input$next_tab6, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    
    # --- Screenshot ---
    observeEvent(input$save_screenshot6, {
      screenshot(
        filename = "sample_Size",
        id = "main6",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Two sample proportion test (equal proportion using OR or RR)-------------------------------------------------
    observeEvent(input$calc6_RR, {
      
      output$wr17 <- renderText({ "Where" })
      output$tail6_RR <- renderText({ paste("Alternative hypothesis:", input$tail6_RR) })
      
      # --- Z-value for confidence interval ---
      z_lookup <- list(
        "90" = c(Two_sided = 1.645, One_sided = 1.28),
        "95" = c(Two_sided = 1.96, One_sided = 1.645),
        "99" = c(Two_sided = 2.576, One_sided = 2.33)
      )
      tail_type <- ifelse(input$tail6_RR == "Two_sided", "Two_sided", "One_sided")
      z6_RR <- z_lookup[[as.character(input$CI6_RR)]][[tail_type]]
      result_text6_RR <- paste("=", z6_RR)
      
      output$z6_RR <- renderUI({HTML(paste0("Z<sub>(1-α*)</sub>", result_text6_RR, " (For ", input$CI6_RR, " % Confidence Interval) ")) })
      
      # --- Z-value for power ---
      beta_lookup <- c("80" = 0.842, "90" = 1.28, "95" = 1.645, "99" = 2.33)
      b6_RR <- beta_lookup[as.character(input$b6_RR)]
      result_6_RR <- paste("=", b6_RR)
      
      output$b_6_RR <- renderUI({ HTML(paste0("Z<sub>(1-β)</sub>", result_6_RR, " (For ", input$b6_RR, " % Power) ")) })
      
      # --- Inputs for proportions ---
      output$p6_1_RR <- renderText({ paste("Anticipated OR or RR  =", input$p6_1_RR) })
      E_p1_RR <- (input$p6_1_RR * input$p6_2_RR / 100) / (1 + input$p6_2_RR / 100 * (input$p6_1_RR - 1))
      output$E_p1_RR <- renderText({ paste("p₁: Proportion of disease in treatment group =", round(E_p1_RR*100, 2), "%") })
      output$p6_2_RR <- renderText({ paste("p₂: Proportion of disease in control group =", input$p6_2_RR, "%") })
      output$q6_2_RR <- renderText({ paste("q₁ =", 100 - round(E_p1_RR*100,2), "%", "and q₂ =", 100 - input$p6_2_RR, "%") })
      
      output$Ref6_RR <- renderText({ HTML("<b><i>Reference/Note:</i></b>", input$Ref6_RR) })
      
      # --- Pooled proportion and difference ---
      P_RR <- (E_p1_RR + input$R6_RR * input$p6_2_RR/100) / (1 + input$R6_RR)
      d_6_RR <- abs(E_p1_RR - input$p6_2_RR/100)
      output$P_RR <- renderText({ paste("Pooled proportion: P =", round(P_RR*100,2), "% and Q =", 100 - round(P_RR*100,2), "%") })
      output$R_6_RR <- renderText({ paste("Allocation ratio: 1:k = 1:", input$R6_RR) })
      
      # --- Sample size calculation ---
      eq1_RR <- sqrt((input$R6_RR + 1) * P_RR * (1 - P_RR))
      eq2_RR <- sqrt(input$R6_RR * E_p1_RR * (1 - E_p1_RR) + input$p6_2_RR/100 * (1 - input$p6_2_RR/100))
      n6_RR <- (z6_RR * eq1_RR + b6_RR * eq2_RR)^2 / (input$R6_RR * (d_6_RR)^2)
      
      # --- Adjust for non-response ---
      if (input$f6_RR == 0) {
        n1 <- n6_RR
        n2 <- n1 * input$R6_RR
        nT <- ceiling(n1) + ceiling(n2)
        sample_text <- ""
      } else {
        n_a6 <- n6_RR / (1 - input$f6_RR / 100)
        n1 <- n_a6
        n2 <- ceiling(n_a6) * input$R6_RR
        nT <- ceiling(n_a6) + ceiling(n2)
        sample_text <- paste("With Non response/Attrition rate =", input$f6_RR, "%")
      }
      
      output$sample_size6_RR <- renderText({
        paste(sample_text,
              paste("n1= Required sample size for group 1:", ceiling(n1)),
              paste("n2= Required sample size for group 2:", ceiling(n2)),
              paste("n= Total sample size required:", nT),
              sep="\n")
      })
    })
    
    
    observeEvent(input$next_tab6_RR, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    observeEvent(input$save_screenshot6_RR, {
      screenshot(
        filename = "sample_Size",
        id = "main6_RR",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    #paired sample test--------------------------------------------------------------------------------------------      
    
    observeEvent(input$calc9, {
      
      # ----------- Helper tables (Cleaner than nested if-else) -------------
      z_table <- list(
        "90" = c(Two_sided = 1.645, One_sided = 1.28),
        "95" = c(Two_sided = 1.96,  One_sided = 1.645),
        "99" = c(Two_sided = 2.576, One_sided = 2.33)
      )
      
      beta_table <- c(
        "80" = 0.842,
        "90" = 1.28,
        "95" = 1.96,
        "99" = 2.33
      )
      
      # ----------- Extract inputs -------------
      CI9      <- as.character(input$CI9)
      tail9    <- input$tail9
      beta_val <- as.character(input$b9)
      
      z9  <- z_table[[CI9]][[tail9]]
      b9  <- beta_table[[beta_val]]
      
      # ----------- Combined SD & Mean diff -------------
      CSD9 <- sqrt(input$SD9_1^2 + input$SD9_2^2 - 
                     2 * input$corr * input$SD9_1 * input$SD9_2)
      Cd9  <- abs(input$mu9_1 - input$mu9_2)
      
      # ----------- Sample size formula -------------
      n_raw <- ((z9 + b9)^2 * CSD9^2) / Cd9^2
      
      if (input$f9 == 0) {
        sample15 <- ""
        final_n  <- ceiling(n_raw)
      } else {
        sample15 <- paste("With Non-response/Attrition rate =", input$f9, "%")
        final_n  <- ceiling(n_raw / (1 - input$f9/100))
      }
      
      # ----------- Output Texts -------------
      output$wr18  <- renderText("Where")
      
      output$tail9 <- renderText(paste("Alternative hypothesis:", input$tail9))
      
      output$z9 <- renderUI({
        HTML(paste0("Z<sub>(1−α*)</sub> = ", z9, 
                    " (For ", CI9, "% Confidence Interval)"))
      })
      
      output$b_9 <- renderUI({
        HTML(paste0("Z<sub>(1−β)</sub> = ", b9, 
                    " (For ", beta_val, "% Power)"))
      })
      
      output$pre <- renderText(
        paste("Pre test: Mean=", input$mu9_1, "and SD=", input$SD9_1)
      )
      
      output$post <- renderText(
        paste("Post test: Mean=", input$mu9_2, "and SD=", input$SD9_2)
      )
      
      output$corr_12 <- renderText(
        paste("Anticipated correlation:", input$corr)
      )
      
      output$SD9 <- renderText(
        paste("σₙ : Combined Standard deviation =", round(CSD9, 2))
      )
      
      output$d9 <- renderText(
        paste("δₙ : Difference between two means =", Cd9)
      )
      
      output$Ref9 <- renderUI({
        HTML(paste0("<b><i>Reference/Note:</i></b> ", input$Ref9))
      })
      
      n9 <- ((z9 + b9)^2 * CSD9^2) / (Cd9^2)
      if (input$f9 > 0) {
        n9 <- n9 / (1 - input$f9 / 100)
        sample15 <- paste("With Non response/Attrition rate =", input$f9, "%")
      } else {
        sample15 <- ""
      }
      
      
      
      # --------- FINAL SAMPLE SIZE OUTPUT (Left aligned + 2 lines) ---------
      output$sample_size9 <- renderText({
        paste(sample15,
              paste("n = Required sample size:", ceiling(n9)),
              sep = "\n")
      })
      
      
    })
    
    observeEvent(input$next_tab7, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    observeEvent(input$save_screenshot9, {
      screenshot(
        filename = "sample_Size",
        id = "main9",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    #Cohen's method(One sample)----------------------------------------------------------------------------------  
    observeEvent(input$calc3c, {
      
      output$wr8 <- renderText("Where")
      output$tail3c <- renderText(paste("Alternative hypothesis:", input$tail3c))
      
      # ---- Z-values lookup table ----
      z_table <- list(
        "90" = c(Two_sided = 1.645, One_sided = 1.28),
        "95" = c(Two_sided = 1.96,  One_sided = 1.645),
        "99" = c(Two_sided = 2.576, One_sided = 2.33)
      )
      
      C2c <- as.character(input$CI3c)
      tail_3c <- input$tail3c
      z3c <- z_table[[C2c]][tail_3c]
      result_text3c <- paste("=", z3c)
      
      # ---- Power Z lookup ----
      b_table <- c("80" = 0.842, "90" = 1.28, "95" = 1.96, "99" = 2.33)
      beta3c <- as.character(input$b3c)
      b_3c <- b_table[beta3c]
      result_3c <- paste("=", b_3c)
      
      # ---- Render Z outputs ----
      output$z3c <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub>", result_text3c, " (For ", input$CI3c, " % Confidence Interval) "))
      })
      
      output$b_3c <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub>", result_3c, " (For ", input$b3c," % Power) "))
      })
      
      
      # ---- Effect size ----
      output$es3c <- renderText(
        paste("Δ : Assumed effect size =", input$es3c)
      )
      
      # ---- Sample size calculation ----
      n3c <- ((z3c + b_3c)^2) / (input$es3c)^2
      n_adj <- n3c / (1 - input$f3c/100)
      
      if (input$f3c == 0) {
        sample_line1 <- ""
        sample_line2 <- paste("n = Required sample size:", ceiling(n3c))
      } else {
        sample_line1 <- paste("With Non-response / Attrition rate =", input$f3c, "%")
        sample_line2 <- paste("n = Required sample size:", ceiling(n_adj))
      }
      
      output$sample_size3c <- renderText(
        paste(sample_line1, sample_line2, sep = "\n")
      )
    })
    
    observeEvent(input$next_tab3c, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    observeEvent(input$save_screenshot3c, {
      screenshot(
        filename = "sample_Size",
        id = "main3c",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    }) 
    #Cohen's method(two sample)----------------------------------------------------------------------------------  
    observeEvent(input$calc4c, {
      
      output$wr11 <- renderText("Where")
      output$tail3c <- renderText(paste("Alternative hypothesis:", input$tail4c))
      
      # -------- Z lookup table --------
      z_table <- list(
        "90" = c(Two_sided = 1.645, One_sided = 1.28),
        "95" = c(Two_sided = 1.96,  One_sided = 1.645),
        "99" = c(Two_sided = 2.576, One_sided = 2.33)
      )
      
      C4c  <- as.character(input$CI4c)
      tail <- input$tail4c
      
      z4c <- z_table[[C4c]][tail]
      result_text4c <- paste("=", z4c)
      
      # -------- Power Z lookup --------
      b_table <- c("80" = 0.842, "90" = 1.28, "95" = 1.645, "99" = 2.33)
      
      beta4c <- as.character(input$b4c)
      b_4c <- b_table[beta4c]
      result_4c <- paste("=", b_4c)
      
      # -------- Output Z values --------
      output$z4c <- renderUI({
        HTML(paste0("Z<sub>(1-α*)</sub>", result_text4c, " (For ", input$CI4c, " % Confidence Interval) "))
      })
      
      output$b_4c <- renderUI({
        HTML(paste0("Z<sub>(1-β)</sub>", result_4c, " (For ", input$b4c, " % Power) "))
      })
      
      
      
      
      
      # -------- Effect size & Allocation ratio --------
      output$es4c <- renderText(
        paste("Δ : Assumed effect size =", input$es4c)
      )
      
      output$eR_4c <- renderText(
        paste("Allocation ratio = 1 : k  =  1 :", input$eR4c)
      )
      
      # -------- Sample size formula --------
      k <- input$eR4c
      n4c <- ((1 + k) / k) * (z4c + b_4c)^2 / (input$es4c)^2
      
      # Attrition adjustment
      if (input$f4c == 0) {
        
        # unadjusted
        n1 <- ceiling(n4c)
        n2 <- ceiling(n4c) * k
        total <- n1 + n2
        attr_line <- ""
        
      } else {
        
        # adjusted for attrition
        n_adj <- n4c / (1 - input$f4c/100)
        n1 <- ceiling(n_adj)
        n2 <- ceiling(n_adj) * k
        total <- n1 + n2
        attr_line <- paste("With Non-response / Attrition rate =", input$f4c, "%")
      }
      
      # -------- Output sample size --------
      output$sample_size4c <- renderText(
        paste(
          attr_line,
          paste("n1 = Required sample size for group 1:", n1),
          paste("n2 = Required sample size for group 2:", n2),
          paste("Total required sample size =", total),
          sep = "\n"
        )
      )
    })
    
    observeEvent(input$next_tab4c, {
      updateNavbarPage(session, inputId = "tabs", selected = "Help")
    })
    observeEvent(input$save_screenshot4c, {
      screenshot(
        filename = "sample_Size",
        id = "main4c",
        scale = 1,
        timer = 0,
        download = TRUE
      )
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#-----------------------------------------------------    
# Sampling methods
#-------------------------------------------------------    
    
    # Reactive expression to perform simple random sampling without replacement
    sampled_data <- eventReactive(input$generate, {
      req(input$pop_size, input$sample_size)
      
      # Input validation
      validate(
        need(input$sample_size <= input$pop_size, "Sample size (n) cannot exceed Population size (N)."),
        need(input$pop_size > 0 && input$sample_size > 0, "Sizes must be positive integers.")
      )
      
      # Create population and take a sample
      population <- 1:input$pop_size
      selected <- sort(sample(population, size = input$sample_size, replace = FALSE))
      
      data.frame(
        Sample_ID = seq_along(selected),
        Selected_Unit_ID = selected
      )
    })
    
    # Display summary text
    output$summary_text <- renderText({
      req(sampled_data())
      paste0("Population Size (N): ", input$pop_size, "\n",
             "Sample Size (n): ", input$sample_size, "\n",
             "Sampling Fraction: ", round((input$sample_size / input$pop_size) * 100, 2), "%")
    })
    
    # Display preview table
    output$sample_table <- renderTable({
      sampled_data()
    })
    
    # Download Handler for Excel Export
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("simple_random_sample_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(sampled_data(), file = file, rowNames = FALSE)
      }
    )
    #Stratified-------------------------------------------------------  
    output$strata_sizes_ui <- renderUI({
      req(input$num_strata)
      k <- input$num_strata
      
      lapply(1:k, function(i) {
        numericInput(
          inputId = paste0("stratum_size_", i),
          label = paste0("Stratum ", i, " Size (N_", i, "):"),
          value = 50,
          min = 1,
          step = 1
        )
      })
    })
    
    # Helper function to collect stratum sizes
    get_strata_sizes <- reactive({
      req(input$num_strata)
      k <- input$num_strata
      
      sizes <- sapply(1:k, function(i) {
        input[[paste0("stratum_size_", i)]]
      })
      
      if (any(sapply(sizes, is.null)) || length(sizes) != k) return(NULL)
      return(sizes)
    })
    
    # Display dynamic Total Population Size (N)
    output$total_pop_display <- renderText({
      sizes <- get_strata_sizes()
      if (is.null(sizes)) return("Total Population Size (N): --")
      paste0("Total Population Size (N): ", sum(sizes))
    })
    
    # Core Sampling Logic
    sampled_result <- eventReactive(input$generates, {
      sizes <- get_strata_sizes()
      req(sizes, input$total_sample_size, input$allocation_type, input$sampling_method)
      
      k_strata <- input$num_strata
      n_total <- input$total_sample_size
      N_total <- sum(sizes)
      
      # Validations
      validate(
        need(n_total <= N_total, "Total sample size cannot exceed total population size."),
        need(all(sizes > 0), "Stratum sizes must be positive integers.")
      )
      
      # 1. Determine Sample Allocation (n_h)
      if (input$allocation_type == "equal") {
        n_per_stratum <- floor(n_total / k_strata)
        validate(
          need(n_per_stratum > 0, "Total sample size is too small for equal allocation across all strata."),
          need(all(sizes >= n_per_stratum), "Allocated sample size exceeds population of one or more strata.")
        )
        allocations <- rep(n_per_stratum, k_strata)
      } else {
        allocations <- pmax(1, round(n_total * (sizes / N_total)))
        validate(
          need(all(sizes >= allocations), "Calculated allocation exceeds size of one or more strata.")
        )
      }
      
      # 2. Perform Sampling within each stratum
      sample_list <- vector("list", k_strata)
      intervals <- numeric(k_strata)
      random_starts <- numeric(k_strata)
      
      for (i in 1:k_strata) {
        stratum_name <- paste0("Stratum_", i)
        N_h <- sizes[i]
        n_h <- allocations[i]
        
        if (input$sampling_method == "srs") {
          # SRS Selection
          selected_idx <- sort(sample(1:N_h, size = n_h, replace = FALSE))
          intervals[i] <- NA
          random_starts[i] <- NA
        } else {
          # Systematic Selection within stratum
          k_interval <- floor(N_h / n_h)
          validate(
            need(k_interval >= 1, paste0("Stratum ", i, " allocation exceeds stratum population for systematic sampling."))
          )
          
          r_start <- sample(1:k_interval, size = 1)
          selected_idx <- seq(from = r_start, to = N_h, by = k_interval)[1:n_h]
          
          intervals[i] <- k_interval
          random_starts[i] <- r_start
        }
        
        sample_list[[i]] <- data.frame(
          Stratum = stratum_name,
          Unit_ID = paste0(stratum_name, "_Unit_", selected_idx),
          Stratum_Index = selected_idx
        )
      }
      
      # Build Summary Table
      summary_df <- data.frame(
        Stratum = paste0("Stratum_", 1:k_strata),
        Population_Size_Nh = sizes,
        Allocated_Sample_nh = allocations,
        Method = ifelse(input$sampling_method == "srs", "SRS", "Systematic"),
        Interval_k = intervals,
        Random_Start_r = random_starts
      )
      
      final_sample <- bind_rows(sample_list)
      
      list(
        summary = summary_df,
        sample = final_sample
      )
    })
    
    # Render Summary Table
    output$summary_tables <- renderTable({
      sampled_result()$summary
    })
    
    # Render Sampled Units Table
    output$sample_tables <- renderTable({
      sampled_result()$sample
    })
    
    # Download Handler for Excel Export
    output$download_excels <- downloadHandler(
      filename = function() {
        paste0("stratified_", input$sampling_method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        addWorksheet(wb, "Sampled Units")
        addWorksheet(wb, "Strata Summary")
        
        writeData(wb, "Sampled Units", sampled_result()$sample)
        writeData(wb, "Strata Summary", sampled_result()$summary)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    #Neyman allocation---------------------------------------------  
    output$strata_params_ui_neyman <- renderUI({
      k <- input$num_strata_neyman
      req(k)
      
      default_N <- c(200, 500, 300)
      default_S <- c(15, 5, 10)
      
      ui_list <- lapply(1:k, function(i) {
        n_val <- if (i <= length(default_N)) default_N[i] else 100
        s_val <- if (i <= length(default_S)) default_S[i] else 10
        
        wellPanel(
          style = "padding: 8px; margin-bottom: 8px;",
          strong(paste("Stratum", i)),
          fluidRow(
            column(6, numericInput(paste0("N_h_", i), "Size (N_h):", value = n_val, min = 1)),
            column(6, numericInput(paste0("S_h_", i), "Std Dev (S_h):", value = s_val, min = 0.01))
          )
        )
      })
      do.call(tagList, ui_list)
    })
    
    # 2. Display Total Population Size
    output$total_pop_display_neyman <- renderText({
      k <- input$num_strata_neyman
      req(k)
      sizes <- sapply(1:k, function(i) {
        val <- input[[paste0("N_h_", i)]]
        if (is.null(val) || is.na(val)) 0 else val
      })
      paste("Total Population (N):", sum(sizes))
    })
    
    # 3. Calculate Neyman Allocation and Draw Random Samples
    # Calculate Neyman Allocation and Draw Random Samples
    sampling_results <- eventReactive(input$generate_neyman, {
      k_strata <- input$num_strata_neyman
      n_total <- input$total_sample_size_neyman
      req(k_strata, n_total)
      
      sizes <- sapply(1:k_strata, function(i) input[[paste0("N_h_", i)]])
      S_h <- sapply(1:k_strata, function(i) input[[paste0("S_h_", i)]])
      
      req(all(!is.null(sizes)), all(!is.null(S_h)), all(!is.na(sizes)), all(!is.na(S_h)))
      
      # Neyman Allocation Formula: n_h = n * (N_h * S_h) / sum(N_i * S_i)
      NS_prod <- sizes * S_h
      sum_NS <- sum(NS_prod)
      
      if (sum_NS == 0) return(NULL)
      
      n_h_exact <- n_total * (NS_prod / sum_NS)
      allocations <- round(n_h_exact)
      
      # Adjust for potential rounding discrepancies
      diff <- n_total - sum(allocations)
      if (diff != 0) {
        max_idx <- which.max(abs(n_h_exact - allocations))
        allocations[max_idx] <- allocations[max_idx] + diff
      }
      
      # Arrays to store systematic sampling parameters
      intervals <- numeric(k_strata)
      random_starts <- numeric(k_strata)
      sampled_list <- list()
      
      for (i in 1:k_strata) {
        N_i <- sizes[i]
        n_i <- min(allocations[i], N_i)
        
        if (n_i > 0) {
          if (input$sampling_method_neyman == "srs") {
            draws <- sort(sample(1:N_i, size = n_i, replace = FALSE))
            intervals[i] <- NA
            random_starts[i] <- NA
          } else {
            # Systematic Random Sampling Parameters
            k_val <- max(1, floor(N_i / n_i))
            r_val <- sample(1:k_val, 1)
            
            intervals[i] <- k_val
            random_starts[i] <- r_val
            
            draws <- seq(r_val, by = k_val, length.out = n_i)
            draws <- draws[draws <= N_i]
          }
          
          sampled_list[[i]] <- data.frame(
            Stratum = paste0("Stratum_", i),
            Sampled_Unit_ID = draws,
            Population_Index = paste0("Unit_", draws)
          )
        } else {
          intervals[i] <- NA
          random_starts[i] <- NA
        }
      }
      
      # Updated Summary Table
      summary_df <- data.frame(
        Stratum = paste0("Stratum_", 1:k_strata),
        Population_Size_Nh = sizes,
        StdDev_Sh = S_h,
        Allocated_Sample_nh = allocations,
        Method = ifelse(input$sampling_method_neyman == "srs", "SRS", "Systematic"),
        Interval_k = intervals,
        Random_Start_r = random_starts
      )
      
      sampled_df <- bind_rows(sampled_list)
      
      list(summary = summary_df, samples = sampled_df)
    })
    
    # 4. Render Summary Table
    output$summary_table_neyman <- renderTable({
      res <- sampling_results()
      req(res)
      res$summary
    }, na = "-")
    
    # 5. Render Sampled Units Table
    output$sample_table_neyman <- renderTable({
      res <- sampling_results()
      req(res)
      res$samples
    })
    
    # 6. Excel Download Handler
    output$download_excel_neyman <- downloadHandler(
      filename = function() {
        paste0("Neyman_Sampling_Results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        res <- sampling_results()
        req(res)
        sheets <- list(
          "Allocation Summary" = res$summary,
          "Sampled Units" = res$samples
        )
        writexl::write_xlsx(sheets, path = file)
      }
    )
    
    
    #Systematic----------------------------------
    output$sample_input_ui <- renderUI({
      req(input$interval_mode)
      
      if (input$interval_mode == "default") {
        numericInput("sample_sizesy", "Desired Sample Size (n):", value = 10, min = 1, step = 1)
      } else {
        numericInput("custom_k", "Enter Sampling Interval (k):", value = 10, min = 1, step = 1)
      }
    })
    
    # Reactive sampling logic
    sampled_results <- eventReactive(input$generatesy, {
      req(input$pop_sizesy, input$interval_mode)
      
      N <- input$pop_sizesy
      
      if (input$interval_mode == "default") {
        # Case 1: Default Interval calculation k = floor(N / n)
        req(input$sample_sizesy)
        n_desired <- input$sample_sizesy
        
        validate(
          need(n_desired <= N, "Sample size (n) cannot exceed Population size (N)."),
          need(n_desired > 0, "Sample size must be greater than 0.")
        )
        
        k <- floor(N / n_desired)
      } else {
        # Case 2: Custom Interval k entered directly
        req(input$custom_k)
        k <- input$custom_k
        
        validate(
          need(k <= N, "Sampling interval (k) cannot exceed Population size (N)."),
          need(k > 0, "Sampling interval (k) must be greater than 0.")
        )
      }
      
      # 1. Select random start (1 <= r <= k)
      random_start <- sample(1:k, size = 1)
      
      # 2. Generate systematic sequence: r, r+k, r+2k, ... <= N
      selected_indices <- seq(from = random_start, to = N, by = k)
      actual_n <- length(selected_indices)
      
      # Build data frames
      sample_df <- data.frame(
        Sample_Order = 1:actual_n,
        Selected_Unit_ID = paste0("Unit_", selected_indices),
        Population_Index = selected_indices
      )
      
      summary_info <- list(
        N = N,
        k = k,
        random_start = random_start,
        n = actual_n,
        mode = input$interval_mode
      )
      
      list(
        summary = summary_info,
        sample = sample_df
      )
    })
    
    # Render Text Summary
    output$summary_textsy <- renderText({
      res <- sampled_results()$summary
      paste0(
        "Population Size (N): ", res$N, "\n",
        "Sampling Interval (k): ", res$k, "\n",
        "Random Start (r): ", res$random_start, " (Randomly selected from 1 to ", res$k, ")\n",
        "Actual Sample Size (n): ", res$n, "\n",
        "Selected Indices Formula: r + (i - 1)*k"
      )
    })
    
    # Render Preview Table
    output$sample_tablesy <- renderTable({
      sampled_results()$sample
    })
    
    # Excel Export Handler
    output$download_excelsy <- downloadHandler(
      filename = function() {
        paste0("systematic_sample_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        
        # Sheet 1: Sample
        addWorksheet(wb, "Systematic Sample")
        writeData(wb, "Systematic Sample", sampled_results()$sample)
        
        # Sheet 2: Parameters Summary
        addWorksheet(wb, "Sampling Parameters")
        s <- sampled_results()$summary
        summary_df <- data.frame(
          Parameter = c("Population Size (N)", "Sampling Interval (k)", "Random Start (r)", "Actual Sample Size (n)"),
          Value = c(s$N, s$k, s$random_start, s$n)
        )
        writeData(wb, "Sampling Parameters", summary_df)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    #cluster sampling-------------------------------------
    selected_info <- reactiveVal(NULL)
    
    # STEP 1: Perform PPS to select k clusters
    observeEvent(input$select_clusters_btn, {
      req(input$pop_sizec, input$num_clusters, input$sample_clusters)
      
      N <- input$pop_sizec
      K <- input$num_clusters
      k <- input$sample_clusters
      
      validate(
        need(k <= K, "Sampled clusters (k) cannot exceed total clusters (K)."),
        need(K <= N, "Total clusters (K) cannot exceed population size (N).")
      )
      
      # Generate background size weights for PPS selection
      set.seed(NULL)
      weights <- runif(K, min = 10, max = 100)
      pps_probs <- weights / sum(weights)
      
      # Sample k clusters without replacement using PPS probabilities
      chosen_ids <- sort(sample(1:K, size = k, replace = FALSE, prob = pps_probs))
      
      selected_info(list(
        chosen_ids = chosen_ids,
        pps_probs = pps_probs
      ))
    })
    
    # STEP 2: Dynamic UI for entering Cluster Sizes (N_i) ONLY
    output$selected_clusters_input_ui <- renderUI({
      info <- selected_info()
      req(info)
      
      chosen <- info$chosen_ids
      
      tagList(
        h5(strong("2. Enter Cluster Sizes (N_i) for Selected Clusters:")),
        helpText("Sample sizes (n_i) will be auto-calculated proportionally from total desired sample size (n)."),
        lapply(seq_along(chosen), function(idx) {
          cluster_num <- chosen[idx]
          numericInput(
            inputId = paste0("size_N_", cluster_num),
            label = paste0("Cluster ", cluster_num, " Size (N_", cluster_num, "):"),
            value = 50 * idx,
            min = 1,
            step = 1
          )
        })
      )
    })
    
    # Render generate button after step 1 is initialized
    output$generate_sample_btn_ui <- renderUI({
      req(selected_info())
      actionButton("generatec", "3. Auto-Calculate & Generate Sample", class = "btn-primary")
    })
    
    # STEP 3: Automatically calculate n_i per cluster and extract sample
    sampled_resultsc <- eventReactive(input$generatec, {
      info <- selected_info()
      req(info, input$total_sample_n)
      
      chosen <- info$chosen_ids
      K <- input$num_clusters
      n_target <- input$total_sample_n
      
      # Gather entered N_i sizes
      N_inputs <- sapply(chosen, function(id) input[[paste0("size_N_", id)]])
      
      validate(
        need(!any(sapply(N_inputs, is.null)), "Please enter all selected cluster sizes."),
        need(all(N_inputs > 0), "Cluster sizes must be positive integers.")
      )
      
      sum_N_selected <- sum(N_inputs)
      
      # Calculate proportional sample size n_i for each selected cluster
      n_allocated <- pmax(1, round(n_target * (N_inputs / sum_N_selected)))
      
      validate(
        need(all(n_allocated <= N_inputs), 
             "Calculated sample size (n_i) exceeds cluster size (N_i) for one or more clusters. Increase cluster sizes or decrease total sample size.")
      )
      
      sample_list <- vector("list", length(chosen))
      summary_rows <- vector("list", K)
      
      for (i in 1:K) {
        is_selected <- i %in% chosen
        
        if (is_selected) {
          idx <- which(chosen == i)
          c_N <- N_inputs[idx]
          c_n <- n_allocated[idx]
          
          # Sub-sample c_n units randomly from c_N
          selected_units <- sort(sample(1:c_N, size = c_n, replace = FALSE))
          
          sample_list[[idx]] <- data.frame(
            Cluster_ID = paste0("Cluster_", i),
            Unit_ID = paste0("Cluster_", i, "_Unit_", selected_units),
            Selected_Unit_Index = selected_units
          )
          
          display_N <- c_N
          display_n <- c_n
        } else {
          display_N <- NA
          display_n <- 0
        }
        
        summary_rows[[i]] <- data.frame(
          Cluster_ID = paste0("Cluster_", i),
          PPS_Selection_Prob = round(info$pps_probs[i], 4),
          Status = ifelse(is_selected, "Selected", "Not Selected"),
          Cluster_Size_Ni = display_N,
          Calculated_Sample_ni = display_n
        )
      }
      
      list(
        summary = bind_rows(summary_rows),
        sample = bind_rows(sample_list)
      )
    })
    
    # Output Tables
    output$summary_tablec <- renderTable({
      sampled_resultsc()$summary
    })
    
    output$sample_tablec <- renderTable({
      sampled_resultsc()$sample
    })
    
    # Excel Export Handler
    output$download_excelc <- downloadHandler(
      filename = function() {
        paste0("pps_auto_allocated_sample_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        addWorksheet(wb, "Sampled Units")
        addWorksheet(wb, "Cluster Summary")
        
        writeData(wb, "Sampled Units", sampled_resultsc()$sample)
        writeData(wb, "Cluster Summary", sampled_resultsc()$summary)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )  
    
    #systematic PPS -----------------------------  
    uploaded_data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      if (ext == "csv") {
        read.csv(input$file$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        openxlsx::read.xlsx(input$file$datapath)
      } else {
        validate("Please upload a valid .csv or .xlsx file.")
      }
    })
    
    # Dynamic UI to map Cluster ID and Size columns from the uploaded dataset
    output$col_selectors_ui <- renderUI({
      req(uploaded_data())
      cols <- names(uploaded_data())
      
      tagList(
        selectInput("cluster_col", "Select Cluster Column:", choices = cols),
        selectInput("size_col", "Select Population Size Column (N_i):", choices = cols)
      )
    })
    
    # Core Systematic PPS Logic
    sampled_resultsp <- eventReactive(input$generatep, {
      req(uploaded_data(), input$cluster_col, input$size_col, input$sample_clusters)
      
      df <- uploaded_data()
      cluster_var <- input$cluster_col
      size_var <- input$size_col
      n_sample <- input$sample_clusters
      
      # Extract sizes and cluster names
      cluster_names <- as.character(df[[cluster_var]])
      sizes <- as.numeric(df[[size_var]])
      K <- length(sizes)
      N <- sum(sizes)
      
      # Validations
      validate(
        need(!any(is.na(sizes)), "Size column contains non-numeric or missing values."),
        need(all(sizes > 0), "All cluster sizes must be positive numbers."),
        need(n_sample <= K, "Number of clusters to sample (k) cannot exceed total available clusters (K).")
      )
      
      # 1. Compute Cumulative Ranges
      cum_sizes <- cumsum(sizes)
      cum_start <- c(1, cum_sizes[-K] + 1)
      cum_end <- cum_sizes
      
      # 2. Compute Interval and Random Start
      interval <- N / n_sample
      r_start <- runif(1, min = 1, max = interval)
      
      # 3. Generate Systematic Sampling Points
      sample_points <- r_start + (0:(n_sample - 1)) * interval
      
      # 4. Map Sampling Points to Clusters
      selected_indices <- integer(n_sample)
      for (j in seq_along(sample_points)) {
        pt <- sample_points[j]
        selected_indices[j] <- which(pt >= cum_start & pt <= cum_end)
      }
      
      unique_selected_idx <- unique(selected_indices)
      
      # Build Summary Data Frame
      summary_df <- data.frame(
        Cluster_Name = cluster_names,
        Population_Size_Ni = sizes,
        Cum_Start = cum_start,
        Cum_End = cum_end,
        Status = ifelse(1:K %in% unique_selected_idx, "Selected", "Not Selected"),
        Times_Selected = sapply(1:K, function(i) sum(selected_indices == i))
      )
      
      # Generate Sample Rows for Selected Clusters
      sample_list <- lapply(unique_selected_idx, function(idx) {
        c_name <- cluster_names[idx]
        c_size <- sizes[idx]
        
        data.frame(
          Cluster_Name = c_name,
          Unit_ID = paste0(c_name, "_Unit_", 1:c_size),
          Unit_Index = 1:c_size
        )
      })
      
      list(
        summary = summary_df,
        sample = bind_rows(sample_list),
        params = list(
          N = N,
          K = K,
          n_sample = n_sample,
          interval = round(interval, 4),
          r_start = round(r_start, 4),
          sample_points = round(sample_points, 4)
        )
      )
    })
    
    # Render Text Summary
    output$params_summary <- renderText({
      p <- sampled_resultsp()$params
      paste0(
        "Total Population (N): ", p$N, "\n",
        "Total Clusters (K): ", p$K, "\n",
        "Sampling Interval (k = N / n_sample): ", p$interval, "\n",
        "Random Start (r): ", p$r_start, "\n",
        "Systematic Points: ", paste(p$sample_points, collapse = ", ")
      )
    })
    
    # Render Summary Table
    output$summary_tablep <- renderTable({
      sampled_resultsp()$summary
    })
    
    # Render Sampled Units Table
    output$sample_tablep <- renderTable({
      head(sampled_resultsp()$sample, 30) # Display preview
    })
    
    # Excel Export Handler
    output$download_excelp <- downloadHandler(
      filename = function() {
        paste0("systematic_pps_upload_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        addWorksheet(wb, "Sampled Units")
        addWorksheet(wb, "Cumulative PPS Table")
        
        writeData(wb, "Sampled Units", sampled_resultsp()$sample)
        writeData(wb, "Cumulative PPS Table", sampled_resultsp()$summary)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )  
    
    
    
    
    
    
    
    
    
}        
        





