library(shiny)
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
})
library(shinythemes)
#library(DT)
library(ggplot2)
library(scales)
library(tidyr)
#library(multcompView)
#library(shinydashboard)
library("shinyWidgets")
library(nortest)  # For Anderson-Darling test (ad.test)
library(moments)
library(dplyr)
library(readxl)
library(flextable)
library(officer)
library(corrplot)
library(rhandsontable)
library(pROC)
library(caret)
library(colourpicker)
library("RColorBrewer")
library("viridisLite")
library("rlang")
#library(shinyWidgets)
#library(car)   # for Levene's test

mvss_logo_available <- file.exists(file.path("www", "MVSSCLOGO.svg"))

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  
  title = div(
   if (mvss_logo_available) img(src = "MVSSCLOGO.svg", height = "30px", style = "margin-right:10px;"),
   "MVSS"
 ),
  
  id = "tabs",
  
  #---------------- HOME ----------------#
 tabPanel("Home", icon = icon("home"),
          withMathJax(),
          if (mvss_logo_available) tags$div(
            style = "text-align:center;",
            tags$img(src = "MVSSCLOGO.svg", width = "8%")
          ),
          h2("$$\\text{MVSS 1.1}$$"),
          h3("$$\\text{MV Statistical Solution}$$"),
          h4("The MV Statistical Solution Software is a comprehensive and reliable platform
designed to support accurate and efficient statistical analysis
for research and academic applications. 
The software is developed with an emphasis on accuracy,
reproducibility, and ease of use, enabling researchers, students,
and professionals to perform statistical analyses with confidence.
Its user-friendly interface and well-structured analytical tools
make complex statistical procedures accessible without unnecessary
complexity."),
          hr(),
          p("Any queries/suggestions or assistance write to:"), 
          tags$a(href = "mailto:mavithats@gmail.com", "mavithats@gmail.com:"),
          tags$a(href = "mailto:vasanthakumarisj@gmail.com", "vasanthakumarisj@gmail.com"),  
          br(),
          div(
            style = "
    text-align: center;
    padding: 15px;
    margin-top: 20px;
    border-top: 1px solid #ddd;
    color: #666;
    font-size: 12px;
  ",
            "© 2026 Mavitha T S. All Rights Reserved.",
            br(),
            "Developed using R and R Shiny."
          )
 ),

  
 tabPanel("Upload Data",
          tabsetPanel(
            
            tabPanel("Upload data",
                     fileInput("file_global", "Upload CSV/Excel File", accept = c(".csv", ".xlsx", ".xls")),
                     uiOutput("sheet_selector"), # Dynamic selector for Excel sheets
                     hr(),
                     DTOutput("preview1")
                
            ),
            
            tabPanel("Data edit",
                     
                     sidebarLayout(
                       sidebarPanel(
                         
                         h4("Add and delete"),
                       # --- Row Actions ---
                       actionButton("add_row", "Add Row", class = "btn-success btn-sm"),
                       actionButton("delete_row", "Del Row", class = "btn-danger btn-sm"),
                       br(),br(),
                       # --- Column Actions (Conditional on user action or always accessible compact) ---
                       div(
                         style = "display: flex; align-items: center; gap: 4px; margin-bottom: 0px;",
                         textInput("new_col_name", NULL, placeholder = "New col name", width = "110px"),
                         actionButton("add_col", "Add Col", class = "btn-primary btn-sm")
                       ),
                       br(),
                       div(
                         style = "display: flex; align-items: center; gap: 4px; margin-bottom: 0px;",
                       selectInput("col_to_delete", NULL, choices = NULL, width = "110px"),
                       actionButton("delete_col_btn", "Del Col", class = "btn-danger btn-sm")
                       ),
                       # --- Stage 1 Subgroup Controls ---
                       h4("Grouping/Filter"),
                       selectInput("subgroup_var1", "Subgroup Var 1:", choices = NULL, width = "160px"),
                       # Appears only when Variable 1 is selected
                       conditionalPanel(
                         condition = "input.subgroup_var1 != ''",
                         uiOutput("subgroup_controls1")
                       ),
                       
                       # --- Stage 2 Subgroup Controls ---
                       selectInput("subgroup_var2", "Subgroup Var 2:", choices = c("None" = "", "Select Var..." = ""), width = "160px"),
                       # Appears only when Variable 2 is selected
                       conditionalPanel(
                         condition = "input.subgroup_var2 != ''",
                         uiOutput("subgroup_controls2")
                       ),
                       br(),
                       h4("Data Validation"),
                       # --- Data Validation Controls ---
                       checkboxInput("handle_na", "Remove rows with missing values (NAs)", value = FALSE),
                       checkboxInput("handle_outliers", "Remove outliers (IQR method)", value = FALSE),
                       
                       # Appears only when outlier removal is checked
                       conditionalPanel(
                         condition = "input.handle_outliers == true",
                         selectizeInput("outlier_cols", "Select Numeric Columns for Outlier Check:", choices = NULL, multiple = TRUE, width = "200px")
                       ),
                       br(),
                       h4("Variable Coding"),   
                       # --- Variable Coding Controls ---
                       selectInput("code_var", "Code Variable:", choices = NULL, width = "160px"),
                       # Appears only when a variable to code is chosen
                       conditionalPanel(
                         condition = "input.code_var != ''",
                         tagList(
                           textInput("coded_col_name", "New Coded Col Name:", value = "coded_variable", width = "160px"),
                           uiOutput("coding_ui"),
                           actionButton("apply_coding", "Append Coded Col", class = "btn-primary btn-sm")
                         )
                       
                     ),
                     br(),
                     h4("Variable Transformation"),
                     
                     # --- Variable Transformation Controls ---
                     selectInput("transform_type", "Transform Operation:", 
                                 choices = c("Select Operation..." = "", 
                                             "Sum / Add Multiple Variables" = "sum", 
                                             "Divide (Numerator / Denominator)" = "div", 
                                             "Custom R Expression" = "custom"), 
                                 width = "180px"),
                     
                     # Appears only when a transformation type is chosen
                     conditionalPanel(
                       condition = "input.transform_type != ''",
                       tagList(
                         textInput("transformed_col_name", "New Transformed Col Name:", value = "transformed_var", width = "180px"),
                         uiOutput("transform_ui"),
                         actionButton("apply_transform", "Apply Transformation", class = "btn-primary btn-sm")
                       )
                     ),
                     br(),hr(),
                     downloadButton("downloadData", "Save", class = "btn-warning btn-sm")
                     
                       ),
                     mainPanel(
                     DTOutput("preview")
                     )
                     )
            
            ),
            
            # --- Tab 2: Quick Analysis ---
            tabPanel("Quick analysis",
                  titlePanel("Classification of variables"),  
                     sidebarLayout(
                       sidebarPanel(
                         checkboxInput(
                           "normality",
                           "Perform normality assessment",
                           TRUE
                         ),      
                         actionButton(
                           "analyse",
                           " Analyse",
                           class = "btn-primary"
                         ),
                         br(), br(),
                         downloadButton(
                           "download_wordQ",
                           "Download Word Report"
                         )
                       ),
                       
                       mainPanel(
                         # Sub-tabs for displaying different analysis tables cleanly
                         tabsetPanel(
                           tabPanel(
                             
                             "Variable Classification",
                             
                             br(),
                             
                             DTOutput(
                               "classification"
                             )
                           ),
                           
                           
                           # ----------------------------------------------------
                           # NUMERICAL
                           # ----------------------------------------------------
                           
                           tabPanel(
                             
                             "Numerical Variables",
                             
                             br(),
                             
                             DTOutput(
                               "numeric_table"
                             )
                           ),
                           
                           
                           # ----------------------------------------------------
                           # CATEGORICAL
                           # ----------------------------------------------------
                           
                           tabPanel(
                             
                             "Categorical Variables",
                             
                             br(),
                             
                             DTOutput(
                               "categorical_table"
                             )
                           ),
                           
                         
                         )
                       )
                     )
            )
            
          )
 ), 
  #---------------- HYPOTHESIS MENU ----------------#
  navbarMenu("Analysis I",

    #-----------------Summary staistics------------------------------
  
  tabPanel("Descriptive statistics",
           titlePanel("Descriptive statistics"),
           tabsetPanel(
            tabPanel("Summary one variable", 
                    
                     
                     sidebarLayout(
                       sidebarPanel(
                         width = 3,
                         
                      selectInput("desc_var", "Select Variable:",
                                 choices = NULL),
                      br(),
                      downloadButton("dl_desc_word", "Download Summary(.docx)")
                       ),
                      mainPanel(
                        # UI snippet for the panel:
                        tableOutput("desc_combined"),
                        
                     )
                     )
                     ), 
            tabPanel(
              "Grouped Descriptive Statistics",
              sidebarLayout(
                sidebarPanel(
                #  h4("Variable Selection"),
                  selectInput("desc_var1", "Select Numerical Variable:", choices = NULL),
                  selectInput("desc_group_var", "Select Categorical (Grouping) Variable:", choices = NULL),
                  hr(),
                  br(),
                  downloadButton("dl_desc_group_word", "Download Grouped Summary (.docx)")
                ),
                
                mainPanel(
                  # UI snippet:
                  tableOutput("desc_group_combined")
                
                )
              )
            ),
            tabPanel("Enter data",
                     
                     
                     sidebarLayout(
                       sidebarPanel(
                         width = 3,
                     textAreaInput("data", "Enter Data (comma separated):",
                                   placeholder = "Enter the data here...", rows=5),
                     br(),
                     
                     downloadButton("download_desc_entered", " Download Descriptives (.CSV)")
                       ),
                     mainPanel(
                     tableOutput("desc_entered_table"),
                     
                     )
                     )
            )
             
           )
           
           
  ),  
  
  #-----------------Normality test------------------------------  
  
  tabPanel("Normal curve and normality test",
           
             titlePanel("Normality Test"),
             
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("norm_var", "Select Variable:", choices = NULL),
               actionButton("analyze", "Analyze", class = "btn-primary w-100"),
               br(),
               downloadButton("dl_normality_report_word", "Download Normality Report (.docx)")
             ),
             
             mainPanel(
               h3("Visual Assessment"),
               plotOutput("histPlot"),
                br(),
               
               plotOutput("qqPlot"),
                hr(),
               
               # Normality Tests Table
               h3("Statistical Normality Tests"),
               tableOutput("normality_tests_table"),
                hr(),
               
               # Shape Statistics Table
               h3("Shape Statistics"),
               tableOutput("shape_stats_table"),
               
               hr(),
               
               # Decision & Interpretation
               h3("Decision & Guidance"),
               #strong("Overall Consensus:"),
               textOutput("final_decision"),
               br(), br()
             )
           )        
    
  ),
  
             
             #================ SINGLE MEAN TEST =================#
             tabPanel("Single Sample t-Test",
                      titlePanel("Single Sample t-Test"),
                      
                      tabsetPanel(
                        
                        
                        #---- Upload Data ----#
                        tabPanel(
                          "Upload Data",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              selectInput("single_mean_var", "Select Variable:", choices = NULL),
                              
                              selectInput(
                                "tail1", "Type of Test:",
                                choices = c(
                                  "Two-tailed (μ ≠ μ₀)" = "two.sided",
                                  "Left-tailed (μ < μ₀)"  = "less",
                                  "Right-tailed (μ > μ₀)" = "greater"
                                )
                              ),
                              
                              numericInput("mu1", "Hypothesized Population Mean (μ₀):", value = 13.5),
                              numericInput("conf1", "Confidence Level:", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
                              
                              hr(),
                              
                              
                              br(),
                             # h4("Download Options"),
                          #    downloadButton("dl_summary1", "Summary Statistics (CSV)", style = "margin-bottom: 5px; width: 100%;"),
                           #   downloadButton("dl_result1", "Test Results (CSV)", style = "margin-bottom: 5px; width: 100%;"),
                              downloadButton("dl_singlet_report", "Download Result(.docx)")
                            ),
                            
                            mainPanel(
                              h4("1. Summary Statistics"),
                              tableOutput("summary1_table"),
                              
                              hr(),
                              
                              h4("2. Hypothesis Test Results"),
                              tableOutput("result1_table"),
                              br(),
                              hr()
                            )
                          )
                        ),
                      #---- Enter Data ----#
                      tabPanel(
                        "Enter Data",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            textAreaInput(
                              "data", "Enter Sample Data (comma separated):",
                              value = "12.5,13.0,12.8,11.9,13.2,12.7,12.6,13.1,12.4,12.9",
                              rows = 4
                            ),
                            selectInput(
                              "tail", "Type of Test:",
                              choices = c(
                                "Two-tailed (μ ≠ μ₀)" = "two.sided",
                                "Left-tailed (μ < μ₀)"  = "less",
                                "Right-tailed (μ > μ₀)" = "greater"
                              )
                            ),
                            numericInput("mu", "Hypothesized Population Mean (μ₀):", value = 13.5),
                            numericInput("conf", "Confidence Level:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                            
                            hr(),
                            
                            br(),
                          
                          #  downloadButton("dl_summary", "Summary Statistics (CSV)", style = "margin-bottom: 5px; width: 100%;"),
                          #  downloadButton("dl_result", "Test Results (CSV)", style = "margin-bottom: 5px; width: 100%;"),
                            downloadButton("dl_singlet_manuals", "Download Report (.docx)")
                          ),
                          
                          mainPanel(
                            h4("1. Summary Statistics"),
                            tableOutput("summary_tablesingle"),
                            
                            hr(),
                            
                            h4("2. Hypothesis Test Results"),
                            tableOutput("result_table"),
                            
                            hr(),
                            br()
                          )
                        )
                      ),
                      )
             ),
             #================ INDEPENDENT T TEST =================#
  tabPanel("Two Independent Sample t-Test",
           titlePanel(" Two Independent Sample t-Test"),
           
           tabsetPanel(
             
             # ==========================================
             # TAB 1: UPLOAD DATA
             # ==========================================
             tabPanel("Upload Data",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          uiOutput("var1"),
                          uiOutput("var2"),
                          
                          selectInput("var_equal3", "Variance Assumption:",
                                      choices = c("Equal (Student's t)" = TRUE, "Unequal (Welch's t)" = FALSE)),
                          
                          selectInput("tail3", "Alternative Hypothesis:", 
                                      choices = c("Two-sided" = "two.sided", "Less" = "less", "Greater" = "greater"),
                                      selected = "two.sided"),
                          
                          numericInput("conf3", "Confidence Level:", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
                          br(),
                          
                          downloadButton("download_word_upload2", " Download Report (.docx)")
                        ),
                        
                        mainPanel(
                          h4("Summary Statistics"),
                          DTOutput("summary_table_upload"),
                          br(),
                          hr(),
                          
                          h4(" Test Results"),
                          DTOutput("result_table_upload"),
                          br(),
                          hr(),
                          
                          h4("Visual Comparison"),
                          plotOutput("boxplot_upload"),
                          br(), br()
                        )
                      )
             ),
           
               # ==========================================
               # TAB 2: ENTER DATA MANUALLY two sample t-test
               # ==========================================
               tabPanel("Enter Data",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            textAreaInput("group1", "Group 1 Data (comma-separated):",
                                          value = "12.5, 13.0, 12.8, 13.2, 14.1, 13.5", rows = 3),
                            
                            textAreaInput("group2", "Group 2 Data (comma-separated):",
                                          value = "11.5, 12.0, 11.8, 12.2, 11.9, 12.4", rows = 3),
                            
                            selectInput("var_equal2", "Variance Assumption:",
                                        choices = c("Equal (Student's t)" = TRUE, "Unequal (Welch's t)" = FALSE)),
                            
                            selectInput("tail2", "Type of Test:",
                                        choices = c("Two-tailed" = "two.sided",
                                                    "Less" = "less",
                                                    "Greater" = "greater")),
                            
                            numericInput("conf2", "Confidence Level:", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
                            tags$hr(),
                            downloadButton("download_word_manual2", "Download Word Report"),
                          br(),
                              ),
                          
                          mainPanel(
                            h4("Summary Statistics"),
                            DTOutput("summary_table_manual"),
                            br(),
                            
                            hr(),
                            
                            h4("Test Results"),
                            DTOutput("result_table_manual"),
                            br(),
                            
                          
                            
                            h4("Visual Comparison"),
                            plotOutput("boxplot_manual"),
                            # Add this in your UI where you want the download button to appear
                            br(), br()
                          )
                        )
               )
               
             )
    ),
  
             
             #================ PAIRED T TEST =================#
             tabPanel("Paired t-test",
                      titlePanel("Paired t-test"),
                      
                      #---- Upload Data ----#
                      tabsetPanel(       
                      tabPanel("Upload Data",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   uiOutput("upvar1"),
                                   uiOutput("upvar2"),
                                   selectInput("uptail", "Type of Test:",
                                               choices = c("Two-tailed" = "two.sided",
                                                           "Less" = "less",
                                                           "Greater" = "greater")),
                                   numericInput("upconf", "Confidence Level:", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
                                   br(),
                                   downloadButton("download_word_paired", "Download Report (.docx)")
                                    ),
                                 
                                 mainPanel(
                                   h4("Summary"),
                                   tableOutput("upload_summary_table"),
                                   br(),
                                   h4("Paired t-Test Results"),
                                   tableOutput("upload_test_results_table"),
                                   br(),
                                   hr(),
                                   h4("Line Chart"),
                                   plotOutput("upload_line_chart"),
                                   # Place this inside your UI where you want the button to appear
                                   br(),br()
                                 )
                               )
                      ),
               
                        
                        # ---- Enter Data Tab paired ---- #
                        tabPanel("Enter Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 3,
                                     textAreaInput("before", "Before (Comma separated):", "120, 130, 125, 140"),
                                     textAreaInput("after", "After (Comma separated):", "115, 128, 120, 135"),
                                     selectInput("ptail", "Type of Test:",
                                                 choices = c("Two-tailed" = "two.sided",
                                                             "Less" = "less",
                                                             "Greater" = "greater")),
                                     numericInput("pconf", "Confidence Level:", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
                                     br(),
                                     downloadButton(
                                       "download_word_manual_paired",
                                       "Download Results (.docx)"
                                     )
                                   ),
                                   mainPanel(
                                     h4("Summary"),
                                     tableOutput("manual_summary_table"),
                                     
                                     br(),
                                     h4("Paired t-Test Results"),
                                     tableOutput("manual_test_results_table"),
                                     br(),
                                     hr(),
                                     h4("Line Chart"),
                                     plotOutput("line_chart"),
                                     # Add this button where you want the download control in your UI
                                     br(), br()
                                   )
                                 )
                        )
                      )       
             ),
  
  tabPanel("One way ANOVA",
           titlePanel("One way ANOVA"),
           
           tabsetPanel(
             
              tabPanel("upload data (Compare groups within variable) ",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          width = 3,
                          uiOutput("anova_y"),
                          uiOutput("anova_group"),
                          
                          numericInput("anova_conf", "Confidence Level:", 0.95),
                          selectInput("posthoc_type", "Post-hoc Test:",
                                      choices = c("None" = "none",
                                                  "Tukey HSD" = "tukey",
                                                  "LSD (Least Significant Difference)" = "lsd",
                                                  "Bonferroni" = "bonf")),
                          br(),
                          downloadButton("download_word_anova", "Download Report (.docx)")
                        ),
                        
                        mainPanel(
                          h4("Group Means"),
                          tableOutput("anova_means"),
                          br(),
                          h4("ANOVA Table"),
                          tableOutput("anova_result"),
                          br(),
                          
                           h4(" Assumption Checks"),
                          tableOutput("assumption_result1"),
                          br(),
                          
                         # checkboxInput("show_assumption_plots", " Show Assumption Plots", value = FALSE),
                          plotOutput("assumption_plots"),
                          br(),
                                h4("Post-hoc Test Result"),
                          tableOutput("posthoc_result"),
                          
                          br(), br()
                                                  )
                      )
             ),
             
             tabPanel("upload data (Compare seperate independent group variable) ",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          width = 3,
                          
                          h4("One-Way ANOVA: Compare Separate Group Columns"),
                          
                          numericInput(
                            "anova_num_groups_upload",
                            "Number of groups to compare:",
                            value = 3,
                            min = 2,
                            step = 1
                          ),
                          
                          uiOutput("anova_group_columns_ui"),
                          
                          numericInput(
                            "anova_columns_conf",
                            "Confidence level (%)",
                            value = 95, min = 80, max = 99, step = 1
                          ),
                          
                          actionButton(
                            "run_anova_columns",
                            "Compare Groups",
                            class = "btn-primary"
                          ),
                          
                          hr(),
                          downloadButton(
                            "download_anova_columns_word",
                            "Download Complete ANOVA Report (Word)",
                            class = "btn-success"
                          )
                          
                        ),
                        
                        mainPanel(
                          hr(),
                          h4("Descriptive Statistics"),
                          tableOutput("anova_columns_summary"),
                          
                          h4("One-Way ANOVA Results"),
                          tableOutput("anova_columns_results"),
                          hr(),
                          h4("Assumption Checks"),
                          tableOutput("anova_columns_assumptions"),
                          
                          hr(),
                          h4("Group Means with Confidence Intervals"),
                          tableOutput("anova_columns_mean_ci"),
                          
                          hr(),
                          h4("ANOVA / Alternative Test Results"),
                          tableOutput("anova_columns_test_results"),
                          
                          uiOutput("anova_columns_posthoc_ui"),
                          
                          hr(),
                          h4("Normal Q-Q Plot"),
                          plotOutput("anova_columns_qq_plot", height = "400px"),
                          
                          h4("Residuals vs Fitted Plot"),
                          plotOutput("anova_columns_residual_plot", height = "400px")
                          
                        )
                        
                      )
             ),
             
             
             tabPanel("upload data (Compare repeated-measurement variables) ",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          width = 3,
                          
                          h4("One-Way ANOVA: Compare repeated-measurement Columns"),
                          
                          
                          # Number of repeated-measurement columns
                          numericInput(
                            "anova_rm_num_groups",
                            "Number of repeated measurements / conditions",
                            value = 3, min = 2, step = 1
                          ),
                          
                          uiOutput("anova_rm_group_columns_ui"),
                          
                          # Confidence level
                          selectInput(
                            "anova_rm_conf",
                            "Confidence level",
                            choices = c("90%" = 90, "95%" = 95, "99%" = 99),
                            selected = 95
                          ),
                          
                          # Choose the nonparametric alternative
                          checkboxGroupInput(
                            "anova_rm_nonparam",
                            "Nonparametric test (select one)",
                            choices = c(
                              "Friedman test" = "friedman",
                              "Quade test" = "quade"
                            ),
                            selected = "friedman"
                          ),
                          
                          checkboxInput(
                            "anova_rm_force_nonparam",
                            "Use the selected nonparametric test instead of repeated-measures ANOVA",
                            value = FALSE
                          ),
                          
                          actionButton(
                            "run_anova_rm",
                            "Run Repeated-Measures Analysis",
                            class = "btn-primary"
                          ),
                          
                          hr(),
                          downloadButton(
                            "download_anova_rm_word",
                            "Download Complete Report (Word)",
                            class = "btn-success"
                          )
                        ),
                        mainPanel(  
                          
                          h4("Descriptive Statistics"),
                          tableOutput("anova_rm_summary"),
                          
                          h4("Repeated-Measures ANOVA / Nonparametric Results"),
                          tableOutput("anova_rm_results"),
                          
                          h4("Confidence Intervals"),
                          tableOutput("anova_rm_ci"),
                          
                          h4("Assumption Checks"),
                          tableOutput("anova_rm_assumptions"),
                          
                          h4("Post-Hoc Comparisons"),
                          uiOutput("anova_rm_posthoc_ui"),
                          
                          h4("Normal Q-Q Plot"),
                          plotOutput("anova_rm_qq_plot", height = "350px"),
                          
                          h4("Residuals vs Fitted Plot"),
                          plotOutput("anova_rm_residual_plot", height = "350px"),
                          
                        )
                        
                      )
             ),
             
             
              tabPanel(" Enter Data",
                      
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("num_groups", "Number of Groups:", value = 3, min = 2, max = 10, step = 1),
                          hr(),
                          h4("Sample Sizes"),
                          uiOutput("sample_sizes_ui"),
                          hr(),
                          actionButton("run_anovaone", "Analyse"),
                          br(), hr(),
                        #  h4("Download Complete Report"),
                          downloadButton(
                            "dl_anova_report",
                            "Download Word Report (.docx)"
                                                )
                          
                          
                        ),
                        
                        mainPanel(
                          h4("Enter Data Values"),
                        #  p("Type the numeric values for each group below:"),
                          uiOutput("data_table1_ui"),
                          
                          hr(),
                          h4("1. Descriptive Statistics"),
                          tableOutput("desc_table1"),
                          
                          
                          hr(),
                          h4("2. ANOVA Assumption Checks"),
                          #p("Tests whether data meets the assumptions of normality (Shapiro-Wilk) and equal variances (Bartlett's):"),
                          tableOutput("assumptions_table1"),
                         
                        hr(),
                        h4("4. One-Way ANOVA Summary Table"),
                        tableOutput("anova_table1"),
                        
                          
                          # Dynamic Non-Parametric section (Kruskal-Wallis & Pairwise Wilcoxon)
                          uiOutput("nonparametric1_ui"),
                          
                          hr(),
                          h4("3. Group Distribution Boxplot"),
                          plotOutput("anova_plot1"),
                          
                        # Dynamic Post-Hoc section (Tukey's HSD)
                          uiOutput("posthoc1_ui")
                        )
                      )
             ),
             
             
           )
  ),
  
  #two way ANOVA(one value per cell)---------------------
  tabPanel("Two Way ANOVA (one value per cell)",
           titlePanel("Two-Way Analysis of Variance (ANOVA)"),
           tabsetPanel(
             tabPanel("Upload data",
           sidebarLayout(
             sidebarPanel(
               # Dynamic UI Renderings
               uiOutput("anova2_y"),
               uiOutput("anova2_factor1"),
               uiOutput("anova2_factor2"),
               
               checkboxInput("interaction", "Include Interaction Term (Factor A * Factor B)", value = TRUE),
               
               hr(),
               downloadButton("download_summary2", "Download Summary Table (CSV)"),
               downloadButton("download_anova2", "Download ANOVA Table (CSV)"),
               downloadButton("download_assumptions2", "Download Assumptions Tests (CSV)"),
               downloadButton("download_interaction_plot", "Download Interaction Plot"),
               downloadButton("download_residual_plot", "Download Residual Plots")
             ),
             
             mainPanel(
               
                       h4("Group Summary Statistics Table"),
                          tableOutput("summary_table2"),
                          br(),
                          h4("Two-Way ANOVA Table"),
                          tableOutput("anova_table2"),
                          br(),
                          hr(),
                          #h4("Model Summary Output"),
                          #verbatimTextOutput("anova_summary2"),
                       h4("Numerical Evaluation of Model Assumptions"),
                       tableOutput("assumption_result2"),
                       br(),
                       h4("Interaction Plot"),
                          plotOutput("interaction_plot"),
                          
                          br(),
                          hr(),
                          h4("Residuals Diagnostic Plot"),
                          plotOutput("residual_plot"),
                          
                         )
               )
             
             ),
           tabPanel("Enter data (one value per cell)",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       h4("1. Define Matrix Dimensions"),
                       numericInput("num_rows", "Number of Factor A Levels (Rows):", value = 3, min = 2, max = 10),
                       numericInput("num_cols", "Number of Factor B Levels (Columns):", value = 3, min = 2, max = 10),
                       
                       hr(),
                       h4("2. Edit Variable Names"),
                       textInput("factor_a_name", "Factor A Name (Rows):", value = "Factor_A"),
                       textInput("factor_b_name", "Factor B Name (Columns):", value = "Factor_B"),
                       textInput("response_name", "Response Variable Name:", value = "Response"),
                       
                       hr(),
                       actionButton("run_anova", "Run Two-Way ANOVA", class = "btn-primary btn-block"),
                       br(),
                       # Single Download Button for Complete Word Report
                       downloadButton("dl_word_report", "Download Full Report (.docx)", class = "btn-success btn-block")
                     ),
                     
                     mainPanel(
                          "Data Entry",
                           br(),
                           p("Enter one numeric value per cell in the table below:"),
                           DTOutput("data_grid"),
                         
                         
                        
                           "ANOVA Results",
                           br(),
                           h3("Two-Way ANOVA Summary"),
                           tableOutput("anova_table"),
                           hr(),
                           h3("Interaction / Main Effects Plot"),
                           plotOutput("main_effects_plot"),
                         
                         
                         
                           "Post-Hoc Analysis",
                           br(),
                           uiOutput("posthoc_ui")
                         
                       
                     )
                   ) 
                   
                   
                   ),
           )
  ),
           #two way ANOVA(repeated value)---------------------
           tabPanel("Two Way ANOVA (Repeated mesurement)",
                    titlePanel("Two-Way Analysis of Variance (ANOVA)"),
                    tabsetPanel(
                      tabPanel("Upload data",
                      ),
           
           
           
           tabPanel("Enter data (Repeated measurement)",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("levels_a", "Groups in Variable 1 (Rows):", value = 2, min = 2, max = 5, step = 1),
                        numericInput("levels_b", "Groups in Variable 2 (Cols):", value = 3, min = 2, max = 5, step = 1),
                        numericInput("replicates2", "Replicates (Repeated measure) (n):", value = 1, min = 1, max = 10, step = 1),
                        hr(),
                        actionButton("run_anovatwo", "Two-Way Analysis"),
                        h4("Download"),
                        downloadButton("dl_desc2", "Download Descriptive Table (.csv)", class = "btn-sm"),
                        downloadButton("dl_assumptions2", "Download Assumptions Table (.csv)"),
                        downloadButton("dl_plot2", "Download Plot (.png)", class = "btn-sm"),
                        downloadButton("dl_anova2", "Download ANOVA Table (.csv)", class = "btn-sm"),
                        
                      ),
                      
                      mainPanel(
                        h4("Enter Data Values by Cell"),
                        #p("Type the numeric observations for each combination of Variable 1 and Variable 2 below:"),
                        uiOutput("grid_table_ui"),
                        
                        hr(),
                        h4("1. Descriptive Statistics"),
                        tableOutput("desc_table2"),
                        
                        
                        hr(),
                        h4("2. ANOVA Assumption Checks"),
                        tableOutput("assumptions_table2"),
                        
                        
                        hr(),
                        h4("3. Interaction Plot"),
                        plotOutput("interaction_plot2"),
                      
                        
                        hr(),
                        h4("4. Two-Way ANOVA Summary Table"),
                        tableOutput("anova_table2"),
                        
                        
                        # Dynamic Post-Hoc section
                        uiOutput("posthoc2_ui")
                      )
                    )
                    
                    
                    
                    ),
           )
  ),
  
  tabPanel("Chi-square indepndent test",
           titlePanel("Chi-square test for association"),
    tabsetPanel(
      # ==============================================================================
      # CHI-SQUARE TEST OF INDEPENDENCE (UI TAB PANEL)
      # ==============================================================================
      
      tabPanel(
        "Chi-Square Test",  # Changed from "upload data" to match the actual analysis
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Variable Selection"),
            # In ui.R (sidebarPanel)
            selectInput("chi_x", "Select Variable 1 (Categorical):", choices = NULL),
            selectInput("chi_y", "Select Variable 2 (Categorical):", choices = NULL), 
            hr(),
            h4("Test Settings"),
            numericInput(
              inputId = "chi_conf", 
              label = "Confidence Level:", 
              value = 0.95, 
              min = 0.80, 
              max = 0.99, 
              step = 0.01
            ),
            checkboxGroupInput(
              inputId = "chi_percentages",
              label = "Display Percentage Tables:",
              choices = c(
                "Row Percentages (%)" = "row",
                "Column Percentages (%)" = "col",
                "Cell Percentages (%)" = "cell"
              ),
              selected = character(0)
            ),
          #  helpText("Used to calculate significance threshold (α = 1 - Confidence Level).")
          h4("Download"),
          downloadButton("dl_chi_table", "Observed Counts (CSV)"),
          downloadButton("dl_chi_expected", "Expected Counts (CSV)"),
          downloadButton("dl_chi_result", "Test Results (CSV)"),
          downloadButton("dl_cramer_result", "Cramér's V Effect Size (CSV)")
          ),
          
          mainPanel(
            width = 9,
            
            # Upper Section: Observed and Expected Tables Side-by-Side
                            h4("Observed Contingency Table"),
                tableOutput("chi_table"),
            uiOutput("chi_percentage_tables_ui"),
            hr(),
                h4("Expected Cell Counts"),
                tableOutput("chi_expected"),
                
            hr(),
            
            # Middle Section: Test Results
            h4("Hypothesis Test Results"),
            tableOutput("chi_result_table"),
            hr(),
            h4("Effect Size (Cramér’s V)"),
                tableOutput("cramer_result_table"),
            
            br(), br()
                    )
        )
      ),
    tabPanel("Enter data",
             sidebarLayout(
               sidebarPanel(
                 width = 3,     
             numericInput("rows", "Number of Rows:", value = 2, min = 2),
             numericInput("cols", "Number of Columns:", value = 2, min = 2),
             actionButton("create", "Create Table"),
             br(), 
              actionButton("analyze", "Analyze"),
             checkboxGroupInput(
               inputId = "manual_percentages",
               label = "Display Percentage Tables:",
               choices = c(
                 "Row Percentages (%)" = "row",
                 "Column Percentages (%)" = "col",
                 "Cell Percentages (%)" = "cell"
               ),
               selected = character(0)
             ),
             br(),
             hr(),
             h3("Download"),
             downloadButton("dl_manual_observed", "Observed Counts (CSV)"),
             downloadButton("dl_manual_expected", "Expected Counts (CSV)"),
             downloadButton("dl_manual_results", "Test Results (CSV)"),
             downloadButton("dl_manual_cramer", "Cramér's V (CSV)"),
               ),
             mainPanel(
               
              
             uiOutput("table_ui"),
               
               br(),
               h4("Observed table"),     
                tableOutput("observed"),
                
                    br(), 
             uiOutput("manual_percentage_tables_ui"),
             br(),
               h4("Expected table"),
                     tableOutput("expected"),
                     
                     br(), 
               
               h4("Test result"),
               tableOutput("test_result_table"),
             
             #  verbatimTextOutput("interpretation"),  
                 br(),
             h4("Cramer's V value"),
             tableOutput("cramer_table"),
             
               br(), br()
              
             )
             )
   
    )   
     
    
    
    )  
  ),
 
  ),
 #----------------------------------
 #---------------------------------
 #navbarMenu("Analysis II",
 
 #),
 #----------------------------------- 
 # Charts & graphs
 #--------------------
 navbarMenu("Tables & Charts",
            
            tabPanel("Univariate Frequency table",
                     titlePanel("Univariate Frequency table"),
                     
                     tabsetPanel(
                     tabPanel(
                       "Upload Data",
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           
                           # 1. Select variable
                           uiOutput("freq_var"),
                           
                           # 2. Frequency type
                           selectInput(
                             "freq_type",
                             "Frequency Type:",
                             choices = c(
                               "Discrete / Categorical" = "discrete",
                               "Inclusive (Closed Intervals)" = "inclusive",
                               "Exclusive (Half-Open Intervals)" = "exclusive"
                             ),
                             selected = "discrete"
                           ),
                           
                           # 3. Class interval settings
                           conditionalPanel(
                             condition = "input.freq_type != 'discrete'",
                             
                             radioButtons(
                               "width_type",
                               "Class Width Type:",
                               choices = c(
                                 "Equal Width" = "equal",
                                 "Unequal / Custom Widths" = "unequal"
                               ),
                               selected = "equal",
                               inline = TRUE
                             ),
                             
                             conditionalPanel(
                               condition = "input.width_type == 'equal'",
                               
                               numericInput(
                                 "class_width",
                                 "Enter Class Width:",
                                 value = 5,
                                 min = 0.1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.width_type == 'unequal'",
                               
                               textInput(
                                 "custom_breaks",
                                 "Enter Custom Class Breaks (comma-separated):",
                                 value = "0, 10, 25, 50, 100",
                                 placeholder = "e.g., 0, 10, 25, 50, 100"
                               )
                             )
                           ),
                           
                           hr(),
                           
                           # 4. Show charts
                           checkboxInput(
                             "show_chart",
                             "Show Charts",
                             value = FALSE
                           ),
                           
                           conditionalPanel(
                             condition = "input.show_chart == true",
                             
                             selectInput(
                               "graph_type",
                               "Select Chart:",
                               choices = c(
                                 "Vertical Bar Chart" = "bar",
                                 "Horizontal Bar Chart" = "bar_horizontal",
                                 "Pie Chart" = "pie",
                                 "Line Chart" = "line",
                                 "Histogram" = "hist",
                                 "Frequency Polygon" = "polygon",
                                 "Ogive (Less Than)" = "ogive_less",
                                 "Ogive (More Than)" = "ogive_more"
                               ),
                               selected = "bar"
                             ),
                             
                             # Matches the current server code
                             selectInput(
                               "freq_display",
                               "Display Type:",
                               choices = c(
                                 "Frequency" = "freq",
                                 "Percentage" = "percent"
                               ),
                               selected = "freq"
                             ),
                             textInput("custom_univariate_title", "Custom Plot Title:", value = "", placeholder = "Leave blank for default title"),
                             # 5. Plot dimensions
                             sliderInput(
                               "pheight",
                               "Plot Height:",
                               min = 180,
                               max = 800,
                               value = 350,
                               step = 10
                             ),
                             
                             sliderInput(
                               "pwidth",
                               "Plot Width:",
                               min = 180,
                               max = 800,
                               value = 350,
                               step = 10
                             ),
                             
                             # 6. Shared shading / transparency
                             sliderInput(
                               "plotAlpha",
                               "Shading / Transparency:",
                               min = 0,
                               max = 1,
                               value = 0.7,
                               step = 0.05
                             ),
                             
                             # 7. Shared colour input
                             colourpicker::colourInput(
                               "plotColor",
                               "Plot Fill / Line Color:",
                               value = "#1f77b4"
                             ),
                             
                             # 8. Border colour for bar charts and histograms
                             conditionalPanel(
                               condition = "input.graph_type == 'bar' ||
             input.graph_type == 'bar_horizontal' ||
             input.graph_type == 'hist'",
                               colourpicker::colourInput(
                                 "borderColor",
                                 "Choose Border Color:",
                                 value = "#000000"
                               )
                             ),
                             
                             # 9. Pie chart palette
                             conditionalPanel(
                               condition = "input.graph_type == 'pie'",
                               
                               selectInput(
                                 "piePalette",
                                 "Pie Chart Palette:",
                                 choices = c(
                                   "Rainbow" = "rainbow",
                                   "Terrain" = "terrain",
                                   "Heat" = "heat",
                                   "Blues" = "blues"
                                 ),
                                 selected = "rainbow"
                               )
                             ),
                             
                             # 10. Frequency polygon and ogive settings
                             conditionalPanel(
                               condition ="input.graph_type == 'line' ||
                                  input.graph_type == 'polygon' ||
             input.graph_type == 'ogive_less' ||
             input.graph_type == 'ogive_more'",
                               #  colourInput("plotColor", "Line Color", value = "#1f77b4"),
                               sliderInput(
                                 "plotLwd",
                                 "Line Thickness:",
                                 min = 1,
                                 max = 5,
                                 value = 2,
                                 step = 0.5
                               ),
                               
                               sliderInput(
                                 "pointSize",
                                 "Marker Point Size:",
                                 min = 0.5,
                                 max = 3,
                                 value = 1.5,
                                 step = 0.2
                               ),
                               
                               selectInput(
                                 "pointShape",
                                 "Marker Shape:",
                                 choices = c(
                                   "Solid Circle (19)" = 19,
                                   "Open Circle (1)" = 1,
                                   "Square (15)" = 15,
                                   "Diamond (18)" = 18,
                                   "Triangle (17)" = 17,
                                   "Cross (4)" = 4
                                 ),
                                 selected = 19
                               )
                             ),
                             
                             actionButton(
                               "reset_defaults",
                               "Reset to Defaults",
                               class = "btn-warning btn-block"
                             )
                           ),
                           
                           hr(),
                           
                           # 11. Downloads
                           h4("Download"),
                           
                           downloadButton(
                             "download_freq_table",
                             "Table (CSV)"
                           ),
                           
                           br(),
                           br(),
                           
                           downloadButton(
                             "download_advanced_plot",
                             "Plot (PNG)"
                           )
                         ),
                         
                         # 12. Main panel
                         mainPanel(
                           width = 9,
                           
                           h4("Frequency Table"),
                           
                           tableOutput("grouped_freq"),
                           
                           br(),
                           
                           conditionalPanel(
                             condition = "input.show_chart == true",
                             
                             h4("Graph / Chart"),
                             
                             plotOutput(
                               "advanced_plot",
                               width = "100%",
                               height = "auto"
                             )
                           )
                         )
                       )
                     )
                     )
            ),
            
            #---------------Two-way table-------------------------------------
            tabPanel("Bivariate Frequecny table",
                     titlePanel("Bivariate Frequency Table"),
                     tabsetPanel(
                       tabPanel("Upload Data",
                     sidebarLayout(
                       sidebarPanel(
                         width = 3,
                         # Variable selection dropdowns
                         selectInput("row_var", "Select Row Variable:", choices = NULL),
                         uiOutput("row_group_ui"),
                         
                         selectInput("col_var", "Select Column Variable:", choices = NULL),
                         uiOutput("col_group_ui"),
                         hr(),
                         h4("Table Totals Options"),
                         checkboxInput("show_row_total", "Add Row Total", value = TRUE),
                         checkboxInput("show_col_total", "Add Column Total", value = TRUE),
                         checkboxInput("show_cell_total", "Add Grand (Cell) Total", value = TRUE),
                         
                         hr(),
                         h4("Percentage Options"),
                         checkboxInput("show_row_pct", "Show Row Percentage (R%)", value = FALSE),
                         checkboxInput("show_col_pct", "Show Column Percentage (C%)", value = FALSE),
                         checkboxInput("show_cell_pct", "Show Cell Percentage (Cell%)", value = FALSE),
                         hr(),
                         h4("Bar Diagram Options"),
                         checkboxInput("show_mult_bar", "Show Multiple Bar Diagram", value = FALSE),
                         checkboxInput("show_comp_bar", "Show Component (Stacked) Bar Diagram", value = FALSE),
                         
                         conditionalPanel(
                           condition = "input.show_mult_bar == true || input.show_comp_bar == true",
                           radioButtons("pct_type", "Percentage Calculation Basis:",
                                        choices = c("Row Percentage" = "row", 
                                                    "Column Percentage" = "col"),
                                        selected = "row", 
                                        inline = TRUE)
                         ),
                         textInput("custom_plot_title", "Custom Plot Title:", value = "", placeholder = "Leave blank for auto title"),
                         selectInput("biv_palette", "Color Palette:", choices = c(
                           "Viridis" = "viridis",
                           "Blues Gradient" = "blues",
                           "Set2 (Pastel)" = "set2",
                           "Accent" = "accent"
                         )),
                         selectInput("legend_pos", "Legend Position:", choices = c("Top" = "top", "Bottom" = "bottom", "Right" = "right", "None" = "none")),
                         checkboxInput("show_bar_labels", "Show Data Values on Bars", value = TRUE),
                         checkboxInput("show_comp_labels", "Show Segment Percentages", value = TRUE),
                         h4("Download"),
                         downloadButton("download_table_csv", "Table (CSV)")
                       ),
                       
                       mainPanel(
                         h4("Cross-Tabulation Table"),
                         tableOutput("twoway_table"),
                         br(),
                         conditionalPanel(
                           condition = "input.show_mult_bar == true",
                           h4("Multiple Bar Diagram (Clustered)"),
                           plotOutput("mult_bar_plot"),
                           downloadButton("download_mult_bar_png", "Save Multiple Bar Plot (PNG)"),
                           br(), br()
                           
                         ),
                         
                         conditionalPanel(
                           condition = "input.show_comp_bar == true",
                           h4("Component Bar Diagram (Stacked)"),
                           plotOutput("comp_bar_plot"),
                           downloadButton("download_comp_bar_png", "Save Component Bar Plot (PNG)"),
                           br(), br()
                         )
                       )
                     )
                       )
                     )
            ),
            
            # 1. Simple Bar Chart
            tabPanel("Simple Bar Chart",
                     titlePanel("Simple bar chart"),
                     tabsetPanel(
                       tabPanel("Enter Data",
                     sidebarLayout(
                       sidebarPanel(
                         # h4("Simple bar Chart"),
                         # h4("Data & Orientation"),
                         numericInput("sb_num_cats", "Number of Categories:", value = 4, min = 1, max = 20),
                         actionButton("sb_update", "Reset Table", class = "btn-primary", width = "100%"),
                         hr(),
                         textInput("sb_title", "Chart Title:", value = "Simple Bar Chart"),
                         textInput("sb_xlab", "X-axis Label:", value = "Category"),
                         textInput("sb_ylab", "Y-axis Label:", value = "Frequency"),
                         checkboxInput("sb_show_labels", "Show Data Labels", value = TRUE),
                         hr(),
                         radioButtons("sb_val_type", "Plot Value:", choices = c("Frequency", "Percentage"), inline = TRUE),
                         radioButtons("sb_orient", "Bar Orientation:", choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"), inline = TRUE),
                         br(),
                         
                         h4("Appearance & Export Settings"),
                         # Colour pickers
                         colourpicker::colourInput(
                           inputId = "sb_fill",
                           label = "Main Bar Colour:",
                           value = "#3498DB",
                           showColour = "both",
                           allowTransparent = FALSE
                         ),
                         
                         colourpicker::colourInput(
                           inputId = "sb_border",
                           label = "Border Colour:",
                           value = "#2C3E50",
                           showColour = "both",
                           allowTransparent = FALSE
                         ),
                         sliderInput("sb_alpha", "Transparency:", min = 0.1, max = 1, value = 0.9, step = 0.1),
                         sliderInput("sb_font", "Base Font Size:", min = 8, max = 24, value = 12, step = 1),
                         sliderInput("sb_title_size", "Title Font Size:", min = 10, max = 32, value = 16, step = 1),
                         selectInput("sb_theme", "Chart Theme:", choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "BW" = "bw", "Gray" = "gray", "Void" = "void"), selected = "minimal"),
                         checkboxInput("sb_grid", "Show Grid Lines", value = TRUE),
                         selectInput("sb_legend", "Legend Position:", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "None" = "none"), selected = "right"),
                         numericInput("sb_out_width", "Export Width (inches):", value = 8, min = 3, max = 20, step = 1),
                         numericInput("sb_out_height", "Export Height (inches):", value = 6, min = 3, max = 20, step = 1),
                         selectInput("sb_dpi", "Export Resolution:", choices = c("96 dpi" = 96, "150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300),
                         hr(),
                         downloadButton("sb_download", "Export PNG", class = "btn-success", style = "width:100%;")
                       ),
                       mainPanel(
                          DTOutput("sb_table"),
                         br(),
                         hr(),
                         plotOutput("sb_plot", height = "480px")
                         
                         
                       )
                     )
                       )
                     )
            ),
            
            # 2. Grouped Bar Chart
            tabPanel("Multiple / Grouped Bar Chart",
                     titlePanel("Multiple/Grouped bar chart"),
                     tabsetPanel(
                       tabPanel("Enter Data",
                     sidebarLayout(
                       sidebarPanel(
                         #h4("Data & Orientation"),
                         numericInput("gb_r", "Categories in Var 1 (Rows):", value = 3, min = 1, max = 15),
                         numericInput("gb_c", "Categories in Var 2 (Cols):", value = 2, min = 1, max = 15),
                         textInput("gb_title", "Chart Title:", value = "Grouped Bar Chart"),
                         textInput("gb_xlab", "X-axis Label:", value = "Variable 1"),
                         textInput("gb_ylab", "Y-axis Label:", value = "Frequency"),
                         checkboxInput("gb_show_labels", "Show Data Labels", value = TRUE),
                         hr(),
                         
                         radioButtons("gb_orient", "Bar Orientation:", choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"), inline = TRUE),
                         br(),
                         actionButton("gb_update", "Reset Table", class = "btn-primary", width = "100%"),
                         hr(),
                         h4("Appearance & Export Settings"),
                      #   colourpicker::colourInput(
                       #    inputId = "gb_fill",
                        #   label = "Main Bar Colour:",
                         #  value = "#3498DB",
                        #   showColour = "both",
                        #   allowTransparent = FALSE
                        # ),
                         
                         colourpicker::colourInput(
                           inputId = "gb_border",
                           label = "Border Colour:",
                           value = "#2C3E50",
                           showColour = "both",
                           allowTransparent = FALSE
                         ),
                         selectInput(
                           "gb_palette",
                           "Group Colour Palette:",
                           choices = c(
                             "Set 1" = "Set1",
                             "Set 2" = "Set2",
                             "Set 3" = "Set3",
                             "Pastel 1" = "Pastel1",
                             "Pastel 2" = "Pastel2",
                             "Dark 2" = "Dark2",
                             "Paired" = "Paired",
                             "Accent" = "Accent",
                             "Tableau 10" = "tableau10",
                             "Viridis" = "viridis",
                             "Magma" = "magma",
                             "Plasma" = "plasma",
                             "Inferno" = "inferno",
                             "Rainbow" = "rainbow"
                           ),
                           selected = "Set2"
                         ),
                         
                          sliderInput("gb_alpha", "Transparency:", min = 0.1, max = 1, value = 0.9, step = 0.1),
                         sliderInput("gb_font", "Base Font Size:", min = 8, max = 24, value = 12, step = 1),
                         sliderInput("gb_title_size", "Title Font Size:", min = 10, max = 32, value = 16, step = 1),
                         selectInput("gb_theme", "Chart Theme:", choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "BW" = "bw", "Gray" = "gray", "Void" = "void"), selected = "minimal"),
                         checkboxInput("gb_grid", "Show Grid Lines", value = TRUE),
                         selectInput("gb_legend", "Legend Position:", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "None" = "none"), selected = "right"),
                         numericInput("gb_out_width", "Export Width (inches):", value = 8, min = 3, max = 20, step = 1),
                         numericInput("gb_out_height", "Export Height (inches):", value = 6, min = 3, max = 20, step = 1),
                         selectInput("gb_dpi", "Export Resolution:", choices = c("96 dpi" = 96, "150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300),
                         hr(),
                         downloadButton("gb_download", "Export PNG", class = "btn-success", style = "width:100%;")
                       ),
                       mainPanel(
                         DTOutput("gb_table"),
                         br(),
                         hr(),
                         
                          plotOutput("gb_plot", height = "480px")
                         
                       )
                     )
                       )
                     )
            ),
            
            # 3. Stacked Bar Chart
            # Stacked Bar Chart
            tabPanel("Stacked/Component Bar Chart",
                     titlePanel("Stacked/Component bar chart"),
                     tabsetPanel(
                       tabPanel("Enter Data",
                     sidebarLayout(
                       sidebarPanel(
                         # h4("Data Dimensions"),
                         numericInput("st_r", "Categories in Var 1 (Rows):", value = 3, min = 1, max = 15),
                         numericInput("st_c", "Categories in Var 2 (Cols):", value = 2, min = 1, max = 15),
                         br(),
                         textInput("st_title", "Chart Title:", value = "Stacked Bar Chart"),
                         textInput("st_xlab", "X-axis Label:", value = "Variable 1"),
                         textInput("st_ylab", "Y-axis Label:", value = "Frequency"),
                         checkboxInput("st_show_labels", "Show Data Labels", value = TRUE),
                         
                         # Options: Swap Variables & Y-axis Value Type
                         checkboxInput("st_swap_vars", "Swap Row & Column Variables on Graph", value = FALSE),
                         radioButtons(
                           "st_val_type", 
                           "Plot Value Type:", 
                           choices = c("Frequency" = "frequency", "Percentage (100% Stacked)" = "percentage"), 
                           selected = "frequency",
                           inline = TRUE
                         ),
                         hr(),
                         br(),
                         actionButton("st_update", "Reset Table", class = "btn-primary", width = "100%"),
                         hr(),
                         h4("Appearance & Export Settings"),
                        # textInput("st_fill", "Main Colour (hex):", value = "#3498DB"),
                         selectInput(
                           "st_palette",
                           "Group Colour Palette:",
                           choices = c(
                             "Set 1" = "Set1",
                             "Set 2" = "Set2",
                             "Set 3" = "Set3",
                             "Pastel 1" = "Pastel1",
                             "Pastel 2" = "Pastel2",
                             "Dark 2" = "Dark2",
                             "Paired" = "Paired",
                             "Accent" = "Accent",
                             "Tableau 10" = "tableau10",
                             "Viridis" = "viridis",
                             "Magma" = "magma",
                             "Plasma" = "plasma",
                             "Inferno" = "inferno",
                             "Rainbow" = "rainbow"
                           ),
                           selected = "Set2"
                         ),
                        colourpicker::colourInput(
                          inputId = "st_border",
                          label = "Border Colour:",
                          value = "#2C3E50",
                          showColour = "both",
                          allowTransparent = FALSE
                        ),
                        # textInput("st_border", "Border Colour (hex):", value = "#2C3E50"),
                         sliderInput("st_alpha", "Transparency:", min = 0.1, max = 1, value = 0.9, step = 0.1),
                         sliderInput("st_font", "Base Font Size:", min = 8, max = 24, value = 12, step = 1),
                         sliderInput("st_title_size", "Title Font Size:", min = 10, max = 32, value = 16, step = 1),
                         selectInput("st_theme", "Chart Theme:", choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "BW" = "bw", "Gray" = "gray", "Void" = "void"), selected = "minimal"),
                         checkboxInput("st_grid", "Show Grid Lines", value = TRUE),
                         selectInput("st_legend", "Legend Position:", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "None" = "none"), selected = "right"),
                         numericInput("st_out_width", "Export Width (inches):", value = 8, min = 3, max = 20, step = 1),
                         numericInput("st_out_height", "Export Height (inches):", value = 6, min = 3, max = 20, step = 1),
                         selectInput("st_dpi", "Export Resolution:", choices = c("96 dpi" = 96, "150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300),
                         hr(),
                         downloadButton("st_download", "Export PNG", class = "btn-success", style = "width:100%;")
                       ),
                       mainPanel(
                         DTOutput("st_table"),
                         br(),
                         br(),
                         plotOutput("st_plot", height = "480px")
                       )
                     )
                       )
                     )
            ),
            
            tabPanel("Pie Chart",
                     titlePanel("Pie chart"),
                     tabsetPanel(
                       tabPanel("Enter Data",
                     sidebarLayout(
                       sidebarPanel(
                         # h4("Chart Customization"),
                         #h4("Data Controls"),
                         numericInput("pie_num_cats", "Number of Categories:", value = 4, min = 1, max = 15),
                         br(),
                         textInput("pie_title", "Chart Title:", value = "Pie Chart Distribution"),
                         checkboxInput("pie_show_labels", "Show Slice Labels", value = TRUE),
                         br(),
                         hr(),
                         
                         actionButton("pie_update", "Reset Table", class = "btn-primary", width = "100%"),
                         hr(),
                         h4("Appearance & Export Settings"),
                         selectInput(
                           "pie_palette",
                           "Group Colour Palette:",
                           choices = c(
                             "Set 1" = "Set1",
                             "Set 2" = "Set2",
                             "Set 3" = "Set3",
                             "Pastel 1" = "Pastel1",
                             "Pastel 2" = "Pastel2",
                             "Dark 2" = "Dark2",
                             "Paired" = "Paired",
                             "Accent" = "Accent",
                             "Tableau 10" = "tableau10",
                             "Viridis" = "viridis",
                             "Magma" = "magma",
                             "Plasma" = "plasma",
                             "Inferno" = "inferno",
                             "Rainbow" = "rainbow"
                           ),
                           selected = "Set2"
                         ),
                         colourpicker::colourInput(
                           inputId = "pie_border",
                           label = "Border Colour:",
                           value = "#2C3E50",
                           showColour = "both",
                           allowTransparent = FALSE
                         ),
                         
                         # textInput("pie_fill", "Main Colour (hex):", value = "#3498DB"),
                         #textInput("pie_border", "Border Colour (hex):", value = "#2C3E50"),
                         sliderInput("pie_alpha", "Transparency:", min = 0.1, max = 1, value = 0.9, step = 0.1),
                         sliderInput("pie_font", "Base Font Size:", min = 8, max = 24, value = 12, step = 1),
                         sliderInput("pie_title_size", "Title Font Size:", min = 10, max = 32, value = 16, step = 1),
                         selectInput("pie_theme", "Chart Theme:", choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "BW" = "bw", "Gray" = "gray", "Void" = "void"), selected = "minimal"),
                         checkboxInput("pie_grid", "Show Grid Lines", value = TRUE),
                         selectInput("pie_legend", "Legend Position:", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "None" = "none"), selected = "right"),
                         numericInput("pie_out_width", "Export Width (inches):", value = 8, min = 3, max = 20, step = 1),
                         numericInput("pie_out_height", "Export Height (inches):", value = 6, min = 3, max = 20, step = 1),
                         selectInput("pie_dpi", "Export Resolution:", choices = c("96 dpi" = 96, "150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300),
                         hr(),
                         downloadButton("pie_download", "Export PNG", class = "btn-success", style = "width:100%;")
                       ),
                       mainPanel(
                         DTOutput("pie_table"),
                         br(),br(),
                          plotOutput("pie_plot", height = "480px"),
                       ) 
                       )
                       )
                     )
            ),
            
            # 5. Line Chart
            
            tabPanel(
              "Line Chart",
              titlePanel("Line Chart"),
              tabsetPanel(
                tabPanel("Enter data",
              sidebarLayout(
                sidebarPanel(
                  numericInput(
                    "line_num_pts", "Number of Categories:",
                    value = 5, min = 2, max = 30
                  ),
                  
                  numericInput(
                    "line_num_lines", "Maximum Number of Lines:",
                    value = 1, min = 1, max = 4, step = 1
                  ),
                  
                  textInput("line_title", "Chart Title:",
                            value = "Line Chart Trend"),
                  textInput("line_xlab", "X-axis Label:",
                            value = "Time Point"),
                  textInput("line_ylab", "Y-axis Label:",
                            value = "Value"),
                  
                  checkboxInput(
                    "line_show_labels", "Show Data Labels", TRUE
                  ),
                  
                  selectInput(
                    "line_measure",
                    "Statistic to Plot:",
                    choices = c(
                      "Frequency" = "Frequency",
                      "Percentage (%)" = "Percentage",
                      "Cumulative Frequency" = "Cum_Frequency",
                      "Cumulative Percentage (%)" = "Cum_Percentage"
                    ),
                    selected = "Frequency"
                  ),
                  
                  actionButton(
                    "line_update", "Reset Table",
                    class = "btn-primary", width = "100%"
                  ),
                  
                  hr(),
                  h4("Line and Point Settings"),
                  
                  
                  
                  selectInput(
                    "line_palette",
                    "Line Colour Palette:",
                    choices = c(
                      "Set 1" = "Set1",
                      "Set 2" = "Set2",
                      "Set 3" = "Set3",
                      "Dark 2" = "Dark2",
                      "Pastel 1" = "Pastel1",
                      "Pastel 2" = "Pastel2",
                      "Paired" = "Paired",
                      "Accent" = "Accent",
                      "Viridis" = "viridis",
                      "Magma" = "magma",
                      "Plasma" = "plasma"
                    ),
                    selected = "Set1"
                  ),
                  
                  sliderInput(
                    "line_width", "Line Width:",
                    min = 0.5, max = 4, value = 1.2, step = 0.1
                  ),
                  
                  selectInput(
                    "line_type", "Line Type:",
                    choices = c(
                      "Solid" = "solid",
                      "Dashed" = "dashed",
                      "Dotted" = "dotted",
                      "Dotdash" = "dotdash",
                      "Longdash" = "longdash",
                      "Twodash" = "twodash"
                    ),
                    selected = "solid"
                  ),
                  
                  selectInput(
                    "line_shape", "Point Shape:",
                    choices = c(
                      "Circle" = 16,
                      "Square" = 15,
                      "Triangle" = 17,
                      "Diamond" = 18,
                      "Plus" = 3,
                      "Cross" = 4,
                      "Filled Circle" = 19,
                      "Filled Square" = 22,
                      "Filled Diamond" = 23
                    ),
                    selected = 16
                  ),
                  
                  sliderInput(
                    "line_point_size", "Point Size:",
                    min = 1, max = 8, value = 3, step = 0.5
                  ),
                  
                  sliderInput(
                    "line_alpha", "Transparency:",
                    min = 0.1, max = 1, value = 0.9, step = 0.1
                  ),
                  
                  hr(),
                  h4("Appearance & Export Settings"),
                  
                  sliderInput(
                    "line_font", "Base Font Size:",
                    min = 8, max = 24, value = 12, step = 1
                  ),
                  
                  sliderInput(
                    "line_title_size", "Title Font Size:",
                    min = 10, max = 32, value = 16, step = 1
                  ),
                  
                  selectInput(
                    "line_theme", "Chart Theme:",
                    choices = c(
                      "Minimal" = "minimal",
                      "Classic" = "classic",
                      "Light" = "light",
                      "BW" = "bw",
                      "Gray" = "gray",
                      "Void" = "void"
                    ),
                    selected = "minimal"
                  ),
                  
                  checkboxInput("line_grid", "Show Grid Lines", TRUE),
                  
                  selectInput(
                    "line_legend", "Legend Position:",
                    choices = c(
                      "Right" = "right",
                      "Left" = "left",
                      "Top" = "top",
                      "Bottom" = "bottom",
                      "None" = "none"
                    ),
                    selected = "right"
                  ),
                  
                  numericInput(
                    "line_out_width", "Export Width (inches):",
                    value = 8, min = 3, max = 20, step = 1
                  ),
                  
                  numericInput(
                    "line_out_height", "Export Height (inches):",
                    value = 6, min = 3, max = 20, step = 1
                  ),
                  
                  selectInput(
                    "line_dpi", "Export Resolution:",
                    choices = c(
                      "96 dpi" = 96,
                      "150 dpi" = 150,
                      "300 dpi" = 300,
                      "600 dpi" = 600
                    ),
                    selected = 300
                  ),
                  
                  downloadButton(
                    "line_download", "Export PNG",
                    class = "btn-success", style = "width:100%;"
                  )
                ),
                
                mainPanel(
                  DTOutput("line_table"),
                  br(), br(),
                  plotOutput("line_plot", height = "480px")
                )
              )
            
              ),
            tabPanel("Upload data",
                     
                      sidebarLayout(
                         sidebarPanel(
                           uiOutput("line_upload_x_selector"),
                           
                           numericInput(
                             "line_upload_num_vars",
                             "Number of Lines / Y Variables:",
                             value = 2, min = 1, max = 10, step = 1
                           ),
                           
                           uiOutput("line_upload_var_selectors"),
                           
                           selectInput(
                             "line_upload_measure",
                             "Statistic to Plot:",
                             choices = c(
                               "Frequency" = "Frequency",
                               "Percentage (%)" = "Percentage",
                               "Cumulative Frequency" = "Cum_Frequency",
                               "Cumulative Percentage (%)" = "Cum_Percentage"
                             ),
                             selected = "Frequency"
                           ),
                           
                           textInput(
                             "line_upload_title",
                             "Chart Title:",
                             value = "Frequency by Year"
                           ),
                           
                           textInput(
                             "line_upload_xlab",
                             "X-axis Label:",
                             value = "Year"
                           ),
                           
                           textInput(
                             "line_upload_ylab",
                             "Y-axis Label:",
                             value = "Frequency"
                           ),
                           
                           selectInput(
                             "line_upload_palette",
                             "Line Colour Palette:",
                             choices = c(
                               "Set 1" = "Set1",
                               "Set 2" = "Set2",
                               "Set 3" = "Set3",
                               "Dark 2" = "Dark2",
                               "Paired" = "Paired",
                               "Viridis" = "viridis",
                               "Magma" = "magma",
                               "Plasma" = "plasma"
                             ),
                             selected = "Set1"
                           ),
                           
                           sliderInput(
                             "line_upload_width", "Line Width:",
                             min = 0.5, max = 4, value = 1.2, step = 0.1
                           ),
                           
                           selectInput(
                             "line_upload_shape", "Point Shape:",
                             choices = c(
                               "Circle" = 16,
                               "Square" = 15,
                               "Triangle" = 17,
                               "Diamond" = 18
                             ),
                             selected = 16
                           ),
                           
                           sliderInput(
                             "line_upload_point_size", "Point Size:",
                             min = 1, max = 8, value = 3, step = 0.5
                           ),
                           
                           checkboxInput(
                             "line_upload_labels", "Show Data Labels", FALSE
                           ),
                           
                           selectInput(
                             "line_upload_legend", "Legend Position:",
                             choices = c(
                               "Right" = "right",
                               "Left" = "left",
                               "Top" = "top",
                               "Bottom" = "bottom",
                               "None" = "none"
                             ),
                             selected = "right"
                           ),
                           
                           checkboxInput(
                             "line_upload_grid", "Show Grid Lines", TRUE
                           ),
                           
                           numericInput(
                             "line_upload_out_width", "Export Width (inches):",
                             value = 8, min = 3, max = 20
                           ),
                           
                           numericInput(
                             "line_upload_out_height", "Export Height (inches):",
                             value = 6, min = 3, max = 20
                           ),
                           
                           selectInput(
                             "line_upload_dpi", "Export Resolution:",
                             choices = c(
                               "96 dpi" = 96,
                               "150 dpi" = 150,
                               "300 dpi" = 300,
                               "600 dpi" = 600
                             ),
                             selected = 300
                           ),
                           
                           downloadButton(
                             "line_upload_download",
                             "Export PNG",
                             class = "btn-success",
                             style = "width:100%;"
                           )
                         ),
                         
                         mainPanel(
                           DT::DTOutput("line_upload_table"),
                           br(),
                           plotOutput("line_upload_plot", height = "480px")
                         )
                       )
                   )
              ) 
            
            ),
            # 6. Scatter Diagram
            
            tabPanel(
              "Scatter Diagram",
              titlePanel("Scatter Diagram"),
              tabsetPanel(
                tabPanel("Enter data",
              sidebarLayout(
                sidebarPanel(
                  numericInput(
                    "scat_num_obs", "Number of Observations:",
                    value = 6, min = 2, max = 100
                  ),
                  
                  textInput("scat_title", "Chart Title:",
                            value = "Scatter Diagram"),
                  textInput("scat_xlab", "X-axis Label:",
                            value = "X Variable"),
                  textInput("scat_ylab", "Y-axis Label:",
                            value = "Y Variable"),
                  
                  checkboxInput(
                    "scat_show_labels",
                    "Show Point Coordinates",
                    value = TRUE
                  ),
                  
                  hr(),
                  h4("Point Settings"),
                  
                  selectInput(
                    "scat_point_colour",
                    "Point Colour:",
                    choices = c(
                      "Blue" = "#3498DB",
                      "Red" = "#E74C3C",
                      "Green" = "#2ECC71",
                      "Purple" = "#9B59B6",
                      "Orange" = "#E67E22",
                      "Black" = "#2C3E50",
                      "Dark Grey" = "#7F8C8D"
                    ),
                    selected = "#E74C3C"
                  ),
                  
                  selectInput(
                    "scat_point_shape",
                    "Point Shape:",
                    choices = c(
                      "Circle" = 16,
                      "Square" = 15,
                      "Triangle" = 17,
                      "Diamond" = 18,
                      "Plus" = 3,
                      "Cross" = 4,
                      "Filled Circle" = 19,
                      "Filled Square" = 22
                    ),
                    selected = 19
                  ),
                  
                  sliderInput(
                    "scat_point_size", "Point Size:",
                    min = 1, max = 8, value = 3.5, step = 0.5
                  ),
                  
                  sliderInput(
                    "scat_alpha", "Point Transparency:",
                    min = 0.1, max = 1, value = 0.9, step = 0.1
                  ),
                  
                  hr(),
                  h4("Trend Line Settings"),
                  
                  checkboxInput(
                    "scat_show_line",
                    "Show Trend Line",
                    value = TRUE
                  ),
                  
                  selectInput(
                    "scat_line_colour",
                    "Trend Line Colour:",
                    choices = c(
                      "Blue" = "#3498DB",
                      "Red" = "#E74C3C",
                      "Green" = "#2ECC71",
                      "Purple" = "#9B59B6",
                      "Orange" = "#E67E22",
                      "Black" = "#2C3E50"
                    ),
                    selected = "#3498DB"
                  ),
                  
                  selectInput(
                    "scat_line_type",
                    "Trend Line Type:",
                    choices = c(
                      "Solid" = "solid",
                      "Dashed" = "dashed",
                      "Dotted" = "dotted",
                      "Dotdash" = "dotdash",
                      "Longdash" = "longdash",
                      "Twodash" = "twodash"
                    ),
                    selected = "solid"
                  ),
                  
                  sliderInput(
                    "scat_line_width", "Trend Line Width:",
                    min = 0.5, max = 4, value = 1, step = 0.1
                  ),
                  
                  selectInput(
                    "scat_line_method",
                    "Trend Line Method:",
                    choices = c(
                      "Linear Regression" = "lm",
                      "LOESS Smoothing" = "loess"
                    ),
                    selected = "lm"
                  ),
                  
                  hr(),
                  actionButton(
                    "scat_update", "Reset Table",
                    class = "btn-primary", width = "100%"
                  ),
                  
                  hr(),
                  h4("Appearance & Export Settings"),
                  
                  sliderInput(
                    "scat_font", "Base Font Size:",
                    min = 8, max = 24, value = 12, step = 1
                  ),
                  
                  sliderInput(
                    "scat_title_size", "Title Font Size:",
                    min = 10, max = 32, value = 16, step = 1
                  ),
                  
                  selectInput(
                    "scat_theme", "Chart Theme:",
                    choices = c(
                      "Minimal" = "minimal",
                      "Classic" = "classic",
                      "Light" = "light",
                      "BW" = "bw",
                      "Gray" = "gray",
                      "Void" = "void"
                    ),
                    selected = "minimal"
                  ),
                  
                  checkboxInput(
                    "scat_grid", "Show Grid Lines",
                    value = TRUE
                  ),
                  
                  numericInput(
                    "scat_out_width", "Export Width (inches):",
                    value = 8, min = 3, max = 20, step = 1
                  ),
                  
                  numericInput(
                    "scat_out_height", "Export Height (inches):",
                    value = 6, min = 3, max = 20, step = 1
                  ),
                  
                  selectInput(
                    "scat_dpi", "Export Resolution:",
                    choices = c(
                      "96 dpi" = 96,
                      "150 dpi" = 150,
                      "300 dpi" = 300,
                      "600 dpi" = 600
                    ),
                    selected = 300
                  ),
                  
                  downloadButton(
                    "scat_download", "Export PNG",
                    class = "btn-success",
                    style = "width:100%;"
                  )
                ),
                
                mainPanel(
                  DTOutput("scat_table"),
                  br(), br(),
                  plotOutput("scat_plot", height = "480px")
                )
              )
            
              ), 
              tabPanel(
                "Upload Data",
                
                sidebarLayout(
                  sidebarPanel(
                    h4("Select Variables"),
                    
                    uiOutput("scat_upload_x_ui"),
                    uiOutput("scat_upload_y_ui"),
                    
                    checkboxInput(
                      "scat_upload_show_line",
                      "Show Regression / Trend Line",
                      value = TRUE
                    ),
                    
                    selectInput(
                      "scat_upload_line_method",
                      "Line Method",
                      choices = c(
                        "Linear Regression" = "lm",
                        "LOESS Smoothing" = "loess"
                      ),
                      selected = "lm"
                    ),
                    
                    selectInput(
                      "scat_upload_point_colour",
                      "Point Colour",
                      choices = c(
                        "Blue" = "blue",
                        "Red" = "red",
                        "Green" = "darkgreen",
                        "Purple" = "purple",
                        "Orange" = "orange",
                        "Black" = "black"
                      ),
                      selected = "blue"
                    ),
                    
                    selectInput(
                      "scat_upload_point_shape",
                      "Point Shape",
                      choices = c(
                        "Circle" = "16",
                        "Square" = "15",
                        "Triangle" = "17",
                        "Diamond" = "18",
                        "Plus" = "3",
                        "Cross" = "4"
                      ),
                      selected = "16"
                    ),
                    
                    sliderInput(
                      "scat_upload_point_size",
                      "Point Size",
                      min = 1,
                      max = 8,
                      value = 3,
                      step = 0.5
                    ),
                    
                    sliderInput(
                      "scat_upload_alpha",
                      "Point Transparency",
                      min = 0.1,
                      max = 1,
                      value = 0.8,
                      step = 0.1
                    ),
                    
                    conditionalPanel(
                      condition = "input.scat_upload_show_line == true",
                      
                      selectInput(
                        "scat_upload_line_colour",
                        "Line Colour",
                        choices = c(
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "darkgreen",
                          "Black" = "black",
                          "Purple" = "purple",
                          "Orange" = "orange"
                        ),
                        selected = "red"
                      ),
                      
                      selectInput(
                        "scat_upload_line_type",
                        "Line Type",
                        choices = c(
                          "Solid" = "solid",
                          "Dashed" = "dashed",
                          "Dotted" = "dotted",
                          "Dotdash" = "dotdash",
                          "Longdash" = "longdash"
                        ),
                        selected = "solid"
                      ),
                      
                      sliderInput(
                        "scat_upload_line_width",
                        "Line Width",
                        min = 0.5,
                        max = 3,
                        value = 1,
                        step = 0.5
                      )
                    ),
                    
                    textInput(
                      "scat_upload_title",
                      "Graph Title",
                      "Scatter Diagram"
                    ),
                    
                    textInput(
                      "scat_upload_xlab",
                      "X-axis Title",
                      "X"
                    ),
                    
                    textInput(
                      "scat_upload_ylab",
                      "Y-axis Title",
                      "Y"
                    ),
                    
                    checkboxInput(
                      "scat_upload_show_labels",
                      "Show Coordinate Labels",
                      value = FALSE
                    ),
                    
                    checkboxInput(
                      "scat_upload_grid",
                      "Show Grid",
                      value = TRUE
                    ),
                    
                    selectInput(
                      "scat_upload_theme",
                      "Graph Theme",
                      choices = c(
                        "Minimal" = "minimal",
                        "Classic" = "classic",
                        "Light" = "light",
                        "Black and White" = "bw",
                        "Gray" = "gray"
                      ),
                      selected = "minimal"
                    ),
                    
                    numericInput(
                      "scat_upload_out_width",
                      "Download Width (inches)",
                      value = 8,
                      min = 3,
                      max = 20
                    ),
                    
                    numericInput(
                      "scat_upload_out_height",
                      "Download Height (inches)",
                      value = 6,
                      min = 3,
                      max = 20
                    ),
                    
                    numericInput(
                      "scat_upload_dpi",
                      "Download Resolution (DPI)",
                      value = 300,
                      min = 72,
                      max = 600
                    ),
                    
                    downloadButton(
                      "scat_upload_download",
                      "Download Scatter Diagram"
                    )
                  ),
                  
                  mainPanel(
                    h4("Selected Data Table"),
                    DT::DTOutput("scat_upload_table"),
                    
                    fluidRow(
                      column(
                        6,
                        actionButton(
                          "scat_upload_add_row",
                          "Add Observation"
                        )
                      ),
                      column(
                        6,
                        actionButton(
                          "scat_upload_delete_row",
                          "Delete Last Observation"
                        )
                      )
                    ),
                    
                    tags$hr(),
                    
                    h4("Scatter Diagram"),
                    plotOutput(
                      "scat_upload_plot",
                      height = "500px"
                    )
                  )
                )
              )
              )
            ),
            # Circular Column Chart
            tabPanel("Circular Column Chart",
                     titlePanel("Circular Column Chart"),
                    tabsetPanel(
                      tabPanel("Enter data",
                      sidebarLayout(
                       sidebarPanel(
                         numericInput("circ_num_cats", "Number of Categories:", value = 8, min = 3, max = 24),
                         br(),
                         textInput("circ_title", "Chart Title:", value = "Circular Column Chart"),
                         textInput("circ_xlab", "Category Label:", value = "Category"),
                         textInput("circ_ylab", "Value Label:", value = "Frequency"),
                         checkboxInput("circ_show_labels", "Show Data Labels", value = TRUE),
                         hr(),
                         
                         h4("Circular Settings"),
                         sliderInput("circ_hole", "Inner Circle Hole Size:", min = 0, max = 50, value = 10, step = 5),
                         radioButtons("circ_val_type", "Plot Value:", choices = c("Frequency", "Percentage"), inline = TRUE),
                         hr(),
                         
                         actionButton("circ_update", "Reset Table", class = "btn-primary", width = "100%"),
                         hr(),
                         h4("Appearance & Export Settings"),
                         selectInput(
                           "circ_palette",
                           "Column Colour Palette:",
                           choices = c(
                             "Spectral" = "Spectral",
                             "Set1" = "Set1",
                             "Set2" = "Set2",
                             "Set3" = "Set3",
                             "Paired" = "Paired",
                             "Dark2" = "Dark2",
                             "Pastel1" = "Pastel1",
                             "Pastel2" = "Pastel2",
                             "Accent" = "Accent"
                           ),
                           selected = "Spectral"
                         ),
                         
                         colourpicker::colourInput(
                           inputId = "circ_border",
                           label = "Border Colour:",
                           value = "#2C3E50",
                           allowTransparent = FALSE
                         ),
                         
                         sliderInput("circ_alpha", "Transparency:", min = 0.1, max = 1, value = 0.9, step = 0.1),
                         sliderInput("circ_font", "Base Font Size:", min = 8, max = 24, value = 12, step = 1),
                         sliderInput("circ_title_size", "Title Font Size:", min = 10, max = 32, value = 16, step = 1),
                         selectInput("circ_theme", "Chart Theme:", choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "BW" = "bw", "Gray" = "gray", "Void" = "void"), selected = "minimal"),
                         checkboxInput("circ_grid", "Show Grid Lines", value = TRUE),
                         selectInput("circ_legend", "Legend Position:", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "None" = "none"), selected = "right"),
                         numericInput("circ_out_width", "Export Width (inches):", value = 8, min = 3, max = 20, step = 1),
                         numericInput("circ_out_height", "Export Height (inches):", value = 6, min = 3, max = 20, step = 1),
                         selectInput("circ_dpi", "Export Resolution:", choices = c("96 dpi" = 96, "150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300),
                         hr(),
                         downloadButton("circ_download", "Export PNG", class = "btn-success", style = "width:100%;")
                       ),
                       mainPanel(
                          DTOutput("circ_table"),
                         br(),br(),
                        plotOutput("circ_plot", height = "520px")
                         
                         
                       )
                     )
            ),
            
            tabPanel(
              "Upload data",
              
             
              sidebarLayout(
                sidebarPanel(
                  h4("Select Variables"),
                  
                  uiOutput("circ_upload_category_ui"),
                  
                  radioButtons(
                    "circ_upload_value_type",
                    "Plot Value",
                    choices = c(
                      "Frequency" = "Frequency",
                      "Percentage" = "Percentage"
                    ),
                    selected = "Frequency",
                    inline = TRUE
                  ),
                  
                  hr(),
                  
                  h4("Chart Settings"),
                  
                  textInput(
                    "circ_upload_title",
                    "Chart Title",
                    "Circular Column Chart"
                  ),
                  
                  textInput(
                    "circ_upload_xlab",
                    "Category Label",
                    "Category"
                  ),
                  
                  textInput(
                    "circ_upload_ylab",
                    "Value Label",
                    "Frequency"
                  ),
                  
                  checkboxInput(
                    "circ_upload_show_labels",
                    "Show Data Labels",
                    TRUE
                  ),
                  
                  sliderInput(
                    "circ_upload_hole",
                    "Inner Circle Hole Size (%)",
                    min = 0,
                    max = 50,
                    value = 0,
                    step = 5
                  ),
                  
                  hr(),
                  
                  h4("Appearance"),
                  
                  selectInput(
                    "circ_upload_palette",
                    "Column Colour Palette",
                    choices = c(
                      "Spectral" = "Spectral",
                      "Set1" = "Set1",
                      "Set2" = "Set2",
                      "Set3" = "Set3",
                      "Paired" = "Paired",
                      "Dark2" = "Dark2",
                      "Pastel1" = "Pastel1",
                      "Pastel2" = "Pastel2",
                      "Accent" = "Accent"
                    ),
                    selected = "Spectral"
                  ),
                  
                  colourpicker::colourInput(
                    "circ_upload_border",
                    "Border Colour",
                    value = "#2C3E50"
                  ),
                  
                  sliderInput(
                    "circ_upload_alpha",
                    "Transparency",
                    min = 0.1,
                    max = 1,
                    value = 0.9,
                    step = 0.1
                  ),
                  
                  sliderInput(
                    "circ_upload_font",
                    "Base Font Size",
                    min = 8,
                    max = 24,
                    value = 12
                  ),
                  
                  sliderInput(
                    "circ_upload_title_size",
                    "Title Font Size",
                    min = 10,
                    max = 32,
                    value = 16
                  ),
                  
                  selectInput(
                    "circ_upload_theme",
                    "Chart Theme",
                    choices = c(
                      "Minimal" = "minimal",
                      "Classic" = "classic",
                      "Light" = "light",
                      "BW" = "bw",
                      "Gray" = "gray",
                      "Void" = "void"
                    ),
                    selected = "minimal"
                  ),
                  
                  selectInput(
                    "circ_upload_legend",
                    "Legend Position",
                    choices = c(
                      "Right" = "right",
                      "Left" = "left",
                      "Top" = "top",
                      "Bottom" = "bottom",
                      "None" = "none"
                    ),
                    selected = "right"
                  ),
                  
                  hr(),
                  
                  h4("Export Settings"),
                  
                  numericInput(
                    "circ_upload_out_width",
                    "Export Width (inches)",
                    value = 8,
                    min = 3,
                    max = 20
                  ),
                  
                  numericInput(
                    "circ_upload_out_height",
                    "Export Height (inches)",
                    value = 8,
                    min = 3,
                    max = 20
                  ),
                  
                  selectInput(
                    "circ_upload_dpi",
                    "Export Resolution",
                    choices = c(
                      "96 dpi" = 96,
                      "150 dpi" = 150,
                      "300 dpi" = 300,
                      "600 dpi" = 600
                    ),
                    selected = 300
                  ),
                  
                  downloadButton(
                    "circ_upload_download",
                    "Export PNG",
                    class = "btn-success",
                    style = "width:100%;"
                  )
                ),
                
                mainPanel(
                  h4("Frequency Table"),
                  DT::DTOutput("circ_upload_table"),
                  
                  br(),
                  
                  h4("Circular Column Chart"),
                  plotOutput(
                    "circ_upload_plot",
                    height = "520px"
                  )
                )
              )
            )
            
            
                    )       
            )
 ),
 
 
 navbarMenu("Correlation & Regression",
   tabPanel("Pearson correlation",
            titlePanel("Pearson's Simple & Multiple Correlation"),
            tabsetPanel(
              tabPanel("Upload data",
            sidebarLayout(
              sidebarPanel(
              
                selectInput("dataset_choice", "Select Analysis Type:",
                            choices = c("Simple Correlation" = "simple", 
                                        "Multiple Correlation Matrix" = "multiple")),
                
                # Conditional inputs based on choice
                conditionalPanel(
                  condition = "input.dataset_choice == 'simple'",
                  selectInput("var_xcor", "Select X Variable (Independent):", choices = NULL),
                  selectInput("var_ycor", "Select Y Variable (Dependent):", choices = NULL)
                ),
                
                conditionalPanel(
                  condition = "input.dataset_choice == 'multiple'",
                  selectInput("dep_var", "Select Dependent Variable (Y):", choices = NULL),
                  checkboxGroupInput("ind_vars", "Select Independent Variables (X1, X2...):", choices = NULL)
                ),
                hr(),
                # Download Word Report Button
                downloadButton("download_word", "Download Full Report (.docx)", class = "btn-primary")
              
              ),
              
              mainPanel(
                         h4("Statistical Results"),
                         tableOutput("summary_table"), # Replaced verbatimTextOutput with tableOutput
                         hr(),
                         br(),
                         plotOutput("main_plot"),
                         br(),br()
                )
              )
              ),
            tabPanel("Enter data",
                     sidebarLayout(
                       sidebarPanel(
                         # Controls for grid dimensions
                         numericInput("cnum_vars", "Number of Variables (Columns):", value = 4, min = 2, max = 10),
                         numericInput("cnum_obs", "Number of Observations (Rows):", value = 12, min = 3, max = 50),
                         actionButton("update_table", "Reset Table"),
                         hr(),
                         
                         selectInput("dataset_choice", "Select Analysis Type:",
                                     choices = list("Simple Correlation" = "simple", 
                                                    "Multiple Correlation Matrix" = "multiple")),
                         hr(),
                         
                         # Conditional inputs for Simple Correlation: Correlation Between Variables
                         conditionalPanel(
                           condition = "input.dataset_choice == 'simple'",
                         #  h4("Correlation Between Variables"),
                           selectInput("var_xcor1", "Select First Variable (X):", choices = NULL),
                           selectInput("var_ycor1", "Select Second Variable (Y):", choices = NULL)
                         ),
                         
                         # Conditional inputs for Multiple Correlation: Correlation Among Variables
                         conditionalPanel(
                           condition = "input.dataset_choice == 'multiple'",
                          # h4("Correlation Among Variables"),
                           selectInput("dep_var1", "Select Target Variable (Y):", choices = NULL),
                           checkboxGroupInput("ind_vars1", "Select Variables for Matrix & Multiple Correlation:", choices = NULL)
                         ),
                         
                         hr(),
                         # Download Word Report Button
                         downloadButton("download_word", "Download Full Report (.docx)", class = "btn-primary")
                       ),
                       
                       mainPanel(
                                    p("Click directly inside any cell to edit values or change column headers."),
                                    rHandsontableOutput("editable_table"),
                                    br(),
                                    h4("Statistical Results"),
                                    tableOutput("csummary_table"),
                                    hr(),
                                    br(),
                                    plotOutput("cmain_plot"),
                                    br()
                                    ),
                                                 
                       
                     )
            )
            )
   ),
  # tabPanel("Spearman's/Kendal correlation",
            
  # ),          
  # tabPanel("Biserial correlation",
            
  # ), 
  # tabPanel("Phi-coefficient correlation",
            
  # ), 
   #tabPanel("Linear regression",
            
  # ), 
   tabPanel(" Binary Logistic regression",
    tabsetPanel(
      tabPanel("Upload data",
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   selectInput(
                     "model_choice",
                     "Select Logistic Regression Type:",
                     choices = list(
                       "Simple Binary Logistic Regression" = "simple",
                       "Multiple Logistic Regression" = "multiple"
                     )
                   ),
                   
                   hr(),
                   
                   # ---------------- SIMPLE MODEL ----------------
                   conditionalPanel(
                     
                     condition = "input.model_choice == 'simple'",
                     
                     selectInput(
                       "bin_y_simple",
                       "Select Binary Outcome Variable (Y - 0/1):",
                       choices = NULL
                     ),
                     
                     selectInput(
                       "bin_x_simple",
                       "Select Predictor Variable (X):",
                       choices = NULL
                     ),
                     
                     uiOutput("ref_cat_simple_ui")
                   ),
                   
                   # ---------------- MULTIPLE MODEL ----------------
                   conditionalPanel(
                     
                     condition = "input.model_choice == 'multiple'",
                     
                     selectInput(
                       "bin_y_multi",
                       "Select Binary Outcome Variable (Y - 0/1):",
                       choices = NULL
                     ),
                     
                     checkboxGroupInput(
                       "bin_x_multi",
                       "Select Predictor Variables:",
                       choices = NULL
                     ),
                     
                     uiOutput("ref_cat_multi_ui")
                   ),
                   
                   hr(),
                   
                   downloadButton(
                     "download_word",
                     "Download Full Report (.docx)",
                     class = "btn-primary"
                   )
                 ),
                 
                 mainPanel(
                       
                       h4(
                         "Detailed Coefficients, Odds Ratios & Confidence Intervals"
                       ),
                       
                       tableOutput("model_summary_table"),
                       
                       hr(),
                       
                       h4(
                         "Classification Performance (AUC, Sensitivity, Specificity)"
                       ),
                       
                       tableOutput("class_perf_table"),
                       
                       hr(),
                       
                       h4("Confusion Matrix"),
                       
        
        
                       verbatimTextOutput("conf_matrix_text"),
                       
                       hr(),
                       
                       h4("ROC Curve"),
                       
                       plotOutput(
                         "roc_plot",
                         height = "500px"
                       ),
                     
                       
                       h4("Deviance & AIC Performance Metrics"),
                       
                       tableOutput("perf_table")
                    
                                 )
               )
      ),
      
      tabPanel("Enter data",
        
        
      ),
      
      
    )        
            
            
   ),
 ),      
            
 navbarMenu("Sample Size",
            tabPanel(title= span("Slovin's formula", title =" For known population size"), 
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         style = "height: 90vh; overflow-y: auto;",
                         h3("Population known"),
                         numericInput("N", "Population size:", value =1, min = 1),
                         numericInput("d2", "Margin of error (%):", value =1, min = 0.01),
                         numericInput("f_2", "Nonresponse/Attrition rate (%):", value =0),
                         textAreaInput("Ref2", "Reference/Note:", placeholder = "Enter reference or note here...", rows=3), 
                         actionButton("calc2", "Calculate"),
                         actionButton("save_screenshotd", "save"),
                         actionButton("next_tab", "Help"),
                         
                       ),  
                       
                       mainPanel(
                         id ="main2",
                         style = "font-size:14px", width = 6,
                         h3("Sample size"),
                         p("The sample size when population size is known:"),
                         tags$head(
                           tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                         ),
                         div(class = "watermark", "MVSSC"),
                         
                         p("$$n = \\frac{N} {1 + N d^2}$$"),
                         
                         textOutput("wr"),
                         textOutput("N"),
                         textOutput("d2"),
                         
                         verbatimTextOutput("sample_size2"),
                         htmlOutput("Ref2"),
                         br()
                       )
                     )
                     
            ),
            tabPanel(title =span("One Sample Mean Estimation", title ="For Cross sectional study and quantitative outcome to estimate average value "),  
                     tabsetPanel(   
                       tabPanel("With mean & SD", 
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh;overflow-y: auto;",
                                    h4("One Mean estimation"),
                                    radioButtons("Popu", "Population size:", choices = c("UnKnown", "Known"),inline = TRUE),
                                    conditionalPanel(
                                      condition="input.Popu == 'Known'",  
                                      numericInput("N1", "Population Size (N):", value = 1),
                                    ),
                                    selectInput("CI1", "Confidence interval(%):", choices = c("90", "95", "99"), selected="95"),
                                    numericInput("SD", "Estimated SD:", value = 1, min = 0.01),
                                    selectInput("error1", "Type of Margin of error:", choices = c("Absolute_error", "Relative_error(%)"),width = "210px"),
                                    conditionalPanel(
                                      condition="input.error1 == 'Relative_error(%)'",  
                                      numericInput("mu1", " Estimated Mean:", value = 1)
                                    ),
                                    numericInput("d1", "Margin of error:", value =1, min = 0.01),
                                    numericInput("f1", "Nonresponse/Attrition rate(%):", value =0, min=0),
                                    textAreaInput("Ref", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("calc1", "Calculate"),
                                    actionButton("save_screenshot1", "save"),
                                    actionButton("next_tab1", "Help"),
                                  ),  
                                  
                                  mainPanel(
                                    id ="main1", style = "font-size:14px", width = 6,
                                    h3("Sample size"),
                                    p("The sample size formula to estimate the mean:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{Z_{1-\\alpha/2}^2 \\sigma^2} {d^2}$$"),                      
                                    textOutput("wr1"),
                                    textOutput("po_1"),
                                    uiOutput("z1"),
                                    textOutput("SD"),
                                    textOutput("d_1"),
                                    verbatimTextOutput("sample_size1"),
                                    htmlOutput("Ref"),
                                    br()
                                  )
                                )
                       ),
                       
                       tabPanel("with Median & SIQR ",
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("One Mean estimation"),
                                    radioButtons("qrPopu", "Population size:", choices = c("UnKnown", "Known"),inline = TRUE),
                                    conditionalPanel(
                                      condition="input.qrPopu == 'Known'",  
                                      numericInput("qrN1", "Population Size (N):", value = 1),
                                    ),
                                    selectInput("qrCI1", "Confidence interval(%):", choices = c("90", "95", "99"), selected ="95"),
                                    numericInput("Q2", "Median:", value = 1, min = 0.01),
                                    numericInput("Q3", "Semi-Interquartile range:", value = 1, min = 0.01),
                                    selectInput("qrerror1", "Type of Margin of error:", choices = c("Absolute_error", "Relative_error(%)")),
                                    numericInput("qrd1", "Margin of error:", value =1, min = 0.01),
                                    numericInput("qrf1", "Non response/Attrition rate (%):", value =0),
                                    textAreaInput("qrRef", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("qrcalc1", "Calculate"),
                                    actionButton("qrsave_screenshot1", "save"),
                                    actionButton("qrnext_tab1", "Help"),
                                  ),  
                                  
                                  mainPanel(
                                    id ="qrmain1", style = "font-size:14px", width = 6,
                                    h3("Sample size"),
                                    p("The sample size formula to estimate the mean:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{Z_{1-\\alpha/2}^2 \\sigma^2} {d^2}$$"),                      
                                    textOutput("wr2"),
                                    textOutput("qrpo_1"),
                                    uiOutput("qrz1"),
                                    textOutput("qrQ"),
                                    textOutput("qrMean"),
                                    textOutput("qrSD"),
                                    textOutput("qrd_1"),
                                    verbatimTextOutput("qrsample_size1"),
                                    htmlOutput("qrRef"),
                                    br()
                                  )
                                )      
                                
                       ),
                     )
            ),
            
            tabPanel(
              title = span("Two Sample Mean Estimation",
                           title = "For comparing two means (difference between two groups)"),
              
              
              tabsetPanel(
                
                # =========================
                # 📌 WITH MEAN & SD
                # =========================
                tabPanel("With Mean & SD",
                         withMathJax(),
                         sidebarLayout(
                           
                           sidebarPanel(
                             style = "height: 90vh;overflow-y: auto;",
                             h4("Two Mean estimation"),
                             
                             radioButtons("Popu2", "Population size:",
                                          choices = c("UnKnown", "Known"), inline = TRUE),
                             
                             conditionalPanel(
                               condition = "input.Popu2 == 'Known'",
                               numericInput("N2", "Population Size (N):", value = 1)
                             ),
                             
                             selectInput("CI2", "Confidence interval(%):",
                                         choices = c("90", "95", "99"), selected = "95"),
                             
                             numericInput("SD1_2", "Group 1 SD:", value = 1, min = 0.01),
                             numericInput("SD2_2", "Group 2 SD:", value = 1, min = 0.01),
                             
                             selectInput("error2", "Type of Margin of error:",
                                         choices = c("Absolute_error", "Relative_error(%)")),
                             
                             conditionalPanel(
                               condition = "input.error2 == 'Relative_error(%)'",
                               numericInput("mu_diff", "Expected Mean Difference:", value = 1)
                             ),
                             
                             numericInput("d2", "Margin of error:", value = 1, min = 0.01),
                             
                             numericInput("f2", "Nonresponse/Attrition rate(%):", value = 0),
                             
                             textAreaInput("Ref2", "Reference/Note:",
                                           placeholder = "Enter reference or note here...", rows = 3),
                             
                             actionButton("calc2", "Calculate"),
                             actionButton("save_screenshot2", "Save"),
                             actionButton("next_tab2", "Help")
                           ),
                           
                           mainPanel(
                             id = "main2", style = "font-size:14px", width = 6,
                             
                             h3("Sample size"),
                             p("The sample size formula for comparing two means:"),
                             
                             tags$head(
                               tags$style(HTML("
                   .watermark {
                     position: fixed;
                     opacity: 0.08;
                     font-size: 15px;
                     transform: rotate(-60deg);
                     color: #000;
                     pointer-events: none;
                     z-index: 9999;
                     user-select: none;
                   }
                 "))
                             ),
                             
                             div(class = "watermark", "MVSSC"),
                             
                             p("$$n = \\frac{2 Z_{1-\\alpha/2}^2 (\\sigma_1^2 + \\sigma_2^2)}{d^2}$$"),
                             
                             textOutput("wr2_1"),
                             textOutput("po_2"),
                             uiOutput("z2"),
                             textOutput("SD_2"),
                             textOutput("d_2"),
                             
                             verbatimTextOutput("sample_size2"),
                             htmlOutput("Ref2"),
                             br()
                           )
                         )
                ),
                
                
                # =========================
                # 📌 WITH MEDIAN & SIQR
                # =========================
                tabPanel("With Median & SIQR",
                         withMathJax(),
                         sidebarLayout(
                           
                           sidebarPanel(
                             style = "height: 90vh; overflow-y: auto;",
                             h4("Two Mean estimation"),
                             
                             radioButtons("qrPopu2", "Population size:",
                                          choices = c("UnKnown", "Known"), inline = TRUE),
                             
                             conditionalPanel(
                               condition = "input.qrPopu2 == 'Known'",
                               numericInput("qrN2", "Population Size (N):", value = 1)
                             ),
                             
                             selectInput("qrCI2", "Confidence interval(%):",
                                         choices = c("90", "95", "99"), selected = "95"),
                             
                             numericInput("Q2_1", "Group 1 Median:", value = 1, min = 0.01),
                             numericInput("Q2_2", "Group 2 Median:", value = 1, min = 0.01),
                             
                             numericInput("SIQR1", "Group 1 SIQR:", value = 1),
                             numericInput("SIQR2", "Group 2 SIQR:", value = 1),
                             
                             selectInput("qrerror2", "Type of Margin of error:",
                                         choices = c("Absolute_error", "Relative_error(%)")),
                             
                             numericInput("qrd2", "Margin of error:", value = 1),
                             
                             numericInput("qrf2", "Non response/Attrition rate (%):", value = 0),
                             
                             textAreaInput("qrRef2", "Reference/Note:",
                                           placeholder = "Enter reference or note here...", rows = 3),
                             
                             actionButton("qrcalc2", "Calculate"),
                             actionButton("qrsave_screenshot2", "Save"),
                             actionButton("qrnext_tab2", "Help")
                           ),
                           
                           mainPanel(
                             id = "qrmain2", style = "font-size:14px", width = 6,
                             
                             h3("Sample size"),
                             p("The sample size formula for comparing two means:"),
                             
                             div(class = "watermark", "MVSSC"),
                             
                             p("$$n = \\frac{2 Z_{1-\\alpha/2}^2 (\\sigma_1^2 + \\sigma_2^2)}{d^2}$$"),
                             
                             textOutput("wr2_2"),
                             textOutput("qrpo_2"),
                             uiOutput("qrz2"),
                             
                             textOutput("qrMean_2"),
                             textOutput("qrSD_2"),
                             textOutput("qrd_2"),
                             
                             verbatimTextOutput("qrsample_size2"),
                             htmlOutput("qrRef2"),
                             br()
                           )
                         )
                )
                
              )
            ),
            
            
            
            tabPanel(title=span("One sample Proportion Estimation",title ="For Cross sectional study and qualitative outcome to estimate prevalence/incidence "),  
                     tabsetPanel(   
                       tabPanel("One Sample Proportion estimation", 
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h5("One Proportion estimation"),
                                    radioButtons("qPopu", "Population size:", choices = c("UnKnown", "Known"),inline = TRUE),
                                    conditionalPanel(
                                      condition="input.qPopu == 'Known'",  
                                      numericInput("N2", "Population Size (N):", value = 1),
                                    ),
                                    selectInput("CI", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    numericInput("p", "Proportion(%):", value = 1, min = 0.01),
                                    selectInput("error", "Type of Margin of error:", choices = c("Absolute_error", "Relative_error")),
                                    numericInput("d", "Margin of error(%):", value =1, min = 0.01),
                                    numericInput("f", "Non response/Attrition rate (%):", value =0),
                                    textAreaInput("Ref1", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("calc", "Calculate"),
                                    actionButton("qsave_screenshot", "save"),
                                    actionButton("next_tab2", "Help"),
                                  ),  
                                  
                                  mainPanel(
                                    id ="qmain", style = "font-size:14px", width = 6,
                                    h3("Sample size"),
                                    p("The sample size(n) formula to estimate the proportion:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{Z_{1-\\alpha/2}^2 p q} {d^2}$$"),
                                    textOutput("wr3"),
                                    textOutput("ppo_1"),
                                    uiOutput("z"),
                                    textOutput("p"),
                                    textOutput("qd_1"),
                                    verbatimTextOutput("sample_size"),
                                    htmlOutput("Ref1"),
                                    br()
                                  )
                                )
                       ),
                       
                       tabPanel("With unknown proportion",
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h5(" One Proportion estimation"),
                                    radioButtons("upPopu", "Population size:", choices = c("UnKnown", "Known"),inline = TRUE),
                                    conditionalPanel(
                                      condition="input.upPopu == 'Known'",  
                                      numericInput("upN2", "Population Size (N):", value = 1),
                                    ),
                                    selectInput("upCI", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    numericInput("upd", "Margin of error(%):", value =1, min = 0.01),
                                    numericInput("upf", "Non response/Attrition rate (%):", value =0),
                                    actionButton("upcalc", "Calculate"),
                                    actionButton("upsave_screenshot", "save"),
                                    actionButton("upnext_tab2", "Help"),
                                  ),  
                                  
                                  mainPanel(
                                    id ="upmain", style = "font-size:14px", width = 6,
                                    h3("Sample size"),
                                    p("The sample size(n) formula to estimate the proportion:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{Z_{1-\\alpha/2}^2 } { 4 d^2}$$"),
                                    textOutput("wr4"),
                                    textOutput("uppo_1"),
                                    uiOutput("upz"),
                                    textOutput("upd_1"),
                                    verbatimTextOutput("upsample_size"),
                                  )
                                )
                       ),
                     )
            ),
            
            tabPanel("Correlation estimation", 
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         style = "height: 90vh; overflow-y: auto;",
                         h4("Correlation estimation"),
                         radioButtons("crPopu", "Population size:", choices = c("UnKnown", "Known"),inline = TRUE),
                         conditionalPanel(
                           condition="input.crPopu == 'Known'",  
                           numericInput("crN1", "Population Size (N):", value = 1),
                         ),
                         selectInput("crCI", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                         sliderInput("cr", " Anticipated correlation:", value="0.5", min=0, max=1, step=0.01),
                         numericInput("crd1", "Margin of error (%):", value =1, min = 0.01),
                         numericInput("crf1", "Non response/Attrition rate (%):", value =0),
                         textAreaInput("crRef", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                         actionButton("crcalc1", "Calculate"),
                         actionButton("crsave_screenshot1", "save"),
                         actionButton("crnext_tab1", "Help"),
                       ),  
                       
                       mainPanel(
                         id ="crmain1", style = "font-size:14px", width = 6,
                         h3("Sample size"),
                         p("The sample size formula to estimate the correlation:"),
                         tags$head(
                           tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                         ),
                         div(class = "watermark", "MVSSC"),
                         
                         p("$$n = \\frac{Z_{1-\\alpha/2}^2 (1-r^2)^2} {d^2} + 1 $$"),                      
                         textOutput("wr5"),
                         textOutput("crpo_1"),
                         uiOutput("crz1"),
                         textOutput("Ancr"),
                         textOutput("crd_1"),
                         verbatimTextOutput("crsample_size1"),
                         htmlOutput("crRef"),
                         br()
                       )
                     )
            ),  
            
            tabPanel("Sensitivity & Specificity estimation",                         
                     sidebarLayout(
              sidebarPanel(
                style = "height: 90vh; overflow-y: auto;",
                h5("Sensitivity & Specificity estimation"),
                selectInput("ssCI", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                numericInput("SN", "Sensitivity(S_N)(%):", value = 0, min = 0.01),
                numericInput("SP", "Specificity(S_P)(%):", value = 0, min = 0.01),
                numericInput("ssp", "Prevalence (p)(%):", value = 0, min = 0.01),
                numericInput("ssd", "Margin of error (d)(%):", value =1, min = 0.01),
                textAreaInput("ssRef", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                actionButton("sscalc", "Calculate"),
                actionButton("sssave_screenshot", "save"),
                actionButton("ssnext_tab", "Help"),
              ),  
              
              mainPanel(
                id ="ssmain", style = "font-size:14px", width = 7,
                h3("Sample size"),
                p("The sample size(n) formula to estimate Sensitivity and Specificity:"),
                tags$head(
                  tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                ),
                div(class = "watermark", "MVSSC"),
                
                p("$$n_{S_N} = \\frac{Z_{1-\\alpha/2}^2 S_N (1-S_N)} {d^2 p} \\quad \\text{and}\\quad n_{S_P} = \\frac{Z_{1-\\alpha/2}^2 S_P (1-S_P)} {d^2 (1-p)}$$"),
                textOutput("wr6"),
                uiOutput("ssz"),
                uiOutput("sn"),
                uiOutput("sp"),
                textOutput("ssp"),
                textOutput("ssd_1"),
                verbatimTextOutput("snsample_size1"),
                verbatimTextOutput("spsample_size2"),
                verbatimTextOutput("snpsample_size"),
                htmlOutput("ssRef"),
                br()
              )
            )
            ),            
            tabPanel(title =span("One Sample Mean Test", title ="Variable with quantitative outcome to test average value with Standard value"),  
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         style = "height: 90vh; overflow-y: auto;",
                         h4("One Sample Mean Test"),
                         radioButtons("tail3", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                         selectInput("CI3", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                         selectInput("b3", "Power of the test(%):", choices = c("80", "90","95","99")),
                         numericInput("ad3", "Assumed Mean:", value =0),
                         numericInput("d3", "Mean:", value =1),
                         numericInput("SD3", "Standard deviation:", value = 1, min = 0.01),
                         numericInput("f3", "Nonresponse/Attrition rate (%):", value =0),
                         textAreaInput("Ref3", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                         actionButton("calc3", "Calculate"),
                         actionButton("save_screenshot3", "save"),
                         actionButton("next_tab3", "Help"),
                       ),  
                       
                       mainPanel(
                         id ="main3", style = "font-size:14px", width = 6,
                         h4("Sample size"),
                         p("For One Sample Mean Test"),
                         tags$head(
                           tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                         ),
                         div(class = "watermark", "MVSSC"),
                         
                         p("$$n = \\frac{(Z_{1-\\alpha*}+Z_{1-\\beta})^2 \\sigma^2} {\\delta^2}$$"),
                         p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                         textOutput("wr7"),
                         htmlOutput("tail3"),
                         uiOutput("z3"),
                         uiOutput("b_3"),
                         textOutput("ad3"),
                         textOutput("d3"),
                         textOutput("SD3"),
                         textOutput("md3"),
                         verbatimTextOutput("sample_size3"),
                         htmlOutput("Ref3"),  
                         br()
                       )
                     )
            ),
            
            #Two samples mean test (Large)---------------------------------------------------------
            tabPanel(title =span("Two Sample Mean Test", title ="To compare two quantitative outcome"), 
                     tabsetPanel(
                       tabPanel(title =span("Unequal variance(σ1≠σ2)", title ="Two Samples from independent populations"),  
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("Two sample mean test"),
                                    radioButtons("tail4", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline=TRUE),
                                    selectInput("CI4", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    selectInput("b4", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    numericInput("m4_1", "Group 1 Mean:", value =0, min = 0.01),
                                    numericInput("SD4_1", "Group 1 SD:", value = 1, min = 0.01),
                                    numericInput("m4_2", "Group 2 Mean:", value =0, min = 0.01),
                                    numericInput("SD4_2", "Group 2 SD:", value = 1, min = 0.01),
                                    numericInput("R4", "Allocation Ratio:", value =1, step = 1),
                                    numericInput("f4", "Attrition rate (%):", value =0),
                                    textAreaInput("Ref4", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("calc4", "Calculate"),
                                    actionButton("save_screenshot4", "save"),
                                    actionButton("next_tab4", "Help"),
                                  ),  
                                  mainPanel(
                                    id ="main4", style = "font-size:14px", width = 6,
                                    h4("Sample size"),
                                    p("To test two independent sample mean:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n =\\frac{(Z_{1-\\alpha*}+Z_{1-\\beta})^2 \\left(\\sigma_1^2+\\frac{1}{k}\\sigma_2^2\\right)} {\\delta^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr9"),
                                    htmlOutput("tail4"),
                                    uiOutput("z4"),
                                    uiOutput("b_4"),
                                    textOutput("d4"),
                                    uiOutput("m4_1"),
                                    uiOutput("m4_2"),
                                    textOutput("d_4"),
                                    textOutput("R_4"),
                                    verbatimTextOutput("sample_size4"),
                                    htmlOutput("Ref4"),
                                    br()
                                  )
                                )
                       ),
                       tabPanel(title =span("Equal variance(σ1=σ2)", title ="Two independent samples from same population"),  
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("Two sample mean test"),
                                    radioButtons("etail4", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                                    selectInput("eCI4", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    selectInput("eb4", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    numericInput("em4_1", "Group 1 Mean:", value =0, min = 0.01),
                                    numericInput("em4_2", "Group 2 Mean:", value =0, min = 0.01),
                                    numericInput("eSD4_1", " Common SD:", value = 1, min = 0.01),
                                    numericInput("eR4", "Allocation ratio:", value =1, step = 1),
                                    numericInput("ef4", "Nonresponse/Attrition rate (%):", value =0),
                                    textAreaInput("eRef4", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3), 
                                    actionButton("ecalc4", "Calculate"),
                                    actionButton("esave_screenshot4", "save"),
                                    actionButton("enext_tab4", "Help"),
                                  ),  
                                  mainPanel(
                                    id ="emain4", style = "font-size:14px", width = 6,
                                    h4("Sample size"),
                                    p("to test two independent sample mean with common variance:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\left(\\frac{1+k}{k}\\right)\\frac{(Z_{1-\\alpha*}+Z_{1-\\beta})^2 \\sigma^2} {\\delta^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr10"),
                                    htmlOutput("etail4"),
                                    uiOutput("ez4"),
                                    uiOutput("eb_4"),
                                    textOutput("ed4"),
                                    textOutput("em4_1"),
                                    textOutput("em4_2"),
                                    textOutput("eSD4_1"),
                                    textOutput("eR_4"),
                                    textOutput("sigma"),
                                    textOutput("ed_4"),
                                    verbatimTextOutput("esample_size4"),
                                    htmlOutput("eRef4"),
                                    br()
                                  )
                                )
                       ),
                     )
            ),
            
            #single proportion test----------------------------------------------                  
            tabPanel(title =span("One sample proportion test", title ="variable with qualitative outcome to test proportion value with Standard value "), 
                     sidebarLayout(
                       sidebarPanel(
                         style = "height: 90vh; overflow-y: auto;",
                         h4("One Proportion Test"),
                         radioButtons("tail5", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                         selectInput("CI5", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                         selectInput("b5", "Power of the test(%):", choices = c("80", "90","95","99")),
                         numericInput("ad5", "Assumed proportion(P0) (%):", value =0),
                         numericInput("p5", " Estimated Proportion(P1)(%):", value = 1, min = 0.01),
                         numericInput("f5", "Attrition rate (%):", value =0),
                         textAreaInput("Ref5", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                         actionButton("calc5", "Calculate"),
                         actionButton("save_screenshot5", "save"),
                         actionButton("next_tab5", "Help"),
                       ),  
                       mainPanel(
                         id ="main5", style = "font-size:14px", width = 6,
                         h4("Sample size"),
                         p("for one sample proportion test:"),
                         tags$head(
                           tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                         ),
                         div(class = "watermark", "MVSSC"),
                         
                         p("$$n = \\frac{(Z_{1-\\alpha*}\\sqrt{P_0 Q_0}+Z_{1-\\beta} \\sqrt{p_1 q_1)})^2} {(P_0-P_1)^2}$$"),
                         p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                         textOutput("wr12"),
                         htmlOutput("tail5"),
                         uiOutput("z5"),
                         uiOutput("b_5"),
                         textOutput("ad5"),
                         textOutput("p5"),
                         verbatimTextOutput("sample_size5"),
                         htmlOutput("Ref5"),
                         br()
                       )
                     )
            ),
            
            #Two Sample proportion test---------------------------------------------
            tabPanel(title =span("Two Sample proportion test", title ="To compare two qualitative outcome"), 
                     tabsetPanel(
                       tabPanel("with estimated proportions",  
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("Two sample proportion test"),
                                    radioButtons("tail6", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                                    selectInput("CI6", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    selectInput("b6", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    numericInput("p6_1", "Group 1 proportion (%):", value = 1, min = 0.01),
                                    numericInput("p6_2", "Group 2 proportion (%):", value = 1, min = 0.01),
                                    numericInput("R6", "Allocation Ratio:", value =1, step = 1),
                                    numericInput("f6", "Attrition rate (%):", value =0),
                                    textAreaInput("Ref6", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("calc6", "Calculate"),
                                    actionButton("save_screenshot6", "save"),
                                    actionButton("next_tab6", "Help"),
                                  ),  
                                  mainPanel(
                                    id ="main6", style = "font-size:14px", width = 6,
                                    h4("Sample size"),
                                    p("to test two samples proportion:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{(Z_{1-\\alpha*}\\sqrt{(k+1)P Q}+Z_{1-\\beta} \\sqrt{k p_1 q_1+p_2 q_2})^2} {k (p_1-p_2)^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr14"),
                                    htmlOutput("tail6"),
                                    uiOutput("z6"),
                                    uiOutput("b_6"),
                                    textOutput("p6_1"),
                                    textOutput("p6_2"),
                                    textOutput("P"),
                                    textOutput("R_6"),
                                    verbatimTextOutput("sample_size6"),
                                    htmlOutput("Ref6"),
                                    br()
                                  )
                                )
                       ),
                       
                       
                       tabPanel(" Using OR or RR ", 
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("Two sample proportion test"),
                                    radioButtons("tail6_RR", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                                    selectInput("CI6_RR", "Confidence interval(%):", choices = c("90", "95", "99"), selected = "95"),
                                    selectInput("b6_RR", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    numericInput("p6_1_RR", "Anticipated OR or RR:", value = 1, min = 0.01),
                                    numericInput("p6_2_RR", "Proportion of disease in control group (%):", value = 1, min = 0.01),
                                    numericInput("R6_RR", "Allocation Ratio:", value =1, step = 1),
                                    numericInput("f6_RR", "Attrition rate (%):", value =0),
                                    textAreaInput("Ref6_RR", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                                    actionButton("calc6_RR", "Calculate"),
                                    actionButton("save_screenshot6_RR", "save"),
                                    actionButton("next_tab6_RR", "Help"),
                                  ),
                                  mainPanel(
                                    id ="main6_RR", style = "font-size:14px", width = 6,
                                    h2("Sample size"),
                                    p("to test two samples proportion:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{(Z_{1-\\alpha*}\\sqrt{(k+1)P Q}+Z_{1-\\beta} \\sqrt{k p_1 q_1+p_2 q_2})^2} {k (p_1-p_2)^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr17"),
                                    htmlOutput("tail6_RR"),
                                    uiOutput("z6_RR"),
                                    uiOutput("b_6_RR"),
                                    textOutput("d6_RR"),
                                    textOutput("p6_1_RR"),
                                    textOutput("E_p1_RR"),
                                    textOutput("p6_2_RR"),
                                    textOutput("q6_2_RR"),
                                    textOutput("P_RR"),
                                    textOutput("R_6_RR"),
                                    verbatimTextOutput("sample_size6_RR"),
                                    htmlOutput("Ref6_RR"),
                                    br()
                                  )
                                )
                       ),
                     )
            ), 
            
            tabPanel(title =span("Paired samples mean test", title ="To Compare pre and post treatment effect"),
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         style = "height: 90vh; overflow-y: auto;",
                         h4("Paired mean test"),
                         radioButtons("tail9", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                         selectInput("CI9", "Confidence interval(%):", choices = c("90", "95", "99"), selected = 95),
                         selectInput("b9", "Power of the test(%):", choices = c("80", "90","95","99")),
                         numericInput("mu9_1", "Pre test mean:", value = 1),
                         numericInput("SD9_1", "pre test SD:", value = 1, min = 0.01),
                         numericInput("mu9_2", "Post test mean:", value = 1),
                         numericInput("SD9_2", "post test SD:", value = 1, min = 0.01),
                         sliderInput("corr", "  anticipated correlation", value="0.5", min=0, max=1, step=0.1),
                         numericInput("f9", "Attrition rate (%):", value =0),
                         textAreaInput("Ref9", "Reference/Note:",placeholder = "Enter reference or note here...", rows=3),
                         actionButton("calc9", "Calculate"),
                         actionButton("save_screenshot9", "save"),
                         actionButton("next_tab7", "Help"),
                       ),  
                       mainPanel(
                         id ="main9", style = "font-size:14px", width = 6,
                         h4("Sample size"),
                         p("to compare paired samples:"),
                         tags$head(
                           tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                         ),
                         div(class = "watermark", "MVSSC"),
                         
                         p("$$n = \\frac{(Z_{1-\\alpha^*}+Z_{1-\\beta})^2 \\sigma_d^2} {\\delta_d^2}$$"),
                         p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                         htmlOutput("tail9"),
                         textOutput("wr18"),
                         uiOutput("z9"),
                         uiOutput("b_9"),
                         textOutput("pre"),
                         textOutput("post"),
                         textOutput("corr_12"),
                         textOutput("SD9"),
                         textOutput("d9"),
                         verbatimTextOutput("sample_size9"),
                         htmlOutput("Ref9"),  
                         br()
                       )
                     )
            ),
            
            
            #Cohen's Method--------------------------------------------------
            tabPanel(title =span("Cohen's Method", title ="For new study or unknown estimated values"),
                     tabsetPanel(
                       tabPanel(title =span("One Sample", title ="To compare one sample value with standard value"),
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h3("One mean test"),
                                    radioButtons("tail3c", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                                    selectInput("CI3c", "Confidence interval(%):", choices = c("90", "95", "99"), selected = 95),
                                    selectInput("b3c", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    sliderInput("es3c", "Effect Size:", value =.2, min=0.1,step = 0.1, max=1),
                                    numericInput("f3c", "Nonresponse/Attrition rate (%):", value =0),
                                    actionButton("calc3c", "Calculate"),
                                    actionButton("save_screenshot3c", "save"),
                                    actionButton("next_tab3c", "Help"),
                                  ),  
                                  mainPanel(
                                    id ="main3c", style = "font-size:14px", width = 6,
                                    h4("Sample size"),
                                    p("For One Sample Mean Test with assumed effect size:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\frac{(Z_{1-\\alpha*}+Z_{1-\\beta})^2} {\\Delta^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr8"),
                                    htmlOutput("tail3c"),
                                    uiOutput("z3c"),
                                    uiOutput("b_3c"),
                                    textOutput("es3c"),
                                    verbatimTextOutput("sample_size3c"),
                                    HTML("<b><i>Reference:</i></b>:", "Cohen, J. (2013). Statistical power analysis for the behavioral sciences. routledge."),
                                    br()
                                  )
                                )
                       ),
                       tabPanel(title =span("Two Samples", title ="To compare two samples values"),
                                withMathJax(),
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "height: 90vh; overflow-y: auto;",
                                    h4("Single mean test"),
                                    radioButtons("tail4c", "Alternative hypothesis:", choices = c("Two_sided","One_sided"),inline = TRUE),
                                    selectInput("CI4c", "Confidence interval(%):", choices = c("90", "95", "99"), selected = 95),
                                    selectInput("b4c", "Power of the test(%):", choices = c("80", "90","95","99")),
                                    sliderInput("es4c", "Effect Size:", value =.2, min=0.1,step = 0.1, max=2),
                                    numericInput("eR4c", "Allocation ratio:", value =1, step = 1),
                                    numericInput("f4c", "Attrition rate (%):", value =0),
                                    actionButton("calc4c", "Calculate"),
                                    actionButton("save_screenshot4c", "save"),
                                    actionButton("next_tab4c", "Help"),
                                  ),  
                                  mainPanel(
                                    id ="main4c", style = "font-size:14px", width = 6,
                                    h4("Sample size"),
                                    p(" Two Sample Mean Test with assumed effect size:"),
                                    tags$head(
                                      tags$style(HTML("
    .watermark {
      position: fixed;
      z-index: 1;
    opacity: 0.08;
      font-size: 15px;
      transform: rotate(-60deg);
      color: #000;
      pointer-events: none; /* so it doesn’t block clicks */
      z-index: 9999;
      user-select: none;   /* text cannot be selected */
    }
  "))
                                    ),
                                    div(class = "watermark", "MVSSC"),
                                    
                                    p("$$n = \\left(\\frac{1+k}{k}\\right)\\frac{(Z_{1-\\alpha*}+Z_{1-\\beta})^2} {\\Delta^2}$$"),
                                    p("(when the alternative hypothesis is one-sided, α*=α; whereas for two-sided, α*=α/2)"),
                                    textOutput("wr11"),
                                    htmlOutput("tail4c"),
                                    uiOutput("z4c"),
                                    uiOutput("b_4c"),
                                    textOutput("es4c"),
                                    textOutput("eR_4c"),
                                    verbatimTextOutput("sample_size4c"),
                                    HTML("<b><i>Reference:</i></b>:", "Cohen, J. (2013). Statistical power analysis for the behavioral sciences. routledge."),
                                  )
                                )
                       ),
                       
                     )
            )  
            
            
            
 ),           
#-----------------------------------------------------------------------------
#Sampling
#--------------------------------------------------------------------------------
 navbarMenu("Sampling",
            tabPanel("Simple Random Sampling",
                     titlePanel("Simple Random Sampling"),
                     
                     sidebarLayout(
                       sidebarPanel(
                         numericInput("pop_size", "Population Size (N):", value = 100, min = 1, step = 1),
                         numericInput("sample_size", "Sample Size (n):", value = 10, min = 1, step = 1),
                         
                         actionButton("generate", "Generate Sample", class = "btn-primary"),
                         hr(),
                         downloadButton("download_excel", "Download Excel Results", class = "btn-success")
                       ),
                       
                       mainPanel(
                         h4("Sampling Summary"),
                         verbatimTextOutput("summary_text"),
                         hr(),
                         h4("Sampled Units"),
                         tableOutput("sample_table")
                       )
                     )
            ),
            tabPanel("Stratified Random samplig",
                     tabsetPanel(
                       tabPanel("Proportional allocation", 
                                titlePanel("Stratified Random Sampling"),
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    numericInput("num_strata", "Number of Strata:", value = 3, min = 1, step = 1),
                                    
                                    # Dynamic UI inputs for individual stratum sizes
                                    uiOutput("strata_sizes_ui"),
                                    hr(),
                                    
                                    # Read-only overall population size
                                    verbatimTextOutput("total_pop_display"),
                                    
                                    numericInput("total_sample_size", "Total Sample Size (n):", value = 15, min = 1, step = 1),
                                    
                                    radioButtons("allocation_type", "Allocation Type:",
                                                 choices = c("Equal Allocation (Case 1)" = "equal",
                                                             "Proportional Allocation (Case 2)" = "proportional")),
                                    
                                    radioButtons("sampling_method", "Within-Stratum Sampling Method:",
                                                 choices = c("Simple Random Sampling (SRS)" = "srs",
                                                             "Systematic Random Sampling" = "systematic")),
                                    
                                    actionButton("generates", "Generate Sample", class = "btn-primary"),
                                    hr(),
                                    downloadButton("download_excels", "Download Excel Results", class = "btn-success")
                                  ),
                                  
                                  mainPanel(
                                    h4("Strata Allocation & Parameters Summary"),
                                    tableOutput("summary_tables"),
                                    hr(),
                                    h4("Sampled Units"),
                                    tableOutput("sample_tables")
                                  )
                                )
                       ),
                       
                       tabPanel(
                         "Neyman Allocation",
                         titlePanel("Stratified Random Sampling (Neyman Allocation)"),
                         
                         sidebarLayout(
                           sidebarPanel(
                             numericInput("num_strata_neyman", "Number of Strata:", value = 3, min = 1, step = 1),
                             
                             # Dynamic UI inputs for individual stratum sizes (N_h) and standard deviations (S_h)
                             uiOutput("strata_params_ui_neyman"),
                             hr(),
                             
                             # Read-only overall population size
                             verbatimTextOutput("total_pop_display_neyman"),
                             
                             numericInput("total_sample_size_neyman", "Total Sample Size (n):", value = 15, min = 1, step = 1),
                             
                             radioButtons("sampling_method_neyman", "Within-Stratum Sampling Method:",
                                          choices = c("Simple Random Sampling (SRS)" = "srs",
                                                      "Systematic Random Sampling" = "systematic")),
                             
                             actionButton("generate_neyman", "Generate Sample", class = "btn-primary"),
                             hr(),
                             downloadButton("download_excel_neyman", "Download Excel Results", class = "btn-success")
                           ),
                           
                           mainPanel(
                             h4("Strata Allocation & Parameters Summary"),
                             tableOutput("summary_table_neyman"),
                             hr(),
                             h4("Sampled Units"),
                             tableOutput("sample_table_neyman")
                           )
                         )
                       )
                     )
            ),
            tabPanel("Systematic Random Sampling",
                     titlePanel("Systematic Random Sampling"),
                     
                     sidebarLayout(
                       sidebarPanel(
                         numericInput("pop_sizesy", "Population Size (N):", value = 100, min = 1, step = 1),
                         
                         radioButtons("interval_mode", "Sampling Interval Option:",
                                      choices = c("Case 1: Default Interval (k = N / n)" = "default",
                                                  "Case 2: Custom Sampling Interval (k)" = "custom")),
                         
                         # Conditional UI depending on mode
                         uiOutput("sample_input_ui"),
                         
                         actionButton("generatesy", "Generate Systematic Sample", class = "btn-primary"),
                         hr(),
                         downloadButton("download_excelsy", "Download Excel Results", class = "btn-success")
                       ),
                       
                       mainPanel(
                         h4("Sampling Parameters & Summary"),
                         verbatimTextOutput("summary_textsy"),
                         hr(),
                         h4("Selected Systematic Sample Units"),
                         tableOutput("sample_tablesy")
                       )
                     )       
                     
            ),
            tabPanel("Cluster sampling",
                     titlePanel("Two-Stage PPS Cluster Sampling (Auto Sample Allocation)"),
                     
                     sidebarLayout(
                       sidebarPanel(
                         numericInput("pop_sizec", "Overall Population Size (N):", value = 500, min = 1, step = 1),
                         numericInput("num_clusters", "Total Clusters in Population (K):", value = 10, min = 1, step = 1),
                         numericInput("sample_clusters", "Number of Clusters to Sample (k):", value = 3, min = 1, step = 1),
                         numericInput("total_sample_n", "Total Desired Sample Size (n):", value = 30, min = 1, step = 1),
                         
                         actionButton("select_clusters_btn", "1. Select Clusters (PPS)", class = "btn-info"),
                         hr(),
                         
                         # Dynamic UI: Prompts ONLY for sizes (N_i) of chosen clusters
                         uiOutput("selected_clusters_input_ui"),
                         
                         uiOutput("generate_sample_btn_ui"),
                         hr(),
                         downloadButton("download_excelc", "Download Excel Results", class = "btn-success")
                       ),
                       
                       mainPanel(
                         h4("Cluster Selection & Auto-Allocation Summary"),
                         tableOutput("summary_tablec"),
                         hr(),
                         h4("Selected Sample Units"),
                         tableOutput("sample_tablec")
                       )
                     )
            ),
            tabPanel("Systematic PPS sampling",
                     titlePanel("Systematic PPS Sampling (File Upload)"),
                     
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("file", "Upload Cluster Data (CSV/Excel):",
                                   accept = c(".csv", ".xlsx", ".xls")),
                         
                         helpText("Required: File must contain a Cluster Name column and a Size/Population column."),
                         
                         # Dynamic dropdowns to select columns after upload
                         uiOutput("col_selectors_ui"),
                         
                         numericInput("sample_clusters", "Number of Clusters to Sample (k):", value = 3, min = 1, step = 1),
                         
                         actionButton("generatep", "Generate Systematic PPS Sample", class = "btn-primary"),
                         hr(),
                         downloadButton("download_excelp", "Download Excel Results", class = "btn-success")
                       ),
                       
                       mainPanel(
                         h4("Systematic PPS Parameters & Cumulative Table"),
                         verbatimTextOutput("params_summary"),
                         tableOutput("summary_tablep"),
                         hr(),
                         h4("Selected Sample Units from Chosen Clusters"),
                         tableOutput("sample_tablep")
                       )
                     )
            )          
            
            
            
 ),
 navbarMenu("Help",
 ),
 
 
  )
