library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
shinyUI(ui = tagList(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))),


  navbarPage(
    theme = shinytheme("lumen"),  # <--- To use a theme, uncomment this
    useShinyjs(),
    strong("Metabolomic Analytics"), # Main title name

    # Home --------------------------------------------------------------------
    tabPanel("Home",
             column(12, align="center",
                    h2(strong("Welcome to the Shiny App for Metabolomic Data")),
                    br(),
                    br(),
                    br(), # Empty line
             #),
             #column(12, align="center",
                    #h4(strong("Basic Procedures:")),
                    #h4("1) Enter your data in the 'Data' tab at the top."),
                    #h4("2) Set your PPCA methods & parameters in the 'Analytics' tab."),
                    #h4("3) Submit the parameters and analyze the results."),
                    br(),
                    tags$img(src = "Homepage.jpg",height=600,width=500),
                    h4(strong("Note: For more help on usage, please look in the 'Guide'tab and 'Vignette' for package."))
             )
    ),



    # Data --------------------------------------------------------------------
    tabPanel("Data",
             sidebarPanel(

               fileInput("main_file", h4("File input:", bsButton("main_data_tooltip", label = "",
                                                                 icon = icon("question"), size = "extra-small")),
                         multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                         placeholder = "Enter Metabolomic Data Here"),
               bsPopover("main_data_tooltip", title="",
                         content="Please make sure: rows are samples/observations, columns are spectral bins",
                         trigger = "hover"),

               tags$hr(),
               h4(helpText("Select the read.table parameters below")),
               checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
               # checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
               radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

               sliderInput("cov_slider", h4("Covariates Columns:"), 0, 10, c(0,0)),
               sliderInput("label_slider", h4("Group Labels Columns:"), 0, 10, 0),

               tags$hr(),
               h4(helpText("Select the scaling parameters below")),
               radioButtons(inputId = 'scale', label = 'Scale Type', choices = c(Centering='centering',Autoscale='autoscale',Paretoscale='paretoscale',Rangescale='rangescale', Vastscale='vastscale'), selected = 'centering')


               # conditionalPanel(condition="input.data_tabs=='main'",
               #                  fileInput("main_file", h4("File input:", bsButton("main_data_tooltip", label = "",
               #                                                                    icon = icon("question"), size = "extra-small")),
               #                            multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
               #                            placeholder = "Enter Metabolomic Data Here"),
               #                  bsPopover("main_data_tooltip", title="",
               #                            content="Please make sure: rows are samples/observations, columns are spectral bins",
               #                            trigger = "hover"),
               #
               #                  tags$hr(),
               #                  h4(helpText("Select the read.table parameters below")),
               #                  checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
               #                  # checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
               #                  radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
               #                  # sliderInput("main_slider", h4("Number of rows shown:"), 1, 50, 10)
               #
               # ),
               #
               # conditionalPanel(condition="input.data_tabs=='covariates'",
               #                  fileInput("covariates_file", h4("File input:", bsButton("covariates_data_tooltip", label = "",
               #                                                                          icon = icon("question"), size = "extra-small")),
               #                            multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
               #                            placeholder = "Enter Covariates Data Here"),
               #                  bsPopover("covariates_data_tooltip", title="",
               #                            content="Please make sure: rows are samples/observations, columns are covariates",
               #                            trigger = "hover"),
               #
               #                  tags$hr(),
               #                  h5(helpText("Select the read.table parameters below")),
               #                  checkboxInput(inputId = 'cov_header', label = 'Header', value = TRUE),
               #                  # checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
               #                  radioButtons(inputId = 'cov_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
               #                  # sliderInput("cov_slider", h4("Number of rows shown:"), 1, 50, 10)
               # ),
               #
               # conditionalPanel(condition="input.data_tabs=='labels'",
               #                  fileInput("labels_file", h4("File input:"), multiple = F,
               #                            accept = c("text/csv", "text/comma-separated-values, text/plain",
               #                                       ".csv"), placeholder = "Enter Labels Data Here"),
               #
               #                  tags$hr(),
               #                  h5(helpText("Select the read.table parameters below")),
               #                  checkboxInput(inputId = 'lab_header', label = 'Header', value = TRUE),
               #                  # checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
               #                  radioButtons(inputId = 'lab_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
               #                  # sliderInput("lab_slider", h4("Number of rows shown:"), 1, 50, 10)
               # )

             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(span("Main Data", title="Data from NMR or Mass Spectroscopy (MS)"),
                          value = 'main',
                          strong(h3(textOutput("main_file_name"))),
                          dataTableOutput("main_data_table"),
                 ),
                 tabPanel(span("Covariates Data", title="Sample attributes (eg: weight, gender, regions, etc)"),
                          value = 'covariates',
                          h3(textOutput("covariates_file_name")),
                          dataTableOutput("covariates_data_table")
                 ),
                 tabPanel(span("Group Labels", title="This data is used for colours in plots (eg: Treatment group vs Control group)"),
                          value = 'labels',
                          h3(textOutput("labels_file_name")),
                          dataTableOutput("labels_data_table")
                 ),
                 id = "data_tabs"
               )
             )
    ),



    # Analytics ---------------------------------------------------------------
    tabPanel("Analytics",
             sidebarPanel(
               tabsetPanel(
                 # PPCA plot controls
                 tabPanel(div("PPCA",id="PPCA_tooltip"), value = "PPCA_tab",
                          bsTooltip("PPCA_tooltip", title="Simple PPCA", trigger = "hover"),

                          conditionalPanel(condition="input.analytics_plot_tabs=='PPCA_description'",
                                           h3(helpText('Model Parameters:')),
                                           div(style="display:inline-block",
                                               numericInput("epsilon", label = "Covergence Criterion:", value=0.01, min=1e-5, max=1e2),
                                           ),
                                           div(style="display:inline-block",
                                               numericInput("max_iter", label = "Max Number of Iteration:", value=1e3, min=1, max=1e6),
                                           ),

                                           h4("Include Covariates?"),

                                           disabled(checkboxInput(inputId = 'covariates_check', label = span('Tick for Yes', title = "Please make sure covariates data are selected in Data tabs to enable this selection"), value = FALSE)),


                                           radioButtons("choose_q", label = h4("Output Optimize Model?"), choices = c(Use_Optimal_PC=TRUE, Use_Maximum_PC=FALSE)),

                                           sliderInput("PC_slider", h4("Range of Principal Components:"),
                                                       min = 1, max = 10, value = c(1, 4)),
                                           sliderInput("bootstrap_n_slider", h4("Numbers of Bootstrap:"), 2, 100, 5),
                                           bsTooltip("bootstrap_n_slider", title="Higher Bootstrap, Better Uncertainty but Longer Wait Time", trigger = "hover"),


                                           tags$h6("Click button to run the model using desired parameters:"),
                                           div(style="display:inline-block",
                                               actionButton("submit_para_btn", "Submit Parameters", class = "btn-primary"),
                                           ),
                                           div(style="display:inline-block",
                                               strong(textOutput("optimal_q")),
                                           ),

                                           tags$hr(),
                          ),


                          conditionalPanel(condition="input.analytics_plot_tabs!='PPCA_description'",
                                           h3(helpText('Graph Controls:'))
                          ),


                          # Main plot
                          conditionalPanel(condition="input.analytics_plot_tabs=='main_PPCA_plot'",
                                           sliderInput("main_plot_PC", h4("Principal Component for Main Plot:"), min=1, max=10, value=1, step=1, ticks=F),
                                           sliderInput("conf_int", h4("Confidence Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                           sliderInput("n_main", h5("Numbers of Significant Bin for Main Plot"), min=1, max=20, value=5, step=1, ticks=F),
                                           downloadButton("dl_significant_btn1", span("Download", title="Download a Table of All Significant Spectral Bins Names"))
                          ),

                          # Score & Loading
                          conditionalPanel(condition="input.analytics_plot_tabs=='score_PPCA_plot' || input.analytics_plot_tabs=='loading_PPCA_plot'",
                                           sliderInput("x_PC", h4("X-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                           sliderInput("y_PC", h4("Y-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),

                                           conditionalPanel(condition="input.analytics_plot_tabs=='score_PPCA_plot'",
                                                            sliderInput("post_int", h4("Posterior Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),


                                           )
                          ),

                          # Significant bins
                          conditionalPanel(condition="input.analytics_plot_tabs=='sig_bins_PPCA_plot'",
                                           sliderInput("x_lim", h4("X-axis limit for Significant Bins Plot"), min=0, max=20, value=c(0,10), step = 0.5),
                                           sliderInput("sample_num", h4("Sample number"), min = 1, max = 2, value = 1, step = 1),
                                           downloadButton("dl_significant_btn2", span("Download", title="Download a Table of All Significant Spectral Bins Names"))
                          ),

                 ),

                 # MPPCA plot controls
                 tabPanel(tags$div("MPPCA",
                                   title="Mixed PPCA that deals with multiple groups"),
                          value = "MPPCA_tab",
                          "Placeholder for Mix PPCA"),

                 # DPPCA plot controls
                 tabPanel(tags$div("DPPCA", title="Dynamic PPCA"),
                          value = "DPPCA_tab",
                          "Placeholder for Dynamic PPCA"),
                 id = "analytics_method_tab"
               )
             ),

             # PPCA Plots
             conditionalPanel(condition="input.analytics_method_tab=='PPCA_tab'",
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Description", value = "PPCA_description",
                                           column(12, align="center",
                                                  h2(strong("PPCA is bla bla bla")),
                                                  h3("This application enables you to perform different PPCA methods using your own metabolomic data."),
                                                  br(),
                                                  br(),
                                                  br() # Empty line
                                           ),
                                           column(12, align="center",
                                                  h4(strong("Basic Procedures:")),
                                                  h4("1) Enter your data in the 'Data' tab at the top."),
                                                  h4("2) Set your PPCA methods & parameters in the 'Analytics' tab."),
                                                  h4("3) Submit the parameters and analyze the results."),
                                                  br(),
                                                  h4(strong("Note: For more help on usage, please look in the 'Guide' tab."))
                                           )
                                  ),
                                  tabPanel("Main Plot", value = "main_PPCA_plot",
                                           plotOutput("PPCA_plot"),
                                           plotOutput("PPCA_influence_report"),
                                  ),
                                  tabPanel("Score Plots", value = "score_PPCA_plot",
                                           plotOutput("PPCA_plot_score"),

                                  ),
                                  tabPanel("Loading Plots", value = "loading_PPCA_plot",
                                           plotOutput("PPCA_plot_loadings"),
                                  ),

                                  tabPanel("Significant Bins Plots", value = "sig_bins_PPCA_plot",
                                           plotOutput("PPCA_plot_significant"),
                                           verbatimTextOutput("PPCA_summary_significant")
                                  ),

                                  tabPanel("BIC/PoV Plots", value = "bic_PPCA_plot",
                                           plotOutput("PPCA_plot_bic"),
                                  ),

                                  tabPanel("Likelihood Covergence Plots", value = "ll_PPCA_plot",
                                           plotOutput("PPCA_plot_ll_conv"),
                                  ),

                                  id = "analytics_plot_tabs"
                                )
                              )
             ),

             # MPCCA plots
             conditionalPanel(condition="input.analytics_method_tab=='MPPCA_tab'",
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Description", value = "MPPCA_description",
                                           column(12, align="center",
                                                  h2(strong("MPPCA is bla bla bla")),
                                           ),
                                  ),
                                  tabPanel("Main Plot"),
                                  tabPanel("Score Plot"),
                                  tabPanel("Loading Plot")


                                )
                              )
             ),

             # DPPCA plots
             conditionalPanel(condition="input.analytics_method_tab=='DPPCA_tab'",
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Description", value = "DPPCA_description",
                                           column(12, align="center",
                                                  h2(strong("DPPCA is bla bla bla")),
                                           )
                                  ),
                                  tabPanel("Main Plot"),
                                  tabPanel("Score Plot"),
                                  tabPanel("Loading Plot")


                                )
                              )
             ),

    ),


    # Guides ------------------------------------------------------------------
    tabPanel("Guides", "This panel is intentionally left blank",
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 3", "This panel is intentionally left blank",
                          textInput("txt", "Text input:", "general"),
                          tags$h5("Default actionButton:"),
                          actionButton("action", "Search"),

                          tags$h5("actionButton with CSS class:"),
                          actionButton("action2", "Action button", class = "btn-primary")
                 )
               )
             ),
    ),





    # About Us ----------------------------------------------------------------
    tabPanel("About Us", "This panel is intentionally left blank",

             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
    )
  )
))
