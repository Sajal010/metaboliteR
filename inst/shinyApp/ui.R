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
  useShinyjs(),
  withMathJax(),

  navbarPage(
    theme = shinytheme("lumen"),  # <--- To use a theme, uncomment this
    strong("Metabolomic Analytics"), # Main title name

    # Home --------------------------------------------------------------------
    tabPanel("Home",
             column(12, align="center",
                    # img(src='logo.png', align = "right", height=120, width=100),
                    h2(strong("Welcome to the Shiny App for Analysing Metabolomic Data Using R")),
                    tags$img(src = "Homepage.jpg",height=627,width=400),
                    h4(strong("Note: For more help on usage, please look into the 'Guide' tab and 'Vignette' for package."))
             )
    ),



    # Design -----------------------------------------------------
    tabPanel("Design",
             sidebarPanel(width = 3,
                          "METSIZER PLACEHOLDER"
             ),

             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("METSIZER plots placeholder")
                       )
             ),
    ),


    # Data --------------------------------------------------------------------
    tabPanel("Data",
             sidebarPanel(width = 3,

                          tabsetPanel(id = "data_type",

                            # PPCA data inputs
                            tabPanel("PPCA/MPPCA",
                                     fileInput("main_file", h4("File input:", bsButton("main_data_tooltip", label = "",
                                                                                       icon = icon("question"), size = "extra-small")),
                                               multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                               placeholder = "Enter Metabolomic Data Here"),
                                     bsPopover("main_data_tooltip", title="",
                                               content="Please make sure: rows are samples/observations, columns are spectral bins",
                                               trigger = "hover"),

                                     # tags$hr(),
                                     h4(helpText("Is there a header in the data?")),
                                     checkboxInput(inputId = 'header', label = 'Tick for Yes', value = TRUE),

                                     radioButtons(inputId = 'sep', label = h4(helpText("How is the data separated?")),
                                                  inline = TRUE, choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

                                     sliderInput("cov_slider", h4("Covariates Columns:"), 0, 10, c(0,0)),
                                     helpText("When covariates are selected, they will appear in the 'Covariates Data' tab"),
                                     sliderInput("label_slider", h4("Group Labels Columns:"), 0, 10, 0),
                                     helpText("When group labels are selected, they will appear in the 'Group Labels' tab"),
                                     textInput("ignore_column", h4("Columns Ignored from Main Data:"), "", placeholder="Column number/names (e.g: 2,3,5 or 9.98,9.94,9.86)"),

                                     h4(helpText("Enter Desired Scaling:", bsButton("scale_type_tooltip", label = "",
                                                                                    icon = icon("question"), size = "extra-small"))),
                                     bsPopover("scale_type_tooltip", title="Types of Scaling",
                                               content="Choosing any scale will pre-process the data by the given formula. Further details on Guide Tab.",
                                               trigger = "hover"),


                                     radioButtons(inputId = 'scale', label = NULL,
                                                  choiceNames =  c("None", "Auto Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}\\)",
                                                                   "Pareto Scale \\(\\space\\) \\(\\frac{x-\\mu}{\\sqrt{\\sigma}}\\)",
                                                                   "Range Scale \\(\\space\\) \\(\\frac{x-\\mu}{x_{max} - x_{min}}\\)",
                                                                   "Vast Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}(\\frac{\\mu}{\\sigma}\\))"),
                                                  choiceValues = c('none', 'autoscale', 'paretoscale', 'rangescale', 'vastscale'),
                                                  selected = 'none'),
                                     div(helpText("*μ is the column mean observation while σ is the standard deviation"), style = "font-size:80%"),

                                     tags$hr(),
                            ),
                            tabPanel("DPPCA",)

                          )

             ),
             mainPanel(width = 9,
                       conditionalPanel(condition="input.data_type=='PPCA/MPPCA'",
                                        tabsetPanel(id = "ppca_data_tabs",
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
                                                    )
                                        )
                       ),
                       conditionalPanel(condition="input.data_type=='DPPCA'",
                                        tabsetPanel(id = "dppca_data_tabs",
                                                    tabPanel(span("Main Data", title="Data from NMR or Mass Spectroscopy (MS)"),
                                                             value = 'dppca_main',
                                                             # strong(h3(textOutput("main_file_name"))),
                                                             # dataTableOutput("main_data_table"),
                                                    ),
                                                    tabPanel(span("Covariates Data", title="Sample attributes (eg: weight, gender, regions, etc)"),
                                                             value = 'dppca_covariates',
                                                             # h3(textOutput("covariates_file_name")),
                                                             # dataTableOutput("covariates_data_table")
                                                    ),
                                                    tabPanel(span("Group Labels", title="This data is used for colours in plots (eg: Treatment group vs Control group)"),
                                                             value = 'dppca_labels',
                                                             # h3(textOutput("labels_file_name")),
                                                             # dataTableOutput("labels_data_table")
                                                    )
                                        )
                       ),


             )
    ),



    # Analytics ---------------------------------------------------------------
    tabPanel("Analytics",
             sidebarPanel(width = 3,
               tabsetPanel(id = "analytics_method_tab",
                 # PPCA plot controls
                 tabPanel(div("PPCA",id="PPCA_tooltip"), value = "PPCA_tab",
                          bsTooltip("PPCA_tooltip", title="Simple PPCA", trigger = "hover"),

                          conditionalPanel(condition="input.analytics_plot_tabs=='PPCA_description'",
                                           h3(helpText('Model Parameters:')),

                                           sliderInput("PC_slider", h4("Range of Principal Components:"),
                                                       min = 1, max = 10, value = c(1, 4)),
                                           sliderInput("bootstrap_n_slider", h4("Numbers of Bootstrap:"), 2, 100, 5),
                                           bsTooltip("bootstrap_n_slider", title="Higher Bootstrap, Better Uncertainty but Longer Wait Time", trigger = "hover"),

                                           h4("Do you want to include", span("Covariates?", style="color:blue")),
                                           disabled(checkboxInput(inputId = 'covariates_check', label = span('Tick for Yes', style="color:blue", title = "Please make sure covariates data are selected in Data tabs to enable this selection"), value = FALSE)),

                                           radioButtons("choose_q", label = h4("Output Optimize Model?"),
                                                        choiceNames = c("Use Optimal PC", "Use Maximum PC"),
                                                        choiceValues = c(TRUE, FALSE)),

                                           div(style="display:inline-block",
                                               numericInput("epsilon", label = "Covergence Criterion:", value=0.01, min=1e-5, max=1e2, ),
                                           ),
                                           div(style="display:inline-block",
                                               "\\(\\space\\space\\)",
                                           ),
                                           div(style="display:inline-block",
                                               numericInput("max_iter", label = "Max Number of Iteration:", value=1e3, min=1, max=1e6),
                                           ),

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
                                           tags$br(),
                                           actionButton("return_param", "Return to Parameters", class = "btn-default"),
                                           h3(helpText('Graph Controls:'))
                          ),

                          # Score & Loading
                          conditionalPanel(condition="input.analytics_plot_tabs=='score_PPCA_plot' || input.analytics_plot_tabs=='loading_PPCA_plot'",
                                           sliderInput("x_PC", h4("X-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                           sliderInput("y_PC", h4("Y-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),


                                           conditionalPanel(condition="input.analytics_plot_tabs=='score_PPCA_plot'",
                                                            sliderInput("post_int", h4("Posterior Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                                            disabled(downloadButton("dl_ppca_score_btn", span("Download", title="Download a Table of All Score Estimates."))),
                                           ),

                                           conditionalPanel(condition="input.analytics_plot_tabs=='loading_PPCA_plot'",
                                                            disabled(downloadButton("dl_ppca_loadings_btn", span("Download", title="Download a Table of All Loadings Estimates."))),
                                           )
                          ),


                          # Loadings Analysis & Influence plot
                          conditionalPanel(condition="input.analytics_plot_tabs=='loading_analysis_PPCA_plot' || input.analytics_plot_tabs=='influence_PPCA_plot'",
                                           sliderInput("main_plot_PC", h4("Principal Component for Plot:"), min=1, max=10, value=1, step=1, ticks=F),
                                           sliderInput("conf_int", h4("Confidence Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),

                                           conditionalPanel(condition="input.analytics_plot_tabs=='loading_analysis_PPCA_plot'",
                                                            sliderInput("n_main", h5("Numbers of Significant Bin for Main Plot"), min=1, max=20, value=5, step=1, ticks=F),
                                                            disabled(downloadButton("dl_ppca_loadings_analysis_btn", span("Download", title="Download a table of data used to produced the graph."))),

                                                            # downloadButton("dl_significant_btn1", span("Download", title="Download a Table of All Significant Spectral Bins Names"))
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
                          bsTooltip("PPCA_tooltip", title="Simple PPCA", trigger = "hover"),

                          conditionalPanel(condition="input.analytics_mix_plot_tabs=='MPPCA_description'",
                                           h3(helpText('Model Parameters:')),

                                           sliderInput("group_mix_slider", h4("Range of Groups Estimated:"),
                                                       min = 1, max = 10, value = c(1,4)),
                                           sliderInput("PC_mix_slider", h4("Range of Principal Components:"),
                                                       min = 1, max = 10, value = c(1, 4)),
                                           sliderInput("bootstrap_mix_n_slider", h4("Numbers of Bootstrap:"), 2, 100, 5),
                                           bsTooltip("bootstrap_mix_n_slider", title="Higher Bootstrap, Better Uncertainty but Longer Wait Time", trigger = "hover"),

                                           # h4("Include Covariates?"),
                                           # disabled(checkboxInput(inputId = 'covariates_mix_check', label = span('Tick for Yes', title = "Please make sure covariates data are selected in Data tabs to enable this selection"), value = FALSE)),

                                           # radioButtons("choose_q", label = h4("Output Optimize Model?"), choices = c(Use_Optimal_PC=TRUE, Use_Maximum_PC=FALSE)),

                                           div(style="display:inline-block",
                                               numericInput("epsilon_mix", label = "Covergence Criterion:", value=0.01, min=1e-5, max=1e2),
                                           ),
                                           div(style="display:inline-block",
                                               "\\(\\space\\space\\)",
                                           ),
                                           div(style="display:inline-block",
                                               numericInput("max_iter_mix", label = "Max Number of Iteration:", value=1e3, min=1, max=1e6),
                                           ),

                                           tags$h6("Click button to run the model using desired parameters:"),
                                           div(style="display:inline-block",
                                               actionButton("submit_mix_para_btn", "Submit Parameters", class = "btn-primary"),
                                           ),
                                           div(style="display:inline-block",
                                               strong(textOutput("optimal_mix_q")),
                                           ),
                                           div(style="display:inline-block",
                                               strong(textOutput("optimal_mix_g")),
                                           ),

                                           tags$hr(),
                          ),

                          # Graph controls
                          conditionalPanel(condition="input.analytics_mix_plot_tabs!='MPPCA_description'",
                                           tags$br(),
                                           actionButton("return_mix_param", "Return to Parameters", class = "btn-default"),
                                           h3(helpText('Graph Controls:'))
                          ),

                          # MPPCA Score
                          conditionalPanel(condition="input.analytics_mix_plot_tabs=='score_MPPCA_plot'",
                                           sliderInput("x_mix_PC", h4("X-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                           sliderInput("y_mix_PC", h4("Y-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                           sliderInput("post_mix_int", h4("Posterior Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                           disabled(downloadButton("dl_mppca_score_btn", span("Download", title="Download a Table of All Score Estimates."))),


                          ),

                          # MPPCA Loadings & Loading Analysis plot
                          conditionalPanel(condition="input.analytics_mix_plot_tabs=='loading_MPPCA_plot' || input.analytics_mix_plot_tabs=='loading_analysis_MPPCA_plot'",
                                           sliderInput("group_mix", h4("Group Selected:"), min=1, max=10, value=4, step=1),

                                           conditionalPanel(condition="input.analytics_mix_plot_tabs=='loading_analysis_MPPCA_plot'",
                                                            sliderInput("analysis_mix_plot_PC", h4("Principal Component for Plot:"), min=1, max=10, value=1, step=1, ticks=F),
                                                            sliderInput("conf_mix_int", h4("Confidence Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                                            sliderInput("n_mix_main", h5("Numbers of Significant Bin for Main Plot"), min=1, max=20, value=5, step=1, ticks=F),
                                                            disabled(downloadButton("dl_mppca_loadings_analysis_btn", span("Download", title="Download a table of data used to produced the graph."))),

                                           ),

                                           conditionalPanel(condition="input.analytics_mix_plot_tabs=='loading_MPPCA_plot'",
                                                            disabled(downloadButton("dl_mppca_loadings_btn", span("Download", title="Download a Table of All Loadings Estimates."))),

                                           )
                          ),

                 ),

                 # DPPCA plot controls
                 tabPanel(tags$div("DPPCA", title="Dynamic PPCA"),
                          value = "DPPCA_tab",
                          "Placeholder for Dynamic PPCA")

               ),

             ),

             # PPCA Plots
             conditionalPanel(condition="input.analytics_method_tab=='PPCA_tab'",
                              mainPanel(width = 9,

                                        # Style for influence tab
                                        tags$style(HTML("
                                        .tabbable > .nav > li > a[data-value='influence_PPCA_plot'] {color:blue}
                                                        ")),

                                tabsetPanel(id = "analytics_plot_tabs",
                                  tabPanel("Description", value = "PPCA_description",
                                           br(),
                                           bsCollapse(id = "PPCA_desc", open = "PPCA/PPCCA Usage Guide",
                                                      bsCollapsePanel("PPCA/PPCCA Usage Guide",

                                                                      includeHTML("PPCA_desc_ug.html"),
                                                                      style = "primary"),
                                                      bsCollapsePanel("PPCA/PPCCA Conceptual Explanation",

                                                                      includeHTML("PPCA_desc_ce.html"),
                                                                      style = "info"),
                                                      bsCollapsePanel("PPCA/PPCCA Technical Details",

                                                                      includeHTML("PPCA_desc_td.html"),
                                                                      style = "info")
                                           )

                                  ),
                                  tabPanel("Score Plots", value = "score_PPCA_plot",
                                           plotOutput("PPCA_plot_score"),

                                  ),
                                  tabPanel("Loadings Plots", value = "loading_PPCA_plot",
                                           plotOutput("PPCA_plot_loadings"),
                                  ),
                                  tabPanel("Loadings Analysis Plot", value = "loading_analysis_PPCA_plot",
                                           plotOutput("PPCA_plot_loadings_analysis"),
                                  ),
                                  tabPanel("Loadings Influence Plot", value = "influence_PPCA_plot",
                                           plotOutput("PPCA_influence_report"),
                                  ),

                                  # tabPanel("Significant Bins Plots", value = "sig_bins_PPCA_plot",
                                  #          plotOutput("PPCA_plot_significant"),
                                  #          verbatimTextOutput("PPCA_summary_significant")
                                  # ),

                                  tabPanel("Model Selection Plot", value = "bic_PPCA_plot",
                                           plotOutput("PPCA_plot_bic"),
                                  ),

                                  tabPanel("Algorithm Covergence Plot", value = "ll_PPCA_plot",
                                           plotOutput("PPCA_plot_ll_conv"),
                                  )
                                )
                              )
             ),


             # MPCCA plots
             conditionalPanel(condition="input.analytics_method_tab=='MPPCA_tab'",
                              mainPanel(width = 9,
                                tabsetPanel(id = "analytics_mix_plot_tabs",
                                  tabPanel("Description", value = "MPPCA_description",
                                           column(12, align="center",
                                                  h2(strong("MPPCA is bla bla bla")),
                                           ),
                                  ),


                                  tabPanel("Score Plot", value = "score_MPPCA_plot",
                                           plotOutput("MPPCA_plot_score")
                                  ),

                                  tabPanel("Loading Plot", value = "loading_MPPCA_plot",
                                           plotOutput("MPPCA_plot_loadings")
                                  ),

                                  tabPanel("Loading Analysis Plot", value = "loading_analysis_MPPCA_plot",
                                           plotOutput("MPPCA_plot_loadings_analysis")
                                  ),

                                  # tabPanel("Contingency Table", value = "contin_MPPCA_table",
                                  #          "If Group Labels are not given, error will appear as:",
                                  #          tags$br(),
                                  #          "Error: all arguments must have the same length",
                                  #          tags$br(),
                                  #          "Fix it by selecting Group Labels Column in 'Data' tab.",
                                  #          verbatimTextOutput("MPPCA_contin_table")
                                  # ),

                                  tabPanel("BIC Plot", value = "BIC_MPPCA_plot",
                                           plotOutput("MPPCA_plot_BIC")
                                  )
                                )
                              )
             ),

             # DPPCA plots
             conditionalPanel(condition="input.analytics_method_tab=='DPPCA_tab'",
                              mainPanel(width = 9,
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
             mainPanel(width = 9,
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
