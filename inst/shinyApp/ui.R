library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
library(metaboliteR)

shinyUI(ui = tagList(
  # Color error output red
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))),
  # Padding for fixed top menu bar
  tags$style(type="text/css", "body {padding-top: 70px;}"),

  useShinyjs(),
  withMathJax(),

  navbarPage(id = "menu_bar",  position = "fixed-top",
             theme = shinytheme("lumen"),  # <--- To use a theme, uncomment this
             strong("Metabolomic Analytics"), # Main title name


             # HOME --------------------------------------------------------------------
             tabPanel("Home",
                      column(12, align="center",
                             img(src='logo.png', align = "right", height=120, width=100),
                             tags$img(src = "Homepage.jpg",height=120,width=800), # placeholder for banner
                             # h2(strong("Welcome to the Shiny App for Analysing Metabolomic Data Using R")),


                             # Buttons box
                             column(12, align="center",

                                    # PPCA method button
                                    column(6, align = "right",
                                           # "PPCA",
                                           br(),
                                           tags$button(
                                             id = "PPCA_method_button",
                                             class = "btn action-button",
                                             tags$img(src = "PPCA-logo.png",
                                                      height = "200px",
                                                      width = "400px"),
                                             style = "background-color:white"

                                           ),
                                           br(),
                                           # "this method is good"
                                    ),

                                    # MPPCA method button
                                    column(6, align = "left",
                                           # "MPPCA",
                                           br(),
                                           tags$button(
                                             id = "MPPCA_method_button",
                                             class = "btn action-button",
                                             tags$img(src = "MPPCA-logo.png",
                                                      height = "200px",
                                                      width = "400px"),
                                             style = "background-color:white"

                                           ),
                                           br(),
                                           # "this method is good"
                                    ),

                                    # Insert vertical spacing between buttons
                                    column(12),

                                    # DPPCA method button
                                    column(6, align = "right",
                                           # "DPPCA",
                                           br(),
                                           tags$button(
                                             id = "DPPCA_method_button",
                                             class = "btn action-button",
                                             tags$img(src = "DPPCA-logo.png",
                                                      height = "200px",
                                                      width = "400px"),
                                             style = "background-color:white"

                                           ),
                                           br(),
                                           # "this method is good"
                                    ),

                                    # Sample Size Estimation button
                                    column(6, align = "left",
                                           # "Sample Size Estimation",
                                           br(),
                                           tags$button(
                                             id = "Sample_Size_Estimation_button",
                                             class = "btn action-button",
                                             tags$img(src = "Sample_Size_Estimation-logo.png",
                                                      height = "200px",
                                                      width = "400px"),
                                             style = "background-color:white"

                                           ),
                                           br(),
                                           # "this method is good"
                                    ),

                             ),

                             column(12, align="center",
                                    br(),
                                    h4(strong("Note: For more help on usage, please look into the 'Guide' tab.")),
                             )
                      )
             ),


             # ANALYTICS ---------------------------------------------------------------
             navbarMenu("Analytics",

                        # ppca --------------------------------------------------------------------
                        tabPanel("PPCA/PPCCA",

                                 # PPCA sidebar controls
                                 sidebarPanel(width = 3,
                                              # tags$style("#PPCA_tooltip+.popover{width:170px;}"),
                                              # bsPopover("PPCA_tooltip",
                                              #           title = "Probabilistic Principal Component Analysis",
                                              #           content = "",
                                              #           trigger = "hover", placement = "right"),

                                              conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_data'",

                                                               actionButton("PPCA_parameter_button", "Go to Model Settings",
                                                                            width = "100%", class = "btn-default"),

                                                               # PPCA data inputs
                                                               fileInput("PPCA_input_file", h4("File input:", bsButton("PPCA_data_tooltip", label = "",
                                                                                                                     icon = icon("question"), size = "extra-small")),
                                                                         multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                                                         placeholder = "Enter Metabolomic Data Here"),
                                                               bsPopover("PPCA_data_tooltip", title="",
                                                                         content="Please make sure: rows are samples/observations, columns are spectral bins",
                                                                         trigger = "hover"),

                                                               # tags$hr(),
                                                               h4(helpText("Is there a header in the data?")),
                                                               checkboxInput(inputId = 'PPCA_header', label = 'Tick for Yes', value = TRUE),

                                                               radioButtons(inputId = 'PPCA_sep', label = h4(helpText("How is the data separated?")),
                                                                            inline = TRUE, choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

                                                               sliderInput("PPCA_cov_slider", h4("Covariates Columns:"), 0, 10, c(0,0)),
                                                               helpText("When covariates are selected, they will appear in the 'Covariates Data' tab"),
                                                               sliderInput("PPCA_label_slider", h4("Group Labels Column:"), 0, 10, 0),
                                                               helpText("When group labels are selected, they will appear in the 'Group Labels' tab"),
                                                               textInput("PPCA_ignore_column", h4("Columns Ignored from Spectral Data:"), "", placeholder="Column number/names (e.g: 2,3,5 or 9.98,9.94,9.86)"),

                                                               h4(helpText("Enter Desired Scaling:", bsButton("PPCA_scale_type_tooltip", label = "",
                                                                                                              icon = icon("question"), size = "extra-small"))),
                                                               bsPopover("PPCA_scale_type_tooltip", title="Types of Scaling",
                                                                         content="Choosing any scale will pre-process the data by the given formula. Further details on Guide Tab.",
                                                                         trigger = "hover"),


                                                               radioButtons(inputId = 'PPCA_scale', label = NULL,
                                                                            choiceNames =  c("None", 'PQN Scale',
                                                                                             "Auto Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}\\)",
                                                                                             "Pareto Scale \\(\\space\\) \\(\\frac{x-\\mu}{\\sqrt{\\sigma}}\\)",
                                                                                             "Range Scale \\(\\space\\) \\(\\frac{x-\\mu}{x_{max} - x_{min}}\\)",
                                                                                             "Vast Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}(\\frac{\\mu}{\\sigma}\\))"),
                                                                            choiceValues = c('none', 'PQN', 'autoscale', 'paretoscale', 'rangescale', 'vastscale'),
                                                                            selected = 'none'),
                                                               div(helpText("*μ is the column mean value while σ is the standard deviation"), style = "font-size:80%"),

                                                               tags$hr(),
                                              ),

                                              conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_description'",
                                                               actionButton("PPCA_data_button", "Go to Data Input",
                                                                            width = "100%", class = "btn-default"),
                                                               h3(helpText('Model Parameters:')),

                                                               sliderInput("PPCA_PC_slider", h4("Range of Principal Components:"),
                                                                           min = 1, max = 10, value = c(1, 4)),
                                                               sliderInput("PPCA_bootstrap_n_slider", h4("Numbers of Bootstrap:"), 2, 100, 5),
                                                               bsPopover("PPCA_bootstrap_n_slider", title="",
                                                                         content="Higher Bootstrap, Better Uncertainty but Longer Wait Time", trigger = "hover"),

                                                               h4("Do you want to include", span("Covariates?", style="color:blue")),
                                                               disabled(checkboxInput(inputId = 'PPCA_covariates_check', label = span('Tick for Yes', style="color:blue", title = "Please make sure covariates data are selected in Data tabs to enable this selection"), value = FALSE)),

                                                               radioButtons("PPCA_choose_q", label = h4("Output Optimize Model?"),
                                                                            choiceNames = c("Use Optimal PC", "Use Maximum PC"),
                                                                            choiceValues = c(TRUE, FALSE)),

                                                               div(style="display:inline-block",
                                                                   numericInput("PPCA_epsilon", label = "Covergence Criterion:", value=0.01, min=1e-5, max=1e2, ),
                                                               ),
                                                               div(style="display:inline-block",
                                                                   "\\(\\space\\space\\)",
                                                               ),
                                                               div(style="display:inline-block",
                                                                   numericInput("PPCA_max_iter", label = "Max Number of Iteration:", value=1e3, min=1, max=1e6),
                                                               ),

                                                               tags$h6("Click button to run the model using desired parameters:"),
                                                               div(style="display:inline-block",
                                                                   actionButton("PPCA_submit_para_btn", "Submit Parameters", class = "btn-primary"),
                                                               ),
                                                               div(style="display:inline-block",
                                                                   strong(textOutput("PPCA_optimal_q")),
                                                               ),

                                                               tags$hr(),
                                              ),


                                              conditionalPanel(condition="input.PPCA_plot_tabs!='PPCA_description' & input.PPCA_plot_tabs!='PPCA_data'",
                                                               # tags$br(),
                                                               actionButton("PPCA_return_param", "Return to Model Parameters",
                                                                            width = "100%", class = "btn-default"),
                                                               h3(helpText('Graph Controls:'))
                                              ),

                                              # Score & Loading
                                              conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_score_plot' || input.PPCA_plot_tabs=='PPCA_loading_plot'",
                                                               sliderInput("PPCA_x_PC", h4("X-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                                               sliderInput("PPCA_y_PC", h4("Y-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),


                                                               conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_score_plot'",
                                                                                sliderInput("PPCA_post_int", h4("Posterior Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                                                                disabled(downloadButton("dl_ppca_score_btn", span("Download", title="Download a Table of All Score Estimates."))),
                                                               ),

                                                               conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_loading_plot'",
                                                                                disabled(downloadButton("dl_ppca_loadings_btn", span("Download", title="Download a Table of All Loadings Estimates."))),
                                                               )
                                              ),


                                              # Loadings Analysis & Influence plot
                                              conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_loading_analysis_plot' || input.PPCA_plot_tabs=='PPCA_influence_plot'",
                                                               sliderInput("PPCA_spectral_plot_PC", h4("Principal Component for Plot:"), min=1, max=10, value=1, step=1, ticks=F),
                                                               sliderInput("PPCA_conf_int", h4("Confidence Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),

                                                               conditionalPanel(condition="input.PPCA_plot_tabs=='PPCA_loading_analysis_plot'",
                                                                                sliderInput("PPCA_n_load_analysis", h5("Numbers of Significant Bin for Loading Analysis Plot"), min=1, max=20, value=5, step=1, ticks=F),
                                                                                disabled(downloadButton("dl_ppca_loadings_analysis_btn", span("Download", title="Download a table of data used to produced the graph."))),

                                                                                # downloadButton("dl_significant_btn1", span("Download", title="Download a Table of All Significant Spectral Bins Names"))
                                                               )
                                              ),


                                 ),
                                 mainPanel(width = 9,

                                           # Style for influence tab, PPCA tabs
                                           tags$style(HTML("
                                                .tabbable > .nav > li > a[data-value='PPCA_influence_plot'] {color:blue}
                                                .tabbable > .nav > li > a[data-value='PPCA_spectral'] {background-color:yellow}
                                                .tabbable > .nav > li > a[data-value='PPCA_covariates'] {background-color:yellow; color:blue}
                                                .tabbable > .nav > li > a[data-value='PPCA_labels'] {background-color:yellow}
                                                .tabbable > .nav > li[class=active] > a {background-color:grey; color:white}
                                                          ")),

                                           tabsetPanel(id = "PPCA_plot_tabs", selected = "PPCA_description",
                                                       tabPanel("PPCA data", value = "PPCA_data",
                                                                br(),
                                                                tabsetPanel(id = "PPCA_data_tabs",
                                                                            tabPanel(span("Spectral Data", title="Data from NMR or Mass Spectroscopy (MS)"),
                                                                                     value = 'PPCA_spectral',
                                                                                     strong(h3(textOutput("PPCA_spectral_file_name"))),
                                                                                     dataTableOutput("PPCA_spectral_data_table"),
                                                                            ),
                                                                            tabPanel(span("Covariates Data", title="Sample attributes (eg: weight, gender, regions, etc)"),
                                                                                     value = 'PPCA_covariates',
                                                                                     # h3(textOutput("PPCA_spectral_file_name")),
                                                                                     dataTableOutput("covariates_data_table")
                                                                            ),
                                                                            tabPanel(span("Group Labels", title="This data is used for colours in plots (eg: Treatment group vs Control group)"),
                                                                                     value = 'PPCA_labels',
                                                                                     # h3(textOutput("PPCA_spectral_file_name")),
                                                                                     dataTableOutput("PPCA_labels_data_table")
                                                                            )
                                                                )
                                                       ),


                                                       tabPanel("Model Description", value = "PPCA_description",
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
                                                       tabPanel("Score Plots", value = "PPCA_score_plot",
                                                                plotOutput("PPCA_plot_score"),

                                                       ),
                                                       tabPanel("Loadings Plots", value = "PPCA_loading_plot",
                                                                plotOutput("PPCA_plot_loadings"),
                                                       ),
                                                       tabPanel("Loadings Analysis Plot", value = "PPCA_loading_analysis_plot",
                                                                plotOutput("PPCA_plot_loadings_analysis"),
                                                       ),
                                                       tabPanel("Loadings Influence Plot", value = "PPCA_influence_plot",
                                                                plotOutput("PPCA_influence_report"),
                                                       ),

                                                       tabPanel("Model Selection Plot", value = "PPCA_bic_plot",
                                                                plotOutput("PPCA_plot_bic"),
                                                       ),

                                                       tabPanel("Algorithm Covergence Plot", value = "PPCA_ll_plot",
                                                                plotOutput("PPCA_plot_ll_conv"),
                                                       )
                                           )
                                 )
                        ),

                        # mppca -------------------------------------------------------------------
                        tabPanel("MPPCA",
                                 sidebarPanel(width=3,
                                              # tags$div("MPPCA",
                                              #          title="Mixed PPCA that deals with multiple groups"),
                                              # value = "MPPCA_tab",


                                              conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_data'",


                                                               actionButton("MPPCA_parameter_button", "Go to Model Settings",
                                                                            width = "100%", class = "btn-default"),

                                                               # MPPCA data inputs
                                                               fileInput("MPPCA_input_file", h4("File input:", bsButton("MPPCA_input_data_tooltip", label = "",
                                                                                                                           icon = icon("question"), size = "extra-small")),
                                                                         multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                                                         placeholder = "Enter Metabolomic Data Here"),
                                                               bsPopover("MPPCA_input_data_tooltip", title="",
                                                                         content="Please make sure: rows are samples/observations, columns are spectral bins",
                                                                         trigger = "hover"),

                                                               # tags$hr(),
                                                               h4(helpText("Is there a header in the data?")),
                                                               checkboxInput(inputId = 'MPPCA_header', label = 'Tick for Yes', value = TRUE),

                                                               radioButtons(inputId = 'MPPCA_sep', label = h4(helpText("How is the data separated?")),
                                                                            inline = TRUE, choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

                                                               sliderInput("MPPCA_cov_slider", h4("Covariates Columns:"), 0, 10, c(0,0)),
                                                               # helpText("When covariates are selected, they will appear in the 'Covariates Data' tab"),

                                                               sliderInput("MPPCA_label_slider", h4("Group Labels Column:"), 0, 10, 0),
                                                               helpText("When group labels are selected, they will appear in the 'Group Labels' tab"),
                                                               textInput("MPPCA_ignore_column", h4("Columns Ignored from Spectral Data:"), "", placeholder="Column number/names (e.g: 2,3,5 or 9.98,9.94,9.86)"),

                                                               h4(helpText("Enter Desired Scaling:", bsButton("MPPCA_scale_type_tooltip", label = "",
                                                                                                              icon = icon("question"), size = "extra-small"))),
                                                               bsPopover("MPPCA_scale_type_tooltip", title="Types of Scaling",
                                                                         content="Choosing any scale will pre-process the data by the given formula. Further details on Guide Tab.",
                                                                         trigger = "hover"),


                                                               radioButtons(inputId = 'MPPCA_scale', label = NULL,
                                                                            choiceNames =  c("None", 'PQN Scale',
                                                                                             "Auto Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}\\)",
                                                                                             "Pareto Scale \\(\\space\\) \\(\\frac{x-\\mu}{\\sqrt{\\sigma}}\\)",
                                                                                             "Range Scale \\(\\space\\) \\(\\frac{x-\\mu}{x_{max} - x_{min}}\\)",
                                                                                             "Vast Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}(\\frac{\\mu}{\\sigma}\\))"),
                                                                            choiceValues = c('none', 'PQN', 'autoscale', 'paretoscale', 'rangescale', 'vastscale'),
                                                                            selected = 'none'),
                                                               div(helpText("*μ is the column mean value while σ is the standard deviation"), style = "font-size:80%"),

                                                               tags$hr(),
                                              ),


                                              conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_description'",

                                                               actionButton("MPPCA_data_button", "Go to Data Input",
                                                                            width = "100%", class = "btn-default"),

                                                               h3(helpText('Model Parameters:')),

                                                               sliderInput("MPPCA_group_slider", h4("Range of Groups Estimated:"),
                                                                           min = 1, max = 10, value = c(1,4)),
                                                               sliderInput("MPPCA_PC_slider", h4("Range of Principal Components:"),
                                                                           min = 1, max = 10, value = c(1, 4)),
                                                               sliderInput("MPPCA_bootstrap_n_slider", h4("Numbers of Bootstrap:"), 2, 100, 5),
                                                               bsTooltip("MPPCA_bootstrap_n_slider", title="Higher Bootstrap, Better Uncertainty but Longer Wait Time", trigger = "hover"),

                                                               # h4("Include Covariates?"),
                                                               # disabled(checkboxInput(inputId = 'covariates_mix_check', label = span('Tick for Yes', title = "Please make sure covariates data are selected in Data tabs to enable this selection"), value = FALSE)),

                                                               # radioButtons("choose_q", label = h4("Output Optimize Model?"), choices = c(Use_Optimal_PC=TRUE, Use_Maximum_PC=FALSE)),

                                                               div(style="display:inline-block",
                                                                   numericInput("MPPCA_epsilon", label = "Covergence Criterion:", value=0.01, min=1e-5, max=1e2),
                                                               ),
                                                               # div(style="display:inline-block",
                                                               #     "\\(\\space\\space\\)",
                                                               # ),
                                                               # div(style="display:inline-block",
                                                               #     numericInput("max_iter_mix", label = "Max Number of Iteration:", value=1e3, min=1, max=1e6),
                                                               # ),

                                                               tags$h6("Click button to run the model using desired parameters:"),
                                                               div(style="display:inline-block",
                                                                   actionButton("MPPCA_submit_para_btn", "Submit Parameters", class = "btn-primary"),
                                                               ),
                                                               div(style="display:inline-block",
                                                                   strong(textOutput("MPPCA_optimal_q")),
                                                               ),
                                                               div(style="display:inline-block",
                                                                   strong(textOutput("MPPCA_optimal_g")),
                                                               ),

                                                               tags$hr(),
                                              ),

                                              # Graph controls
                                              conditionalPanel(condition="input.MPPCA_plot_tabs!='MPPCA_description' & input.MPPCA_plot_tabs!='MPPCA_data'",
                                                               # tags$br(),
                                                               actionButton("MPPCA_return_param", "Return to Parameters",
                                                                            width = "100%", class = "btn-default"),
                                                               h3(helpText('Graph Controls:'))
                                              ),

                                              # MPPCA Score
                                              conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_score_plot'",
                                                               sliderInput("MPPCA_x_PC", h4("X-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                                               sliderInput("MPPCA_y_PC", h4("Y-axis Principal Component:"), min=1, max=10, value=4, step=1, ticks=F),
                                                               sliderInput("MPPCA_post_int", h4("Posterior Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                                               disabled(downloadButton("dl_mppca_score_btn", span("Download", title="Download a Table of All Score Estimates."))),


                                              ),

                                              # MPPCA Loadings & Loading Analysis plot
                                              conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_loading_plot' || input.MPPCA_plot_tabs=='MPPCA_loading_analysis_plot'",
                                                               sliderInput("MPPCA_group", h4("Group Selected:"), min=1, max=10, value=4, step=1),

                                                               conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_loading_analysis_plot'",
                                                                                sliderInput("MPPCA_analysis_plot_PC", h4("Principal Component for Plot:"), min=1, max=10, value=1, step=1, ticks=F),
                                                                                sliderInput("MPPCA_conf_int", h4("Confidence Interval:"), min=0.8, max=0.9999, value=0.95, ticks=F),
                                                                                sliderInput("MPPCA_n_load_analysis", h5("Numbers of Significant Bin for Loading Analysis Plot"), min=1, max=20, value=5, step=1, ticks=F),
                                                                                disabled(downloadButton("dl_mppca_loadings_analysis_btn", span("Download", title="Download a table of data used to produced the graph."))),

                                                               ),

                                                               conditionalPanel(condition="input.MPPCA_plot_tabs=='MPPCA_loading_plot'",
                                                                                disabled(downloadButton("dl_mppca_loadings_btn", span("Download", title="Download a Table of All Loadings Estimates."))),

                                                               )
                                              ),

                                 ),

                                 # MPCCA plots
                                 mainPanel(width = 9,

                                           # Style for MPPCA tabs
                                           tags$style(HTML("
                                                  .tabbable > .nav > li > a[data-value='MPPCA_spectral'] {background-color:yellow}
                                                  .tabbable > .nav > li > a[data-value='MPPCA_covariates'] {background-color:yellow; color:blue}
                                                  .tabbable > .nav > li > a[data-value='MPPCA_labels'] {background-color:yellow}
                                                  .tabbable > .nav > li[class=active] > a {background-color:grey; color:white}
                                                        ")),



                                           tabsetPanel(id = "MPPCA_plot_tabs", selected = "MPPCA_description",
                                                       tabPanel("MPPCA data", value = "MPPCA_data",
                                                                br(),
                                                                tabsetPanel(id = "MPPCA__data_tabs",
                                                                            tabPanel(span("Spectral Data", title="Data from NMR or Mass Spectroscopy (MS)"),
                                                                                     value = 'MPPCA_spectral',
                                                                                     strong(h3(textOutput("MPPCA_spectral_file_name"))),
                                                                                     dataTableOutput("MPPCA_spectral_data_table"),
                                                                            ),

                                                                            # tabPanel(span("Covariates Data", title="Sample attributes (eg: weight, gender, regions, etc)"),
                                                                            #          value = 'MPPCA_covariates',
                                                                            #          dataTableOutput("MPPCA_covariates_data_table")
                                                                            # ),

                                                                            tabPanel(span("Group Labels", title="This data is used for colours in plots (eg: Treatment group vs Control group)"),
                                                                                     value = 'MPPCA_labels',
                                                                                     dataTableOutput("MPPCA_labels_data_table")
                                                                            )
                                                                )
                                                       ),
                                                       tabPanel("Description", value = "MPPCA_description",
                                                                br(),
                                                                bsCollapse(id = "MPPCA_desc", open = "MPPCA Usage Guide",
                                                                           bsCollapsePanel("MPPCA Usage Guide",

                                                                                           includeHTML("MPPCA_desc_ug.html"),
                                                                                           style = "primary"),
                                                                           bsCollapsePanel("MPPCA Conceptual Explanation",

                                                                                           # includeHTML("MPPCA_desc_ce.html"),
                                                                                           style = "info"),
                                                                           bsCollapsePanel("MPPCA Technical Details",

                                                                                           includeHTML("MPPCA_desc_td.html"),
                                                                                           style = "info")
                                                                )
                                                       ),


                                                       tabPanel("Score Plot", value = "MPPCA_score_plot",
                                                                plotOutput("MPPCA_plot_score")
                                                       ),

                                                       tabPanel("Loading Plot", value = "MPPCA_loading_plot",
                                                                plotOutput("MPPCA_plot_loadings")
                                                       ),

                                                       tabPanel("Loading Analysis Plot", value = "MPPCA_loading_analysis_plot",
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

                                                       tabPanel("BIC Plot", value = "MPPCA_BIC_plot",
                                                                plotOutput("MPPCA_plot_BIC")
                                                       )
                                           )
                                 )
                        ),



                        # dppca -------------------------------------------------------------------
                        tabPanel("DPPCA",

                                 # DPPCA sidebar controls
                                 sidebarPanel(width = 3,
                                              # tags$style("#DPPCA_tooltip+.popover{width:170px;}"),
                                              # bsPopover("DPPCA_tooltip",
                                              #           title = "Dynamic Probabilistic Principal Component Analysis",
                                              #           content = "",
                                              #           trigger = "hover", placement = "right"),

                                              conditionalPanel(condition="input.DPPCA_plot_tabs=='DPPCA_data'",

                                                               actionButton("DPPCA_parameter_button", "Go to Model Settings",
                                                                            width = "100%", class = "btn-default"),

                                                               # DPPCA data inputs
                                                               fileInput("DPPCA_input_file", h4("File input:", bsButton("DPPCA_data_tooltip", label = "",
                                                                                                                       icon = icon("question"), size = "extra-small")),
                                                                         multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                                                         placeholder = "Enter Metabolomic Data Here"),
                                                               bsPopover("DPPCA_data_tooltip", title="",
                                                                         content="Please make sure: rows are samples/observations, columns are spectral bins",
                                                                         trigger = "hover"),

                                                               # tags$hr(),
                                                               h4(helpText("Is there a header in the data?")),
                                                               checkboxInput(inputId = 'DPPCA_header', label = 'Tick for Yes', value = TRUE),

                                                               radioButtons(inputId = 'DPPCA_sep', label = h4(helpText("How is the data separated?")),
                                                                            inline = TRUE, choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

                                                               # sliderInput("DPPCA_subjectID_slider", h4("Subject ID Column:"), 0, 10, 0),
                                                               sliderInput("DPPCA_time_slider", h4("Time Column:"), 0, 10, 3),
                                                               helpText("Specify Time Column to enable time selection below"),
                                                               selectInput( "DPPCA_time_selected", label = "Time Point Selected for Data Shown", choices = NULL),


                                                               sliderInput("DPPCA_label_slider", h4("Group Labels Column:"), 0, 10, 4),
                                                               helpText("When group labels are selected, they will appear in the 'Group Labels' tab"),
                                                               textInput("DPPCA_ignore_column", h4("Columns Ignored from Spectral Data:"),
                                                                         value = "1,2",
                                                                         placeholder="Column number/names (e.g: 2,3,5 or 9.98,9.94,9.86)"),

                                                               h4(helpText("Enter Desired Scaling:", bsButton("DPPCA_scale_type_tooltip", label = "",
                                                                                                              icon = icon("question"), size = "extra-small"))),
                                                               bsPopover("DPPCA_scale_type_tooltip", title="Types of Scaling",
                                                                         content="Choosing any scale will pre-process the data by the given formula. Further details on Guide Tab.",
                                                                         trigger = "hover"),


                                                               radioButtons(inputId = 'DPPCA_scale', label = NULL,
                                                                            choiceNames =  c("None", 'PQN Scale',
                                                                                             "Auto Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}\\)",
                                                                                             "Pareto Scale \\(\\space\\) \\(\\frac{x-\\mu}{\\sqrt{\\sigma}}\\)",
                                                                                             "Range Scale \\(\\space\\) \\(\\frac{x-\\mu}{x_{max} - x_{min}}\\)",
                                                                                             "Vast Scale \\(\\quad\\) \\(\\frac{x-\\mu}{\\sigma}(\\frac{\\mu}{\\sigma}\\))"),
                                                                            choiceValues = c('none', 'PQN', 'autoscale', 'paretoscale', 'rangescale', 'vastscale'),
                                                                            selected = 'none'),
                                                               div(helpText("*μ is the column mean value while σ is the standard deviation"), style = "font-size:80%"),


                                                               tags$hr(),
                                              ),
                                 ),

                                 mainPanel(width = 9,
                                           # Style for DPPCA tabs
                                           tags$style(HTML("
                                                .tabbable > .nav > li > a[data-value='DPPCA_original'] {background-color:yellow}
                                                .tabbable > .nav > li > a[data-value='DPPCA_spectral'] {background-color:yellow}
                                                .tabbable > .nav > li > a[data-value='DPPCA_labels'] {background-color:yellow}
                                                .tabbable > .nav > li[class=active] > a {background-color:grey; color:white}
                                                        ")),
                                           tabsetPanel(id = "DPPCA_plot_tabs",
                                             tabPanel("DPPCA data", value = "DPPCA_data",
                                                      br(),
                                                      tabsetPanel(id = "DPPCA_data_tabs",
                                                                  # tabPanel(span("Original Data", title="Data from NMR or Mass Spectroscopy (MS)"),
                                                                  #          value = 'DPPCA_original',
                                                                  #          strong(h3(textOutput("DPPCA_spectral_file_name"))),
                                                                  #          dataTableOutput("DPPCA_original_data_table"),
                                                                  # ),
                                                                  tabPanel("Spectral Data",
                                                                           value = 'DPPCA_spectral',
                                                                           strong(h3(textOutput("DPPCA_spectral_file_name"))),
                                                                           h4(textOutput("DPPCA_time_point_showing")),
                                                                           dataTableOutput("DPPCA_spectral_data_table")
                                                                  ),

                                                                  tabPanel(span("Group Labels", title="This data is used for colours in plots (eg: Treatment group vs Control group)"),
                                                                           value = 'DPPCA_labels',
                                                                           dataTableOutput("DPPCA_labels_data_table")
                                                                  ),

                                                                  tabPanel("Check with Luiza",
                                                                           verbatimTextOutput("DPPCA_data_output_for_model")
                                                                  )
                                                      )
                                             ),


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

                        )

             ),


             # TOOLS -------------------------------------------------------------------
             navbarMenu("Tools",

                        ### Sample Size Estimation --------------------------------------------------
                        tabPanel("Sample Size Estimation",
                                 sidebarPanel(width = 3,
                                              "METSIZER PLACEHOLDER"
                                 ),

                                 mainPanel(width = 9,
                                           tabsetPanel(
                                             tabPanel("METSIZER plots placeholder")
                                           )
                                 ),
                        )
             ),


             # GUIDES ------------------------------------------------------------------
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
                                ),
                                verbatimTextOutput("debug")
                      ),
             ),


             # ABOUT US ----------------------------------------------------------------
             tabPanel("About Us", "This panel is intentionally left blank",

                      selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
                      verbatimTextOutput("summary"),
                      tableOutput("table")
             )
  )
))
