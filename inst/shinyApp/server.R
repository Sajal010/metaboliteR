library(shiny)

default_urine_data <- UrineSpectra[[1]]
default_brain_data <- BrainSpectra[[1]]
default_dynamic_data <- UrineDynamic

shinyServer(function(input, output, session) {


    # HOME --------------------------------------------------------------------

    # PPCA button
    observeEvent(input$PPCA_method_button,
                 isolate({
                     updateNavbarPage(session, "menu_bar",
                                      selected = "PPCA/PPCCA")
                     updateTabsetPanel(session, "PPCA_plot_tabs",
                                       selected = "PPCA_data")
                 })
    )

    # MPPCA button
    observeEvent(input$MPPCA_method_button,
                 isolate({
                     updateNavbarPage(session, "menu_bar",
                                      selected = "MPPCA")
                     updateTabsetPanel(session, "MPPCA_plot_tabs",
                                       selected = "MPPCA_data")
                 })
    )

    # DPPCA button
    observeEvent(input$DPPCA_method_button,
                 isolate({
                     updateNavbarPage(session, "menu_bar",
                                      selected = "DPPCA")
                     updateTabsetPanel(session, "DPPCA_plot_tabs",
                                       selected = "DPPCA_data")
                 })
    )

    # Sample Size Estimation button
    observeEvent(input$Sample_Size_Estimation_button,
                 isolate({
                     updateNavbarPage(session, "menu_bar",
                                      selected = "Sample Size Estimation")
                 })
    )


    # DATA --------------------------------------------------------------------

    # ppca --------------------------------------------------------------------

    ## PPCA Original Data
    original_data <- reactive({
        file1 <- input$PPCA_input_file
        if(is.null(file1)){return(default_urine_data)}
        ori_data <- read.table(file=file1$datapath, sep=input$PPCA_sep, header = input$PPCA_header, check.names = F)
        updateSliderInput(session, "PPCA_cov_slider", max = ceiling(ncol(ori_data)/10))
        ori_data_rows <- nrow(ori_data)
        updateSliderInput(session, "PPCA_bootstrap_n_slider", min = ori_data_rows, value = ori_data_rows)
        ori_data
    })

    ## PPCA Spectral Data
    PPCA_spectral_data <- reactive({ # Read data
        main_columns <- 1:ncol(original_data())
        main_columns <- as.numeric(na.omit(main_columns[!colnames(original_data()) %in% unlist(strsplit(input$PPCA_ignore_column, ","))])) # remove ignore by colname
        main_columns <- main_columns[!main_columns %in% input$PPCA_cov_slider[1]:input$PPCA_cov_slider[2]] # remove covariates
        main_columns <- main_columns[main_columns!=input$PPCA_label_slider] # remove label
        main_columns <- main_columns[!main_columns %in% as.numeric(unlist(strsplit(input$PPCA_ignore_column, ",")))] # remove ignore by col number
        if(input$PPCA_scale=='PQN'){
            a<-scale_data(original_data()[, main_columns],"PQN")
            b<-as.data.frame(a$newXtrain)
        }
        else
            apply(original_data()[, main_columns], 2,function (y) scale_data(y,input$PPCA_scale))
        #original_data()[, main_columns]
    })

    # This reactive output contains the dataset and display the dataset in table format
    output$PPCA_spectral_data_table <- renderDataTable({ # Render Table
        if(is.null(PPCA_spectral_data())){return ()}
        # head(main_data(), input$lab_slider)
        PPCA_spectral_data()
    }, options = list(pageLength=10), escape = FALSE)

    PPCA_spectral_file_name <- reactive({ # Obtain file name
        inFile <- input$PPCA_input_file
        if (is.null(inFile))
            return('Urine Data from Metabol Analyze Package')
        return (inFile$name)
    })
    output$PPCA_spectral_file_name <- renderText({ PPCA_spectral_file_name() })

    ## PPCA Covariates Data
    PPCA_covariates_data <- reactive({ # Read data
        if(input$PPCA_cov_slider[1]==input$PPCA_cov_slider[2]) {
            cov_data <- as.data.frame(original_data()[, input$PPCA_cov_slider[1]])
            colnames(cov_data) <- colnames(original_data())[input$PPCA_cov_slider[1]]
            cov_data

        } else {
            original_data()[, input$PPCA_cov_slider[1]:input$PPCA_cov_slider[2]]
        }
    })

    output$PPCA_covariates_data_table <- renderDataTable({ # Render Table
        if(is.null(PPCA_covariates_data())){return ()}
        validate(
            need(nrow(PPCA_covariates_data()) == nrow(original_data()),
                 "Please make sure equal rows in spectral and covariates data")
        )
        PPCA_covariates_data()
    }, options = list(pageLength=10), escape = FALSE)

    ## PPCA Labels Data
    PPCA_labels_data <- reactive({ # Read data
        if(input$PPCA_label_slider==0) { # no labels
            NULL
        }
        else {
            label_data <- as.data.frame(original_data()[, input$PPCA_label_slider])
            colnames(label_data) <- colnames(original_data())[input$PPCA_label_slider]
            label_data
        }

    })

    output$PPCA_labels_data_table <- renderDataTable({ # Render Table
        validate(
            need(input$PPCA_label_slider != 0,
                 "Group Labels are not selected")
        )
        validate(
            need(!input$PPCA_label_slider %in% input$PPCA_cov_slider[1]:input$PPCA_cov_slider[2],
                 "Group Labels are using Covariates Columns")
        )
        PPCA_labels_data()
    }, options = list(pageLength=10), escape = FALSE)


    # mppca -------------------------------------------------------------------

    ## MPPCA Original Data
    MPPCA_original_data <- reactive({
        file2 <- input$MPPCA_input_file
        if(is.null(file2)){return(default_brain_data)}
        ori_data2 <- read.table(file=file2$datapath, sep=input$MPPCA_sep, header = input$MPPCA_header, check.names = F)
        updateSliderInput(session, "PPCA_cov_slider", max = ceiling(ncol(ori_data2)/10))
        ori_data_rows2 <- nrow(ori_data2)
        updateSliderInput(session, "PPCA_bootstrap_n_slider", min = ori_data_rows2, value = ori_data_rows2)
        ori_data2
    })

    ## MPPCA Spectral Data
    MPPCA_spectral_data <- reactive({ # Read data
        main_columns2 <- 1:ncol(MPPCA_original_data())
        main_columns2 <- as.numeric(na.omit(main_columns2[!colnames(MPPCA_original_data()) %in% unlist(strsplit(input$MPPCA_ignore_column, ","))])) # remove ignore by colname
        main_columns2 <- main_columns2[!main_columns2 %in% input$MPPCA_cov_slider[1]:input$MPPCA_cov_slider[2]] # remove covariates
        main_columns2 <- main_columns2[main_columns2!=input$MPPCA_label_slider] # remove label
        main_columns2 <- main_columns2[!main_columns2 %in% as.numeric(unlist(strsplit(input$MPPCA_ignore_column, ",")))] # remove ignore by col number
        if(input$MPPCA_scale=='PQN'){
            a2<-scale_data(MPPCA_original_data()[, main_columns2],"PQN")
            b2<-as.data.frame(a$newXtrain)
        }
        else
            apply(MPPCA_original_data()[, main_columns2], 2,function (y) scale_data(y,input$MPPCA_scale))
        #MPPCA_original_data()[, main_columns2]
    })

    # This reactive output contains the dataset and display the dataset in table format
    output$MPPCA_spectral_data_table <- renderDataTable({ # Render Table
        if(is.null(MPPCA_spectral_data())){return ()}
        # head(main_data(), input$lab_slider)
        MPPCA_spectral_data()
    }, options = list(pageLength=10), escape = FALSE)

    MPPCA_spectral_file_name <- reactive({ # Obtain file name
        inFile2 <- input$MPPCA_input_file
        if (is.null(inFile2))
            return('Brain Data from Metabol Analyze Package')
        return (inFile2$name)
    })
    output$MPPCA_spectral_file_name <- renderText({ MPPCA_spectral_file_name() })

    ## MPPCA Covariates Data

    hide("MPPCA_cov_slider") # hiding covariates slider for MPPCA since covariates model not done

    MPPCA_covariates_data <- reactive({ # Read data
        if(input$MPPCA_cov_slider[1]==input$MPPCA_cov_slider[2]) {
            MPPCA_cov_data <- as.data.frame(MPPCA_original_data()[, input$MPPCA_cov_slider[1]])
            colnames(MPPCA_cov_data) <- colnames(MPPCA_original_data())[input$MPPCA_cov_slider[1]]
            MPPCA_cov_data

        } else {
            MPPCA_original_data()[, input$MPPCA_cov_slider[1]:input$MPPCA_cov_slider[2]]
        }
    })

    output$MPPCA_covariates_data_table <- renderDataTable({ # Render Table
        if(is.null(MPPCA_covariates_data())){return ()}
        validate(
            need(nrow(MPPCA_covariates_data()) == nrow(MPPCA_original_data()),
                 "Please make sure equal rows in spectral and covariates data")
        )
        MPPCA_covariates_data()
    }, options = list(pageLength=10), escape = FALSE)

    ## MPPCA Labels Data
    MPPCA_labels_data <- reactive({ # Read data
        if(input$MPPCA_label_slider==0) { # no labels
            NULL
        }
        else {
            label_data2 <- as.data.frame(MPPCA_original_data()[, input$MPPCA_label_slider])
            colnames(label_data2) <- colnames(MPPCA_original_data())[input$MPPCA_label_slider]
            label_data2
        }

    })

    output$MPPCA_labels_data_table <- renderDataTable({ # Render Table
        validate(
            need(input$MPPCA_label_slider != 0,
                 "Group Labels are not selected")
        )
        validate(
            need(!input$MPPCA_label_slider %in% input$MPPCA_cov_slider[1]:input$MPPCA_cov_slider[2],
                 "Group Labels are using Covariates Columns")
        )
        MPPCA_labels_data()
    }, options = list(pageLength=10), escape = FALSE)



    # dppca --------------------------------------------------------------------

    ## PPCA Original Data
    DPPCA_original_data <- reactive({
        file3 <- input$DPPCA_input_file
        if(is.null(file3)){return(default_dynamic_data)}

        # Reset controls
        updateSliderInput(session, "DPPCA_time_slider", value = 0)
        updateSliderInput(session, "DPPCA_label_slider", value = 0)
        updateTextInput(session, "DPPCA_ignore_column", value = "")

        # Read input files
        ori_data <- read.table(file=file3$datapath, sep=input$DPPCA_sep, header = input$DPPCA_header, check.names = F)
        ori_data_rows <- nrow(ori_data)
        updateSliderInput(session, "DPPCA_bootstrap_n_slider", min = ori_data_rows, value = ori_data_rows)
        ori_data
    })

    # This reactive output contains the dataset and display the dataset in table format
    output$DPPCA_original_data_table <- renderDataTable({ # Render Table
        if(is.null(DPPCA_original_data())){return ()}
        # head(main_data(), input$lab_slider)
        DPPCA_original_data()
    }, options = list(pageLength=10), escape = FALSE)

    output$DPPCA_original_data_table_UI <- renderUI({
        tagList(
            dataTableOutput("DPPCA_original_data_table"),
            tags$style(type="text/css", paste0("#DPPCA_original_data_table td:nth-child(", input$DPPCA_time_slider, ") {background-color:cyan}")),
            tags$style(type="text/css", paste0("#DPPCA_original_data_table td:nth-child(", input$DPPCA_label_slider, ") {background-color:lightgreen}")),

        )
    })

    output$DPPCA_original_data_table_UI_color <- renderUI({
        lapply(as.numeric(unlist(strsplit(input$DPPCA_ignore_column, ","))), function(i) {
            tags$style(type="text/css", paste0("#DPPCA_original_data_table td:nth-child(", i, ") {background-color:grey}"))
        })
    })

    ## DPPCA Data list by Time
    DPPCA_time_data_list <- reactive({ # Read data
        if(input$DPPCA_time_slider == 0) {return(DPPCA_original_data())}
        time_list <- by(DPPCA_original_data(), DPPCA_original_data()[,input$DPPCA_time_slider], function(x) x)

        updateSelectInput(session, "DPPCA_time_selected", choices = names(time_list))
        time_list
    })


    ## DPPCA Spectral Data
    DPPCA_spectral_data_list <- reactive({ # Read data
        if(input$DPPCA_time_slider == 0) {return(DPPCA_original_data())}
        main_columns <- 1:ncol(DPPCA_original_data())
        main_columns <- as.numeric(na.omit(main_columns[!colnames(DPPCA_original_data()) %in% unlist(strsplit(input$DPPCA_ignore_column, ","))])) # remove ignore by colname
        main_columns <- main_columns[main_columns!=input$DPPCA_label_slider] # remove label
        main_columns <- main_columns[main_columns!=input$DPPCA_time_slider] # remove time
        main_columns <- main_columns[!main_columns %in% as.numeric(unlist(strsplit(input$DPPCA_ignore_column, ",")))] # remove ignore by col number

        # Removing specified columns in each time point
        only_spectral_data <- lapply(DPPCA_time_data_list(), function(x) x[,main_columns])

        # Performing scaling to data in each time point
        only_spectral_data_scaled <- lapply(only_spectral_data, function(x) apply(x, 2, function(y) scale_data(y, input$DPPCA_scale)))
        only_spectral_data_scaled_matrix <- lapply(only_spectral_data_scaled, function(x) as.matrix(x))
    })

    # This reactive output contains the dataset and display the dataset in table format
    output$DPPCA_spectral_data_table <- renderDataTable({ # Render Table
        if(input$DPPCA_time_slider == 0) {return(DPPCA_original_data())}
        DPPCA_spectral_data_list()[[input$DPPCA_time_selected]]
    }, options = list(pageLength=10), escape = FALSE)


    # DPPCA Labels Data
    DPPCA_labels_data <- reactive({ # Read data
        if(input$DPPCA_label_slider==0) { # no labels
            NULL
        }
        else {
            DPPCA_label_data <- as.data.frame(DPPCA_original_data()[, input$DPPCA_label_slider])
            colnames(DPPCA_label_data) <- colnames(DPPCA_original_data())[input$DPPCA_label_slider]
            DPPCA_label_data
        }

    })

    output$DPPCA_labels_data_table <- renderDataTable({ # Render Table
        validate(
            need(input$DPPCA_label_slider != 0,
                 "Group Labels are not selected")
        )
        DPPCA_labels_data()
    }, options = list(pageLength=10), escape = FALSE)


    # Specify Time Column to enable time point selection
    observeEvent(input$DPPCA_time_slider,
                 isolate({
                     if(input$DPPCA_time_slider != 0) {
                         output$DPPCA_time_point_showing <- renderText(
                             paste("Time Point", input$DPPCA_time_selected, "is Selected")
                         )
                         enable("DPPCA_time_selected")
                     }
                     else {
                         disable("DPPCA_time_selected")
                         output$DPPCA_time_point_showing <- renderText(
                             NULL
                         )
                     }
                 })
    )

    # Write file name on screen
    DPPCA_spectral_file_name <- reactive({ # Obtain file name
        inFile3 <- input$DPPCA_input_file
        if (is.null(inFile3))
            return('Dynamic Urine Data from UCD')
        return (inFile3$name)
    })
    output$DPPCA_spectral_file_name <- renderText({ DPPCA_spectral_file_name() })




    # Analytics ---------------------------------------------------------------


    # ppca --------------------------------------------------------------------

    # PPCA parameter button
    observeEvent(input$PPCA_parameter_button,
                 isolate({
                     updateTabsetPanel(session, "PPCA_plot_tabs",
                                       selected = "PPCA_description")
                 })
    )

    # PPCA data button
    observeEvent(input$PPCA_data_button,
                 isolate({
                     updateTabsetPanel(session, "PPCA_plot_tabs",
                                       selected = "PPCA_data")
                 })
    )

    # Return Parameter Button (PPCA tab)
    observeEvent(input$PPCA_return_param,
                 isolate({
                     updateTabsetPanel(session, "PPCA_plot_tabs",
                                       selected = "PPCA_description")
                 })
    )

    # Check if covariates data are selected
    cov_column_check <- reactive(input$PPCA_cov_slider[2])

    observe(
        if(cov_column_check() == 0) {
            updateCheckboxInput(session, "PPCA_covariates_check", value = FALSE)
            disable('PPCA_covariates_check')
        }
        else {
            enable('PPCA_covariates_check')
        }
    )

    # Check if covariates are ticked for modelling
    cov_check <- reactive(input$PPCA_covariates_check)


    # Submit PPCA Parameter Button
    observeEvent(input$PPCA_submit_para_btn,
                 isolate({

                     showNotification("Running PPCA...", duration = NULL, id = "ppca_progress",
                                      closeButton = FALSE, type = "message")

                     # Run PPCA model
                     if(cov_check()) { # include covariates
                         PPCA_object <- PPCA(PPCA_spectral_data(), covariates_data = PPCA_covariates_data(),
                                             q_min = input$PPCA_PC_slider[1], q_max=input$PPCA_PC_slider[2],
                                             B=input$PPCA_bootstrap_n_slider, eps = input$PPCA_epsilon, max_it = input$PPCA_max_iter,
                                             choose_q = input$PPCA_choose_q)
                     }
                     else { # no covariates
                         PPCA_object <- PPCA(PPCA_spectral_data(),
                                             q_min = input$PPCA_PC_slider[1], q_max=input$PPCA_PC_slider[2],
                                             B=input$PPCA_bootstrap_n_slider, eps = input$PPCA_epsilon, max_it = input$PPCA_max_iter,
                                             choose_q = input$PPCA_choose_q)
                     }


                     # PPCA Score plot
                     output$PPCA_plot_score <- renderPlot({
                         if(is.null(PPCA_labels_data())) {
                             plot(PPCA_object$score,
                                  x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC, conf_level = input$PPCA_post_int)
                         }
                         else {
                             plot(PPCA_object$score, labels = unlist(PPCA_labels_data()),
                                  x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC, conf_level = input$PPCA_post_int)
                         }
                     })

                     # PPCA Score plot data download button
                     output$dl_ppca_score_btn <- downloadHandler(
                         filename = "PPCA_score.csv",
                         content = function(file) {
                             if(is.null(PPCA_labels_data())) {
                                 score_data <- plot(PPCA_object$score,
                                                    x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC, conf_level = input$PPCA_post_int)
                             }
                             else {
                                 score_data <- plot(PPCA_object$score, labels = unlist(PPCA_labels_data()),
                                                    x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC, conf_level = input$PPCA_post_int)
                             }
                             write.table(score_data, file = file, sep = ",", row.names = FALSE)
                         }
                     )
                     enable('dl_ppca_score_btn')

                     # PPCA Loadings plot
                     output$PPCA_plot_loadings <- renderPlot({
                         plot(PPCA_object$loadings, analysis = FALSE, x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC)
                     })

                     # PPCA Loadings plot data download button
                     output$dl_ppca_loadings_btn <- downloadHandler(
                         filename = "PPCA_loadings.csv",
                         content = function(file) {
                             loadings_data <- plot(PPCA_object$loadings, analysis = FALSE, x_axis_PC = input$PPCA_x_PC, y_axis_PC = input$PPCA_y_PC)
                             write.table(loadings_data, file = file, sep = ",", col.names = NA)
                         }
                     )
                     enable('dl_ppca_loadings_btn')

                     # PPCA Loadings Analysis plot
                     output$PPCA_plot_loadings_analysis <- renderPlot({
                         plot(PPCA_object$loadings, analysis = TRUE, PC=input$PPCA_spectral_plot_PC, conf_level=input$PPCA_conf_int, n=input$PPCA_n_load_analysis)
                     })

                     # PPCA Loadings Analysis plot data download button
                     output$dl_ppca_loadings_analysis_btn <- downloadHandler(
                         filename = "PPCA_loadings_analysis.csv",
                         content = function(file) {
                             loadings__analysis_data <- plot(PPCA_object$loadings, analysis = TRUE, PC=input$PPCA_spectral_plot_PC, conf_level=input$PPCA_conf_int, n=input$PPCA_n_load_analysis)
                             write.table(loadings__analysis_data, file = file, sep = ",", col.names = NA)
                         }
                     )
                     enable('dl_ppca_loadings_analysis_btn')

                     # PPCA Influence plot
                     if(!is.null(PPCA_object$influence_report)) {
                         showTab(inputId = "PPCA_plot_tabs", target = "PPCA_influence_plot")
                         output$PPCA_influence_report <- renderPlot({
                             plot(PPCA_object$influence_report, PC=input$PPCA_spectral_plot_PC)
                         })
                     }
                     else {
                         hideTab(inputId = "PPCA_plot_tabs", target = "PPCA_influence_plot")
                     }

                     # PPCA BIC plot
                     output$PPCA_plot_bic <- renderPlot({
                         plot(PPCA_object$diagnostic)
                     })

                     # PPCA Likelihood Convergence plot
                     output$PPCA_plot_ll_conv <- renderPlot({
                         plot(PPCA_object$diagnostic, max_ll=TRUE)
                     })


                     # Sliders update
                     val <- input$PPCA_PC_slider
                     if(val[1]==val[2]) { # Same PC
                         PC_max_slider <- val[2]
                         output$PPCA_optimal_q <- renderText("")
                     }
                     else{ # Different PC
                         output$PPCA_optimal_q <- renderText(paste("Optimal PC =", PPCA_object$optimal_q))
                         if(input$PPCA_choose_q == TRUE) {
                             PC_max_slider <- PPCA_object$optimal_q
                         }
                         else {
                             PC_max_slider <- val[2]
                         }
                     }

                     updateSliderInput(session, "PPCA_spectral_plot_PC", value = val[1],
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "PPCA_x_PC", value = PC_max_slider-1,
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "PPCA_y_PC", value = PC_max_slider,
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "PPCA_n_load_analysis", max = length(PPCA_object$significant_x[[input$PPCA_spectral_plot_PC]]))


                     # Switches to Score plot if User is in Description when model has finished running.
                     if(input$PPCA_plot_tabs == 'PPCA_description') {
                         updateTabsetPanel(session, "PPCA_plot_tabs",
                                           selected = "PPCA_score_plot"
                         )
                     }

                     on.exit(removeNotification("ppca_progress"), add = TRUE)
                 })
    )


    # mppca -------------------------------------------------------------------

    # MPPCA parameter button
    observeEvent(input$MPPCA_parameter_button,
                 isolate({
                     updateTabsetPanel(session, "MPPCA_plot_tabs",
                                       selected = "MPPCA_description")
                 })
    )

    # MPPCA data button
    observeEvent(input$MPPCA_data_button,
                 isolate({
                     updateTabsetPanel(session, "MPPCA_plot_tabs",
                                       selected = "MPPCA_data")
                 })
    )

    # Return Parameter Button (MPPCA tab)
    observeEvent(input$MPPCA_return_param,
                 isolate({
                     updateTabsetPanel(session, "MPPCA_plot_tabs",
                                       selected = "MPPCA_description")
                 })
    )

    # Submit MPPCA Parameter Button
    observeEvent(input$MPPCA_submit_para_btn,
                 isolate({

                     showNotification("Running MPPCA...", duration = NULL, id = "mppca_progress",
                                      closeButton = FALSE, type = "message")

                     # Run MPPCA model
                     MPPCA_object <- MPPCA(MPPCA_spectral_data(),
                                           q_min = input$MPPCA_PC_slider[1], q_max = input$MPPCA_PC_slider[2],
                                           g_min = input$MPPCA_group_slider[1], g_max = input$MPPCA_group_slider[2],
                                           B=input$MPPCA_bootstrap_n_slider, eps = input$MPPCA_epsilon)

                     # MPPCA score plot
                     output$MPPCA_plot_score <- renderPlot({
                         if(is.null(MPPCA_labels_data())) {
                             plot(MPPCA_object$score, x_axis_PC = input$MPPCA_x_PC, y_axis_PC = input$MPPCA_y_PC,
                                  conf_level = input$MPPCA_post_int)
                         }
                         else {
                             plot(MPPCA_object$score, x_axis_PC = input$MPPCA_x_PC, y_axis_PC = input$MPPCA_y_PC,
                                  clustering = MPPCA_object$clustering, labels = MPPCA_labels_data(),
                                  conf_level = input$MPPCA_post_int)
                         }

                     }, height = ceiling(MPPCA_object$optimal_g/2)*400)
                     output$dl_mppca_score_btn <- downloadHandler(
                         filename = "MPPCA_score.csv",
                         content = function(file) {
                             if(is.null(MPPCA_labels_data())) {
                                 mppca_score_data <- plot(MPPCA_object$score, x_axis_PC = input$MPPCA_x_PC, y_axis_PC = input$MPPCA_y_PC,
                                                          conf_level = input$MPPCA_post_int)
                             }
                             else {
                                 mppca_score_data <- plot(MPPCA_object$score, x_axis_PC = input$MPPCA_x_PC, y_axis_PC = input$MPPCA_y_PC,
                                                          clustering = MPPCA_object$clustering, labels = MPPCA_labels_data(),
                                                          conf_level = input$MPPCA_post_int)
                             }
                             write.table(mppca_score_data, file = file, sep = ",", row.names = FALSE)
                         }
                     )
                     enable('dl_mppca_score_btn')

                     # MPPCA loadings plot
                     output$MPPCA_plot_loadings <- renderPlot({
                         plot(MPPCA_object$loadings, group = input$MPPCA_group, analysis = FALSE)
                     })
                     output$dl_mppca_loadings_btn <- downloadHandler(
                         filename = function(){paste0("MPPCA_Group", input$MPPCA_group, "_loadings.csv")}, #function needed to make it reactive
                         content = function(file) {
                             mppca_loadings_data <- plot(MPPCA_object$loadings, group = input$MPPCA_group, analysis = FALSE)
                             write.table(mppca_loadings_data, file = file, sep = ",", col.names = NA)
                         }
                     )
                     enable('dl_mppca_loadings_btn')

                     # MPPCA loadings analysis plot
                     output$MPPCA_plot_loadings_analysis <- renderPlot({
                         plot(MPPCA_object$loadings, group = input$MPPCA_group, analysis = TRUE,
                              PC=input$MPPCA_analysis_plot_PC, conf_level=input$MPPCA_conf_int, n=input$MPPCA_n_load_analysis)
                     })
                     output$dl_mppca_loadings_analysis_btn <- downloadHandler(
                         filename = "MPPCA_loadings_analysis.csv",
                         content = function(file) {
                             mppca_loadings__analysis_data <- plot(MPPCA_object$loadings, group = input$MPPCA_group, analysis = TRUE,
                                                                   PC=input$MPPCA_analysis_plot_PC, conf_level=input$MPPCA_conf_int, n=input$MPPCA_n_load_analysis)
                             write.table(mppca_loadings__analysis_data, file = file, sep = ",", col.names = NA)
                         }
                     )
                     enable('dl_mppca_loadings_analysis_btn')

                     # output$MPPCA_contin_table <- renderPrint({
                     #     table(unlist(MPPCA_object$groupings), unlist(MPPCA_labels_data()), dnn = c("Predicted","Actual"))
                     # })

                     output$MPPCA_plot_BIC <- renderPlot({
                         plot(MPPCA_object$bic_results)
                     })

                     PC_mix_val <- input$MPPCA_PC_slider
                     if(PC_mix_val[1]==PC_mix_val[2]) { # Same PC
                         PC_mix_max_slider <- PC_mix_val[2]
                         output$MPPCA_optimal_q <- renderText("")
                     }
                     else{ # Different PC
                         output$MPPCA_optimal_q <- renderText(paste("Optimal: PC =", MPPCA_object$optimal_q))
                         # if(input$PPCA_choose_q == TRUE) {
                         PC_mix_max_slider <- MPPCA_object$optimal_q
                         # }
                         # else {
                         #     PC_mix_max_slider <- PC_mix_val[2]
                         # }
                     }

                     group_mix_val <- input$MPPCA_group_slider
                     if(group_mix_val[1]==group_mix_val[2]) { # Same PC
                         group_mix_max_slider <- group_mix_val[2]
                         output$MPPCA_optimal_g <- renderText("")
                     }
                     else{ # Different PC
                         output$MPPCA_optimal_g <- renderText(paste("Group =", MPPCA_object$optimal_g))
                         # if(input$PPCA_choose_q == TRUE) {
                         group_mix_max_slider <- MPPCA_object$optimal_g
                         # }
                         # else {
                         #     group_mix_max_slider <- group_mix_val[2]
                         # }
                     }

                     updateSliderInput(session, "MPPCA_x_PC", value = PC_mix_val[1],
                                       min = PC_mix_val[1], max = PC_mix_max_slider)
                     updateSliderInput(session, "MPPCA_y_PC", value = PC_mix_val[1]+1,
                                       min = PC_mix_val[1], max = PC_mix_max_slider)
                     updateSliderInput(session, "MPPCA_n_load_analysis",
                                       max = ceiling(dim(MPPCA_object$loadings$loadings[[1]])[1]/4))

                     updateSliderInput(session, "MPPCA_analysis_plot_PC", value = PC_mix_val[1],
                                       min = PC_mix_val[1], max = PC_mix_max_slider)
                     updateSliderInput(session, "MPPCA_group", value = group_mix_val[1],
                                       min = group_mix_val[1], max = group_mix_max_slider)

                     # Switches to Score plot if User is in Description when model has finished running.
                     if(input$MPPCA_plot_tabs == 'MPPCA_description') {
                         updateTabsetPanel(session, "MPPCA_plot_tabs",
                                           selected = "MPPCA_score_plot"
                         )
                     }

                     on.exit(removeNotification("mppca_progress"), add = TRUE)
                 })

    )

    # dppca --------------------------------------------------------------------

    # DPPCA parameter button
    observeEvent(input$DPPCA_parameter_button,
                 isolate({
                     updateTabsetPanel(session, "DPPCA_plot_tabs",
                                       selected = "DPPCA_description")
                 })
    )

    # DPPCA data button
    observeEvent(input$DPPCA_data_button,
                 isolate({
                     updateTabsetPanel(session, "DPPCA_plot_tabs",
                                       selected = "DPPCA_data")
                 })
    )

    # Return Parameter Button (DPPCA tab)
    observeEvent(input$DPPCA_return_param,
                 isolate({
                     updateTabsetPanel(session, "DPPCA_plot_tabs",
                                       selected = "DPPCA_description")
                 })
    )

    observeEvent(input$DPPCA_priors_choice,
                 if(input$DPPCA_priors_choice == "Custom") {
                     isolate({
                         updateTabsetPanel(session, "DPPCA_sidetab",
                                           selected = "DPPCA_model_priors")
                     })
                 }
                 else if(input$DPPCA_priors_choice == "Default") {
                     isolate({
                         updateTextInput(session, "prior_alpha", value = 6)
                         updateTextInput(session, "prior_beta", value = 0.5)
                         updateTextInput(session, "prior_sigma2_nu", value = 10)
                         updateTextInput(session, "prior_mu_phi", value = 0.75)
                         updateTextInput(session, "prior_sigma2_phi", value = 0.1)
                         updateTextInput(session, "prior_alpha_V", value = 6)
                         updateTextInput(session, "prior_beta_V", value = 0.5)
                         updateTextInput(session, "prior_sigma2_mu", value = 10)
                         updateTextInput(session, "prior_sigma2_PHI", value = 0.1)
                         updateTextInput(session, "prior_mu_PHI", value = 0.75)
                         updateTextInput(session, "prior_omega_inv", value = 1)
                     })
                 }
    )

    ### Put pre-run values into dppca_output
    dppca_output <- reactiveValues(
      result=dppca_results, lmm = lmm_fit)

    # Submit DPPCA Parameter Button
    observeEvent(input$DPPCA_submit_para_btn,
                 isolate({

                     ### Removing pre-run results
                     hide(id = "Pre-run_results_warning")

                     showNotification("Running DPPCA...", duration = NULL, id = "dppca_progress",
                                      closeButton = FALSE, type = "message")

                     # Obtain DPPCA priors
                     DPPCA_model_priors <- list(alpha = as.numeric(input$prior_alpha),
                                                beta = as.numeric(input$prior_beta),
                                                sigma2_nu = as.numeric(input$prior_sigma2_nu),
                                                mu_phi = as.numeric(input$prior_mu_phi),
                                                sigma2_phi = as.numeric(input$prior_sigma2_phi),
                                                alpha_V = as.numeric(input$prior_alpha_V),
                                                beta_V = as.numeric(input$prior_beta_V),
                                                sigma2_mu = as.numeric(input$prior_sigma2_mu),
                                                sigma2_Phi = as.numeric(input$prior_sigma2_PHI),
                                                mu_Phi = as.numeric(input$prior_mu_PHI),
                                                omega_inv = rep(list(diag(input$prior_omega_inv, input$DPPCA_PC_slider)),length(DPPCA_spectral_data_list()))
                     )

                     # Run DPPCA model
                     DPPCA_object <- DPPCA(data_time = DPPCA_spectral_data_list(),
                                           q = input$DPPCA_PC_slider,
                                           chain_output = input$DPPCA_chain_slider,
                                           prior_params = DPPCA_model_priors,
                                           burn_in = input$DPPCA_burn_slider,
                                           thin = input$DPPCA_thin_slider)



                     # Switches to Score plot if User is in Description when model has finished running.
                     if(input$DPPCA_plot_tabs == 'DPPCA_description') {
                         updateTabsetPanel(session, "DPPCA_plot_tabs",
                                           selected = "DPPCA_chain_conver"
                         )
                     }


                     ### Removing pre-run results
                     dppca_output$result <- DPPCA_object

                     on.exit(removeNotification("dppca_progress"), add = TRUE)
                 })
    )

    # Chain Convergence Tab plot
    output$DPPCA_plot_load_score_chain <- renderPlot({
        par(mfrow=c(1,2))
        plot(dppca_output$result$U_chain)
        plot(dppca_output$result$W_chain)
        par(mfrow=c(1,1))
    })
    observeEvent(input$DPPCA_convergence_next_btn,
                 isolate({
                     output$DPPCA_plot_load_score_chain <- renderPlot({
                         par(mfrow=c(1,2))
                         plot(dppca_output$result$U_chain)
                         plot(dppca_output$result$W_chain)
                         par(mfrow=c(1,1))
                     })
                 })
    )
    output$DPPCA_plot_persistence_chain <- renderPlot({
        plot(dppca_output$result$persistance, type = "chain")
    })
    # Time Influence Tab plots
    output$DPPCA_plot_persistence_histogram <- renderPlot({
        plot(dppca_output$result$persistance)
    })
    output$DPPCA_plot_persistence_summary <- renderPrint({
        summary(dppca_output$result$persistance)
    })
    # Loading Analysis Tab plots
    DPPCA_top <- reactive({
        DPPCA_top_loadings(n = input$DPPCA_n_load_analysis,
                           PC = input$DPPCA_spectral_plot_PC,
                           W = dppca_output$result$W_chain,
                           cred_level = input$DPPCA_cred_int)
    })
    output$DPPCA_plot_top_loadings <- renderPlot({
        plot.top_loadings(DPPCA_top(), M=input$DPPCA_load_analysis_time)
    })
    # LMM Results Tab plots
    observeEvent(input$DPPCA_submit_LMM_btn,
                 isolate({
                     ### Removing pre-run results
                     hide(id = "Pre-run_results_warning2")
                     output$DPPCA_LMM_summary <- renderPrint({
                         NULL
                     })
                     output$DPPCA_plot_LMM_fit <- renderPlot({
                         NULL
                     })

                     showNotification("Running DPPCA_LMM...", duration = NULL, id = "dppca_lmm_progress",
                                      closeButton = FALSE, type = "message")

                     lmm_results <- fit_LMMs(DPPCA_top(), alpha = input$DPPCA_LMM_cred_int,
                                             data = DPPCA_spectral_data_list())

                     ### Removing pre-run results
                     dppca_output$lmm <- lmm_results

                     on.exit(removeNotification("dppca_lmm_progress"), add = TRUE)
                 })
    )
    output$DPPCA_LMM_summary <- renderPrint({
        summary(dppca_output$lmm)
    })
    output$DPPCA_plot_LMM_fit <- renderPlot({
        plot(dppca_output$lmm)
    })

    # Reactive expression to create data frame of all input values ----
    sliderValues <- reactive({

      data.frame(
        Name = c("Spectral Bins",
                 "Proportion of significant bins (m)",
                 "Target FDR"),
        Value = as.character(c(input$bins,
                               input$sig.bins,
                               input$target.fdr)),
        stringsAsFactors = FALSE)

    })

    # Show the values in an HTML table ----
    output$values <- renderTable({
      input$button
      isolate(sliderValues())
    })

    sample_data <- reactive({
      file1 <- input$sample_file
      if(is.null(file1)){return(default_urine_data)}
      ori_data <- read.table(file=file1$datapath, sep=input$sep, header = input$header, check.names = F)
      updateSliderInput(session, "cov_slider", max = ceiling(ncol(ori_data)/10))
      ori_data_rows <- nrow(ori_data)
      updateSliderInput(session, "bootstrap_n_slider", min = ori_data_rows, value = ori_data_rows)
      ori_data
    })

    p<-reactive({
      input$bins
    })

    prop<-reactive({
      input$sig.bins
    })
    target.fdr<-reactive({
      input$target.fdr
    })

    n1<-reactive({
      input$n1
    })

    n2<-reactive({
      input$n2
    })
    plot.prop<-reactive({
      input$graphtype
    })

    global <- reactiveValues(response = FALSE)

    mett<-function(){
      if(global$response==T){
        metsize(pilot=sample_data(), n1(), n2(), p(), prop(), plot.prop=F,target.fdr())
      }
      else return(NULL)
    }


    metf<-function(){
      if(global$response==T){
        metsize(pilot=sample_data(), n1(), n2(), p(), prop(), plot.prop=T,target.fdr())
      }
      else return(NULL)
    }


    output$met1<-renderPlot({
      input$button
      isolate(if(global$response==T){
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(x = numeric(0), y = numeric(0))

        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          n <- 10

          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))

            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
          mett()
        })
      } else  return(NULL)

      )
    })

    output$met2<-renderPlot({
      input$button
      isolate(if(global$response==T){
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(x = numeric(0), y = numeric(0))

        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          n <- 10

          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))

            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
          metf()
        })
      } else  return(NULL)

      )
    })

    observeEvent(input$button, {
      # Show a modal when the button is pressed
      shinyalert("calculating.....please wait", type = "info",showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "OK",
                 cancelButtonText = "Cancel",callbackR = function(x) {
                   global$response <- x
                 }
      )
    })


    # Guides ------------------------------------------------------------------

    # Debug purpose
    output$debug <- renderPrint({
        sessionInfo()
    })



    # About Us ----------------------------------------------------------------





})
