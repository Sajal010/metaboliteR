library(shiny)

default_data <- UrineSpectra[[1]]

shinyServer(function(input, output, session) {

    # Data --------------------------------------------------------------------
    original_data <- reactive({
        file1 <- input$main_file
        if(is.null(file1)){return(default_data)}
        read.table(file=file1$datapath, sep=input$sep, header = input$header, check.names = F)

    })

    main_data <- reactive({ # Read data
        main_columns <- 1:ncol(original_data())
        main_columns <- main_columns[!main_columns %in% input$cov_slider[1]:input$cov_slider[2]]
        main_columns <- main_columns[main_columns!=input$label_slider]
        original_data()[, main_columns]
    })

    # This reactive output contains the dataset and display the dataset in table format
    output$main_data_table <- renderDataTable({ # Render Table
        if(is.null(main_data())){return ()}
        # head(main_data(), input$lab_slider)
       main_data()
    }, options = list(pageLength=10))

    main_file_name <- reactive({ # Obtain file name
        inFile <- input$main_file
        if (is.null(inFile))
            return('Urine Data from Metabol Analyze Package')
        return (inFile$name)
    })
    output$main_file_name <- renderText({ main_file_name() })

    ## Covariates Data
    covariates_data <- reactive({ # Read data
        # file1 <- input$covariates_file
        # if(is.null(file1)){return()}
        # read.table(file=file1$datapath, sep=input$cov_sep, header = input$cov_header, check.names = F)
        if(input$cov_slider[1]==input$cov_slider[2]) {
            cov_data <- as.data.frame(original_data()[, input$cov_slider[1]])
            colnames(cov_data) <- colnames(original_data())[input$cov_slider[1]]
            cov_data

        } else {
            original_data()[, input$cov_slider[1]:input$cov_slider[2]]
        }
    })

    output$covariates_data_table <- renderDataTable({ # Render Table
        if(is.null(covariates_data())){return ()}
        validate(
            need(nrow(covariates_data()) == nrow(original_data()),
                 "Please make sure equal rows in main and covariates data")
        )
        covariates_data()
    }, options = list(pageLength=10))

    # covariates_file_name <- reactive({ # Obtain file name
    #     inFile <- input$covariates_file
    #     if (is.null(inFile))
    #         return(NULL)
    #     return (inFile$name)
    # })
    # output$covariates_file_name <- renderText({ covariates_file_name() })

    ## Labels Data
    labels_data <- reactive({ # Read data
        # file1 <- input$labels_file
        # if(is.null(file1)){return(-1)}
        # read.table(file=file1$datapath, sep=input$lab_sep, header = input$lab_header, check.names = F)
        if(input$label_slider==0) { # no labels
            NULL
        }
        else {
            label_data <- as.data.frame(original_data()[, input$label_slider])
            colnames(label_data) <- colnames(original_data())[input$label_slider]
            label_data
        }

    })

    output$labels_data_table <- renderDataTable({ # Render Table
        # if(labels_data()==-1){return ()}
        # validate(
        #     need(nrow(labels_data()) == nrow(main_data()),
        #          "Please make sure equal rows in main and covariates data")
        # )
        # validate(
        #     need(ncol(labels_data()) == 1,
        #          "Only accepts 1 column of labelling data")
        # )
        validate(
            need(input$label_slider != 0,
                 "Group Labels are not selected")
        )
        validate(
            need(!input$label_slider %in% input$cov_slider[1]:input$cov_slider[2],
                 "Group Labels are using Covariates Columns")
        )
        labels_data()
    }, options = list(pageLength=10))

    # labels_file_name <- reactive({ # Obtain file name
    #     inFile <- input$labels_file
    #     if (is.null(inFile))
    #         return(NULL)
    #     return (inFile$name)
    # })
    # output$labels_file_name <- renderText({ labels_file_name() })



    # Analytics ---------------------------------------------------------------
    cov_check <- reactive(input$covariates_check)

    # Check if covariates data are selected
    cov_column_check <- reactive(input$cov_slider[2])
    observe(
        if(cov_column_check() == 0) {
            disable('covariates_check')
        }
        else {
            enable('covariates_check')
        }
    )

    observeEvent(input$submit_para_btn,
                 isolate({

                     showNotification("Running PPCA...", duration = NULL, id = "ppca_progress",
                                      closeButton = FALSE, type = "message")

                     if(cov_check()) { # include covariates
                         PPCA_object <- PPCA(main_data(), covariates_data = covariates_data(),
                                             q_min = input$PC_slider[1], q_max=input$PC_slider[2],
                                             B=input$bootstrap_n_slider, eps = input$epsilon, max_it = input$max_iter,
                                             choose_q = input$choose_q)
                     }
                     else { # no covariates
                         PPCA_object <- PPCA(main_data(),
                                             q_min = input$PC_slider[1], q_max=input$PC_slider[2],
                                             B=input$bootstrap_n_slider, eps = input$epsilon, max_it = input$max_iter,
                                             choose_q = input$choose_q)
                     }


                     output$PPCA_plot <- renderPlot({
                         plot(PPCA_object, PC=input$main_plot_PC, conf_level=input$conf_int, n=input$n_main)
                     })

                     if(!is.null(PPCA_object$influence_report)) {
                         output$PPCA_influence_report <- renderPlot({
                             plot(PPCA_object$influence_report, PC=input$main_plot_PC)
                         })
                     }

                     output$PPCA_plot_score <- renderPlot({
                         if(is.null(labels_data())) {
                             plot(PPCA_object$score,
                                  x_axis_PC = input$x_PC, y_axis_PC = input$y_PC, conf_level = input$post_int)
                         }
                         else {
                             plot(PPCA_object$score, labels = unlist(labels_data()),
                                  x_axis_PC = input$x_PC, y_axis_PC = input$y_PC, conf_level = input$post_int)
                         }
                     })

                     output$PPCA_plot_loadings <- renderPlot({
                         plot(PPCA_object$loadings, x_axis_PC = input$x_PC, y_axis_PC = input$y_PC)
                     })

                     output$PPCA_plot_bic <- renderPlot({
                         plot(PPCA_object$diagnostic)
                     })

                     output$PPCA_plot_ll_conv <- renderPlot({
                         plot(PPCA_object$diagnostic, max_ll=TRUE)
                     })

                     output$PPCA_plot_significant <- renderPlot({
                         plot(PPCA_object$significant_x, ori_data=main_data(), PC=input$main_plot_PC,
                              sample_no = input$sample_num,
                              xlim=c(input$x_lim[1], input$x_lim[2]))
                     })

                     output$PPCA_summary_significant <- renderPrint({
                         summary(PPCA_object$significant_x)
                     })


                     val <- input$PC_slider
                     if(val[1]==val[2]) { # Same PC
                         PC_max_slider <- val[2]
                         output$optimal_q <- renderText("")
                     }
                     else{ # Different PC
                         output$optimal_q <- renderText(paste("Optimal Q =", PPCA_object$optimal_q))
                         if(input$choose_q == TRUE) {
                             PC_max_slider <- PPCA_object$optimal_q
                         }
                         else {
                             PC_max_slider <- val[2]
                         }
                     }

                     updateSliderInput(session, "main_plot_PC", value = val[1],
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "x_PC", value = PC_max_slider-1,
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "y_PC", value = PC_max_slider,
                                       min = val[1], max = PC_max_slider)
                     updateSliderInput(session, "n_main", max = length(PPCA_object$significant_x[[input$main_plot_PC]]))
                     main_data_names <- as.numeric(colnames(main_data()))
                     updateSliderInput(session, "x_lim", value = c(min(main_data_names),max(main_data_names)),
                                       min = min(main_data_names), max = max(main_data_names))
                     updateSliderInput(session, "sample_num", max = nrow(main_data()))


                     output$dl_significant_btn1 <- downloadHandler(
                         filename = "PPCA_Significant_x.csv",
                         content = function(file) {
                             PPCA_export_significant(PPCA_object$significant_x, shiny_path = file)
                         }
                     )

                     output$dl_significant_btn2 <- downloadHandler(
                         filename = "PPCA_Significant_x.csv",
                         content = function(file) {
                             PPCA_export_significant(PPCA_object$significant_x, shiny_path = file)
                         }
                     )

                     on.exit(removeNotification("ppca_progress"), add = TRUE)

                     # Switches to Main plot if User is in Description when model has finished running.
                     if(input$analytics_plot_tabs == 'PPCA_description') {
                         updateTabsetPanel(session, "analytics_plot_tabs",
                                           selected = "main_PPCA_plot"
                         )
                     }


                 }))


    # Guides ------------------------------------------------------------------




    # About Us ----------------------------------------------------------------





})
