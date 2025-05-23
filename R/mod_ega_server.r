#' Exploratory Graph Analysis (EGA) Server Module Logic
#'
#' Handles the server-side logic for performing EGA, including input validation,
#' running the EGAnet::EGA function, rendering results (network matrix, plot,
#' dimensionality summary, item-community assignments), and providing download options.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @import shiny
#' @importFrom EGAnet EGA 
#' @importFrom utils write.csv 
#' @importFrom grDevices svg dev.off 
#' @importFrom graphics plot text 
#' @importFrom stats var na.omit
#' @noRd
ega_server <- function(input, output, session, data) {
  # reactiveValues to store EGA results
  ega_analysis_results_rv <- reactiveValues(
    ega_object = NULL, 
    network_matrix = NULL,
    # plot_object will now store the ega_output itself if Plot.EGA is not found
    # but we will primarily rely on plot(ega_object)
    dimensionality_summary = NULL,
    item_community_assignments = NULL
  )

  observeEvent(input$run_ega_button, {
    # --- Input Validations ---
    validate(
      need(data(), "Please upload your dataset to run EGA."),
      need(input$ega_estimation_method_select, "Please select an EGA estimation method."),
      need(input$ega_correlation_type_radio, "Please select a correlation type for EGA.")
    )
    
    current_data <- data() 

    validate(
      need(all(sapply(current_data, is.numeric)), "All columns in the dataset must be numeric for EGA."),
      need(ncol(current_data) > 1, "Dataset must have at least two variables for EGA."),
      need(nrow(current_data) > ncol(current_data), "Sample size should ideally be greater than the number of variables for stable network estimation.")
    )
    col_variances <- apply(current_data, 2, var, na.rm = TRUE)
    validate(
      need(all(col_variances > 1e-6), "One or more variables have zero or near-zero variance. Please remove them or check your data.")
    )

    progress_id <- showNotification("Running Exploratory Graph Analysis...", duration = NULL, type = "message") 
    on.exit(removeNotification(progress_id), add = TRUE)

    # --- Perform EGA Analysis ---
    tryCatch({
      correlation_method_for_ega <- input$ega_correlation_type_radio

      message("DEBUG ega_server: Calling EGAnet::EGA with model: ", input$ega_estimation_method_select, ", corr: ", correlation_method_for_ega)
      ega_output <- EGAnet::EGA(
        data = current_data,
        model = input$ega_estimation_method_select,
        corr = correlation_method_for_ega, 
        plot.EGA = TRUE, # Keep TRUE, as it might influence internal object structure for plotting
        plot.type = "qgraph",     
        plot.args = list(
          vsize = 7,             
          label.cex = 1,         
          edge.width = 1.5,      
          layout = "spring",     
          theme = "TeamFortress", 
          legend.cex = 0.7,
          GLratio = 1.5 
        ),
        verbose = FALSE 
      )
      
      message("DEBUG ega_server: EGAnet::EGA call completed.")
      message("DEBUG ega_server: Class of ega_output: ", paste(class(ega_output), collapse=", "))
      message("DEBUG ega_server: Names in ega_output: ", paste(names(ega_output), collapse=", "))
      
      if("network" %in% names(ega_output)){
          message("DEBUG ega_server: ega_output$network exists. Class: ", paste(class(ega_output$network), collapse=", "))
          if(is.matrix(ega_output$network) || is.data.frame(ega_output$network)){
              message("DEBUG ega_server: ega_output$network dimensions: ", nrow(ega_output$network), "x", ncol(ega_output$network))
          }
      } else {
          message("DEBUG ega_server: ega_output$network does NOT exist.")
      }
      
      if("Plot.EGA" %in% names(ega_output) && !is.null(ega_output$Plot.EGA)){
          message("DEBUG ega_server: ega_output$Plot.EGA exists. Class: ", paste(class(ega_output$Plot.EGA), collapse=", "))
      } else {
          message("DEBUG ega_server: ega_output$Plot.EGA does NOT exist or is NULL.")
      }

        ega_analysis_results_rv$ega_object <- ega_output 
      
      if (!is.null(ega_output$network)) {
        ega_analysis_results_rv$network_matrix <- as.data.frame(as.matrix(ega_output$network)) 
      } else {
        ega_analysis_results_rv$network_matrix <- data.frame(Message = "Network matrix not available from EGA output.")
      }
      
      dim_summary_text <- paste0(
        "Number of Dimensions (Communities) Identified: ", ega_output$n.dim %||% "N/A", "\n\n",
        "Item to Community Assignments:\n"
      )
      ega_analysis_results_rv$dimensionality_summary <- dim_summary_text
      
      if(!is.null(ega_output$wc)){
          item_comm_df <- data.frame(
              Item = names(ega_output$wc),
              Community = ega_output$wc,
              stringsAsFactors = FALSE
          )
          ega_analysis_results_rv$item_community_assignments <- item_comm_df
      } else {
          ega_analysis_results_rv$item_community_assignments <- data.frame(Message="Community assignments (wc) not available.")
      }

      # --- Render Outputs ---
      output$ega_network_table_output <- renderTable({
        validate(need(!is.null(ega_analysis_results_rv$network_matrix) && nrow(ega_analysis_results_rv$network_matrix) > 0 && !("Message" %in% colnames(ega_analysis_results_rv$network_matrix)), 
                      "Network matrix is not available or empty."))
        round(ega_analysis_results_rv$network_matrix, 3)
      }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)

      output$ega_network_plot_output <- renderPlot({
        validate(need(!is.null(ega_analysis_results_rv$ega_object), "EGA result object is not available for plotting."))
        # Use the S3 plot method for EGA objects
        plot(ega_analysis_results_rv$ega_object) 
      }, width = 750, height = 550) 

      output$ega_dimensionality_summary_output <- renderPrint({
        req(ega_analysis_results_rv$dimensionality_summary)
        cat(ega_analysis_results_rv$dimensionality_summary)
      })
      
      output$ega_item_community_table_output <- renderTable({
          req(ega_analysis_results_rv$item_community_assignments)
          validate(need(!("Message" %in% colnames(ega_analysis_results_rv$item_community_assignments)), "")) 
          ega_analysis_results_rv$item_community_assignments
      }, striped = TRUE, hover = TRUE, bordered = TRUE)

      showNotification("EGA analysis completed successfully!", type = "message", duration = 4)

    }, error = function(e) {
      cat("--- ERROR CAUGHT IN ega_server.R ---\n"); print(e); cat("--- END ---\n")
      user_error_message <- if (!is.null(conditionMessage(e))) conditionMessage(e) else "An unspecified error occurred."
      showNotification(paste("Error (EGA):", user_error_message), type = "error", duration = 10)
      ega_analysis_results_rv$ega_object <- NULL
      ega_analysis_results_rv$network_matrix <- data.frame(Error = paste("Analysis failed:",user_error_message)) 
      ega_analysis_results_rv$dimensionality_summary <- paste("Analysis failed:",user_error_message)
      ega_analysis_results_rv$item_community_assignments <- data.frame(Error = paste("Analysis failed:",user_error_message))
      output$ega_network_table_output <- renderTable({ ega_analysis_results_rv$network_matrix })
      output$ega_network_plot_output <- renderPlot({ plot(NULL, xlim=c(0,1),ylim=c(0,1),main="Plot Error"); text(0.5,0.5,user_error_message)})
      output$ega_dimensionality_summary_output <- renderPrint({ cat(ega_analysis_results_rv$dimensionality_summary) })
      output$ega_item_community_table_output <- renderTable({ ega_analysis_results_rv$item_community_assignments })
    }) 
  }) 

  output$download_ega_network_button <- downloadHandler(
    filename = function() {
      paste0("ega_network_matrix_", input$ega_estimation_method_select, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      validate(need(!is.null(ega_analysis_results_rv$network_matrix) && !("Error" %in% colnames(ega_analysis_results_rv$network_matrix)) && nrow(ega_analysis_results_rv$network_matrix) > 0, 
                    "Network matrix is not available for download or contains an error."))
      write.csv(ega_analysis_results_rv$network_matrix, file, row.names = TRUE) 
    }
  )

  output$download_ega_plot_button <- downloadHandler(
    filename = function() {
      paste0("ega_network_plot_", input$ega_estimation_method_select, "_", Sys.Date(), ".svg") 
    },
    content = function(file) {
      validate(need(!is.null(ega_analysis_results_rv$ega_object), "EGA results are not available for plot download."))
      svg(file, width = 10, height = 7.5) 
      
      plot(ega_analysis_results_rv$ega_object) 
      dev.off() 
    }
  )
}
