
# Define server logic 
  
function(input, output, session) { 
  
  observeEvent(input$input_type, {
    
    if(input$input_type==1){
      
        # Reload app if disconnected
        observeEvent(input$disconnect, {
          session$close()
        })
        
        # Reload app button
        observeEvent(input$reload,session$reload())
        
        # On session end
        session$onSessionEnded(stopApp)
        
        # Upload message
        observeEvent(input$file, 
                     once=T,
                     ignoreInit=T,
                     {showModal(modalDialog(
                       title = "Reading Data", "Please Wait",
                       footer = NULL,
                       fade = FALSE,
                       easyClose = TRUE
                     ))
                       Sys.sleep(2)}, 
                     priority=100)
        
        # Upload data
        datainput <- reactive({ 
          
          ### Validations ###
          
          validate(need(input$file$datapath != "", "Please upload a CSV file."))
          
          validate(need(tools::file_ext(input$file$datapath) == "csv", "Error. Not a CSV file. Please upload a CSV file."))
          
          validate(need(try({enc_guessed <- guess_encoding(input$file$datapath)
          enc_guessed_first <- enc_guessed[[1]][1]
          datainput1 <- fread(input$file$datapath, 
                              header = "auto", 
                              sep="auto", 
                              dec="auto", 
                              encoding = "unknown",
                              data.table = FALSE, 
                              na.strings = "")
          colnames(datainput1) <- iconv(colnames(datainput1), enc_guessed_first, "UTF-8")
          col_names <- sapply(datainput1, is.character)
          
          datainput1[ ,col_names] <- sapply(datainput1[, col_names], function(col) iconv(col, enc_guessed_first, "UTF-8"))
          datainput1}),
          "Error. File cannot be read. Input is either empty, fully whitespace, or skip has been set after the last non-whitespace."
          )
          )
          
          validate(need(tryCatch(datainput1 <- fread(input$file$datapath,
                                                     header = "auto",
                                                     sep="auto",
                                                     dec="auto",
                                                     encoding = "unknown",
                                                     data.table = FALSE,
                                                     na.strings = ""), warning=function(w) {}),
                        "Error. The file cannot be read correctly. Discarded single-line footer."
          )
          )
          
          if (is.null(input$file))
            return(NULL)
          
          ### Data Input ###
          
          return(tryCatch({
            
            enc_guessed <- guess_encoding(input$file$datapath)
            enc_guessed_first <- enc_guessed[[1]][1]
            
            datainput1 <- fread(input$file$datapath, 
                                header = "auto", 
                                sep="auto", 
                                dec ="auto", 
                                encoding = "unknown", 
                                data.table = FALSE, 
                                na.strings = "")
            
            colnames(datainput1) <- iconv(colnames(datainput1), enc_guessed_first, "UTF-8")
            col_names <- sapply(datainput1, is.character)
            
            datainput1[ ,col_names] <- sapply(datainput1[, col_names], function(col) iconv(col, enc_guessed_first, "UTF-8"))
            datainput1}
            ,error=function(e) stop(safeError(e))
            
          ))
        }
        )
        
        # Row limits 
        observe({
          req(input$file, datainput())
          removeModal()
          
          if (nrow(datainput()) > 100000){
            showNotification("Maximum sample size exceeded. ", duration=30)
            Sys.sleep(5)
            session$close()
          }
          
          if (nrow(datainput()) < 20){
            showNotification("Error: Minimum 20 observations required. ", duration=30)
            Sys.sleep(5)
            session$close()
          }
        })
        
        # Select Exposure Variable
        #To dynamically scale selection window
        selection_length_min = 7
        selection_length_max = 15
        
        output$selection_exposure <- renderUI({
          
          req(datainput())
          removeModal()
          
          chooserInput("selection_exposure", "Available", "Selected",
                       colnames(datainput()), c(),
                       size = min(c(max(c(length(colnames(datainput())), selection_length_min)), selection_length_max)), 
                       multiple = FALSE)
          
        })
        
        observeEvent(input$selection_exposure, {
          
          factorinterest <- input$selection_exposure$right
          choices <-  unique(datainput()[,factorinterest])  
          updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reference_exposure", choices = choices)
          
        })
        
        referencename_exposure <- reactive({
          
          req(input$reference_exposure)
          input$reference_exposure
          
        })
        
        # Select Outcome Variable
        output$selection_outcome <- renderUI({
          
          req(datainput())
          removeModal()
          
          chooserInput("selection_outcome", "Available", "Selected",
                       colnames(datainput()), c(), size = min(c(max(c(length(colnames(datainput())), selection_length_min)), selection_length_max)), multiple = FALSE)
          
        })
        
        #  Dynamic component for the reference level of the factor of interest
        
        observeEvent(input$selection_outcome, {
          
          factorinterest <- input$selection_outcome$right
          choices <-  unique(datainput()[,factorinterest])  
          updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reference_outcome", choices = choices) 
        })
        
        referencename_outcome <- reactive({
          
          req(input$reference_outcome)
          input$reference_outcome
          
        })
        
        # Stop if column names not distinct or if too many columns selected
        
        observe({
          
          req(input$file, datainput())
          removeModal()
          
          if (length(input$selection_outcome$right) > 1 ){
            showNotification("Please select only one outcome variable.", duration=30)
            Sys.sleep(5)
            session$close()
          }
          
          if (length(input$selection_exposure$right) > 1 ){
            showNotification("Please select only one exposure variable.", duration=30)
            Sys.sleep(5)
            session$close()
          }
        })
        
        # check if a1,a2,b1,b2 is a numeric and suitable value
        observe({
          #debounced reactive inputs for the prior input fields
          debounce_a1 <- debounce(reactive(input$a1), 3000)
          debounce_a2 <- debounce(reactive(input$a2), 3000)
          debounce_b1 <- debounce(reactive(input$b1), 3000)
          debounce_b2 <- debounce(reactive(input$b2), 3000)
          
          #immediate validation for invalid values (non-numeric or ≤ 0, if the field is not empty)
          
          observeEvent(input$a1, {
            if ((!is.numeric(input$a1) && !is.na(input$a1)) || (input$a1 <= 0) && !is.na(input$a1)) {
              updateNumericInput(session, "a1", value = 0.5)
              showNotification("Please select a positive numeric value for a1.", type = "error", duration = 5)
            }
          })
          
          observeEvent(input$a2, {
            if ((!is.numeric(input$a2) && !is.na(input$a2)) || (input$a2 <= 0) && !is.na(input$a2)) {
              updateNumericInput(session, "a2", value = 0.5)
              showNotification("Please select a positive numeric value for a2.", type = "error", duration = 5)
            }
          })
          
          observeEvent(input$b1, {
            if ((!is.numeric(input$b1) && !is.na(input$b1)) || (input$b1 <= 0) && !is.na(input$b1)){
              updateNumericInput(session, "b1", value = 0.5)
              showNotification("Please select a positive numeric value for b1.", type = "error", duration = 5)
            }
          })
          
          observeEvent(input$b2, {
            if ((!is.numeric(input$b2) && !is.na(input$b2)) || (input$b2 <= 0) && !is.na(input$b2)) {
              updateNumericInput(session, "b2", value = 0.5)
              showNotification("Please select a positive numeric value for b2.", type = "error", duration = 5)
            }
          })
          
          #delayed validation for empty fields
          observeEvent(debounce_a1(), {
            if (is.na(debounce_a1()) || debounce_a1() == "") {
              updateNumericInput(session, "a1", value = 0.5)
              showNotification("Please enter a value for a1.", type = "error", duration = 5)
            }
          })
          
          observeEvent(debounce_a2(), {
            if (is.na(debounce_a2()) || debounce_a2() == "") {
              updateNumericInput(session, "a2", value = 0.5)
              showNotification("Please enter a value for a2.", type = "error", duration = 5)
            }
          })
          
          observeEvent(debounce_b1(), {
            if (is.na(debounce_b1()) || debounce_b1() == "") {
              updateNumericInput(session, "b1", value = 0.5)
              showNotification("Please enter a value for b1.", type = "error", duration = 5)
            }
          })
          
          observeEvent(debounce_b2(), {
            if (is.na(debounce_b2()) || debounce_b2() == "") {
              updateNumericInput(session, "b2", value = 0.5)
              showNotification("Please enter a value for b2.", type = "error", duration = 5)
            }
          })
        })
        # check if there are negative values or decimal values in the matrix and change them
        observe({
          matrix_values <- input$sample
          if (all(!is.na(matrix_values) & matrix_values != "")) {
            
            #extract and rename matrix values
            value_11 <- matrix_values[1, 1]
            value_12 <- matrix_values[1, 2]
            value_21 <- matrix_values[2, 1]
            value_22 <- matrix_values[2, 2]
            
            #define a function to correct invalid (negative or decimal) values
            validate_positive_integer <- function(value, row, col) {
              if (!is.numeric(value) || value < 0 || value != round(value)) {
                #correct the value if invalid (negative number or not an int)
                return(0)  
              }
              return(value)
            }
            
            #check and correct values for each matrix element
            corrected_value_11 <- validate_positive_integer(value_11, 1, 1)
            corrected_value_12 <- validate_positive_integer(value_12, 1, 2)
            corrected_value_21 <- validate_positive_integer(value_21, 2, 1)
            corrected_value_22 <- validate_positive_integer(value_22, 2, 2)
            
            #check if any changes were made
            if (value_11 != corrected_value_11 || value_12 != corrected_value_12 || 
                value_21 != corrected_value_21 || value_22 != corrected_value_22) {
              
              #create the corrected matrix
              corrected_matrix <- matrix(c(corrected_value_11, corrected_value_12, corrected_value_21, corrected_value_22), 
                                         nrow = 2, 
                                         ncol = 2, 
                                         byrow = TRUE,
                                         dimnames = list(c("Non-Outcome", "Outcome"), c("Non-Exposure", "Exposure")))
              
              #update the matrix in the UI
              updateMatrixInput(session, "sample", value = corrected_matrix)
              
              #show notification that invalid values were corrected
              showNotification("Invalid values (negative numbers or decimal values) have been set to 0.", 
                               type = "error", duration = 5)
            }
          }
        })
        
        # This creates a short-term storage location for a filepath 
        report <- reactiveValues(filepath = NULL) 
        
        # Render report
        observeEvent(input$generate, once=T,
                     ignoreInit=T,{
                       
                       req(input$file, datainput(), input$selection_outcome$right)
                       
                       src1 <- normalizePath('report_html.Rmd')
                       
                       # Temporarily switch to the temp dir
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src1, 'report_html.Rmd', overwrite = TRUE)
                       
                       # Set up parameters to pass to Rmd document
                       enc_guessed <- guess_encoding(input$file$datapath)
                       enc_guessed_first <- enc_guessed[[1]][1]
                       
                       params <- list(data = datainput(), 
                                      filename=input$file, 
                                      enc_guessed = enc_guessed_first, 
                                      outcome = input$selection_outcome$right, 
                                      exposure = input$selection_exposure$right, 
                                      presence_outcome = referencename_outcome(),
                                      presence_exposure = referencename_exposure(), 
                                      a1 = input$a1, 
                                      b1 = input$b1, 
                                      a2 = input$a2, 
                                      b2 = input$b2,
                                      user_selection_function_param = input$user_selection_function_param) 
                       
                       tryCatch({
                         
                         withProgress(message = 'Please wait, the Statsomat app is computing. This may take a while.', value=0, {
                           
                           for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                           }

                             tmp_file <- render('report_html.Rmd', html_document(),
                                                params = params,
                                                envir = new.env(parent = globalenv())
                             )
                           
                           report$filepath <- tmp_file 
                         })
                         
                         showNotification("Now you can download the file.", duration=20)
                         
                       },
                       
                       error=function(e) {
                         
                         # Report not available 
                         showNotification("Something went wrong. Please contact the support@statsomat.com. ",duration=20)
                         
                       }
                       )
                     })
        
        # Enable downloadbutton 
        observe({
          req(!is.null(report$filepath))
          session$sendCustomMessage("check_generation", list(check_generation  = 1))
        })
        
        # Download report
        output$download <- downloadHandler(
          filename = function() {
              paste('MyReport',sep = '.','html')
          },
          
          content = function(file) {
            
            file.copy(report$filepath, file)
            
          }
        )
      
      } else {
        
          # Reload app if disconnected
          observeEvent(input$disconnect, {
            session$close()
          })
          
          # Reload app button
          observeEvent(input$reload,session$reload())
          
          # On session end
          session$onSessionEnded(stopApp)
          
          report <- reactiveValues(filepath = NULL) 
          
          # Render report
          observeEvent(input$generate, once=TRUE,
                       ignoreInit=TRUE,{
                         
                         src1 <- normalizePath('report_html.Rmd')
                         src2 <- normalizePath('references.bib')
                         src5 <- normalizePath('FiraSans-Bold.otf')
                         src6 <- normalizePath('FiraSans-Regular.otf')
                         
                         # Temporarily switch to the temp dir
                         owd <- setwd(tempdir())
                         on.exit(setwd(owd))
                         file.copy(src1, 'report_html.Rmd', overwrite = TRUE)
                         file.copy(src2, 'references.bib', overwrite = TRUE)
                         file.copy(src5, 'FiraSans-Bold.otf', overwrite = TRUE)
                         file.copy(src6, 'FiraSans-Regular.otf', overwrite = TRUE)
                         
                         # Set up parameters to pass to Rmd document
                         params <- list(exposure = input$name_Exposure, 
                                        outcome = input$name_Outcome,
                                        a1 = input$a1, 
                                        b1 = input$b1, 
                                        a2 = input$a2, 
                                        b2 = input$b2,
                                        user_selection_function_param = input$user_selection_function_param,
                                        n_exposure1_outcome1 = input$sample[2,2], 
                                        n_exposure0_outcome1 = input$sample[2,1], 
                                        n_exposure1 = (input$sample[2,2]+input$sample[1,2]),
                                        n_exposure0 = (input$sample[2,1]+input$sample[1,1]))
                         
                         tryCatch({
                           
                           withProgress(message = 'Please wait, the Statsomat app is computing. This may take a while.', value=0, {
                             for (i in 1:15) {
                               incProgress(1/15)
                               Sys.sleep(0.25)
                             }
                             
                            
                            tmp_file <- render('report_html.Rmd', html_document(),
                                                  params = params,
                                                  envir = new.env(parent = globalenv())
                            )
                             
                             
                             
                            report$filepath <- tmp_file 
                             
                           })
                           
                           showNotification("Now you can download the file.", duration=20)
                           
                         },
                         
                         error=function(e) {
                           
                           # Report not available 
                           showNotification("Something went wrong. Please contact the support@statsomat.com. ",duration=20)
                         }
                         )
                       })
          
          # Enable downloadbutton 
          observe({
            req(!is.null(report$filepath))
            session$sendCustomMessage("check_generation", list(check_generation  = 1))
          })
          
          # Download report 
          output$download <- downloadHandler(
            
            filename = function() {
              paste('MyReport',sep = '.','html')
            },
            
            content = function(file) {
              
              file.copy(report$filepath, file)
            }
          )
      }
    }
    )
  }