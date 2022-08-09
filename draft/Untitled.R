shinyServer(
  function(input, output, session){
    keep_alive <- shiny::reactiveTimer(intervalMs = 10000, 
                                       session = shiny::getDefaultReactiveDomain())
    
    shiny::observe({keep_alive()})
    
    
    # keep app alive
    # autoInvalidate <- reactiveTimer(59000)
    #  observe({
    #   autoInvalidate()
    #  cat(".")
    # })  
    
    # Download template data
    dataUpload <- read_csv2("Fichier template app.csv")
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv2(dataUpload, file, na = "") 
      }
    )
    
    # upload data
    data_upload <- reactive({
      req(input$upload, cancelOutput = TRUE)
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ";" , 
                                locale = locale(grouping_mark = ".", decimal_mark = ",", encoding = "UTF-8")),
             #tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
             validate("Invalid file; Please upload a template.csv file")
      )
    })
    
    # Filter upload data by id
    filterData <- reactive({
      filteredData <- data_upload() |> 
        dplyr::rename(Regie = regie,
                      Soil_type = `type de sol`,
                      mean_temp = `temp moy saisonale`,
                      total_precip = `precip total saisonale`,
                      frozen = `nbr de jour de gel saisonale`,
                      PhEau = phEau,
                      Age = `age (annee)`, 
                      Purete = `purete (%)`,
                      N_Fert = `N_Fert (lbs/ac)`,
                      P_Fert = `P_Fert (lbs/ac)`,
                      K_Fert = `K_Fert (lbs/ac)`,
                      Ca_Fert = `Ca_Fert (lbs/ac)`,
                      Cu_Fert = `Cu_Fert (lbs/ac)`,
                      B_Fert = `B_Fert (lbs/ac)`,
                      Zn_Fert = `Zn_Fert (lbs/ac)`,
                      Mg_Fert = `Mg_Fert (lbs/ac)`,
                      S_Fert = `S_Fert (lbs/ac)`,
                      Mn_Fert = `Mn_Fert (lbs/ac)`) |>
        filter(id %in% input$pick)
      return(filteredData)
    })
    
    # id picker
    output$picker <- renderUI({
      data_upload <- data_upload() 
      pickerInput(inputId = 'pick', 
                  label = 'Choose field id', 
                  choices = data_upload$id)
    })
    
    #  output$picker2 <- renderUI({
    #   req(input$pick, cancelOutput = TRUE)
    #  filterData <- filterData()
    #   fluidRow(
    #    column(width = 12,
    #          textInput("name_field", label = "Field name", value = filterData$`id champs`),
    #         textInput("Regie", label = "Regie", value =  filterData$Regie),#c("Conventional", "Organic")),#
    #        textInput("Soil_type", label = "Soil type", value = filterData$Soil_type)#c("Sand", "Organic"))#
    #   )
    # )
    # }) 
    
    # Display upload data
    output$table <- renderDataTable({
      dt <- datatable(data_upload(), extensions = c("FixedColumns", 'Buttons'),
                      rownames = FALSE, 
                      options = list(scrollX = TRUE, scrollY = TRUE,
                                     pageLength = 20, 
                                     lengthMenu = list(c(10, 20, 30, -1), c("10", "20", "30", "Tout")),
                                     dom = 'Blfrtip', 
                                     buttons = c('csv', 'excel', 'pdf'),
                                     fixedColumns = list(leftColumns = 4)
                                     
                      )
      )
      return(dt)
    })
    
    # Feedbacks   
    observeEvent(input$P_fertilizer, {
      #req(input$P_fertilizer)
      #req(input$name_field, cancelOutput = TRUE)
      
      #filterData <- filterData()
      #req(input$name_field)
      #req(input$P_fertilizer)
      isp2 <- (input$P_Sol_ppm/ 31)*100 / ((input$Al_Sol_ppm/27) + (input$Fe_Sol_ppm/56)) 
      
      if(isp2 <= 3.5 & input$P_fertilizer > 31.22 & input$P_fertilizer <= 35.68){
        feedbackWarning(inputId = "P_fertilizer", show = TRUE, text = "Eutrophisation risk / Please lower P dosage")
      }else if(isp2 > 3.5 & isp2 <= 7 & input$P_fertilizer > 13.38 & input$P_fertilizer <= 17.84){
        feedbackWarning(inputId = "P_fertilizer", show = TRUE, text = "Eutrophisation risk / Please lower P dosage")
      }else if(isp2 <= 3.5 & input$P_fertilizer <= 31.22 | isp2 > 3.5 & isp2 <= 7 & input$P_fertilizer <= 13.38 | isp2 > 7 & input$P_fertilizer <= 0){ 
        
        feedback("P_fertilizer", show = FALSE) 
        
      }else{
        feedbackWarning(inputId = "P_fertilizer", show = TRUE, text = "Eutrophisation risk / Please lower P dosage")
      }
    })
    
    observeEvent(input$N_Fol_percent, {
      if(input$N_Fol_percent < 0.001 | input$N_Fol_percent > round(max(data2$`N_Fol (%)`), 2) | is.na(input$N_Fol_percent)){
        feedbackDanger(inputId = "N_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.001 - Max: ", round(max(data2$`N_Fol (%)`), 3)))
      } else{
        
        feedbackDanger("N_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$P_Fol_percent, {
      if(input$P_Fol_percent < 0.0001 | input$P_Fol_percent > round(max(data2$`P_Fol (%)`), 2) | is.na(input$P_Fol_percent)){
        feedbackDanger(inputId = "P_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.00001 - Max: ", round(max(data2$`P_Fol (%)`), 3)))
      } else{
        feedbackDanger("P_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$K_Fol_percent, {
      if(input$K_Fol_percent < 0.0001 | input$K_Fol_percent > round(max(data2$`K_Fol (%)`), 2) | is.na(input$K_Fol_percent)){
        feedbackDanger(inputId = "K_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.0001 - Max: ", round(max(data2$`K_Fol (%)`), 3)))
      } else{
        feedbackDanger("K_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Ca_Fol_percent, {
      if(input$Ca_Fol_percent < 0.0001 | input$Ca_Fol_percent > round(max(data2$`Ca_Fol (%)`), 2) | is.na(input$Ca_Fol_percent)){
        feedbackDanger(inputId = "Ca_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.0001 - Max: ", round(max(data2$`Ca_Fol (%)`), 3)))
      } else{
        feedbackDanger("Ca_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Mg_Fol_percent, {
      if(input$Ca_Fol_percent < 0.001 | input$Mg_Fol_percent > round(max(data2$`Mg_Fol (%)`), 2) | is.na(input$Mg_Fol_percent)){
        feedbackDanger(inputId = "Mg_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.001 - Max: ", round(max(data2$`Mg_Fol (%)`), 3)))
      } else{
        feedbackDanger("Ca_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$B_Fol_percent, {
      if(input$B_Fol_percent < 0.00001 | input$B_Fol_percent > round(max(data2$`B_Fol (%)`), 2) | is.na(input$B_Fol_percent)){
        feedbackDanger(inputId = "B_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.00001 - Max: ", round(max(data2$`B_Fol (%)`), 3)))
      } else{
        feedbackDanger("B_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Cu_Fol_percent, {
      if(input$Cu_Fol_percent < 0.000000001 | input$Cu_Fol_percent > round(max(data2$`Cu_Fol (%)`), 2) | is.na(input$Cu_Fol_percent)){
        feedbackDanger(inputId = "Cu_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.000000001 - Max: ", round(max(data2$`Cu_Fol (%)`), 3)))
      } else{
        feedbackDanger("Cu_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Fe_Fol_percent, {
      if(input$Fe_Fol_percent < 0.000001 | input$Fe_Fol_percent > round(max(data2$`Fe_Fol (%)`), 2) | is.na(input$Fe_Fol_percent)){
        feedbackDanger(inputId = "Fe_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.000001 - Max: ", round(max(data2$`Fe_Fol (%)`), 3)))
      } else{
        feedbackDanger("Fe_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Mn_Fol_percent, {
      if(input$Mn_Fol_percent < 0.0000001 | input$Mn_Fol_percent > round(max(data2$`Mn_Fol (%)`), 2) | is.na(input$Mn_Fol_percent)){
        feedbackDanger(inputId = "Mn_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.0000001 - Max: ", round(max(data2$`Mn_Fol (%)`), 3)))
      } else{
        feedbackDanger("Mn_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Zn_Fol_percent, {
      if(input$Zn_Fol_percent < 0.000000001 | input$Zn_Fol_percent > round(max(data2$`Zn_Fol (%)`), 2) | is.na(input$Zn_Fol_percent)){
        feedbackDanger(inputId = "Zn_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.000000001 - Max: ", round(max(data2$`Zn_Fol (%)`), 3)))
      } else{
        feedbackDanger("Zn_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Al_Fol_percent, {
      if(input$Al_Fol_percent < 0.000000001 | input$Al_Fol_percent > round(max(data2$`Al_Fol (%)`), 2) | is.na(input$Al_Fol_percent)){
        feedbackDanger(inputId = "Al_Fol_percent", show = TRUE, 
                       text = paste("Min: 0.000000001 - Max: ", round(max(data2$`Al_Fol (%)`), 3)))
      } else{
        feedbackDanger("Al_Fol_percent", show = FALSE)
      }
    })
    observeEvent(input$Ca_Sol_ppm, {
      if(input$Ca_Sol_ppm < round(min(data2$`Ca_Sol (ppm)`), 2) | input$Ca_Sol_ppm > round(max(data2$`Ca_Sol (ppm)`), 2) | is.na(input$Ca_Sol_ppm)){
        feedbackDanger(inputId = "Ca_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Ca_Sol (ppm)`), 2), "- Max:", round(max(data2$`Ca_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Ca_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$P_Sol_ppm, {
      if(input$P_Sol_ppm < round(min(data2$`P_Sol (ppm)`), 2) | input$P_Sol_ppm > round(max(data2$`P_Sol (ppm)`), 2) | is.na(input$P_Sol_ppm)){
        feedbackDanger(inputId = "P_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`P_Sol (ppm)`), 2), "- Max:", round(max(data2$`P_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("P_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Al_Sol_ppm, {
      if(input$Al_Sol_ppm < round(min(data2$`Al_Sol (ppm)`), 2) | input$Al_Sol_ppm > round(max(data2$`Al_Sol (ppm)`), 2) | is.na(input$Al_Sol_ppm)){
        feedbackDanger(inputId = "Al_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Al_Sol (ppm)`), 2), "- Max:", round(max(data2$`Al_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Al_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$K_Sol_ppm, {
      if(input$K_Sol_ppm < round(min(data2$`K_Sol (ppm)`), 2) | input$K_Sol_ppm > round(max(data2$`K_Sol (ppm)`), 2) | is.na(input$K_Sol_ppm)){
        feedbackDanger(inputId = "K_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`K_Sol (ppm)`), 2), "- Max:", round(max(data2$`K_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("K_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Mg_Sol_ppm, {
      if(input$Mg_Sol_ppm < round(min(data2$`Mg_Sol (ppm)`), 2) | input$Mg_Sol_ppm > round(max(data2$`Mg_Sol (ppm)`), 2) | is.na(input$Mg_Sol_ppm)){
        feedbackDanger(inputId = "Mg_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Mg_Sol (ppm)`), 2), "- Max:", round(max(data2$`Mg_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Mg_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Zn_Sol_ppm, {
      if(input$Zn_Sol_ppm < round(min(data2$`Zn_Sol (ppm)`), 2) | input$Zn_Sol_ppm > round(max(data2$`Zn_Sol (ppm)`), 2) | is.na(input$Zn_Sol_ppm)){
        feedbackDanger(inputId = "Zn_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Zn_Sol (ppm)`), 2), "- Max:", round(max(data2$`Zn_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Zn_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Cu_Sol_ppm, {
      if(input$Cu_Sol_ppm < round(min(data2$`Cu_Sol (ppm)`), 2) | input$Cu_Sol_ppm > round(max(data2$`Cu_Sol (ppm)`), 2) | is.na(input$Cu_Sol_ppm)){
        feedbackDanger(inputId = "Cu_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Cu_Sol (ppm)`), 2), "- Max:", round(max(data2$`Cu_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Cu_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Mn_Sol_ppm, {
      if(input$Mn_Sol_ppm < round(min(data2$`Mn_Sol (ppm)`), 2) | input$Mn_Sol_ppm > round(max(data2$`Mn_Sol (ppm)`), 2) | is.na(input$Mn_Sol_ppm)){
        feedbackDanger(inputId = "Mn_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Mn_Sol (ppm)`), 2), "- Max:", round(max(data2$`Mn_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Mn_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$B_Sol_ppm, {
      if(input$B_Sol_ppm < round(min(data2$`B_Sol (ppm)`), 2) | input$B_Sol_ppm > round(max(data2$`B_Sol (ppm)`), 2) | is.na(input$B_Sol_ppm)){
        feedbackDanger(inputId = "B_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`B_Sol (ppm)`), 2), "- Max:", round(max(data2$`B_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("B_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$Fe_Sol_ppm, {
      if(input$Fe_Sol_ppm < round(min(data2$`Fe_Sol (ppm)`), 2) | input$Fe_Sol_ppm > round(max(data2$`Fe_Sol (ppm)`), 2) | is.na(input$Fe_Sol_ppm)){
        feedbackDanger(inputId = "Fe_Sol_ppm", show = TRUE, 
                       text = paste("min:", round(min(data2$`Fe_Sol (ppm)`), 2), "- Max:", round(max(data2$`Fe_Sol (ppm)`), 3)))
      } else{
        feedbackDanger("Fe_Sol_ppm", show = FALSE)
      }
    })
    observeEvent(input$mean_temp, {
      if(input$mean_temp < round(min(data2$mean_temp), 2) | input$mean_temp > round(max(data2$mean_temp), 2) | is.na(input$mean_temp)){
        feedbackDanger(inputId = "mean_temp", show = TRUE, 
                       text = paste("min:", round(min(data2$mean_temp), 2), "- Max:", round(max(data2$mean_temp), 3)))
      } else{
        feedbackDanger("mean_temp", show = FALSE)
      }
    })
    observeEvent(input$total_precip, {
      if(input$total_precip < round(min(data2$total_precip), 2) | input$total_precip > round(max(data2$total_precip), 2) | is.na(input$total_precip)){
        feedbackDanger(inputId = "total_precip", show = TRUE, 
                       text = paste("min:", round(min(data2$total_precip), 2), "- Max:", round(max(data2$total_precip), 3)))
      } else{
        feedbackDanger("total_precip", show = FALSE)
      }
    })
    observeEvent(input$frozen, {
      if(input$frozen < round(min(data2$frozen), 2) | input$frozen > round(max(data2$frozen), 2) | is.na(input$frozen)){
        feedbackDanger(inputId = "frozen", show = TRUE, 
                       text = paste("min:", round(min(data2$frozen), 2), "- Max:", round(max(data2$frozen), 3)))
      } else{
        feedbackDanger("frozen", show = FALSE)
      }
    })
    observeEvent(input$PhEau, {
      if(input$PhEau < round(min(data2$PhEau), 2) | input$PhEau > round(max(data2$PhEau), 2) | is.na(input$PhEau)){
        feedbackDanger(inputId = "PhEau", show = TRUE, 
                       text = paste("min:", round(min(data2$PhEau), 2), "- Max:", round(max(data2$PhEau), 3)))
      } else{
        feedbackDanger("PhEau", show = FALSE)
      }
    })
    observeEvent(input$Age, {
      if(input$Age < round(min(data2$Age), 2) | input$Age > round(max(data2$Age), 2) | is.na(input$Age)){
        feedbackDanger(inputId = "Age", show = TRUE, 
                       text = paste("min:", round(min(data2$Age), 2), "- Max:", round(max(data2$Age), 3)))
      } else{
        feedbackDanger("Age", show = FALSE)
      }
    })
    observeEvent(input$Purety, {
      if(input$Purety < round(min(data2$Pureté), 2) | input$Purety > round(max(data2$Pureté), 2) | is.na(input$Purety)){
        feedbackDanger(inputId = "Purety", show = TRUE, 
                       text = paste("min:", round(min(data2$Pureté), 2), "- Max:", round(max(data2$Pureté), 3)))
      } else{
        feedbackDanger("Purety", show = FALSE)
      }
    })
    
    
    
    # Reset parameters
    observeEvent(input$reset, {
      updateMaterialSwitch(session, 
                           "NcheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "PcheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "KcheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "ScheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "CacheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "MgcheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "CucheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "BcheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "ZncheckBox", value = TRUE)
      updateMaterialSwitch(session, 
                           "MncheckBox", value = TRUE)
    })
    
    # Update fertilizers
    ## Update all fertlizers
    observeEvent(input$all_data, {
      filterData <- filterData()
      updateNumericInput(session,
                         "N_fertilizer", value = filterData$N_Fert)
      updateNumericInput(session,
                         "P_fertilizer", value = filterData$P_Fert)
      updateNumericInput(session,
                         "K_fertilizer", value = filterData$K_Fert)
      updateNumericInput(session,
                         "Ca_fertilizer", value = filterData$Ca_Fert)
      updateNumericInput(session,
                         "Mg_fertilizer", value = filterData$Mg_Fert)
      updateNumericInput(session,
                         "S_fertilizer", value = filterData$S_Fert)
      updateNumericInput(session,
                         "Zn_fertilizer", value = filterData$Zn_Fert)
      updateNumericInput(session,
                         "Cu_fertilizer", value = filterData$Cu_Fert)
      updateNumericInput(session,
                         "B_fertilizer", value = filterData$B_Fert)
      updateNumericInput(session,
                         "Mn_fertilizer", value = filterData$Mn_Fert)
      updateTextInput(session,
                      "name_field", value = filterData$`id champs`)
      updateSelectInput(session,
                        "Regie", choices = filterData$Regie)
      updateSelectInput(session,
                        "Soil_type", choices = filterData$Soil_type)
      updateNumericInput(session,
                         "Ca_Sol_ppm", value = filterData$`Ca_Sol (ppm)`)
      updateNumericInput(session,
                         "P_Sol_ppm", value = filterData$`P_Sol (ppm)`)
      updateNumericInput(session,
                         "AL_Sol_ppm", value = filterData$`Al_Sol (ppm)`)
      updateNumericInput(session,
                         "K_Sol_ppm", value = filterData$`K_Sol (ppm)`)
      updateNumericInput(session,
                         "Mg_Sol_ppm", value = filterData$`Mg_Sol (ppm)`)
      updateNumericInput(session,
                         "Zn_Sol_ppm", value = filterData$`Zn_Sol (ppm)`)
      updateNumericInput(session,
                         "Cu_Sol_ppm", value = filterData$`Cu_Sol (ppm)`)
      updateNumericInput(session,
                         "Mn_Sol_ppm", value = filterData$`Mn_Sol (ppm)`)
      updateNumericInput(session,
                         "B_Sol_ppm", value = filterData$`B_Sol (ppm)`)
      updateNumericInput(session,
                         "Fe_Sol_ppm", value = filterData$`Fe_Sol (ppm)`)
      updateNumericInput(session,
                         "N_Fol_percent", value = filterData$`N_Fol (%)`)
      updateNumericInput(session,
                         "P_Fol_percent", value = filterData$`P_Fol (%)`)
      updateNumericInput(session,
                         "K_Fol_percent", value = filterData$`K_Fol (%)`)
      updateNumericInput(session,
                         "Ca_Fol_percent", value = filterData$`Ca_Fol (%)`)
      updateNumericInput(session,
                         "Mg_Fol_percent", value = filterData$`Mg_Fol (%)`)
      updateNumericInput(session,
                         "B_Fol_percent", value = filterData$`B_Fol (%)`)
      updateNumericInput(session,
                         "Cu_Fol_percent", value = filterData$`Cu_Fol (%)`)
      updateNumericInput(session,
                         "Fe_Fol_percent", value = filterData$`Fe_Fol (%)`)
      updateNumericInput(session,
                         "Mn_Fol_percent", value = filterData$`Mn_Fol (%)`)
      updateNumericInput(session,
                         "Zn_Fol_percent", value = filterData$`Zn_Fol (%)`)
      updateNumericInput(session,
                         "Al_Fol_percent", value = filterData$`Al_Fol (%)`)
      updateNumericInput(session,
                         "mean_temp", value = filterData$mean_temp)
      updateNumericInput(session,
                         "total_precip", value = filterData$total_precip)
      updateNumericInput(session,
                         "frozen", value = filterData$frozen)
      updateNumericInput(session,
                         "PhEau", value = filterData$PhEau)
      updateNumericInput(session,
                         "Age", value = filterData$Age)
      updateNumericInput(session,
                         "Purety", value = filterData$Purete)
    })
    
    ## Update N fertilizer
    observeEvent(input$updateNFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "mean_temp", value = filterData$N_Fert)
    })
    
    ## Update P fertilizer
    observeEvent(input$updatePFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "P_fertilizer", value = filterData$P_Fert)
    })
    
    ## Update K fertilizer
    observeEvent(input$updateKFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "K_fertilizer", value = filterData$K_Fert)
    })
    
    ## Update Ca fertilizer
    observeEvent(input$updateCaFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "Ca_fertilizer", value = filterData$Ca_Fert)
    })
    
    ## Update Zn fertilizer
    observeEvent(input$updateZnFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "Zn_fertilizer", value = filterData$Zn_Fert)
    })
    
    ## Update Mg fertilizer
    observeEvent(input$updateMgFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "Mg_fertilizer", value = filterData$Mg_Fert)
    })
    
    ## Update S fertilizer
    observeEvent(input$updateSFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "S_fertilizer", value = filterData$S_Fert)
    })
    
    ## Update Cu fertilizer
    observeEvent(input$updateCuFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "Cu_fertilizer", value = filterData$Cu_Fert)
    })
    
    ## Update B fertilizer
    observeEvent(input$updateBFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "B_fertilizer", value = filterData$B_Fert)
    })
    
    ## Update Mn fertilizer
    observeEvent(input$updateMnFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "Mn_fertilizer", value = filterData$Mn_Fert)
    }) 
    
    
    # Climate index and others input
    #  output$climate_others <- renderUI({
    # req(filterData())
    #    filterData <- filterData()
    #     fluidRow(
    #      column(width = 12,
    #            shinydashboard::box(numericInput("mean_temp", label = "Seasonal mean temperature(°C)", round(filterData$mean_temp, 2))),
    #            shinydashboard::box(numericInput("total_precip", label = "Seasonal total precipitation(mm)", value = round(filterData$total_precip, 2))),
    #            shinydashboard::box(numericInput("frozen", label = "Seasonal number of freezing days", round(filterData$frozen, 2))),
    #           shinydashboard::box(numericInput("PhEau", label = "pHeau", round(filterData$PhEau, 2))),
    #           shinydashboard::box(numericInput("Age", label = "Field age (year)", round(filterData$Age, 2))),
    #           shinydashboard::box(numericInput("Purety", label = "Purety (%)",round(filterData$Purete, 2), step = .01))
    #    )
    #  )
    #}) |>
    #  bindCache(filterData())
    
    # Soil analysis input
    #  output$balance_soil <- renderUI({
    #   filterData <- filterData()
    #  fluidRow(
    #   column(width = 12,
    #         shinydashboard::box(numericInput("Ca_Sol_ppm", label = "Ca_Sol (ppm)", round(filterData$`Ca_Sol (ppm)`, 2))),
    #        shinydashboard::box(numericInput("P_Sol_ppm", label = "P_Sol (ppm)", round(filterData$`P_Sol (ppm)`, 2))),
    #       shinydashboard::box(numericInput("Al_Sol_ppm", label = "Al_Sol (ppm)", round(filterData$`Al_Sol (ppm)`, 2))),
    #      shinydashboard::box(numericInput("K_Sol_ppm", label = "K_Sol (ppm)", round(filterData$`K_Sol (ppm)`, 2))),
    #     shinydashboard::box(numericInput("Mg_Sol_ppm", label = "Mg_Sol (ppm)", round(filterData$`Mg_Sol (ppm)`, 2))),
    #    shinydashboard::box(numericInput("Zn_Sol_ppm", label = "Zn_Sol (ppm)", round(filterData$`Zn_Sol (ppm)`, 2))),
    #   shinydashboard::box(numericInput("Cu_Sol_ppm", label = "Cu_Sol (ppm)", round(filterData$`Cu_Sol (ppm)`, 2))),
    #  shinydashboard::box(numericInput("Mn_Sol_ppm", label = "Mn_Sol (ppm)", round(filterData$`Mn_Sol (ppm)`, 2))),
    #             shinydashboard::box(numericInput("B_Sol_ppm", label = "B_Sol (ppm)", round(filterData$`B_Sol (ppm)`, 2))),
    #            shinydashboard::box(numericInput("Fe_Sol_ppm", label = "Fe_Sol (ppm)", round(filterData$`Fe_Sol (ppm)`, 2)))
    #    )
    #     )
    #  })|>
    #   bindCache(filterData())
    
    # Leaf analysis input
    # output$balance_leaf <- renderUI({
    #  filterData <- filterData()
    # fluidRow(
    #  column(width = 12,
    #        shinydashboard::box(numericInput("N_Fol_percent", label = "N_Fol (%)", 
    #                        value = filterData$`N_Fol (%)`)),
    #      shinydashboard::box(numericInput("P_Fol_percent", label = "P_Fol (%)", 
    #                      value = filterData$`P_Fol (%)`)),
    #    shinydashboard::box(numericInput("K_Fol_percent", label = "K_Fol (%)", 
    #                    value = filterData$`K_Fol (%)`)),
    #            shinydashboard::box(numericInput("Ca_Fol_percent", label = "Ca_Fol (%)", 
    #                            value = filterData$`Ca_Fol (%)`)),
    #          shinydashboard::box(numericInput("Mg_Fol_percent", label = "Mg_Fol (%)", 
    #                          value = filterData$`Mg_Fol (%)`)),
    #        shinydashboard::box(numericInput("B_Fol_percent", label = "B_Fol (%)",   
    #                        value = filterData$`B_Fol (%)`)),
    #      shinydashboard::box(numericInput("Cu_Fol_percent", label = "Cu_Fol (%)",   
    #                      value = filterData$`Cu_Fol (%)`)),
    #    shinydashboard::box(numericInput("Fe_Fol_percent", label = "Fe_Fol (%)",   
    #                    value = filterData$`Fe_Fol (%)`)),
    #              shinydashboard::box(numericInput("Mn_Fol_percent", label = "Mn_Fol (%)",   
    #                              value = filterData$`Mn_Fol (%)`)),
    #            shinydashboard::box(numericInput("Zn_Fol_percent", label = "Zn_Fol (%)",   
    #                            value = filterData$`Zn_Fol (%)`)),
    #          shinydashboard::box(numericInput("Al_Fol_percent", label = "Al_Fol (%)",   
    #                          value = filterData$`Al_Fol (%)`))
    #      )
    #    )
    #  })
    
    # Soil balance computation
    bal_soil_reactive <- reactive({
      # filterData <- filterData()
      # req(input$name_field)#, cancelOutput = TRUE)
      bal_soil <- ilr(
        tibble(
          "Ca_Sol (ppm)" =  input$Ca_Sol_ppm,
          "P_Sol (ppm)" = input$P_Sol_ppm,
          "Al_Sol (ppm)" = input$Al_Sol_ppm,
          "K_Sol (ppm)" = input$K_Sol_ppm,
          "Mg_Sol (ppm)" = input$Mg_Sol_ppm,
          "Zn_Sol (ppm)" = input$Zn_Sol_ppm,
          "Cu_Sol (ppm)" = input$Cu_Sol_ppm,
          "Mn_Sol (ppm)" = input$Mn_Sol_ppm,
          "B_Sol (ppm)" = input$B_Sol_ppm,
          "Fe_Sol (ppm)" =  input$Fe_Sol_ppm) |>
          mutate( 
            "Fv" = 1e6 - `P_Sol (ppm)` - `K_Sol (ppm)` - `Ca_Sol (ppm)` - `Mg_Sol (ppm)` - `B_Sol (ppm)` - 
              `Cu_Sol (ppm)` - `Fe_Sol (ppm)` - `Al_Sol (ppm)` - `Mn_Sol (ppm)` - `Zn_Sol (ppm)`),
        V = gsi.buildilrBase(t(sbp_soil))
      ) 
      tibble(bal_soil)
      names(bal_soil) <- paste0("soil_", soil_bal_def)
      return(bal_soil)
    }) #|>
    #bindCache(filterData())
    
    
    # Leaf balance computation
    bal_leaf_reactive <- reactive({
      # filterData <- filterData()
      #req(input$name_field)#, cancelOutput = TRUE)
      bal_leaf <- ilr(
        tibble(
          "N_Fol (%)" = input$N_Fol_percent,
          "P_Fol (%)" = input$P_Fol_percent,
          "K_Fol (%)" = input$K_Fol_percent,
          "Ca_Fol (%)" = input$Ca_Fol_percent,
          "Mg_Fol (%)" = input$Mg_Fol_percent,
          "B_Fol (%)" = input$B_Fol_percent,
          "Cu_Fol (%)" = input$Cu_Fol_percent,
          "Fe_Fol (%)" = input$Fe_Fol_percent,
          "Mn_Fol (%)" = input$Mn_Fol_percent,
          "Zn_Fol (%)" = input$Zn_Fol_percent,
          "Al_Fol (%)" = input$Al_Fol_percent) |>
          mutate( 
            "Fv" = 100 - `N_Fol (%)` - `P_Fol (%)` - `K_Fol (%)` - `Ca_Fol (%)` - `Mg_Fol (%)` - 
              `B_Fol (%)` - `Cu_Fol (%)` - `Fe_Fol (%)` - `Mn_Fol (%)` - `Zn_Fol (%)` - `Al_Fol (%)`),
        V = gsi.buildilrBase(t(sbp_leaf))
      ) 
      tibble(bal_leaf)
      names(bal_leaf) <- paste0("Leaf_", leaf_bal_def)
      return(bal_leaf)
    }) 
    
    #  # Soil value box
    # soil_box_fn <- function(x) {
    #  renderValueBox({
    #      Box1 <- bal_soil_reactive() 
    #       valueBox(value = round(Box1[x], 2), subtitle = names(Box1)[x])
    #    })|>
    #      bindCache(bal_soil_reactive())
    #  }
    
    #   output$box <- soil_box_fn(1)
    #  output$box2 <- soil_box_fn(2)
    #  output$box3 <- soil_box_fn(3)
    #  output$box4 <- soil_box_fn(4)
    #  output$box5 <- soil_box_fn(5)
    #  output$box6 <- soil_box_fn(6)
    #  output$box7 <- soil_box_fn(7)
    #   output$box8 <- soil_box_fn(8)
    #  output$box9 <- soil_box_fn(9)
    #  output$box10 <- soil_box_fn(10)
    
    #  # Leaf value box
    #   leaf_box_fn <- function(x) {
    #    renderValueBox({
    #      Box_leaf <- bal_leaf_reactive()
    #       valueBox(value = round(Box_leaf[x], 2), subtitle = names(Box_leaf)[x])
    #     })|>
    #       bindCache(bal_leaf_reactive())
    #  }
    
    #   output$box_leaf1 <- leaf_box_fn(1)
    #   output$Box_leaf2 <- leaf_box_fn(2)
    #   output$box_leaf3 <- leaf_box_fn(3)
    #  output$box_leaf4 <- leaf_box_fn(4)
    #  output$box_leaf5 <- leaf_box_fn(5)
    #  output$box_leaf6 <- leaf_box_fn(6)
    #  output$box_leaf7 <- leaf_box_fn(7)
    #  output$box_leaf8 <- leaf_box_fn(8)
    #  output$box_leaf9 <- leaf_box_fn(9)
    #  output$box_leaf10 <- leaf_box_fn(10)
    #  output$box_leaf11 <- leaf_box_fn(11)
    
    # Yield prediction
    data_to_predict <- reactive({
      # filterData <- filterData()
      Box_leaf <- bal_leaf_reactive()
      Box1 <- bal_soil_reactive()
      #req(input$name_field, cancelOutput = TRUE)
      dt <- tibble(
        "mean_temp" = input$mean_temp,
        "total_precip" = input$total_precip,
        "frozen" = input$frozen,
        "Pureté" = input$Purety,
        "N_Fert" = input$N_fertilizer,
        "P_Fert" = input$P_fertilizer,
        "K_Fert" = input$K_fertilizer,
        "Mg_Fert" = input$Mg_fertilizer,
        "So_Fert" = input$S_fertilizer,
        "Ca_Fert" = input$Ca_fertilizer,
        "Zn_Fert" = input$Zn_fertilizer,
        "Cu_Fert" = input$Cu_fertilizer,
        "B_Fert" = input$B_fertilizer,
        "Mn_Fert" = input$Mn_fertilizer,
        "PhEau" = input$PhEau,
        "Age" = input$Age,
        "Leaf_Fv.AlZnMnFeCuBMgCaKPN" = Box_leaf[1],
        "Leaf_Al.ZnMnFeCuBMgCaKPN" = Box_leaf[2],
        "Leaf_Mn.Fe" = Box_leaf[3],
        "Leaf_ZnCuBMgCaKPN.MnFe" = Box_leaf[4],
        "Leaf_Cu.Zn" = Box_leaf[5],
        "Leaf_B.ZnCu" = Box_leaf[6],
        "Leaf_MgCaKPN.ZnCuB" = Box_leaf[7],
        "Leaf_Ca.Mg" = Box_leaf[8],
        "Leaf_K.MgCa" = Box_leaf[9],
        "Leaf_PN.MgCaK" = Box_leaf[10],
        "Leaf_P.N" = Box_leaf[11],
        "soil_Fv.FeBMnCuZnMgKAlPCa" = Box1[1], 
        "soil_Fe.Al" = Box1[2], 
        "soil_FeAl.BMnCuZnMgKPCa" = Box1[3], 
        "soil_Mn.B" = Box1[4],
        "soil_BMn.Zn" = Box1[5],
        "soil_BMnZn.CuMgKPCa" = Box1[6],
        "soil_Cu.Mg" = Box1[7],
        "soil_CuMg.Ca" = Box1[8],
        "soil_CuMgCa.KP" = Box1[9],
        "soil_K.P" = Box1[10],
        "Regie" = if_else(input$Regie == "Organic", 0, 1),
        "Soil_type" = if_else(input$Soil_type == "Sand", 0, 1)
      )
      return(dt)
    }) #|>
    # bindCache(
    #filterData(), 
    #   bal_leaf_reactive(), bal_soil_reactive(),
    #   input$name_field, input$N_fertilizer, input$P_fertilizer,
    #   input$K_fertilizer, input$Mg_fertilizer, input$Cu_fertilizer,
    #   input$Ca_fertilizer, input$Zn_fertilizer, input$S_fertilizer,
    #   input$B_fertilizer, input$Mn_fertilizer
    # )
    
    pred <- reactive({
      data_to_predict <- data_to_predict()
      pred <- stats::predict(
        model, bake(gaussian_recipe, data_to_predict)
      )
    })#|>
    # bindCache(data_to_predict())
    
    output$Yield_prediction <- renderValueBox({
      pred <- pred()
      valueBox(
        value =   round(pred^2, 0), 
        subtitle = paste0(format(as_date(now()), "%Y"), "\n", "Predicted", "\n",  
                          "Yield(lbs/acre)"), color = "olive",  icon = icon("chart-line")
      )
    })#|>
    # bindCache(pred())
    
    
    optimYield <- eventReactive(input$optimButton, {
      # filterData <- filterData()
      Box_leaf <- bal_leaf_reactive()
      Box1 <- bal_soil_reactive()
      data_to_predict <- data_to_predict()
      
      features_if <- c(
        if (input$NcheckBox == FALSE) print ("N_Fert"),
        if (input$PcheckBox == FALSE) print ("P_Fert"),
        if (input$KcheckBox == FALSE) print ("K_Fert"),
        if (input$ScheckBox == FALSE) print ("So_Fert"),
        if (input$CacheckBox == FALSE) print ("Ca_Fert"),
        if (input$MgcheckBox == FALSE) print ("Mg_Fert"),
        if (input$CucheckBox == FALSE) print ("Cu_Fert"),
        if (input$BcheckBox == FALSE) print ("B_Fert"),
        if (input$ZncheckBox == FALSE) print ("Zn_Fert")
      )
      
      conditional_features <- data_to_predict |>
        select(mean_temp, frozen, PhEau, total_precip, Regie, `Pureté`, Age,
               starts_with(c("Leaf_", "soil_")),  Soil_type)|>
        names()
      conditional_features <- c(conditional_features,features_if)
      
      managed_features <-  data_to_predict |>
        select(ends_with("Fert"), -Mn_Fert) |> 
        names()
      managed_features <- managed_features[!(managed_features %in% features_if)]
      
      observation <- bake(gaussian_recipe, data_to_predict)
      obs_managed <- observation |> select(all_of(managed_features))
      obs_conditions <- observation |> select(all_of(conditional_features))
      
      x_conditions <- as.vector(obs_conditions)
      
      minim_f <- function(x) {
        x <- data.frame(t(unlist(x)))
        all_x <- cbind(x, x_conditions)
        y <- stats::predict(model, all_x) 
        return(-y)
      }
      
      data_train_baked <-  bake(gaussian_recipe, data_train)
      data_test_baked <-  bake(gaussian_recipe, data_test)
      
      managed_min <- data_train_baked |> select(all_of(managed_features)) |> summarize_all(min)
      managed_max <- data_train_baked  |> select(all_of(managed_features)) |> summarize_all(max)
      
      berry_opt <- optim(
        par = obs_managed,
        fn = minim_f,
        method = "L-BFGS-B",
        lower = managed_min,
        upper = managed_max,
        control = list(maxit = 1000, factr = 1e5)
      )
      berry_opt
      
      managed_mean <- data_train |> select(all_of(managed_features)) |> summarize_all(mean)
      managed_sd <- data_train |> select(all_of(managed_features)) |> summarize_all(sd)
      
      berry_par_sc <- berry_opt$par
      berry_par <- data.frame(t(data.frame(berry_par_sc)))
      berry_par |>
        apply(MARGIN = 1, FUN = sum)
      berry_final  <- berry_par * managed_sd + managed_mean 
      berry_final
      return(berry_final)
    })
    
    observeEvent(input$optimButton, {
      #req(input$name_field, cancelOutput = TRUE)
      progressSweetAlert(
        session = session, id = "myprogress",
        title = "Optimization in progress",
        display_pct = TRUE, value = 0
      )
      for (i in seq_len(50)) {
        Sys.sleep(0.04)
        updateProgressBar(
          session = session,
          id = "myprogress",
          value = i*2
        )
      }
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Completed !",
        type = "success"
      )
    })
    
    # Update optimizer
    ## Update all optimization
    observeEvent(input$optimButton, {
      optimYield <- optimYield()
      updateNumericInput(session,
                         "N_fertilizer", value = optimYield$N_Fert)
      updateNumericInput(session,
                         "P_fertilizer", value = optimYield$P_Fert)
      updateNumericInput(session,
                         "K_fertilizer", value = optimYield$K_Fert)
      updateNumericInput(session,
                         "Ca_fertilizer", value = optimYield$Ca_Fert)
      updateNumericInput(session,
                         "Mg_fertilizer", value = optimYield$Mg_Fert)
      updateNumericInput(session,
                         "S_fertilizer", value = optimYield$So_Fert)
      updateNumericInput(session,
                         "Zn_fertilizer", value = optimYield$Zn_Fert)
      updateNumericInput(session,
                         "Cu_fertilizer", value = optimYield$Cu_Fert)
      updateNumericInput(session,
                         "B_fertilizer", value = optimYield$B_Fert)
    })
    
    
    
    # Next year yield prediction
    ## Reactive function for next year data to predict
    data_to_predict_next <- reactive({
      # filterData <- filterData()
      pred <- pred()
      data_to_predict <- data_to_predict() |> 
        mutate(Rendement = round(pred^2, 0))
      
      data_to_predict_next <- data_to_predict |>
        mutate(
          "Age" = 1+input$Age,
          
          "Leaf_Fv.AlZnMnFeCuBMgCaKPN" = stats::predict(model_Leaf_Fv_vs_AlZnMnFeCuBMgCaKPN_next,
                                                        data_to_predict), 
          "Leaf_Al.ZnMnFeCuBMgCaKPN" = stats::predict(model_Leaf_Al_vs_ZnMnFeCuBMgCaKPN, 
                                                      data_to_predict), 
          "Leaf_Mn.Fe" = stats::predict(model_Leaf_Mn_vs_Fe, 
                                        data_to_predict), 
          "Leaf_ZnCuBMgCaKPN.MnFe" = stats::predict(model_Leaf_ZnCuBMgCaKPN_vs_MnFe, 
                                                    data_to_predict), 
          "Leaf_Cu.Zn" = stats::predict(model_Leaf_Cu_vs_Zn, 
                                        data_to_predict), 
          "Leaf_B.ZnCu" = stats::predict(model_Leaf_B_vs_ZnCu, 
                                         data_to_predict), 
          "Leaf_MgCaKPN.ZnCuB" = stats::predict(model_Leaf_MgCaKPN_vs_ZnCuB, 
                                                data_to_predict), 
          "Leaf_Ca.Mg" = stats::predict(model_Leaf_Ca_vs_Mg, 
                                        data_to_predict), 
          "Leaf_K.MgCa" = stats::predict(model_Leaf_K_vs_MgCa, 
                                         data_to_predict), 
          "Leaf_PN.MgCaK" = stats::predict(model_Leaf_PN_vs_MgCaK, 
                                           data_to_predict), 
          "Leaf_P.N" = stats::predict(model_Leaf_P_vs_N, 
                                      data_to_predict), 
          "soil_Fv.FeBMnCuZnMgKAlPCa" = stats::predict(model_soil_Fv_vs_FeBMnCuZnMgKAlPCa, 
                                                       data_to_predict), 
          "soil_Fe.Al" = stats::predict(model_soil_Fe_vs_Al, 
                                        data_to_predict), 
          "soil_FeAl.BMnCuZnMgKPCa" = stats::predict(model_soil_FeAl_vs_BMnCuZnMgKPCa, 
                                                     data_to_predict), 
          "soil_Mn.B" = stats::predict(model_soil_Mn_vs_B_next, 
                                       data_to_predict), 
          "soil_BMn.Zn" = stats::predict(model_soil_BMn_vs_Zn_next, 
                                         data_to_predict), 
          "soil_BMnZn.CuMgKPCa" = stats::predict(model_soil_BMnZn_vs_CuMgKPCa_next, 
                                                 data_to_predict), 
          "soil_Cu.Mg" = stats::predict(model_soil_Cu_vs_Mg_next, 
                                        data_to_predict), 
          "soil_CuMg.Ca" = stats::predict(model_soil_CuMg_vs_Ca_next, 
                                          data_to_predict), 
          "soil_CuMgCa.KP" = stats::predict(model_soil_CuMgCa_vs_KP_next, 
                                            data_to_predict), 
          "soil_K.P" = stats::predict(model_soil_K_vs_P_next, 
                                      data_to_predict) 
        )
      return(data_to_predict_next)
    }) #|>
    #  bindCache(
    #   filterData(), pred(), data_to_predict()
    # )
    
    output$next_year_Yield_prediction <- renderValueBox({
      pred_next <- stats::predict(
        model, bake(gaussian_recipe, data_to_predict_next())
      )
      
      valueBox(
        value = round(pred_next^2, 0), 
        subtitle = paste0( format(as_date(now()) %m+% years(1), "%Y"), "\n", "Predicted", 
                           "\n Yield(lbs/acre)"), color = "navy", icon("chart-line")
      )
    })
    
    ###########################################
    
    values <- reactiveValues()
    values$df <- x
    
    observeEvent(input$add.button,{
      #req(input$name_field, cancelOutput = TRUE)
      cat("addEntry\n")
      print(input$name_field)
      print(input$N_fertilizer)
      print(input$P_fertilizer)
      print(input$K_fertilizer)
      print(input$Mg_fertilizer)
      print(input$S_fertilizer)
      print(input$Ca_fertilizer)
      print(input$Zn_fertilizer)
      print(input$Cu_fertilizer)
      print(input$B_fertilizer)
      print(input$Mn_fertilizer)
      newRow <- data.frame(input$name_field,
                           input$N_fertilizer,
                           input$P_fertilizer,
                           input$K_fertilizer,
                           input$Mg_fertilizer,
                           input$S_fertilizer,
                           input$Ca_fertilizer,
                           input$Zn_fertilizer,
                           input$Cu_fertilizer,
                           input$B_fertilizer,
                           input$Mn_fertilizer)
      colnames(newRow)<-colnames(values$df)
      values$df <- rbind(values$df,newRow)
      print(nrow(values$df))
    })
    
    observeEvent(input$delete.button,{
      cat("deleteEntry\n")
      if(is.null(input$tablePred_rows_selected)){
        values$df <- values$df[-nrow(values$df), ]
      } else {
        values$df <- values$df[-input$tablePred_rows_selected-1, ]
      }
    })  
    
    output$tablePred = renderDataTable({
      datatable(values$df[-1,], extensions = c("FixedColumns", 'Buttons'),
                rownames = FALSE, 
                options = list(scrollX = TRUE, scrollY = TRUE,
                               pageLength = 20, 
                               lengthMenu = list(c(10, 20, 30, -1), c("10", "20", "30", "Tout")),
                               dom = 'Blfrtip', 
                               buttons = c('excel', 'pdf'),
                               fixedColumns = list(leftColumns = 1)
                               
                )
      )
    })
    
    observeEvent(input$add.button, {
      # req(input$name_field, cancelOutput = TRUE)
      show_alert(title = "Saved",
                 # text = "Saved",
                 type = "success")
    })
    
    observeEvent(input$reset, {
      # req(input$name_field, cancelOutput = TRUE)
      show_alert(title = "Reset",
                 # text = "Saved",
                 type = "success")
    })  
    
    
    
    
  }
)
