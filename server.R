shinyServer(
  function(input, output, session){
    keep_alive <- shiny::reactiveTimer(intervalMs = 10000, 
                                       session = shiny::getDefaultReactiveDomain())
    
    shiny::observe({keep_alive()})
    # Download template data
    dataUpload <- read_csv2("Data/Fichier template app.csv")
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv2(dataUpload, file, na = "", row.names = FALSE) 
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
                      #id = unique_id,
                      Soil_type = `type de sol`,
                      #mean_temp = `temp moy saisonale`,
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
        dplyr::mutate(Variete = tolower(Variete), 
          Variete = case_when(Variete %in% new_productive ~  "new_productive",
                                                    Variete %in% old ~ "old",
                                                    Variete %in% stevens_grygleski_gh1 ~ "stevens_grygleski_gh1"), 
          Regie = str_replace_all(Regie, 
                                              c("biologique" = "Organic", "conventionnelle" = "Conventional")),
                      Soil_type = str_replace_all(Soil_type, 
                                                  c("sable" = "Sand", "organique" = "Organic"))) |>
        filter(id %in% input$pick)
      return(filteredData)
    })
    
    # id picker----
    output$picker <- renderUI({
      data_upload <- data_upload() 
      pickerInput(inputId = 'pick', 
                  label = 'Choose field id', 
                  choices = data_upload$id,
                  options = pickerOptions(liveSearch = TRUE, size = 10))
    })
    
    # Display upload data----
    output$table <- renderDataTable({
      dt <- datatable(data_upload(), extensions = c("FixedColumns", 'Buttons'),
                      rownames = FALSE, 
                      options = list(scrollX = TRUE, scrollY = TRUE,
                                     pageLength = 20, 
                                     lengthMenu = list(c(10, 20, 30, -1), c("10", "20", "30", "Tout")),
                                     dom = 'Blfrtip', 
                                     buttons = c('excel', 'print'),
                                     fixedColumns = list(leftColumns = 2)
                      )
      )
      return(dt)
    })
    
   
    # Reset parameters -----
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
    
    # Update fertilizers-----
    ## Update all fertlizers----
    update_fn <- function(x, y){
      updateNumericInput(session, x, value = y)
    }
    observeEvent(input$all_data, {
      req(input$pick, cancelOutput = TRUE)
      filterData <- filterData()
      
      update_fn("N_fertilizer", filterData$N_Fert)
      update_fn("P_fertilizer", filterData$P_Fert)
      update_fn("K_fertilizer", filterData$K_Fert)
      update_fn("Ca_fertilizer", filterData$Ca_Fert)
      update_fn("Mg_fertilizer", filterData$Mg_Fert)
      update_fn("S_fertilizer", filterData$S_Fert)
      update_fn("Zn_fertilizer", filterData$Zn_Fert)
      update_fn("Cu_fertilizer", filterData$Cu_Fert)
      update_fn("B_fertilizer", filterData$B_Fert)
      update_fn("Mn_fertilizer", filterData$Mn_Fert)
      update_fn("Ca_Sol_ppm", filterData$`Ca_Sol (ppm)`)
      update_fn("P_Sol_ppm", filterData$`P_Sol (ppm)`)
      update_fn("Al_Sol_ppm", filterData$`Al_Sol (ppm)`)
      update_fn("K_Sol_ppm", filterData$`K_Sol (ppm)`)
      update_fn("Mg_Sol_ppm", filterData$`Mg_Sol (ppm)`)
      update_fn("Zn_Sol_ppm", filterData$`Zn_Sol (ppm)`)
      update_fn("Cu_Sol_ppm", filterData$`Cu_Sol (ppm)`)
      update_fn("Mn_Sol_ppm", filterData$`Mn_Sol (ppm)`)
      update_fn("B_Sol_ppm", filterData$`B_Sol (ppm)`)
      update_fn("Fe_Sol_ppm", filterData$`Fe_Sol (ppm)`)
      update_fn("N_Fol_percent", filterData$`N_Fol (%)`)
      update_fn("P_Fol_percent", filterData$`P_Fol (%)`)
      update_fn("K_Fol_percent", filterData$`K_Fol (%)`)
      update_fn("Ca_Fol_percent", filterData$`Ca_Fol (%)`)
      update_fn("Mg_Fol_percent", filterData$`Mg_Fol (%)`)
      update_fn("B_Fol_percent", filterData$`B_Fol (%)`)
      update_fn("Cu_Fol_percent", filterData$`Cu_Fol (%)`)
      update_fn("Fe_Fol_percent", filterData$`Fe_Fol (%)`)
      update_fn("Mn_Fol_percent", filterData$`Mn_Fol (%)`)
      update_fn("Zn_Fol_percent", filterData$`Zn_Fol (%)`)
      update_fn("Al_Fol_percent", filterData$`Al_Fol (%)`)
      #update_fn("mean_temp", filterData$mean_temp)
      update_fn("total_precip", filterData$total_precip)
      update_fn("frozen", filterData$frozen)
      update_fn("PhEau", filterData$PhEau)
      update_fn("Age", filterData$Age)
      update_fn("Purety", filterData$Purete)
      updateTextInput(session, "name_field", value = filterData$`id champs`)
      updateTextInput(session, "yield_last_year", value = filterData$`rendement annee precedante`)
      updateSelectInput(session, "Regie", choices = filterData$Regie)
      updateSelectInput(session, "Soil_type", choices = filterData$Soil_type)
      updateSelectInput(session, "Variete", choices = filterData$Variete)
    })
    
    ## Update each fertilizer ------
    ## Update N fertilizer
    observeEvent(input$updateNFertilizer, {
      filterData <- filterData()
      updateNumericInput(session,
                         "N_fertilizer", value = filterData$N_Fert)
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
    
    
    
    # Soil balance computation-----
    bal_soil_reactive <- reactive({
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
    }) 
    
    
    # Leaf balance computation-----
    bal_leaf_reactive <- reactive({
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
    
    # Soil value box-----
    soil_box_fn <- function(x) {
      renderValueBox({
        Box1 <- bal_soil_reactive() 
        valueBox(value = round(Box1[x], 2), subtitle = names(Box1)[x])
      })
    }
    
    output$box <- soil_box_fn(1)
    output$box2 <- soil_box_fn(2)
    output$box3 <- soil_box_fn(3)
    output$box4 <- soil_box_fn(4)
    output$box5 <- soil_box_fn(5)
    output$box6 <- soil_box_fn(6)
    output$box7 <- soil_box_fn(7)
    output$box8 <- soil_box_fn(8)
    output$box9 <- soil_box_fn(9)
    output$box10 <- soil_box_fn(10)
    
    # Leaf value box-----
    leaf_box_fn <- function(x) {
      renderValueBox({
        Box_leaf <- bal_leaf_reactive()
        valueBox(value = round(Box_leaf[x], 2), subtitle = names(Box_leaf)[x])
      })
    }
    
    
    output$box_leaf1 <- leaf_box_fn(1)
    output$box_leaf2 <- leaf_box_fn(2)
    output$box_leaf3 <- leaf_box_fn(3)
    output$box_leaf4 <- leaf_box_fn(4)
    output$box_leaf5 <- leaf_box_fn(5)
    output$box_leaf6 <- leaf_box_fn(6)
    output$box_leaf7 <- leaf_box_fn(7)
    output$box_leaf8 <- leaf_box_fn(8)
    output$box_leaf9 <- leaf_box_fn(9)
    output$box_leaf10 <- leaf_box_fn(10)
    output$box_leaf11 <- leaf_box_fn(11)
    
    # Yield prediction-----
    data_to_predict <- reactive({
      Box_leaf <- bal_leaf_reactive()
      Box1 <- bal_soil_reactive()
      req(input$Al_Sol_ppm, cancelOutput = TRUE)
      
      #Validate output error-----
        validate(need(input$Purety != "", "Purety: missing value, please check app guideline"))
        validate(need(input$frozen != "", "number of freezing day: missing value, please check app guideline")) 
        #validate(need(input$mean_temp != "", "mean temperature: missing value, please check app guideline")) 
        validate(need(input$total_precip != "", "total precipitation: missing value, please check app guideline")) 
        validate(need(input$Age != "", "Age: missing value, please check app guideline")) 
        validate(need(input$PhEau != "", "pH: missing value, please check app guideline")) 
        validate(need(input$Ca_Sol_ppm != "", "Ca soil analysis: missing value, please check app guideline"))
        validate(need(input$P_Sol_ppm != "", "P soil analysis: missing value, please check app guideline")) 
        validate(need(input$K_Sol_ppm != "", "K soil analysis: missing value, please check app guideline")) 
        validate(need(input$Al_Sol_ppm != "", "Al soil analysis: missing value, please check app guideline")) 
        validate(need(input$Mg_Sol_ppm != "", "Mg soil analysis: missing value, please check app guideline")) 
        validate(need(input$Zn_Sol_ppm != "", "Zn soil analysis: missing value, please check app guideline")) 
        validate(need(input$Cu_Sol_ppm != "", "Cu soil analysis: missing value, please check app guideline")) 
        validate(need(input$Mn_Sol_ppm != "", "Mn soil analysis: missing value, please check app guideline")) 
        validate(need(input$B_Sol_ppm != "", "B soil analysis: missing value, please check app guideline")) 
        validate(need(input$Fe_Sol_ppm != "", "Fe soil analysis: missing value, please check app guideline")) 
        if(!(input$Regie %in% c("Organic", "Conventional"))){
          validate("Regie: missing value, please check app guideline") 
        }
        if(!(input$Soil_type %in% c("Sand", "Organic"))){
          validate("Soil type: missing value, please check app guideline") 
        }
        if(!(input$Variete %in% c("stevens_grygleski_gh1", "old", "new_productive"))){
          validate("variety: missing value, please check app guideline") 
        }
      
      dt <- tibble(
        #"mean_temp" = input$mean_temp,
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
        "Variete" = case_when(input$Variete == "stevens_grygleski_gh1" ~ 1,
                              input$Variete == "old" ~ 2,
                              input$Variete == "new_productive" ~ 3),
        "Regie" = if_else(input$Regie == "Organic", 0, 1),
        "Soil_type" = if_else(input$Soil_type == "Sand", 0, 1)
      )
      return(dt)
    }) 
    
    pred <- reactive({
      data_to_predict <- data_to_predict()
      pred <- stats::predict(
        model, bake(gaussian_recipe, data_to_predict)
      )
    })
    
    output$Yield_prediction <- renderValueBox({
      pred <- pred()
      Sys.sleep(0.01)
      valueBox(
        value =   round(pred^2, 0), 
        subtitle = paste0(format(as_date(now()), "%Y"), "\n", "Predicted", "\n",  
                          "Yield(lbs/acre)"), color = "olive", #"olive"
        icon = icon("chart-line")
      )
    })
    
    optimYield <- eventReactive(input$optimButton, {
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
        select(frozen, PhEau, total_precip, Regie, Variete, `Pureté`, Age,
               starts_with(c("Leaf_", "soil_")),  Soil_type)|>
        names()
      conditional_features <- c(conditional_features,features_if)
      
      managed_features <-  data_to_predict |>
        select(ends_with("Fert"), -Mn_Fert) |> 
        names()
      managed_features <- managed_features[!(managed_features %in% features_if)]
      
      observation <- bake(gaussian_recipe, data_to_predict())
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
    
    # Update optimizer-------
    ## Update all optimization
    observeEvent(input$optimButton, {
      #input$optimButton
      progress <- Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = 'Optimization in progress',
                   detail = 'This may take a while...')
      
      for (i in 1:15) {
        progress$set(value = i)
        Sys.sleep(0.1)
      }
      
      optimYield <- optimYield()
      update_fn("N_fertilizer", optimYield$N_Fert)
      update_fn("P_fertilizer", optimYield$P_Fert)
      update_fn("K_fertilizer", optimYield$K_Fert)
      update_fn("Ca_fertilizer", optimYield$Ca_Fert)
      update_fn("Mg_fertilizer", optimYield$Mg_Fert)
      update_fn("S_fertilizer", optimYield$So_Fert)
      update_fn("Zn_fertilizer", optimYield$Zn_Fert)
      update_fn("Cu_fertilizer", optimYield$Cu_Fert)
      update_fn("B_fertilizer", optimYield$B_Fert)
    })
    
    # Next year yield prediction-------
    ## Reactive function for next year data to predict----
    data_to_predict_next <- reactive({
      # filterData <- filterData()
      pred <- pred()
      data_to_predict <- data_to_predict() |> 
        mutate(Rendement = round(pred^2, 0))
      
      data_next <- data_to_predict() |>
        mutate(
          "Age" = 1+input$Age,
          
          "Leaf_Fv.AlZnMnFeCuBMgCaKPN" = as.numeric(stats::predict(model_Leaf_Fv_vs_AlZnMnFeCuBMgCaKPN,
                                                        data_to_predict)), 
          "Leaf_Al.ZnMnFeCuBMgCaKPN" = as.numeric(stats::predict(model_Leaf_Al_vs_ZnMnFeCuBMgCaKPN, 
                                                      data_to_predict)), 
          "Leaf_Mn.Fe" = as.numeric(stats::predict(model_Leaf_Mn_vs_Fe, 
                                        data_to_predict)), 
          "Leaf_ZnCuBMgCaKPN.MnFe" = as.numeric(stats::predict(model_Leaf_ZnCuBMgCaKPN_vs_MnFe, 
                                                    data_to_predict)), 
          "Leaf_Cu.Zn" = as.numeric(stats::predict(model_Leaf_Cu_vs_Zn, 
                                        data_to_predict)), 
          "Leaf_B.ZnCu" = as.numeric(stats::predict(model_Leaf_B_vs_ZnCu, 
                                         data_to_predict)), 
          "Leaf_MgCaKPN.ZnCuB" = as.numeric(stats::predict(model_Leaf_MgCaKPN_vs_ZnCuB, 
                                                data_to_predict)), 
          "Leaf_Ca.Mg" = as.numeric(stats::predict(model_Leaf_Ca_vs_Mg, 
                                        data_to_predict)), 
          "Leaf_K.MgCa" = as.numeric(stats::predict(model_Leaf_K_vs_MgCa, 
                                         data_to_predict)), 
          "Leaf_PN.MgCaK" = as.numeric(stats::predict(model_Leaf_PN_vs_MgCaK, 
                                           data_to_predict)), 
          "Leaf_P.N" = as.numeric(stats::predict(model_Leaf_P_vs_N, 
                                      data_to_predict)), 
          "soil_Fv.FeBMnCuZnMgKAlPCa" = as.numeric(stats::predict(model_soil_Fv_vs_FeBMnCuZnMgKAlPCa, 
                                                       data_to_predict)), 
          "soil_Fe.Al" = as.numeric(stats::predict(model_soil_Fe_vs_Al, 
                                        data_to_predict)), 
          "soil_FeAl.BMnCuZnMgKPCa" = as.numeric(stats::predict(model_soil_FeAl_vs_BMnCuZnMgKPCa, 
                                                     data_to_predict)), 
          "soil_Mn.B" = as.numeric(stats::predict(model_soil_Mn_vs_B_next, 
                                       data_to_predict)), 
          "soil_BMn.Zn" = as.numeric(stats::predict(model_soil_BMn_vs_Zn_next, 
                                         data_to_predict)), 
          "soil_BMnZn.CuMgKPCa" = as.numeric(stats::predict(model_soil_BMnZn_vs_CuMgKPCa_next, 
                                                 data_to_predict)), 
          "soil_Cu.Mg" = as.numeric(stats::predict(model_soil_Cu_vs_Mg_next, 
                                        data_to_predict)), 
          "soil_CuMg.Ca" = as.numeric(stats::predict(model_soil_CuMg_vs_Ca_next, 
                                          data_to_predict)), 
          "soil_CuMgCa.KP" = as.numeric(stats::predict(model_soil_CuMgCa_vs_KP_next, 
                                            data_to_predict)), 
          "soil_K.P" = as.numeric(stats::predict(model_soil_K_vs_P_next, 
                                      data_to_predict)) 
        )
      return(data_next)
    }) 
    
    # next year Leaf balance value box-----
    leaf_box_fn_next <- function(x, model) {
      renderValueBox({
        pred <- pred()
        Box_leaf <- bal_leaf_reactive()
        data_to_predict <- data_to_predict() |> 
          mutate(Rendement = round(pred^2, 0))
        pred_next_fn <- stats::predict(model,
                                       data_to_predict)
        valueBox(value = round(pred_next_fn, 2), 
                 subtitle = names(Box_leaf)[x])
      })
    }
    
    output$box_leaf_next1 <- leaf_box_fn_next(1, model_Leaf_Fv_vs_AlZnMnFeCuBMgCaKPN)
    output$box_leaf_next2 <- leaf_box_fn_next(2, model_Leaf_Al_vs_ZnMnFeCuBMgCaKPN)
    output$box_leaf_next3 <- leaf_box_fn_next(3, model_Leaf_Mn_vs_Fe)
    output$box_leaf_next4 <- leaf_box_fn_next(4, model_Leaf_ZnCuBMgCaKPN_vs_MnFe)
    output$box_leaf_next5 <- leaf_box_fn_next(5, model_Leaf_Cu_vs_Zn)
    output$box_leaf_next6 <- leaf_box_fn_next(6, model_Leaf_B_vs_ZnCu)
    output$box_leaf_next7 <- leaf_box_fn_next(7, model_Leaf_MgCaKPN_vs_ZnCuB)
    output$box_leaf_next8 <- leaf_box_fn_next(8, model_Leaf_Ca_vs_Mg)
    output$box_leaf_next9 <- leaf_box_fn_next(9, model_Leaf_K_vs_MgCa)
    output$box_leaf_next10 <- leaf_box_fn_next(10, model_Leaf_PN_vs_MgCaK)
    output$box_leaf_next11 <- leaf_box_fn_next(11, model_Leaf_P_vs_N)
    
    # next year soil balance value box-----
    soil_box_fn_next <- function(x, model) {
      renderValueBox({
        pred <- pred()
        Box1 <- bal_soil_reactive() 
        data_to_predict <- data_to_predict() |> 
          mutate(Rendement = round(pred^2, 0))
        pred_next_fn <- stats::predict(model,
                                       data_to_predict)
          valueBox(value = round(pred_next_fn, 2), subtitle = names(Box1)[x])
        
      })
    }
    
    output$box_next1 <- soil_box_fn_next(1, model_soil_Fv_vs_FeBMnCuZnMgKAlPCa)
    output$box_next2 <- soil_box_fn_next(2, model_soil_Fe_vs_Al)
    output$box_next3 <- soil_box_fn_next(3, model_soil_FeAl_vs_BMnCuZnMgKPCa)
    output$box_next4 <- soil_box_fn_next(4, model_soil_Mn_vs_B_next)
    output$box_next5 <- soil_box_fn_next(5, model_soil_BMn_vs_Zn_next)
    output$box_next6 <- soil_box_fn_next(6, model_soil_BMnZn_vs_CuMgKPCa_next)
    output$box_next7 <- soil_box_fn_next(7, model_soil_Cu_vs_Mg_next)
    output$box_next8 <- soil_box_fn_next(8, model_soil_CuMg_vs_Ca_next)
    output$box_next9 <- soil_box_fn_next(9, model_soil_CuMgCa_vs_KP_next)
    output$box_next10 <- soil_box_fn_next(10, model_soil_K_vs_P_next)
    
 #   pred <- reactive({
  #    data_to_predict <- data_to_predict()
   #   pred <- stats::predict(
    #    model, bake(gaussian_recipe, data_to_predict)
     # )
    #})
    pred_next <- reactive({
      data_to_predict_next <- data_to_predict_next()
      stats::predict(
        model, bake(gaussian_recipe, data_to_predict_next)#data_test[1,])#data_to_predict_next)
      )
    })
    
    output$next_year_Yield_prediction <- renderValueBox({
      Sys.sleep(0.01)
      valueBox(
        value = round(pred_next()^2, 0), 
        subtitle = paste0( format(as_date(now()) %m+% years(1), "%Y"), "\n", "Predicted", 
                           "\n Yield(lbs/acre)"), color = "navy", icon("chart-line")
      )
    })
    
    # Saved data-----
    
    values <- reactiveValues()
    values$df <- x
    observeEvent(input$add.button,{
      cat("addEntry\n")
      print(input$name_field) # Afficher le rendement de l'année préceédante
      print(input$Regie)
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
      print(round(pred()^2, 0))
      print(round(pred_next()^2, 0))
      print(input$comment)
      newRow <- data.frame(input$name_field,
                           input$Regie,
                           input$N_fertilizer,
                           input$P_fertilizer,
                           input$K_fertilizer,
                           input$Mg_fertilizer,
                           input$S_fertilizer,
                           input$Ca_fertilizer,
                           input$Zn_fertilizer,
                           input$Cu_fertilizer,
                           input$B_fertilizer,
                           input$Mn_fertilizer,
                           round(pred()^2, 0),
                           round(pred_next()^2, 0),
                           input$comment)
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
                               buttons = c('excel', 'print'),
                               fixedColumns = list(leftColumns = 1)
                               
                )
      )
    })
    
    observeEvent(input$add.button, {
      show_alert(title = "Saved",
                 type = "success")
    })
    #observeEvent(input$reset, {
    #  show_alert(title = "Reset",
   #              type = "success")
   # })  
    
    # Feedbacks-------
    feedback_fn <- function(Id, Input, value){
        if(Input < round(min(value), 3) | Input > round(max(value), 3) | is.na(Input)){
          feedbackDanger(inputId = Id, show = TRUE, 
                         text = paste("Min: ", round(min(value), 3), "- Max: ", round(max(value), 3)))
        } else{
          
          feedbackDanger(Id, show = FALSE)
        }
    }
    
    observeEvent(input$N_Fol_percent, {
    feedback_fn("N_Fol_percent", input$N_Fol_percent, data2$`N_Fol (%)`)
    })
    observeEvent(input$P_Fol_percent, {
      feedback_fn("P_Fol_percent", input$P_Fol_percent, data2$`P_Fol (%)`)
    })
    observeEvent(input$K_Fol_percent, {
      feedback_fn("K_Fol_percent", input$K_Fol_percent, data2$`K_Fol (%)`)
    })
    observeEvent(input$Ca_Fol_percent, {
      feedback_fn("Ca_Fol_percent", input$Ca_Fol_percent, data2$`Ca_Fol (%)`)
    })
    observeEvent(input$Mg_Fol_percent, {
      feedback_fn("Mg_Fol_percent", input$Mg_Fol_percent, data2$`Mg_Fol (%)`)
    })
    observeEvent(input$B_Fol_percent, {
      feedback_fn("B_Fol_percent", input$B_Fol_percent, data2$`B_Fol (%)`)
    })
    observeEvent(input$Cu_Fol_percent, {
      feedback_fn("Cu_Fol_percent", input$Cu_Fol_percent, data2$`Cu_Fol (%)`)
    })
    observeEvent(input$Fe_Fol_percent, {
      feedback_fn("Fe_Fol_percent", input$Fe_Fol_percent, data2$`Fe_Fol (%)`)
    })
    observeEvent(input$Mn_Fol_percent, {
      feedback_fn("Mn_Fol_percent", input$Mn_Fol_percent, data2$`Mn_Fol (%)`)
    })
    observeEvent(input$Zn_Fol_percent, {
      feedback_fn("Zn_Fol_percent", input$Zn_Fol_percent, data2$`Zn_Fol (%)`)
    })
    observeEvent(input$Al_Fol_percent, {
      feedback_fn("Al_Fol_percent", input$Al_Fol_percent, data2$`Al_Fol (%)`)
    })
    observeEvent(input$Ca_Sol_ppm, {
      feedback_fn("Ca_Sol_ppm", input$Ca_Sol_ppm, data2$`Ca_Sol (ppm)`)
    })
    observeEvent(input$P_Sol_ppm, {
      feedback_fn("P_Sol_ppm", input$P_Sol_ppm, data2$`P_Sol (ppm)`)
    })
    observeEvent(input$Al_Sol_ppm, {
      feedback_fn("Al_Sol_ppm", input$Al_Sol_ppm, data2$`Al_Sol (ppm)`)
    })
    observeEvent(input$K_Sol_ppm, {
      feedback_fn("K_Sol_ppm", input$K_Sol_ppm, data2$`K_Sol (ppm)`)
    })
    observeEvent(input$Mg_Sol_ppm, {
      feedback_fn("Mg_Sol_ppm", input$Mg_Sol_ppm, data2$`Mg_Sol (ppm)`)
    })
    observeEvent(input$Zn_Sol_ppm, {
      feedback_fn("Zn_Sol_ppm", input$Zn_Sol_ppm, data2$`Zn_Sol (ppm)`)
    })
    observeEvent(input$Cu_Sol_ppm, {
      feedback_fn("Cu_Sol_ppm", input$Cu_Sol_ppm, data2$`Cu_Sol (ppm)`)
    })
    observeEvent(input$Mn_Sol_ppm, {
      feedback_fn("Mn_Sol_ppm", input$Mn_Sol_ppm, data2$`Mn_Sol (ppm)`)
    })
    observeEvent(input$B_Sol_ppm, {
      feedback_fn("B_Sol_ppm", input$B_Sol_ppm, data2$`B_Sol (ppm)`)
    })
    observeEvent(input$Fe_Sol_ppm, {
      feedback_fn("Fe_Sol_ppm", input$Fe_Sol_ppm, data2$`Fe_Sol (ppm)`)
    })
    observeEvent(input$PhEau, {
      feedback_fn("PhEau", input$PhEau, data2$PhEau)
    })
    observeEvent(input$Purety, {
      feedback_fn("Purety", input$Purety, data2$Pureté)
    })
    
    
  }
)
