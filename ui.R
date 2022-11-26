shinyUI(
  shinydashboardPlus::dashboardPage(
    # III-1- Dashboard header----
    shinydashboardPlus::dashboardHeader(
      title = tags$b(
        h4("Cranberry fertilizer prediction")
      ),
      dropdownMenu(type = 'message',
                   headerText = "Send feedback",
                   messageItem(
                     from = "wilfrieddossouyovo16@gmail.com",
                     message =  "",
                     icon = icon("envelope"),
                     href = "mailto:wilfrieddossouyovo16@gmail.com"
                   )
      ),
      controlbarIcon = NULL),
    # III-2- Dashboard Sidebar----
    shinydashboard::dashboardSidebar(
      sidebarMenu(
        shinyFeedback::useShinyFeedback(),
        downloadBttn(
          label = "Download template",
          outputId = "downloadData",
          style = "bordered",
          color = "warning",
          size = "sm"
        ),
        fileInput("upload", label = "Upload data", accept = c(".csv", ".tsv")),
        menuItem(text = "Predictions", tabName = "Modele_prediction", icon = icon("chart-line")),
        menuItem(text = "Datas", tabName = "datas", icon = icon("database"),
                 menuSubItem(text = "Uploaded data", tabName = "uploaded_data"),
                 menuSubItem(text = "Saved predictions", tabName = "saved_pred")),
        actionBttn("all_data", label = "Update data", icon = icon("arrows-rotate"),
                   size = "sm", style = "simple"),
        uiOutput("picker"),
        fluidRow(
          column(width = 12,
                 textInput("yield_last_year", label = "Yield (last year)", placeholder = "Add last year yield"),
                 textInput("name_field", label = "Field name", placeholder = "Add field name"),
                 textInput("comment", label = "Note", placeholder = "leave note")
          )
        ),
        menuItem(text = "Guideline", tabName = "guideline", icon = icon("book-open")),
        actionButton("github",
                     label = "Code source",
                     icon = icon("github"),
                     width = "115px",
                     onclick ="window.open(`https://github.com/DosouYovoW/cranberry_fertilizer_doses_prediction_app`, '_blank')",
                     style="color: #fff; background-color: #767676; border-color: #767676"
        )
      )
    ),
    
    # III-3  Dashboard Body----
    shinydashboard::dashboardBody(
      tags$style(".recalculating { opacity: inherit !important; }"), # stop flickering
      # dynamically adjust height and/or width of shiny-plotly output based on window size
      tags$head(tags$script(' 
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')), 
      ### III-3-1 Defining tabItems----
      tabItems(
        tabItem(tabName = "Modele_prediction",
                tags$head(
                  tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
                ),
                shinycssloaders::withSpinner(
                  proxy.height = "10px", hide.ui = FALSE, type = 2, size = .5,
                  color.background = "white", color = "blue",
                  valueBoxOutput("Yield_prediction", width = 3)
                ),
                shinycssloaders::withSpinner(
                  proxy.height = "10px", hide.ui = FALSE, type = 0,
                  valueBoxOutput("next_year_Yield_prediction", width = 3)
                ), 
                tabsetPanel(id = "tabset3", type = "tabs", 
                            tabPanel(title = "Fertilizer doses",
                                     
                                     width = 12,
                                     actionBttn("optimButton", label = 
                                                  paste0("Optimize \n", format(as_date(now()), "%Y"), "\n yield"),
                                                style = "simple", color = "success", icon = icon("arrow-trend-up")),
                                     
                                     # Add button
                                     actionBttn(inputId = "add.button", label = "Save prediction", 
                                                style = "simple", color = "warning", icon = icon("floppy-disk")
                                     ),
                                     fluidRow(
                                       column(width = 5,
                                              wellPanel(
                                                dropdownButton(
                                                  actionBttn("updateNFertilizer", label = "Update N fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !")
                                                ),
                                                sliderInput("N_fertilizer", label = "N fertilizer (lbs/acre)", min = 0, max = 80, 
                                                            value = round(mean(data$N_Fert), 1), step = .01),
                                                dropdownButton(
                                                  actionBttn("updatePFertilizer", label = "Update P fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("P_fertilizer", label = "P fertilizer (lbs/acre)", min = 0, max = 70,
                                                            value = round(mean(data$P_Fert), 1), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateKFertilizer", label = "Update K fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("K_fertilizer", label = "K fertilizer (lbs/acre)", min = 0, max = 200, 
                                                            value = round(mean(data$K_Fert), 1), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateSFertilizer", label = "Update S fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("S_fertilizer", label = "S fertilizer (lbs/acre)", min = 0, max = 1000,
                                                            value = round(mean(data$So_Fert), 1), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateCaFertilizer", label = "Update Ca fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("Ca_fertilizer", label = "Ca fertilizer (lbs/acre)", min = 0, max = 110, 
                                                            value = round(mean(data$Ca_Fert), 1), step = .01))),
                                       column(width = 5,
                                              wellPanel(
                                                dropdownButton(
                                                  actionBttn("updateMgFertilizer", label = "Update Mg fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("Mg_fertilizer", label = "Mg fertilizer (lbs/acre)", min = 0,  max = 70,
                                                            value = round(mean(data$Mg_Fert), 1), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateZnFertilizer", label = "Update Zn fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("Zn_fertilizer", label = "Zn fertilizer (lbs/acre)", min = 0, max = 4, 
                                                            value = round(mean(data$Zn_Fert), 2), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateCuFertilizer", label = "Update Cu fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("Cu_fertilizer", label = "Cu fertilizer (lbs/acre)", min = 0, max = 4, 
                                                            value = round(mean(data$Cu_Fert), 2), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateBFertilizer", label = "Update B fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("B_fertilizer", label = "B fertilizer (lbs/acre)", min = 0, max = 2, 
                                                            value = round(mean(data$B_Fert), 2), step = .01),
                                                dropdownButton(
                                                  actionBttn("updateMnFertilizer", label = "Update Mn fertilizers",
                                                             style = "simple", color = "royal", size = "xs"),
                                                  size = "xs", icon = icon("ellipsis"),
                                                  tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                  br()
                                                ),
                                                sliderInput("Mn_fertilizer", label = "Mn fertilizer (lbs/acre)", min = 0, max = .3,
                                                            value = round(mean(data$Mn_Fert), 2), step = .001)
                                                
                                              )),
                                       column(width = 2,
                                              wellPanel(
                                                tags$b(
                                                  h5(
                                                    strong("Optimizer control")
                                                    
                                                  )
                                                ),
                                                materialSwitch("NcheckBox", label = "N", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("PcheckBox", label = "P", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("KcheckBox", label = "K", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("ScheckBox", label = "S", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("CacheckBox", label = "Ca", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("MgcheckBox", label = "Mg", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("ZncheckBox", label = "Zn", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("CucheckBox", label = "Cu", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("BcheckBox", label = "B", value = TRUE, status = "success", right = TRUE),
                                                materialSwitch("MncheckBox", label = "Mn", value = TRUE, status = "success", right = TRUE),
                                                actionButton("reset", label = "Reset", icon = icon("refresh"))
                                              )
                                       )
                                     )
                            ),
                            tabPanel(title = "Soil analysis", 
                                     column(width = 12,
                                            tags$h5("Current year soils analysis")
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              shinydashboard::box(numericInput("PhEau", label = "spring water pH ", value = round(mean(data$PhEau), 2), step = .01)),
                                              shinydashboard::box(numericInput("Ca_Sol_ppm", label = "Ca_Sol (ppm)", round(mean(data2$`Ca_Sol (ppm)`), 2))),
                                              shinydashboard::box(numericInput("P_Sol_ppm", label = "P_Sol (ppm)", round(mean(data2$`P_Sol (ppm)`), 2))),
                                              shinydashboard::box(numericInput("Al_Sol_ppm", label = "Al_Sol (ppm)", round(mean(data2$`Al_Sol (ppm)`), 2))),
                                              shinydashboard::box(numericInput("K_Sol_ppm", label = "K_Sol (ppm)", round(mean(data2$`K_Sol (ppm)`), 2))),
                                              shinydashboard::box(numericInput("Mg_Sol_ppm", label = "Mg_Sol (ppm)", round(mean(data2$`Mg_Sol (ppm)`), 2), step = .01)),
                                              shinydashboard::box(numericInput("Zn_Sol_ppm", label = "Zn_Sol (ppm)", round(mean(data2$`Zn_Sol (ppm)`), 2), step = .01)),
                                              shinydashboard::box(numericInput("Cu_Sol_ppm", label = "Cu_Sol (ppm)", round(mean(data2$`Cu_Sol (ppm)`), 2), step = .01)),
                                              shinydashboard::box(numericInput("Mn_Sol_ppm", label = "Mn_Sol (ppm)", round(mean(data2$`Mn_Sol (ppm)`), 2), step = .01)),
                                              shinydashboard::box(numericInput("B_Sol_ppm", label = "B_Sol (ppm)", round(mean(data2$`B_Sol (ppm)`), 2), step = .01)),
                                              shinydashboard::box(numericInput("Fe_Sol_ppm", label = "Fe_Sol (ppm)", round(mean(data2$`Fe_Sol (ppm)`), 2)))
                                       )
                                     ),
                                     fluidRow(
                                       shinydashboardPlus::box(title = tags$h5("Current year soils balances"),
                                                               collapsible = TRUE,
                                                               valueBoxOutput("box", width = 6),
                                                               valueBoxOutput("box2", width = 6),
                                                               valueBoxOutput("box3", width = 6),
                                                               valueBoxOutput("box4", width = 6),
                                                               valueBoxOutput("box5", width = 6),
                                                               valueBoxOutput("box6", width = 6),
                                                               valueBoxOutput("box7", width = 6),
                                                               valueBoxOutput("box8", width = 6),
                                                               valueBoxOutput("box9", width = 6),
                                                               valueBoxOutput("box10", width = 6)
                                       ),
                                       shinydashboardPlus::box(title = tags$h5("predicted Next year soils balances"),
                                                               collapsible = TRUE,
                                                               valueBoxOutput("box_next1", width = 6),
                                                               valueBoxOutput("box_next2", width = 6),
                                                               valueBoxOutput("box_next3", width = 6),
                                                               valueBoxOutput("box_next4", width = 6),
                                                               valueBoxOutput("box_next5", width = 6),
                                                               valueBoxOutput("box_next6", width = 6),
                                                               valueBoxOutput("box_next7", width = 6),
                                                               valueBoxOutput("box_next8", width = 6),
                                                               valueBoxOutput("box_next9", width = 6),
                                                               valueBoxOutput("box_next10", width = 6)
                                       )
                                     )
                            ),
                            tabPanel(title = "Leaf analysis", 
                                     column(width = 12,
                                            tags$h5("Last year leaves analysis")
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              shinydashboard::box(numericInput("N_Fol_percent", label = "N_Fol (%)", 
                                                                               value = round(mean(data2$`N_Fol (%)`), 2), step = .001)),
                                              shinydashboard::box(numericInput("P_Fol_percent", label = "P_Fol (%)", 
                                                                               value = round(mean(data2$`P_Fol (%)`), 2), step = .0001)),
                                              shinydashboard::box(numericInput("K_Fol_percent", label = "K_Fol (%)", 
                                                                               value = round(mean(data2$`K_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Ca_Fol_percent", label = "Ca_Fol (%)", 
                                                                               value = round(mean(data2$`Ca_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Mg_Fol_percent", label = "Mg_Fol (%)", 
                                                                               value = round(mean(data2$`Mg_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("B_Fol_percent", label = "B_Fol (%)",   
                                                                               value = 0.0032 + round(mean(data2$`B_Fol (%)`), 2), step = .0001)),
                                              shinydashboard::box(numericInput("Cu_Fol_percent", label = "Cu_Fol (%)",   
                                                                               value = 0.00033 + round(mean(data2$`Cu_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Fe_Fol_percent", label = "Fe_Fol (%)",   
                                                                               value = round(mean(data2$`Fe_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Mn_Fol_percent", label = "Mn_Fol (%)",   
                                                                               value = round(mean(data2$`Mn_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Zn_Fol_percent", label = "Zn_Fol (%)",   
                                                                               value = 0.0032 + round(mean(data2$`Zn_Fol (%)`),2), step = .0001)),
                                              shinydashboard::box(numericInput("Al_Fol_percent", label = "Al_Fol (%)",   
                                                                               value = round(mean(data2$`Al_Fol (%)`),2), step = .0001))
                                       )
                                     ),
                                     fluidRow(
                                       shinydashboardPlus::box(title = tags$h5("Last year leaves balances"),
                                                               collapsible = TRUE,
                                                               valueBoxOutput("box_leaf1", width = 6),
                                                               valueBoxOutput("box_leaf2", width = 6),
                                                               valueBoxOutput("box_leaf3", width = 6),
                                                               valueBoxOutput("box_leaf4", width = 6),
                                                               valueBoxOutput("box_leaf5", width = 6),
                                                               valueBoxOutput("box_leaf6", width = 6),
                                                               valueBoxOutput("box_leaf7", width = 6),
                                                               valueBoxOutput("box_leaf8", width = 6),
                                                               valueBoxOutput("box_leaf9", width = 6),
                                                               valueBoxOutput("box_leaf10", width = 6),
                                                               valueBoxOutput("box_leaf11", width = 6)
                                       ),
                                       shinydashboardPlus::box(title = tags$h5("Predicted current year leaves balances"),
                                                               collapsible = TRUE,
                                                               valueBoxOutput("box_leaf_next1", width = 6),
                                                               valueBoxOutput("box_leaf_next2", width = 6),
                                                               valueBoxOutput("box_leaf_next3", width = 6),
                                                               valueBoxOutput("box_leaf_next4", width = 6),
                                                               valueBoxOutput("box_leaf_next5", width = 6),
                                                               valueBoxOutput("box_leaf_next6", width = 6),
                                                               valueBoxOutput("box_leaf_next7", width = 6),
                                                               valueBoxOutput("box_leaf_next8", width = 6),
                                                               valueBoxOutput("box_leaf_next9", width = 6),
                                                               valueBoxOutput("box_leaf_next10", width = 6),
                                                               valueBoxOutput("box_leaf_next11", width = 6)
                                       )
                                     ) 
                            ),
                            tabPanel(title = "Climate index & others", 
                                     column(width = 12,
                                            tags$h5("Climate index date range:", strong("May 01 - October 31"))
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              shinydashboard::box(numericInput("total_precip", label = "Seasonal total precipitation(mm)", value = round(mean(data$total_precip), 2))),
                                              shinydashboard::box(numericInput("frozen", label = "Seasonal number of freezing (min temp < 5°C) days", value = round(mean(data$frozen), 0))),
                                              shinydashboard::box(numericInput("Age", label = "Field age (year)", value = round(mean(data$Age), 0))),
                                              shinydashboard::box(numericInput("Purety", label = "Purety",value = round(mean(data$Pureté), 2), step = .01)),
                                              shinydashboard::box(selectInput("Regie", label = "Regie", choices =  c("Conventional", "Organic"))),
                                              shinydashboard::box(selectInput("Soil_type", label = "Soil type", choices = c("Sand", "Organic"))),
                                              shinydashboard::box(selectInput("Variete", label = "Variety", choices = c("new_productive", "old", "stevens_grygleski_gh1")))
                                       )
                                     )
                            )
                )
        ),
        tabItem(tabName = "uploaded_data", 
                dataTableOutput("table")
        ),
        tabItem(tabName = "saved_pred", 
                actionButton(inputId = "delete.button", label = "Delete rows", icon = 
                               icon("minus")),
                dataTableOutput('tablePred')
        ),
        tabItem(tabName = "guideline")
      )
    )
  )
)
