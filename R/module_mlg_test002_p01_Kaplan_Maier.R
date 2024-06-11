


# # # 01) UI - Selection for 'database'
module_mlg_test002_p01_ui <- function(id){
  ns <- shiny::NS(id)

  the_package_name <- "Rscience.MLG"
  special_folder <- file.path("inst", "www")

  input_path_package   <- base::system.file(package = the_package_name)
  input_folder_package <- file.path(input_path_package, special_folder)
  input_folder_local   <- file.path(getwd(), special_folder)
  input_folder_master <-  ifelse(input_folder_package != "",
                                 input_folder_package, input_folder_local)


  div(
    tags$head(
      tags$style(HTML("
      .shiny-output-error-AVISO {
        color: #0000ff;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
      .shiny-output-error-ERROR {
        color: #ff0000;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
        .content-wrapper, .right-side {
          overflow-y: hidden !important;
        }
      "))
    ),
    shinyjs::useShinyjs(),
    id = ns("input-panel"),
    shiny::h1("Test 002 - Kaplan-Maier"),
    # shiny::fluidRow(
    #   shiny::column(12,
    #
    #                 uiOutput(ns("box01_database")),
    #                 shiny::br(),
    #                 shiny::br()
    #   )
    # ),
    #h2("LA TABLA"),
    #DTOutput(ns("table_database")), br(),
    shiny::fluidRow(
      shiny::column(5,
                    uiOutput(ns("box02_var_selector"))),

      shiny::column(7,
                    uiOutput(ns("box03_control_de_mision")))
    ),
    shiny::br(),
    shiny::br(),

    shiny::fluidRow(
      shiny::column(12,
                    shiny::textOutput(ns("text_control_general")))),

    shiny::fluidRow(
      shiny::column(12,
                    uiOutput(ns("box04_report")),
                    br(), br(), br()
      )
    )
  ) # End div
}






# Var selection - Render - Show Results
module_mlg_test002_p01_serverB <- function(id, database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns



      output$box02_var_selector <- renderUI({

        shinydashboard::box(
          title = "02 - Selección de variables",
          status = "primary",
          id = ns("my_box02"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          shiny::uiOutput(ns("var_selector01")),
          shiny::uiOutput(ns("var_selector02")),
          shiny::uiOutput(ns("var_selector03")),
          shiny::uiOutput(ns("var_selector04"))

        )
      })


      output$box03_control_de_mision <- renderUI({

        ns <- shiny::NS(id)

        div(shinyjs::useShinyjs(), id = ns("render_files"),
            shinydashboard::box(
              title = "03 - Control de Misión",
              status = "primary",
              id = ns("my_box03"),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              width = 12,
              div(
                #h2("Generacion de Reportes"), br(),
                #h3("- Base de datos - OK!"),
                #h3("- Variable cuantitativa seleccionada - OK!"),
                #h3("- Reporte y script - OK!"),
                actionButton(ns("render_report_button"), "Render Report", width = "100%"),
                downloadButton(outputId = ns('download_button_html'),  label = "HTML", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_pdf'),   label = "PDF", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_word'),  label = "WORD", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_Rcode'), label = "Rcode", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_zip'),   label = "All (ZIP)", width = "100%", disabled = TRUE)
              )
            )
        )
      })


      output$box04_report <- renderUI({


        ns <- NS(id)

        # shinydashboard::box(
        #   title = "04 - Reporte y Script",
        #   status = "primary",
        #   id = ns("my_box02"),
        #   solidHeader = TRUE,
        #   collapsible = TRUE,
        #   collapsed = FALSE,
        #   closable = FALSE,# Colapsado por defecto
        #   width = 12,
          div(
            # fluidRow(
            #   column(4, h2("Temporal()"),
            #          shinycssloaders::withSpinner(htmlOutput(ns('pdfviewer_temporal'))))
            #   ),
            tabsetPanel(
              tabPanel(title = "All", value = 1,
                fluidRow(
                  column(12,
                         shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
                )
              ),
              tabPanel(title = "Resumen", value = 2,
                fluidRow(
                  column(12,
                         shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal02"))))
                )
            )
          )
        )

        # div(
        #   fluidRow(
        #     column(4, h2("Web pdf file"), htmlOutput(ns('pdfviewer_online'))),
        #     column(4, h2("Local file with addResourcePath()"), htmlOutput(ns('pdfviewer_local'))),
        #     column(4, h2("Temporal()"), shinycssloaders::withSpinner(htmlOutput(ns('pdfviewer_temporal'))))
        #   ), br(), br(),
        #   fluidRow(
        #     column(12, h2("EL HTML"), shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
        #   )
        # )
      })
    }
  )
}








module_mlg_test002_p01_serverA <- function(id, database, code_database){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns







      #--- 2.2) control_02 - Control del database()
      control_03 <- reactive({

        # # # Control sobre database()
        #req(control_02(), database())
        req(database(), code_database())

        validate(
          need(!is.null(database()), 'Error 003: Problemas en la base de datos. Vuelva a cargar el archivo'),
          need(!is.null(code_database()), 'Error 003: Problemas en la base de datos. Vuelva a cargar el archivo'),
          errorClass = "ERROR"
        )

        validate(
          need(is.data.frame(database()), 'Error 004: El objeto debe ser un data.frame.'),
          errorClass = "ERROR"
        )

        validate(
          need((ncol(database())>= 1), 'Error 005: La base de datos debe contener al menos una columna.'),
          need((nrow(database())>= 1), 'Error 006: La base de datos debe contener al menos una fila.'),
          errorClass = "ERROR"
        )

        validate(
          need(is.vector(code_database()), 'Error 007: El objeto code_database() debe ser un vector.'),
          need(length(code_database()) == 1, 'Error 007: El objeto code_database() debe ser un vector de longitud 1.'),
          need(is.character(code_database()), 'Error 007: El objeto code_database() debe ser un vector de longitud 1 tipo character.'),
          need(!is.na(code_database()), 'Error 007: El objeto code_database() no debe ser "NA"'),
          errorClass = "ERROR"
        )

        return(TRUE)

      })



      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 3)  Seleccion de una variable
      #--- 3.1) Crear input$selected_vr_name
      render_button_status  <- shiny::reactiveVal()
      render_button_counter <- shiny::reactiveVal()
      vector_var_names  <- shiny::reactiveVal()

      shiny::observe({
        # Control hasta inputFile + database()


        req(control_03())

        # Nombre de las columnas
        vector_var_names(base::colnames(database()))

        # Seleccion de una variable cuantitati a
        output$var_selector01 <- shiny::renderUI({



          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_pos <- 1:ncol(database())
          vector_letters <- openxlsx::int2col(vector_pos)
          vector_colnames <- colnames(database())


          # Determinar la cantidad máxima de dígitos
          max_digits <- max(nchar(vector_pos))
          max_digits <- max(max_digits, 2)
          vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)
          vector_numeric <- sapply(database(), is.numeric)

          # Para el usuario
          vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)

          # Vector de opcion interno (nombre de columnas)
          vector_options <- vector_colnames
          names(vector_options) <- vector_names
          vector_options <- c("Selecciona una..." = "", vector_options)

          div(
            shiny::selectInput(inputId = ns("selected_time_name"), label = "Time",
                               choices = vector_options,
                               selected = vector_options[1])
          )

        })

        output$var_selector02 <- shiny::renderUI({



          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_pos <- 1:ncol(database())
          vector_letters <- openxlsx::int2col(vector_pos)
          vector_colnames <- colnames(database())


          # Determinar la cantidad máxima de dígitos
          max_digits <- max(nchar(vector_pos))
          max_digits <- max(max_digits, 2)
          vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)
          vector_numeric <- sapply(database(), is.numeric)

          # Para el usuario
          vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)

          # Vector de opcion interno (nombre de columnas)
          vector_options <- vector_colnames
          names(vector_options) <- vector_names
          vector_options <- c("Selecciona una..." = "", vector_options)

          div(
            shiny::selectInput(inputId = ns("selected_status_name"), label = "Status",
                               choices = vector_options,
                               selected = vector_options[1])
          )

        })

        output$var_selector03 <- shiny::renderUI({



          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_pos <- 1:ncol(database())
          vector_letters <- openxlsx::int2col(vector_pos)
          vector_colnames <- colnames(database())


          # Determinar la cantidad máxima de dígitos
          max_digits <- max(nchar(vector_pos))
          max_digits <- max(max_digits, 2)
          vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)
          vector_numeric <- sapply(database(), is.numeric)

          # Para el usuario
          vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)

          # Vector de opcion interno (nombre de columnas)
          vector_options <- vector_colnames
          names(vector_options) <- vector_names
          vector_options <- c("Selecciona una..." = "", vector_options)

          div(
            shiny::selectInput(inputId = ns("selected_treatment_name"), label = "Treatment",
                               choices = vector_options,
                               selected = vector_options[1])
          )

        })

        output$var_selector04 <- shiny::renderUI({



          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_choices <- c("0.10   (10%)" = "0.10",
                              "0.05    (5%)"  = "0.05",
                              "0.01    (1%)"  = "0.01")

          div(
            shiny::selectInput(inputId = ns("alpha_value"), label = "Valor alfa",
                               choices = vector_choices,
                               selected = vector_choices[2])
          )

        })

      })

      # Para VR
      selected_time_name    <- shiny::reactiveVal()
      selected_time_pos     <- shiny::reactiveVal()
      selected_time_letter  <- shiny::reactiveVal()

      # Para Status
      selected_status_name    <- shiny::reactiveVal()
      selected_status_pos     <- shiny::reactiveVal()
      selected_status_letter  <- shiny::reactiveVal()

      # Para Treatment
      selected_treatment_name    <- shiny::reactiveVal()
      selected_treatment_pos     <- shiny::reactiveVal()
      selected_treatment_letter  <- shiny::reactiveVal()


      # Para alfa
      selected_alpha_value <- shiny::reactiveVal()

      control_04 <- reactive({

        render_button_status(FALSE)
        render_button_counter(0)
        shinyjs::disable("render_report_button")
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))


        # # # Control sobre input$selected_vr_name
        req(control_03(), input$selected_time_name, input$selected_status_name,
            input$selected_treatment_name, input$alpha_value)


        validate(
          need(!is.null(input$selected_time_name),        "Error 006: Problemas con la variable 'Time' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(input$selected_status_name),      "Error 006: Problemas con la variable 'Status' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(input$selected_treatment_name),   "Error 006: Problemas con la variable 'Treatment' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(input$alpha_value),               "Error 006: Problemas con el valor de alfa seleccionado. Vuelva a cargar el archivo."),
          errorClass = "ERROR"
        )


        validate(
          need(input$selected_time_name != "",      "Seleccione una variable para indicar 'Time' en su base de datos."),
          need(input$selected_status_name != "",    "Seleccione una variable para indicar 'Status' en su base de datos."),
          need(input$selected_treatment_name != "", "Seleccione una variable para indicar 'Treatment' en su base de datos."),
          need(input$alpha_value != "", 'Seleccione un valor de alpha.'),
          errorClass = "AVISO"
        )

        # Para TIME
        selected_time_name(input$selected_time_name)
        selected_time_pos(match(selected_time_name(), vector_var_names()))
        selected_time_letter(openxlsx::int2col(selected_time_pos()))


        # Para STATUS
        selected_status_name(input$selected_status_name)
        selected_status_pos(match(selected_status_name(), vector_var_names()))
        selected_status_letter(openxlsx::int2col(selected_status_pos()))

        # Para TREATMENT
        selected_treatment_name(input$selected_treatment_name)
        selected_treatment_pos(match(selected_treatment_name(), vector_var_names()))
        selected_treatment_letter(openxlsx::int2col(selected_treatment_pos()))


        # Para Alfa
        selected_alpha_value(as.numeric(as.character(input$alpha_value)))

        validate(
          need(!is.null(selected_time_name()),   "Error 007: Problemas con la variable 'TIME' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_time_pos()),    "Error 008: Problemas con la variable 'TIME' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_time_letter()), "Error 009: Problemas con la variable 'TIME' seleccionada. Vuelva a cargar el archivo."),
          errorClass = "ERROR"
        )

        validate(
          need(!is.null(selected_status_name()),   "Error 007: Problemas con la variable 'Status' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_status_pos()),    "Error 008: Problemas con la variable 'Status' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_status_letter()), "Error 009: Problemas con la variable 'Status' seleccionada. Vuelva a cargar el archivo."),
          errorClass = "ERROR"
        )

        validate(
          need(!is.null(selected_treatment_name()),   "Error 007: Problemas con la variable 'Treatment' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_treatment_pos()),    "Error 008: Problemas con la variable 'Treatment' seleccionada. Vuelva a cargar el archivo."),
          need(!is.null(selected_treatment_letter()), "Error 009: Problemas con la variable 'Treatment' seleccionada. Vuelva a cargar el archivo."),
          errorClass = "ERROR"
        )

        validate(
          need(!is.null(selected_alpha_value()),   'Error 007: Problemas con el valor de alfa seleccionado. Vuelva a cargar el archivo.'),
        )


        validate(
          need(sum(colnames(database()) == selected_time_name()) == 1,      "Error 010: El nombre de variable 'Time' seleccionado no pertenece a la base."),
          need(sum(colnames(database()) == selected_status_name()) == 1,    "Error 010: El nombre de variable 'Status' seleccionado no pertenece a la base."),
          need(sum(colnames(database()) == selected_treatment_name()) == 1, "Error 010: El nombre de variable 'Treatment' seleccionado no pertenece a la base."),
          need(selected_time_name() != selected_status_name(),              "Error 010: Las variables 'Time' y 'Status' deben ser diferentes."),
          need(selected_time_name() != selected_treatment_name(),           "Error 010: Las variables 'Time' y 'Treatment' deben ser diferentes."),
          need(selected_status_name() != selected_treatment_name(),         "Error 010: Las variables 'Status' y 'Treatment' deben ser diferentes."),
          errorClass = "ERROR"
        )

        validate(
          need(ncol(database()) >= selected_time_pos(),      "Error 011: La posición de variable 'Time' no pertenece a la base de datos."),
          need(ncol(database()) >= selected_status_pos(),    "Error 011: La posición de variable 'Status' no pertenece a la base de datos."),
          need(ncol(database()) >= selected_treatment_pos(), "Error 011: La posición de variable 'Treatment' no pertenece a la base de datos."),
          errorClass = "ERROR"
        )

        ##### SOLO ALFA ##############################
        validate(
          need(is.numeric(selected_alpha_value()), 'Error 014: Problemas internos con el valor de alfa elegido.
               El valor alfa internamente debe ser numérico.'),
          errorClass = "ERROR"
        )

        # ##### SOLO VR ##############################
        # vector_time <- database()[,selected_time_pos()]
        #
        #
        # validate(
        #   need((sum(is.na(vector_vr))==0), "Error 014: La variable 'Time' seleccionada posee al menos una celda sin datos. \n
        #       Usted, ¿está trabajando con una base de datos que corresponde a la Diplomatura?.
        #       La diplomatura es una iniciación a R por lo que solo veremos código de R aplicable a bases de datos sin celdas vacías."),
        #   errorClass = "ERROR"
        # )
        #
        #
        # validate(
        #   need((is.numeric(vector_vr)), 'Error 015:
        #   La variable respuesta (VR) debe ser numérica para poder realizar el análisis.
        #   La variable seleccionada no es numérica.
        #   Verifique las siguientes opciones:\n
        #        1) Se equivocó al seleccionar variable VR con la cual trabajar. Elija la variable cuantitativa correcta.\n
        #        2) No abrió el archivo csv como archivo de texto en su computadora para tomar noción sobre
        #        si el archivo tiene como primera fila al nombre de columnas, cual es el separador de columna
        #        y del separador decimal.\n
        #        3) Regrese al menú de carga, y verifique que todos las opciones seleccionadas (header, sep, dec) correspondan con lo que usted observa en su archivo CSV en su computadora.\n
        #        4) Si usted está trabajando con una base de datos que no corresponde a la Semana 01 de la Diplomatura, verifique que todas las celdas de la columna elegida solo contienen números.
        #        Posiblemente alguna celda de la columna seleccionada posee algún caracter no numérico en alguna celda.\n\n\n\n\n\n\n\n'
        #   ),
        #   errorClass = "ERROR"
        # )
        #
        # validate(
        #   need((length(vector_vr)>=1), 'Error 015: La variable VR no posee datos. \n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        # validate(
        #   need((var(vector_vr)!=0), 'Error 015: La variable seleccionada tiene varianza cero. \n
        #       Eso imposibilita realizar el análisis de Anova.
        #        Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        #
        # ##### SOLO FACTOR ##############################
        # vector_factor <- database()[,selected_factor_pos()]
        #
        # validate(
        #   need((sum(is.na(vector_factor))==0), 'Error 014: La variable FACTOR seleccionada posee al menos una celda sin datos. \n
        #       Usted, ¿está trabajando con una base de datos que corresponde a la Diplomatura?.
        #       La diplomatura es una iniciación a R por lo que solo veremos código de R aplicable a bases de datos sin celdas vacías.'),
        #   errorClass = "ERROR"
        # )
        #
        # validate(
        #   need((length(vector_factor)>=1), 'Error 015: La variable FACTOR seleccionada no posee datos. \n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        #
        # vector_n <- tapply(vector_vr, vector_factor, length)
        # validate(
        #   need((length(vector_n)>=2), 'Error 015: Para realizar el test de ANOVA la variable FACTOR debe tener al menos 2 niveles. \n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        #
        # check_reps_lvl <- sum(vector_n >= 2) == length(vector_n)
        # validate(
        #   need(check_reps_lvl, 'Error 015: Para realizar el test de ANOVA la variable FACTOR debe tener al menos 2 repeticiones en cada nivel del FACTOR. \n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        # vector_var <- tapply(vector_vr, vector_factor, var)
        # validate(
        #   need(sum(is.null(vector_var)) == 0, 'Error 015:
        #   Para realizar el test de ANOVA debe ser posible calcular la varianza de cada nivel del factor.\n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        # validate(
        #   need(sum(is.na(vector_var)) == 0, 'Error 015:
        #   Para realizar el test de ANOVA debe ser posible calcular la varianza de cada nivel del factor.\n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        #
        # validate(
        #   need(sum(vector_var == 0) == 0, 'Error 015:
        #   Para realizar el test de ANOVA debe ser posible calcular la varianza de cada nivel del factor.\n
        #       Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
        #   errorClass = "ERROR"
        # )
        # Como todo esta OK, se habilita el boton de render.
        shinyjs::enable("render_report_button")
        render_button_status(TRUE)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))

        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###





      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_04())

        render_button_counter(render_button_counter() + 1)
      })


      observeEvent(render_button_counter(), {

        # Todo lo anterior tiene que estar OK.
        req(control_04())

        #load_button_counter(load_button_counter() + 1)
        if(render_button_counter() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))

        }

        if(render_button_counter() == 0){
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))
        }


      })




      #--- 5.2) Salida como texto del control_04().
      output$text_control_general <- renderText({
        req(control_04())
        ""
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###



      #--- 7) Control sobre el input$render_report_button
      # segun el detalle del render_button_status()
      # Esto es verificar que el boton este en estado "enable".
      control_05 <- reactive({

        req(control_04(), input$render_report_button, render_button_status(), render_button_counter()>=1)

        validate(
          need(!is.null(input$render_report_button),   'Error 006: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )

        validate(
          need(is.logical(render_button_status()),   'Error 006: Problemas con render_button_status(). Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )

        validate(
          need(is.numeric(render_button_counter()),   'Error 006: Problemas con render_button_counter(). Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )
        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###



      #--- 8) Render
      #--- 8.1) Inicializamos objetos
      the_time              <- reactiveVal()
      output_temp_folder    <- reactiveVal()

      output_path_rmd       <- reactiveVal()
      output_path_pdf       <- reactiveVal()
      output_path_html      <- reactiveVal()
      output_path_word      <- reactiveVal()

      output_path_rmd02     <- reactiveVal()
      output_path_pdf02     <- reactiveVal()
      output_path_html02    <- reactiveVal()
      output_path_word02      <- reactiveVal()

      output_path_Rcode   <- reactiveVal()
      output_path_zip     <- reactiveVal()

      #--- 8.2) Renderizacion y modificaciones de los objetos 8.1)
      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_05())

        #render_button_counter(render_button_counter() + 1)

        # # # Execution time...
        original_time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        execution_time <- gsub("[[:punct:]]", "_", original_time)
        execution_time <- gsub(" ", "_", execution_time)

        print("VA BIEN 01")
        # Selected class
        selected_family_test <- "FT001_mlg"
        selected_test_part <- "test002_p01"

        # # # Special folder
        the_package_name <- "Rscience.MLG"
        special_folder_package <- file.path("extdata", selected_family_test, selected_test_part)
        special_folder_local <- file.path("inst", special_folder_package)


        # # # ---- Input objects ---- # # #
        input_old_str <- "_master"
        input_new_str <- "_mod"
        input_file_rmd   <- paste0("report_", selected_test_part, "_file01_master.Rmd")
        input_file_rmd02   <- paste0("report_test002_p01_file02_master.Rmd")
        #input_file_rmd02   <- input_file_rmd
        input_file_css   <- "styles.css"
        input_file_png01 <- "logo_01_unc.png"
        input_file_png02 <- "logo_02_fcefyn.png"
        input_file_png03 <- "logo_03_efadoc.png"
        input_file_png04 <- "logo_04_rscience.png"
        input_file_png05 <- "Red_X.png"
        # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


        # # # Folder package
        # Depende de si lo toma como local o como parte del package
        # En Desarrollo lo toma local.
        # Para el usuario final lo toma como package.
        # De esta forma corre bien para cualquiera de los dos.
        input_path_package   <- base::system.file(package = the_package_name)
        input_folder_package <- file.path(input_path_package, special_folder_package)
        input_folder_local   <- file.path(getwd(), special_folder_local)

        check_cantidad_files_local <- length(list.files(input_folder_local)) > 0
        check_cantidad_files_package <- length(list.files(input_folder_package)) > 0

        input_folder_master <-  ifelse(check_cantidad_files_package,
                                       input_folder_package, input_folder_local)


        # # # Input paths
        input_path_rmd     <- file.path(input_folder_master, input_file_rmd)
        input_path_rmd02   <- file.path(input_folder_master, input_file_rmd02)
        input_path_css     <- file.path(input_folder_master, input_file_css)
        input_path_png01   <- file.path(input_folder_master, input_file_png01)
        input_path_png02   <- file.path(input_folder_master, input_file_png02)
        input_path_png03   <- file.path(input_folder_master, input_file_png03)
        input_path_png04   <- file.path(input_folder_master, input_file_png04)
        input_path_png05   <- file.path(input_folder_master, input_file_png05)


        # # # Output File
        p1_name <- tools::file_path_sans_ext(input_file_rmd)
        p2_name <- tools::file_ext(input_file_rmd)

        print("VA BIEN 02 A")
        output_file_rmd   <- paste0(p1_name, "_", execution_time, ".", p2_name)
        output_file_rmd   <- gsub(pattern = input_old_str,
                                  replacement = input_new_str,
                                  x = output_file_rmd)

        p1_name02 <- tools::file_path_sans_ext(input_file_rmd02)
        p2_name02 <- tools::file_ext(input_file_rmd02)
        output_file_rmd02   <- paste0(p1_name02, "_", execution_time, ".", p2_name02)
        output_file_rmd02   <- gsub(pattern = input_old_str,
                                  replacement = input_new_str,
                                  x = output_file_rmd02)
        print("VA BIEN 02 B")

        output_file_css   <- input_file_css
        output_file_png01 <- input_file_png01
        output_file_png02 <- input_file_png02
        output_file_png03 <- input_file_png03
        output_file_png04 <- input_file_png04
        output_file_png05 <- input_file_png05

        output_general_name <- tools::file_path_sans_ext(output_file_rmd)
        output_file_pdf     <- paste0(output_general_name, ".pdf")
        output_file_html    <- paste0(output_general_name, ".html")
        output_file_word    <- paste0(output_general_name, ".docx")

        output_file_Rcode     <- paste0(output_general_name, ".R")
        output_file_zip       <- paste0(output_general_name, ".zip")

        output_general_name02 <- tools::file_path_sans_ext(output_file_rmd02)
        output_file_pdf02     <- paste0(output_general_name02, ".pdf")
        output_file_html02    <- paste0(output_general_name02, ".html")
        output_file_word02    <- paste0(output_general_name02, ".docx")


        # # # New temporal file
        new_temp_folder <- tempdir()
        new_sub_folder <- paste0("Rsience_FOLDER_", execution_time)
        new_temp_folder <- file.path(new_temp_folder, new_sub_folder)
        dir.create(new_temp_folder)

        # # # Output paths
        output_path_rmd    <- file.path(new_temp_folder, output_file_rmd)
        output_path_rmd02  <- file.path(new_temp_folder, output_file_rmd02)
        output_path_png01  <- file.path(new_temp_folder, output_file_png01)
        output_path_png02  <- file.path(new_temp_folder, output_file_png02)
        output_path_png03  <- file.path(new_temp_folder, output_file_png03)
        output_path_png04  <- file.path(new_temp_folder, output_file_png04)
        output_path_png05  <- file.path(new_temp_folder, output_file_png05)

        output_path_css    <- file.path(new_temp_folder, output_file_css)

        output_path_pdf    <- file.path(new_temp_folder, output_file_pdf)
        output_path_html   <- file.path(new_temp_folder, output_file_html)
        output_path_word   <- file.path(new_temp_folder, output_file_word)

        output_path_Rcode  <- file.path(new_temp_folder, output_file_Rcode)
        output_path_zip    <- file.path(new_temp_folder, output_file_zip)

        output_path_pdf02    <- file.path(new_temp_folder, output_file_pdf02)
        output_path_html02   <- file.path(new_temp_folder, output_file_html02)
        output_path_word02   <- file.path(new_temp_folder, output_file_word02)
        # # # Copy some file from original input to new_temp_folder
        # El archivo .Rmd se va a renderizar en la carpeta temporal,
        # entonces hacemos una copia de los archivos originales a
        # la carpeta temporal
        file.copy(from = input_path_rmd,   to = output_path_rmd,   overwrite = TRUE)
        file.copy(from = input_path_rmd02, to = output_path_rmd02, overwrite = TRUE)
        file.copy(from = input_path_png01, to = output_path_png01, overwrite = TRUE)
        file.copy(from = input_path_png02, to = output_path_png02, overwrite = TRUE)
        file.copy(from = input_path_png03, to = output_path_png03, overwrite = TRUE)
        file.copy(from = input_path_png04, to = output_path_png04, overwrite = TRUE)
        file.copy(from = input_path_css,   to = output_path_css,   overwrite = TRUE)
        file.copy(from = input_path_png05, to = output_path_png05, overwrite = TRUE)


        # # # Objetos de entorno
        # Estos objetos seran usados como si estuvieran
        # detallados dentro del archivo .Rmd.
        # Por ejemplo, tomamos la base de datos.
        render_env <- new.env()
        render_env$"BASE" <- database()
        render_env$"the_time" <- original_time
        render_env$"data_source" <- input$data_source


        # # # Objetos de reemplazo
        # Estos elementos seran suplantados sobre la copia
        # del archivo .Rmd que esta en la carpeta temporal
        replacement_list <- list()
        replacement_list$"__selected_time_name__" <- selected_time_name()
        replacement_list$"__selected_status_name__" <- selected_status_name()
        replacement_list$"__selected_treatment_name__" <- selected_treatment_name()

        print(selected_treatment_name())

        replacement_list$"__code_database__" <- code_database()
 #       replacement_list$".user_file" <- paste0("\"", input$csv_file_path$name, "\"")
#        replacement_list$".user_header" <- as.logical(as.character(input$csv_header))
 #       replacement_list$".user_sep" <- paste0("\"", input$csv_sep, "\"")
  #      replacement_list$".user_dec" <- paste0("\"", input$csv_dec, "\"")
        replacement_list$".diplo_name_database" <- input$diplo_database
        replacement_list$".r_name_database" <- input$r_database

        # # # Aplicacion de modificaciones
        # Aplicamos todas las modificaciones sobre el archivo .Rmd
        # que se encuentra en la carpeta temporal.

        # Lo abrimos leyendo todo su contenido...
        lineas_modificadas <- readLines(output_path_rmd)

        print("VA BIEN 03 A")
        # Lo modificamos...
        for (k in 1:length(replacement_list)){
          selected_name <- names(replacement_list)[k]
          lineas_modificadas <- gsub(pattern = selected_name,
                                     replacement = replacement_list[[selected_name]],
                                     x = lineas_modificadas)
        }
        print("VA BIEN 03 B")
        # Guardamos las modificaciones en la carpeta temporal.
        writeLines(lineas_modificadas, output_path_rmd)


        # # # Render All
        rmarkdown::render(output_path_rmd, rmarkdown::pdf_document(),  output_file = output_path_pdf, envir = render_env)
        rmarkdown::render(output_path_rmd, rmarkdown::html_document(), output_file = output_path_html, envir = render_env)
        rmarkdown::render(output_path_rmd, rmarkdown::word_document(), output_file = output_path_word, envir = render_env)
        knitr::purl(output_path_rmd, output = output_path_Rcode)

        rmarkdown::render(output_path_rmd02, rmarkdown::pdf_document(),  output_file = output_path_pdf02, envir = render_env)
        rmarkdown::render(output_path_rmd02, rmarkdown::html_document(), output_file = output_path_html02, envir = render_env)
        rmarkdown::render(output_path_rmd02, rmarkdown::word_document(), output_file = output_path_word02, envir = render_env)


        # # # Zip all
        #files_to_zip <- c(output_path_pdf, output_path_html, output_path_word, output_path_Rcode)
        files_to_zip <- new_temp_folder
        utils::zip(output_path_zip, files = files_to_zip)

        # Add info to ReactiveVals()...
        the_time(execution_time)
        output_temp_folder(new_temp_folder)
        output_path_rmd(output_path_rmd)
        output_path_pdf(output_path_pdf)
        output_path_html(output_path_html)
        output_path_word(output_path_word)
        output_path_Rcode(output_path_Rcode)
        output_path_zip(output_path_zip)

        output_path_rmd02(output_path_rmd02)
        output_path_pdf02(output_path_pdf02)
        output_path_html02(output_path_html02)
        output_path_word02(output_path_word02)


      })


      #--- 8.3) Control de los objetos 8.1)


      download_counter_pdf  <- reactiveVal()
      download_counter_html <- reactiveVal()
      download_counter_word <- reactiveVal()
      download_counter_Rcode <- reactiveVal()
      download_counter_zip  <- reactiveVal()


      control_06 <- reactive({

        # Todos los botones por defecto van a estar desarmados...
        shinyjs::disable("download_button_pdf")
        shinyjs::disable("download_button_html")
        shinyjs::disable("download_button_word")
        shinyjs::disable("download_button_Rcode")
        shinyjs::disable("download_button_zip")

        # Los dejamos en naranja...
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_Rcode")))
        runjs(sprintf('$("#%s").css({"background-color": "grey", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))

        # Activamos todos los contadores de los botones a cero.
        download_counter_pdf(0)
        download_counter_html(0)
        download_counter_word(0)
        download_counter_Rcode(0)
        download_counter_zip(0)


        # Controlamos que todos los objetos 8.1) no sean NUll.
        req(control_05(), render_button_counter() >= 1,
            the_time(), output_temp_folder(),
            output_path_rmd(), output_path_pdf(), output_path_html(),
            output_path_word(), output_path_Rcode(), output_path_zip())



        validate(
          need(file.exists(output_path_rmd()),    'Error 020: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )
        validate(
          need(file.exists(output_path_pdf()),    'Error 021: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )

        validate(
          need(file.exists(output_path_html()),   'Error 022: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )
        validate(
          need(file.exists(output_path_word()),   'Error 023: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )
        validate(
          need(file.exists(output_path_Rcode()),   'Error 023: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )
        validate(
          need(file.exists(output_path_zip()),    'Error 024: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )


        # # # Si todo se cumplio, activamos los botones.
        shinyjs::enable("download_button_pdf")
        shinyjs::enable("download_button_html")
        shinyjs::enable("download_button_word")
        shinyjs::enable("download_button_Rcode")
        shinyjs::enable("download_button_zip")

        # Los dejamos en naranja...
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_Rcode")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))

        # # # # Y los dejamos en verde...
        # if(download_counter_pdf()  >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
        # if(download_counter_html() >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        # if(download_counter_word() >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
        # if(download_counter_zip()  >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))


        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 9.2) Accion de cada boton de descarga



      observeEvent(input$selected_vr_name, {

        render_button_counter(0)
        download_counter_pdf(0)
        download_counter_html(0)
        download_counter_word(0)
        download_counter_Rcode(0)
        download_counter_zip(0)
      })


      observeEvent(input$selected_factor_name, {

        render_button_counter(0)
        download_counter_pdf(0)
        download_counter_html(0)
        download_counter_word(0)
        download_counter_Rcode(0)
        download_counter_zip(0)
      })


      observeEvent(download_counter_pdf(), {


        req(control_06())
        if(download_counter_pdf() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))

        } else

          if(download_counter_pdf() == 0){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
          }


      })


      observeEvent(download_counter_html(), {


        req(control_06())
        if(download_counter_html() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))

        } else

          if(download_counter_html() == 0){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
          }


      })


      observeEvent(download_counter_word(), {


        req(control_06())
        if(download_counter_word() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))

        } else

          if(download_counter_word() == 0){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
          }


      })

      observeEvent(download_counter_Rcode(), {


        req(control_06())
        if(download_counter_Rcode() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_Rcode")))

        } else

          if(download_counter_Rcode() == 0){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_Rcode")))
          }


      })

      observeEvent(download_counter_zip(), {


        req(control_06())
        if(download_counter_zip() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))

        } else

          if(download_counter_word() == 0){
            runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
          }


      })


      # # # PDF
      output$download_button_pdf <- downloadHandler(
        filename = function() {
          basename(output_path_pdf())
        },
        content = function(file) {
          file.copy(output_path_pdf(), file, overwrite = TRUE)
          download_counter_pdf (download_counter_pdf() + 1)
        }
      )


      # # # HTML
      output$download_button_html <- downloadHandler(
        filename = function() {
          basename(output_path_html())
        },
        content = function(file) {
          file.copy(output_path_html(), file, overwrite = TRUE)
          download_counter_html(download_counter_html() + 1)
        }
      )


      # # # WORD
      output$download_button_word <- downloadHandler(
        filename = function() {
          basename(output_path_word())
        },
        content = function(file) {
          file.copy(output_path_word(), file, overwrite = TRUE)
          download_counter_word(download_counter_word() + 1)

        }
      )

      output$download_button_Rcode <- downloadHandler(
        filename = function() {
          basename(output_path_Rcode())
        },
        content = function(file) {
          file.copy(output_path_Rcode(), file, overwrite = TRUE)
          download_counter_Rcode(download_counter_Rcode() + 1)

        }
      )

      # # # ZIP
      output$download_button_zip <- downloadHandler(
        filename = function() {
          basename(output_path_zip())
        },
        content = function(file) {
          #files <- c(output_path_pdf(), output_path_html(), output_path_word())
          #zip(file, files)
          file.copy(output_path_zip(), file, overwrite = TRUE)
          download_counter_zip(download_counter_zip() + 1)

        }
      )


      #--- 10) Visualizacion del HTML de la carpeta temporal
      output$htmlviewer_temporal <- renderText({

        req(control_06())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        my_path <- output_temp_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(output_path_html())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })

      output$htmlviewer_temporal02 <- renderText({

        req(control_06())

        # # # Definimos como un "alias" o un "bindeo".
        # A la carpeta temporal le damos como un "alias".
        # Esto es por que los HTML no pueden ser tomados de cualquier lado.
        my_path <- output_temp_folder()
        addResourcePath(prefix = "output_temp_folder", directoryPath = my_path)

        # Armamos ahora un path con el "alias" como folder.
        my_file <- basename(output_path_html02())
        my_local_file <- file.path("output_temp_folder", my_file)

        # Levantamos el html
        armado_v <- paste('<div style="height: 100%; width: 100%; overflow: auto;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })







    }) # Fin Module
}

