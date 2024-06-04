


# # # 01) UI - Selection for 'database'
module_diplo_001_clase02_p01_ui <- function(id){
  ns <- shiny::NS(id)

  the_package_name <- "Rscience.MLG"
  special_folder <- file.path("inst", "www")

  input_path_package   <- base::system.file(package = the_package_name)
  input_folder_package <- file.path(input_path_package, special_folder)
  input_folder_local   <- file.path(getwd(), special_folder)
  input_folder_master <-  ifelse(input_folder_package != "",
                                 input_folder_package, input_folder_local)

  input_path_css <- file.path(input_folder_master, "style.css")

  div(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "inst/estilos.css"),
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
    includeCSS(system.file("www/style.css", package = "Rscience.Diplo")),
    #    includeCSS("inst/www/style.css"),
    shinyjs::useShinyjs(),
    id = ns("input-panel"),
    shiny::h1("Clase 02 - Parte 01 - Medidas de Posición y Dispersión para 1 Variable Cuantitativa"),
    shiny::fluidRow(
      shiny::column(12,

                    uiOutput(ns("box01_database")),
                    shiny::br(),
                    shiny::br(),

                    shiny::fluidRow(
                      shiny::column(4, uiOutput(ns("box02_var_selector"))),
                      shiny::column(8, uiOutput(ns("box03_control_de_mision")))
                    ),


                    shiny::textOutput(ns("text_control_general")),


                    uiOutput(ns("box04_report")),
                    br(), br(), br()
      )
    )
  ) # End div
}












module_diplo_001_clase02_p01_serverA <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      # Objetos iniciales importantes
      database <- shiny::reactiveVal()
      load_button_status  <- reactiveVal()
      load_button_counter <- reactiveVal()


      # 1) Control de lo detallado como "data_source"
      # y de lo elegido en el subtipo de fuente de datos. (control01).
      # 2) Obtener database() y control de database() (control02).
      # .Desde la fuente de datos hasta tener el database()
      # 2)


      ###---###---###---###---###---###---###---###---###---###---###---###---###---###

      #--- 1) control_01 - Control del inputfile
      #--- 6) Enable/Disable - Render Report Button




      control_01 <- reactive({

        database(NULL)
        load_button_status(FALSE)
        load_button_counter <- reactiveVal(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))

        ns <- session$ns

        # # # Control sobre input$data_source
        req(input$"data_source")

        validate(
          need(!is.null(input$"data_source"), 'Error 001: Problemas en input$data_source.'),
          errorClass = "ERROR"
        )

        # Si el archivo es CSV...
        if(input$"data_source" == "csv_source"){

          req(input$csv_file_path$name)

          csv_file_name <- input$csv_file_path$name
          final_ext <- tools::file_ext(csv_file_name)

          validate(
            need(final_ext == "csv", 'Error 002: Solo es posible cargar archivos CSV.\n    Prueba con otro archivo!'),
            errorClass = "ERROR"
          )
        } else

          # Si el archivo es una base de R...
          if(input$"data_source" == "diplo_source"){

            req(input$diplo_database)

            validate(
              need(exists(input$diplo_database), 'Error 002: La base seleccionada de la Diplomatura no se encuentra en el entorno de R.\n'),
              errorClass = "ERROR"
            )
          } else
            # Si el archivo es una base de R...
            if(input$"data_source" == "r_source"){

              req(input$r_database)

              validate(
                need(exists(input$r_database), 'Error 002: La base de R seleccionada no se encuentra en el entorno de R.\n'),
                errorClass = "ERROR"
              )
            }


        shinyjs::enable("action_load")
        load_button_status(TRUE)
        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###



      observeEvent(input$action_load, {

        # Todo lo anterior tiene que estar OK.
        req(control_01())

        load_button_counter(load_button_counter() + 1)
      })


      observeEvent(load_button_counter(), {

        # Todo lo anterior tiene que estar OK.
        req(control_01())

        #load_button_counter(load_button_counter() + 1)
        if(load_button_counter() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))

        }

        if(load_button_counter() == 0){
          database(NULL)
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        }


      })



      shiny::observeEvent(input$data_source,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$diplo_database,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_database,{

        database(NULL)
        load_button_counter(0)



      })

      shiny::observeEvent(input$csv_header,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_sep,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_dec,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$csv_quote,{
        database(NULL)
        load_button_counter(0)
      })

      shiny::observeEvent(input$r_database,{


        database(NULL)
        load_button_counter(0)
      })

      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 2.2) control_02 - load button
      control_02 <- reactive({

        # # # Control sobre database()
        req(control_01(),  load_button_status(), load_button_counter()>=1)

        return(TRUE)

      })


      #--- 2) Fuente de datos y creacion de database
      #--- 2.1) Database!!!
      observe({

        #print(control_02())
        # Todo lo anterior tiene que estar OK.
        # Solo ejecuta si el status del load es TRUE y si tiene
        # un contador de load >= 1, lo que implica que el usuario hizo
        # clic en "LOAD".
        req(control_02())

        if(input$data_source == "csv_source"){

          database(utils::read.csv(file = input$csv_file_path$datapath,
                                   header = as.logical(as.character(input$csv_header)),
                                   sep = input$csv_sep,
                                   dec = input$csv_dec,
                                   stringsAsFactors = FALSE)
          )


        } else

          if(input$data_source == "diplo_source"){

            database(base::eval(base::parse(text = input$"diplo_database")))
          } else

            if(input$data_source == "r_source"){

              database(base::eval(base::parse(text = input$"r_database")))
            }


      })




      #--- 2.2) control_02 - Control del database()
      control_03 <- reactive({

        # # # Control sobre database()
        req(control_02(), database())

        validate(
          need(!is.null(database()), 'Error 003: Problemas en la base de datos. Vuelva a cargar el archivo'),
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



        return(TRUE)

      })


      #--- 2.3) Salida en tabla de la database()
      output$table_database <- DT::renderDT({


        req(control_03())

        # Usar lapply para mostrar los elementos deseados

        mi_tabla <- database()
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = FALSE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            #dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '15px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "15px"
        )
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 3)  Seleccion de una variable
      #--- 3.1) Crear input$selected_var_name
      render_button_status  <- shiny::reactiveVal()
      render_button_counter <- shiny::reactiveVal()
      vector_var_names  <- shiny::reactiveVal()
      shiny::observe({
        # Control hasta inputFile + database()


        req(control_03())

        # Nombre de las columnas
        vector_var_names(base::colnames(database()))

        # Seleccion de una variable cuantitati a
        output$var_selector <- shiny::renderUI({



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
            shiny::selectInput(inputId = ns("selected_var_name"), label = "Selecciona una variable cuantitativa...",
                               choices = vector_options,
                               selected = vector_options[1])
          )

        })


      })


      selected_var_name    <- shiny::reactiveVal()
      selected_var_pos     <- shiny::reactiveVal()
      selected_var_letter  <- shiny::reactiveVal()
      control_04 <- reactive({

        render_button_status(FALSE)
        render_button_counter(0)
        shinyjs::disable("render_report_button")
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("render_report_button")))


        # # # Control sobre input$selected_var_name
        req(control_03(), input$selected_var_name)

        validate(
          need(!is.null(input$selected_var_name),   'Error 006: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )


        validate(
          need(input$selected_var_name != "", 'Seleccione una variable cuantitativa de su base de datos.'),
          errorClass = "AVISO"
        )

        selected_var_name(input$selected_var_name)
        selected_var_pos(match(selected_var_name(), vector_var_names()))
        selected_var_letter(openxlsx::int2col(selected_var_pos()))

        validate(
          need(!is.null(selected_var_name()),   'Error 007: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          need(!is.null(selected_var_pos()),    'Error 008: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          need(!is.null(selected_var_letter()), 'Error 009: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )



        validate(
          need(sum(colnames(database()) == selected_var_name()) == 1, 'Error 010: El nombre de variable seleccionado no pertenece a la base.'),
          errorClass = "ERROR"
        )

        validate(
          need(ncol(database()) >= selected_var_pos(), 'Error 011: La posición de variable no pertenece a la base de datos.'),
          errorClass = "ERROR"
        )


        vector_vr <- database()[,selected_var_pos()]

        validate(
          need((sum(is.na(vector_vr))==0), 'Error 014: La columna seleccionada posee al menos una celda sin datos. \n
              Usted, ¿está trabajando con una base de datos que no corresponde a la Diplomatura?.
              La diplomatura es una iniciación a R por lo que solo veremos código de R aplicable a bases de datos sin celdas vacías.'),
          errorClass = "ERROR"
        )

        validate(
          need((is.numeric(vector_vr)), 'Error 015:
          Las medidas resumen solo son aplicables a variables numéricas.
          La variable seleccionada no es numérica.
          Verifique las siguientes opciones:\n
               1) Se equivocó al seleccionar variable con la cual trabajar. Elija la variable cuantitativa correcta.\n
               2) No abrió el archivo csv como archivo de texto en su computadora para tomar noción sobre
               si el archivo tiene como primera fila al nombre de columnas, cual es el separador de columna
               y del separador decimal.\n
               3) Regrese al menú de carga, y verifique que todos las opciones seleccionadas (header, sep, dec) correspondan con lo que usted observa en su archivo CSV en su computadora.\n
               4) Si usted está trabajando con una base de datos que no corresponde a la Semana 01 de la Diplomatura, verifique que todas las celdas de la columna elegida solo contienen números.
               Posiblemente alguna celda de la columna seleccionada posee algún caracter no numérico en alguna celda.\n\n\n\n\n\n\n\n'
          ),
          errorClass = "ERROR"
        )

        validate(
          need((length(vector_vr)>=1), 'Error 015: La columna seleccionada no posee datos. \n
              Usted, ¿está trabajando con una base de datos de la Diplomatura?.'),
          errorClass = "ERROR"
        )

        # Como todo esta OK, se habilita el boton de render.
        shinyjs::enable("render_report_button")
        render_button_status(TRUE)

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
      the_time           <- reactiveVal()
      output_temp_folder <- reactiveVal()
      output_path_rmd    <- reactiveVal()
      output_path_pdf    <- reactiveVal()
      output_path_html   <- reactiveVal()
      output_path_word   <- reactiveVal()
      output_path_zip    <- reactiveVal()

      #--- 8.2) Renderizacion y modificaciones de los objetos 8.1)
      observeEvent(input$render_report_button, {

        # Todo lo anterior tiene que estar OK.
        req(control_05())

        #render_button_counter(render_button_counter() + 1)

        # # # Execution time...
        original_time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        execution_time <- gsub("[[:punct:]]", "_", original_time)
        execution_time <- gsub(" ", "_", execution_time)

        # Selected class
        selected_class_part <- "clase02_p01"

        # # # Special folder
        the_package_name <- "Rscience.Diplo"
        special_folder_package <- file.path("extdata", "master_diplo", selected_class_part)
        special_folder_local <- file.path("inst", special_folder_package)

        # # # ---- Input objects ---- # # #
        input_old_str <- "_master"
        input_new_str <- "_mod"
        input_file_rmd   <- paste0("report_diplo_", selected_class_part, "_master.Rmd")
        input_file_css   <- "styles.css"
        input_file_png01 <- "logo_01_unc.png"
        input_file_png02 <- "logo_02_fcefyn.png"
        input_file_png03 <- "logo_03_efadoc.png"
        input_file_png04 <- "logo_04_rscience.png"

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
        input_path_rmd   <- file.path(input_folder_master, input_file_rmd)
        input_path_css   <- file.path(input_folder_master, input_file_css)
        input_path_png01 <- file.path(input_folder_master, input_file_png01)
        input_path_png02 <- file.path(input_folder_master, input_file_png02)
        input_path_png03 <- file.path(input_folder_master, input_file_png03)
        input_path_png04 <- file.path(input_folder_master, input_file_png04)


        # # # Output File
        p1_name <- tools::file_path_sans_ext(input_file_rmd)
        p2_name <- tools::file_ext(input_file_rmd)

        output_file_rmd   <- paste0(p1_name, "_", execution_time, ".", p2_name)
        output_file_rmd   <- gsub(pattern = input_old_str,
                                  replacement = input_new_str,
                                  x = output_file_rmd)

        output_file_css   <- input_file_css
        output_file_png01 <- input_file_png01
        output_file_png02 <- input_file_png02
        output_file_png03 <- input_file_png03
        output_file_png04 <- input_file_png04

        output_general_name <- tools::file_path_sans_ext(output_file_rmd)
        output_file_pdf     <- paste0(output_general_name, ".pdf")
        output_file_html    <- paste0(output_general_name, ".html")
        output_file_word    <- paste0(output_general_name, ".docx")
        output_file_zip     <- paste0(output_general_name, ".zip")


        # # # New temporal file
        new_temp_folder <- tempdir()

        # # # Output paths
        output_path_rmd    <- file.path(new_temp_folder, output_file_rmd)
        output_path_png01  <- file.path(new_temp_folder, output_file_png01)
        output_path_png02  <- file.path(new_temp_folder, output_file_png02)
        output_path_png03  <- file.path(new_temp_folder, output_file_png03)
        output_path_png04  <- file.path(new_temp_folder, output_file_png04)

        output_path_css    <- file.path(new_temp_folder, output_file_css)
        output_path_pdf    <- file.path(new_temp_folder, output_file_pdf)
        output_path_html   <- file.path(new_temp_folder, output_file_html)
        output_path_word   <- file.path(new_temp_folder, output_file_word)
        output_path_zip    <- file.path(new_temp_folder, output_file_zip)


        # # # Copy some file from original input to new_temp_folder
        # El archivo .Rmd se va a renderizar en la carpeta temporal,
        # entonces hacemos una copia de los archivos originales a
        # la carpeta temporal
        file.copy(from = input_path_rmd,   to = output_path_rmd,   overwrite = TRUE)
        file.copy(from = input_path_png01, to = output_path_png01, overwrite = TRUE)
        file.copy(from = input_path_png02, to = output_path_png02, overwrite = TRUE)
        file.copy(from = input_path_png03, to = output_path_png03, overwrite = TRUE)
        file.copy(from = input_path_png04, to = output_path_png04, overwrite = TRUE)
        file.copy(from = input_path_css,   to = output_path_css,   overwrite = TRUE)


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
        replacement_list$"selected_var_pos" <- selected_var_pos()
        replacement_list$".user_file" <- paste0("\"", input$csv_file_path$name, "\"")
        replacement_list$".user_header" <- as.logical(as.character(input$csv_header))
        replacement_list$".user_sep" <- paste0("\"", input$csv_sep, "\"")
        replacement_list$".user_dec" <- paste0("\"", input$csv_dec, "\"")
        replacement_list$".diplo_name_database" <- input$diplo_database
        replacement_list$".r_name_database" <- input$r_database

        # # # Aplicacion de modificaciones
        # Aplicamos todas las modificaciones sobre el archivo .Rmd
        # que se encuentra en la carpeta temporal.

        # Lo abrimos leyendo todo su contenido...
        lineas_modificadas <- readLines(output_path_rmd)

        # Lo modificamos...
        for (k in 1:length(replacement_list)){
          selected_name <- names(replacement_list)[k]
          lineas_modificadas <- gsub(pattern = selected_name,
                                     replacement = replacement_list[[selected_name]],
                                     x = lineas_modificadas)
        }

        # Guardamos las modificaciones en la carpeta temporal.
        writeLines(lineas_modificadas, output_path_rmd)


        # # # Render All
        rmarkdown::render(output_path_rmd, rmarkdown::pdf_document(),  output_file = output_path_pdf, envir = render_env)
        rmarkdown::render(output_path_rmd, rmarkdown::html_document(), output_file = output_path_html, envir = render_env)
        rmarkdown::render(output_path_rmd, rmarkdown::word_document(), output_file = output_path_word, envir = render_env)

        # # # Zip all
        files_to_zip <- c(output_path_pdf, output_path_html, output_path_word)
        utils::zip(output_path_zip, files = files_to_zip)

        # Add info to ReactiveVals()...
        the_time(execution_time)
        output_temp_folder(new_temp_folder)
        output_path_rmd(output_path_rmd)
        output_path_pdf(output_path_pdf)
        output_path_html(output_path_html)
        output_path_word(output_path_word)
        output_path_zip(output_path_zip)


      })


      #--- 8.3) Control de los objetos 8.1)


      download_counter_pdf  <- reactiveVal()
      download_counter_html <- reactiveVal()
      download_counter_word <- reactiveVal()
      download_counter_zip  <- reactiveVal()

      control_06 <- reactive({

        # Todos los botones por defecto van a estar desarmados...
        shinyjs::disable("download_button_pdf")
        shinyjs::disable("download_button_html")
        shinyjs::disable("download_button_word")
        shinyjs::disable("download_button_zip")

        # Los dejamos en naranja...
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))

        # Activamos todos los contadores de los botones a cero.
        download_counter_pdf(0)
        download_counter_html(0)
        download_counter_word(0)
        download_counter_zip(0)


        # Controlamos que todos los objetos 8.1) no sean NUll.
        req(control_05(), render_button_counter() >= 1,
            the_time(), output_temp_folder(),
            output_path_rmd(), output_path_pdf(), output_path_html(),
            output_path_word(), output_path_zip())



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
          need(file.exists(output_path_zip()),    'Error 024: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          errorClass = "ERROR"
        )


        # # # Si todo se cumplio, activamos los botones.
        shinyjs::enable("download_button_pdf")
        shinyjs::enable("download_button_html")
        shinyjs::enable("download_button_word")
        shinyjs::enable("download_button_zip")

        # # # # Y los dejamos en verde...
        # if(download_counter_pdf()  >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_pdf")))
        # if(download_counter_html() >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_html")))
        # if(download_counter_word() >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_word")))
        # if(download_counter_zip()  >= 1) runjs(sprintf('$("#%s").css({"background-color": "green", "color": "white", "border": "none", "padding": "10px 20px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "16px", "margin": "4px 2px", "cursor": "pointer", "border-radius": "12px"});', ns("download_button_zip")))


        return(TRUE)
      })
      ###---###---###---###---###---###---###---###---###---###---###---###---###---###


      #--- 9.2) Accion de cada boton de descarga



      observeEvent(input$selected_var_name, {

        render_button_counter(0)
        download_counter_pdf(0)
        download_counter_html(0)
        download_counter_word(0)
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
        armado_v <- paste('<div style="height: 1000vh; width: 100%; overflow: hidden;"><iframe style="height: 1000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

        return(armado_v)
      })




    }) # Fin Module
}


module_diplo_001_clase02_p01_serverB <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      output$box01_database <- renderUI({

        shinydashboard::box(
          title = "01 - Seleccionar base de datos",
          status = "primary",
          id = ns("my_box01"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          width = 12,
          fluidRow(
            column(2,
                   shiny::selectInput(
                     inputId = ns("data_source"),
                     label = "Fuente de datos",
                     choices = c("01 - CSV files" = "csv_source",
                                 "02 - Diplo UNC" = "diplo_source",
                                 "03 - R examples" = "r_source")
                   )
            ),
            column(7,
                   div(shinyjs::useShinyjs(), id = ns("input-var-02-A"),
                       shiny::conditionalPanel(
                         condition = "input.data_source == 'csv_source'",
                         ns = ns,
                         fluidRow(
                           column(4,
                                  fileInput(
                                    inputId = ns("csv_file_path"),
                                    label = "Elija un archivo CSV",
                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                    #accept = "*/*"
                                  )
                           )
                         ),
                         fluidRow(
                           column(2, radioButtons(
                             inputId = ns("csv_header"),
                             label = "header",
                             choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                             selected = TRUE
                           )),
                           column(3, radioButtons(
                             inputId = ns("csv_sep"),
                             label = "Separador de columnas",
                             choices = c("semicolon (;)" = ";", "comma (,)" = ","),
                             selected = ";"
                           )),
                           column(3, radioButtons(
                             inputId = ns("csv_dec"),
                             label = "Decimal",
                             choices = c("Period (.)" = ".", "Comma (,)" = ","),
                             selected = "."
                           )),
                           column(3, radioButtons(
                             inputId = ns("csv_quote"),
                             label = "Comillas",
                             choices = c("Double quotes (\")" = "\"", "Simple quotes (')" = "'"),
                             selected = "\""
                           ))
                         )#,
                         #tags$hr()
                       )
                   ),
                   div(shinyjs::useShinyjs(), id = ns("input-var-02-B"),
                       shiny::conditionalPanel(
                         condition = "input.data_source == 'diplo_source'",
                         ns = ns,
                         fluidRow(
                           column(5,
                                  shiny::selectInput(
                                    inputId = ns("diplo_database"),
                                    label = "Bases de la Diplomatura",
                                    choices = c(    "Seleccione una..." = "",
                                                "01 - SEMANA02_BASE01"  = "SEMANA02_BASE01",
                                                "02 - SEMANA02_BASE02"  = "SEMANA02_BASE02",
                                                "03 - SEMANA02_BASE03"  = "SEMANA02_BASE03",
                                                "04 - SEMANA02_BASE04"  = "SEMANA02_BASE04")
                                  )
                           )
                         )
                       )
                   ),
                   shiny::conditionalPanel(
                     condition = "input.data_source == 'r_source'",
                     ns = ns,
                     fluidRow(
                       column(3,
                              shiny::selectInput(
                                inputId = ns("r_database"),
                                label = "Bases de R",
                                choices = c("01 - mtcars" = "mtcars", "02 - iris" = "iris")
                              )
                       )
                     )
                   )
            ),
            column(3, actionButton(ns("action_load"),
                                   label = "LOAD"),
            )
          ),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_database"))),

        )

      })


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
          shiny::uiOutput(ns("var_selector"))

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
                downloadButton(outputId = ns('download_button_pdf'),  label = "PDF", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_html'), label = "HTML", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_word'), label = "WORD", width = "100%", disabled = TRUE),
                downloadButton(outputId = ns('download_button_zip'),  label = "All (ZIP)", width = "100%", disabled = TRUE)
              )
            )
        )
      })


      output$box04_report <- renderUI({


        ns <- NS(id)

        shinydashboard::box(
          title = "04 - Reporte y Script",
          status = "primary",
          id = ns("my_box02"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,# Colapsado por defecto
          width = 12,
          div(
            # fluidRow(
            #   column(4, h2("Temporal()"),
            #          shinycssloaders::withSpinner(htmlOutput(ns('pdfviewer_temporal'))))
            #   ),
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
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

