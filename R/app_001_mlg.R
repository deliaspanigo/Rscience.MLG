# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


# https://ubco-biology.github.io/BIOL202/pdf-latex-is-not-found.html
#install.packages("tinytex", dependencies = TRUE)
#library("tinytex")
#tinytex::install_tinytex()

app_001_mlg <- function(){

  library(stringr)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(openxlsx)
  library(shinyjs)
  library(markdown)
  library(rmarkdown)
  library(Rscience.MLG)
  #ruta_css <- system.file("www", "estilos.css", package = "miPaquete")

#includeCSS(ruta_css)

  # Definir la interfaz de usuario
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Rscience"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      "MLG - 0.0.1",
      shiny::br(),
      shiny::br(),
      shinydashboard::menuItem(text = "Inicio", tabName = "tab_intro", icon = shiny::icon("th")),
      br(),
      shinydashboard::menuItem(text = "database", tabName = "tab_database", icon = shiny::icon("th")),


      shinydashboard::menuItem(text = "Test 001 - Anova 1 Factor", tabName = "tab_test001_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Test 002 - Kaplan-Maier", tabName = "tab_test002_p01", icon = shiny::icon("th"))


    )
  ),
  shinydashboard::dashboardBody(

    # Incluir CSS personalizado
    htmltools::includeCSS(system.file("www/style.css", package = "Rscience.MLG")),
    shinyjs::useShinyjs(),

    shinydashboard::tabItems(

       shinydashboard::tabItem(tabName = "tab_intro",
                               module_diplo_001_clase99_ui("space_intro")), # Final - tab_clase99


       shinydashboard::tabItem(tabName = "tab_database",
                              module_mlg_000_p01_database_ui("space_database")), # Final - tab_clase99





      shinydashboard::tabItem(tabName = "tab_test001_p01",
                           #   tableOutput("la_tabla"),
                              module_mlg_test001_p01_ui("space_test001_p01")),#, # Final - tab_clase01

      shinydashboard::tabItem(tabName = "tab_test002_p01",
                              #   tableOutput("la_tabla"),
                              module_mlg_test002_p01_ui("space_test002_p01"))#, # Final - tab_clase01

     )
    )
    )


# addResourcePath("tmpuser", getwd())

# Definir la lógica del servidor
server <- function(input, output) {


  module_diplo_001_clase99_server("space_intro")

  # Server - Clase 01
                         module_mlg_000_p01_database_serverA(id = "space_database")
  input_list_database <- module_mlg_000_p01_database_serverB(id = "space_database")




  # Server - Test001 - Anova 1 Factor
  module_mlg_test001_p01_serverA(id = "space_test001_p01",
                                 database = input_list_database$"database",
                                 code_database = input_list_database$"code_database")

  module_mlg_test001_p01_serverB(id = "space_test001_p01", database = input_list_database$database)



  # Server - Test001 - Anova 1 Factor
  module_mlg_test002_p01_serverA(id = "space_test002_p01",
                                 database = input_list_database$"database",
                                 code_database = input_list_database$"code_database")

  module_mlg_test002_p01_serverB(id = "space_test002_p01", database = input_list_database$database)


} #--- Fin server

# Ejecutar la aplicación
shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



