


# # # 01) UI - Selection for 'database'
module_diplo_001_clase99_ui <- function(id){
  ns <- shiny::NS(id)



  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      shiny::h1("Pasos..."),
      "1) Selecciona la semana...",br(),
      "2) Selecciona una base de datos...",br(),
      "3) Dar clic en LOAD",br(),
      "4) Completar la Selección de variables",br(),
      "5) Dar clic en Render!"

  ) # End div
}












module_diplo_001_clase99_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns



    }) # Fin Module
}



