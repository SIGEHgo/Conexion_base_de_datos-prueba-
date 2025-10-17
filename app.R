library(shiny)
library(DBI)
library(RPostgres)
library(DT)

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput(inputId = "tabla", label =  "Selecciona una tabla:", choices = Lista_BUIG),
      verbatimTextOutput("estado_conexion"),
      actionButton(inputId = "reconectar", label = "Reconectar DB")
    ),
    column(
      width = 8,
      DTOutput("vista_tabla")
    )
  )
)


server <- function(input, output, session) {
  

  # Botón para reconectar
  observeEvent(input$reconectar, {
    try({
      buig <<- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = db_name,
        host = db_host,
        port = db_port,
        user = db_user,
        password = db_pass
      )
      showNotification("Conexión restablecida correctamente.", type = "message")
    }, silent = TRUE)
  })
  
  # Tabla
  output$vista_tabla = renderDT({
    req(input$tabla)
    if (!DBI::dbIsValid(buig)) {
      return(DT::datatable(data.frame(Error = "Conexión no válida")))
    }
    datos = DBI::dbReadTable(buig, input$tabla)
    if (is.null(datos)) {
      DT::datatable(data.frame(Error = paste("No se pudo leer la tabla:", input$tabla)))
    } else {
      DT::datatable(datos, options = list(pageLength = 10))
    }
  })
}


shinyApp(ui, server)
