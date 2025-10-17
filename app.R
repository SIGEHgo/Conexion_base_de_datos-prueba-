library(shiny)
library(DBI)
library(RPostgres)
library(DT)


# Obtener las variables de entorno para la conexión
db_host <- Sys.getenv("db_host")
db_user <- Sys.getenv("db_user")
db_pass <- Sys.getenv("db_pass")
db_port <- as.numeric(Sys.getenv("db_port")) # Asegúrate de convertir el puerto a numérico
db_name <- Sys.getenv("db_name")

##Codigo para conectarnos a sql usando dplyr
library(DBI)
library(RPostgres)
buig <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname=db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)
#DBI::dbExecute(buig, "SET search_path TO emergencia;")


Lista_BUIG=DBI::dbListTables(buig) |> as.list()

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
