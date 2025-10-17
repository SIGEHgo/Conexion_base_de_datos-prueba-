library(shiny)
library(DBI)
library(RPostgres)
library(DT)
library(leaflet)
library(pool)
library(dbplyr)


# # Obtener las variables de entorno para la conexión
db_host <- Sys.getenv("db_host")
db_user <- Sys.getenv("db_user")
db_pass <- Sys.getenv("db_pass")
db_port <- as.numeric(Sys.getenv("db_port")) # Asegúrate de convertir el puerto a numérico
db_name <- Sys.getenv("db_name")

##Codigo para conectarnos a sql usando dplyr
buig <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)

onStop(function() {
  pool::poolClose(buig)
})


Lista_BUIG= pool::dbListTables(buig) |> as.list()


ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput(inputId = "tabla", label =  "Selecciona una tabla:", choices = Lista_BUIG),
    ),
    column(
      width = 8,
      DTOutput("vista_tabla"),
      leafletOutput("mapa")
    )
  )
)


server <- function(input, output, session) {
  
  datos_reactivos <- reactive({
    req(input$tabla)
    
    
    datos = dplyr::tbl(buig, input$tabla) |>
      head() |>
      dplyr::collect() |>
      dplyr::mutate(geom = sf::st_as_sfc(geom, EWKB = TRUE))
    
    
    coordenadas = sf::st_coordinates(datos$geom[1])[1, 1]
    if (coordenadas > 30) {
      datos = datos |> sf::st_as_sf(crs = 32614) |> sf::st_transform(crs = 4326) |> sf::st_zm()
    } else {
      datos = datos |> sf::st_as_sf(crs = 4326) |> sf::st_zm()
    }
    
    return(datos)
  })
  
  
  
  # Tabla
  output$vista_tabla <- renderDT({
    datos = datos_reactivos()
    
    if (is.null(datos)) {
      DT::datatable(data.frame(Error = paste("No se pudo leer la tabla:", input$tabla)))
    } else {
      DT::datatable(datos, options = list(pageLength = 10))
    }
  })
  
  # Mapa
  output$mapa <- renderLeaflet({ 
    datos = datos_reactivos()
    
    leaflet() |> 
      addTiles() |> 
      addMarkers(data = datos)
  }) 
}



shinyApp(ui, server)