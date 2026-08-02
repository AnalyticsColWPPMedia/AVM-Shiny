# app.R (Opcional, solo si necesitas un punto de entrada explícito)
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)