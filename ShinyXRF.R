source("global.R")  # Load libraries and functions
source("ui.R")      # Load the user interface
source("server.R")  # Load the server


shinyApp(ui = ui, server = server)
