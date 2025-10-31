#!/usr/bin/env Rscript
# -----
# Author: Wesley Stubenbord
# Description: This file generates a local web server to display the interactive
# map in your browser. You can serve the interactive-map.html file locally in 
# any other way you might prefer, however, or simply access it online.
# ----

serve_map <- function() {
  # Load required library
  if (!require("httpuv", quietly = TRUE)) {
    stop("Package 'httpuv' is required. Install it with: install.packages('httpuv')")
  }
  
  # Check if HTML file exists
  if (!file.exists("interactive-map.html")) {
    stop("Error: File 'interactive-map.html' not found in current directory")
  }
  
  # Simple HTTP handler
  app <- list(
    call = function(req) {
      path <- req$PATH_INFO
      if (path == "/" || path == "") {
        path <- "/interactive-map.html"
      }
      
      file_path <- sub("^/", "", path)
      
      if (file.exists(file_path)) {
        content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
        list(status = 200L, headers = list("Content-Type" = "text/html"), body = content)
      } else {
        list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not found")
      }
    }
  )
  
  # Start server
  server <- startServer("127.0.0.1", 8000, app)
  
  cat("\n")
  cat("Local interactive map server started/\n")
  cat("Map served at: http://127.0.0.1:8000/interactive-map.html\n")
  cat("In RStudio, press Esc to stop (or Ctrl+C elsewhere).\n")

  # Open browser
  browseURL("http://127.0.0.1:8000/interactive-map.html")
  
  # Keep running until interrupted
  tryCatch({
    while (TRUE) {
      Sys.sleep(0.1)
      httpuv::service()
    }
  }, interrupt = function(e) {
    cat("Stopping...\n")
  })
  
  # Stop server
  stopServer(server)
  cat("Server stopped.\n")
}

# Run the function to open the interactive map
serve_map()