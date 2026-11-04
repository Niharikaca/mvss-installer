# start_app.R --------------------------------------------------------------
# Entry point used by the installer – runs the Shiny app offline.

app_dir <- dirname(normalizePath(sys.frame(1)))

# port = 0 -> let Shiny pick any free local port automatically
shiny::runApp(app_dir, launch.browser = TRUE, port = 0)
