args <- commandArgs(trailingOnly = TRUE)
.libPaths(.Library)

app_dir <- if (length(args)) {
  normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath("app", winslash = "/", mustWork = TRUE)
}

port <- suppressWarnings(as.integer(Sys.getenv("MVSS_PORT", "4242")))
if (is.na(port) || port < 1L || port > 65535L) {
  stop("MVSS_PORT must be a valid TCP port from 1 to 65535.")
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The bundled Shiny package is missing. Repair or reinstall MVSS.")
}

shiny::runApp(
  app_dir,
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE
)
