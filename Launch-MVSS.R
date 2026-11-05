args <- commandArgs(trailingOnly = TRUE)
.libPaths(.Library)
app_dir <- if (length(args)) normalizePath(args[[1]], winslash = "/", mustWork = TRUE) else normalizePath("app", winslash = "/", mustWork = TRUE)
port <- suppressWarnings(as.integer(Sys.getenv("MVSS_PORT", "4242")))
if (is.na(port) || port < 1L || port > 65535L) stop("MVSS_PORT must be a valid TCP port from 1 to 65535.")
if (!requireNamespace("filelock", quietly = TRUE)) stop("The bundled filelock package is missing. Repair or reinstall MVSS.")
lock_dir <- Sys.getenv("MVSS_DATA_DIR", file.path(Sys.getenv("LOCALAPPDATA"), "MVSS"))
dir.create(lock_dir, recursive = TRUE, showWarnings = FALSE)
browser_callback <- function(url) {
  tryCatch(utils::browseURL(url), error = function(e) message("Open this address in your browser: ", url))
}
lock <- filelock::lock(file.path(lock_dir, sprintf("mvss-%d.lock", port)), timeout = 0)
if (is.null(lock)) {
  browser_callback(sprintf("http://127.0.0.1:%d", port))
  message("MVSS is already running; opened its local page in your browser.")
  quit(save = "no", status = 0)
}
on.exit(filelock::unlock(lock), add = TRUE)
if (!requireNamespace("shiny", quietly = TRUE)) stop("The bundled Shiny package is missing. Repair or reinstall MVSS.")
shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = browser_callback)
