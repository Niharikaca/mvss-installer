# Build the MVSS Windows installer with a bundled portable R runtime.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "build_installer.R"
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
app_dir <- file.path(project_dir, "app")
portable_root <- file.path(project_dir, "R-portable")
installer_dir <- file.path(project_dir, "installer")
iss_template <- file.path(project_dir, "mvss_installer.iss")

required_packages <- c("shiny", "shinydashboard", "DT", "shinythemes", "ggplot2", "scales", "tidyr", "shinyWidgets", "nortest", "moments", "dplyr", "readxl", "flextable", "officer", "corrplot", "rhandsontable", "pROC", "caret", "colourpicker", "RColorBrewer", "viridisLite", "rlang", "car", "openxlsx", "writexl", "filelock")
stop_with <- function(...) stop(sprintf(...), call. = FALSE)
for (path in c(app_dir, portable_root, iss_template)) if (!file.exists(path)) stop_with("Required project path is missing: %s", path)
for (name in c("server.R", "ui.R")) if (!file.exists(file.path(app_dir, name))) stop_with("Required app file is missing: app/%s", name)

runtime_candidates <- c(file.path(portable_root, "bin", "Rscript.exe"), list.files(portable_root, pattern = "^Rscript\\.exe$", recursive = TRUE, full.names = TRUE))
runtime_candidates <- unique(runtime_candidates[file.exists(runtime_candidates)])
if (!length(runtime_candidates)) stop_with("No bundled Rscript.exe found below %s", portable_root)
runtime_root <- dirname(dirname(normalizePath(runtime_candidates[[1]], winslash = "/")))
if (!file.exists(file.path(runtime_root, "etc")) || !file.exists(file.path(runtime_root, "library"))) stop_with("Portable R is incomplete; expected etc/ and library/ beside bin/ in %s", runtime_root)

iscc_candidates <- c(Sys.which("ISCC.exe"), "C:/Program Files (x86)/Inno Setup 6/ISCC.exe", "C:/Program Files/Inno Setup 6/ISCC.exe")
iscc_candidates <- unique(iscc_candidates[nzchar(iscc_candidates) & file.exists(iscc_candidates)])
if (!length(iscc_candidates)) stop_with("Inno Setup 6 compiler (ISCC.exe) was not found. Install it from https://jrsoftware.org/isdl.php, then rerun this build.")
iscc_path <- normalizePath(iscc_candidates[[1]], winslash = "/")

bundled_library <- file.path(runtime_root, "library")
missing <- required_packages[!file.exists(file.path(bundled_library, required_packages, "DESCRIPTION"))]
if (length(missing)) stop_with("Missing bundled R packages: %s. Install these into this portable R library before building: install.packages(c(%s), repos='https://cloud.r-project.org')", paste(missing, collapse = ", "), paste(sprintf("'%s'", missing), collapse = ","))

if (!dir.exists(installer_dir)) dir.create(installer_dir, recursive = TRUE)
stage_dir <- tempfile("mvss-installer-")
payload_dir <- file.path(stage_dir, "payload")
dir.create(payload_dir, recursive = TRUE)
copy_contents <- function(source, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  items <- list.files(source, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  if (length(items) && !all(file.copy(items, destination, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE))) {
    stop_with("Failed to stage files from %s", source)
  }
}
copy_contents(app_dir, file.path(payload_dir, "app"))
copy_contents(runtime_root, file.path(payload_dir, "R-portable"))
for (name in c("Launch-MVSS.bat", "Launch-MVSS.vbs", "Launch-MVSS.ps1", "Launch-MVSS.R", "Stop-MVSS.ps1", "Stop-MVSS.vbs")) if (!file.copy(file.path(project_dir, name), payload_dir, overwrite = TRUE)) stop_with("Failed to stage %s.", name)

output_exe <- file.path(installer_dir, "MVSS-Setup-1.0.1.exe")
if (file.exists(output_exe)) unlink(output_exe)
source_define <- paste0('/DSourceRoot=', shQuote(normalizePath(payload_dir, winslash = "/"), type = "cmd"))
output_define <- paste0('/DOutputRoot=', shQuote(normalizePath(installer_dir, winslash = "/"), type = "cmd"))
compile_output <- system2(iscc_path, args = c("/Q", source_define, output_define, shQuote(iss_template, type = "cmd")), stdout = TRUE, stderr = TRUE)
compile_status <- attr(compile_output, "status")
cat(paste(compile_output, collapse = "\n"), "\n")
if (is.null(compile_status)) compile_status <- 0L
if (compile_status != 0L) stop_with("Inno Setup compilation failed with exit code %s.", compile_status)
if (!file.exists(output_exe) || file.info(output_exe)$size <= 0) stop_with("Compiler returned success but installer EXE is missing or empty.")
unlink(stage_dir, recursive = TRUE, force = TRUE)
cat(sprintf("Installer created: %s\nSize: %.1f MB\n", normalizePath(output_exe, winslash = "/"), file.info(output_exe)$size / 1024^2))
