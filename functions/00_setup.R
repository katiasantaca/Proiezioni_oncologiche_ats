# ============================================================
# SETUP PACCHETTI E FUNZIONE DI RENDER
# ============================================================
 
# 1. Pacchetti richiesti ----
packages <- c(
  "readr",
  "haven",
  "lubridate",
  "dplyr",
  "tidyr",
  "forecast",
  "ggplot2",
  "ggpattern",
  "knitr",
  "gridExtra",
  "gtable",
  "rmarkdown",
  "stringr"
)

# 2. Installa solo i pacchetti mancanti ----
missing <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if (length(missing) > 0) {
  install.packages(missing)
}

# 3. Carica i pacchetti ----
invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

# 4. Funzione per generare i report ----
render_report <- function(file) {
  suppressWarnings(
    rmarkdown::render(
      file,
      output_dir = "output",
      quiet = TRUE
    )
  )
}


# 5. aggiugo cartella qc ----
dir.create(
  "output/qc",
  showWarnings = FALSE,
  recursive = TRUE
)

