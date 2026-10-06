# ==============================================================================
# MAIN ANALYSIS SCRIPT -----
# ==============================================================================
#
# Script name : main.R
# Authors     : Katia Santacà, Andrea Fontana
# Date        : 2026-09-24
# Project     : Proiezioni dell'incidenza oncologica - ATS Val Padana
# Institution : Università degli Studi di Verona
#
# ==============================================================================
# DESCRIPTION
# ==============================================================================
#
# Questo script esegue l'intera pipeline di analisi del Registro Tumori in questo ordine:
#
#   1. configurazione dell'ambiente di analisi;
#   2. importazione e preparazione dei dati; 
#   3. preparazione dei denominatori;
#   4. controlli di qualità del Registro Tumori;
#   5. trasformazione del Registro Tumori nel formato richiesto dalle analisi;
#   6. creazione della tabella descrittiva della popolazione oncologica;
#   7. calcolo delle tabelle utilizzate per le analisi e le proiezioni;
#   8. generazione automatica dei report finali.
#
# ==============================================================================
# ISTRUZIONI PER L'ESECUZIONE
# ==============================================================================
#
# 1. Estrarre/copiare l'intera cartella del progetto mantenendone invariata
#    la struttura.
#
# 2. Verificare che nella cartella "dati" sia presente il file:
#
#       rt_2009_2021_crypt.sas7bdat
#
# 3. Impostare la WORKING DIRECTORY del progetto.
#
# Impostare qui il percorso della cartella principale del progetto.
# Esempio:
# setwd("/percorso/della/cartella/proiezioni-oncologiche-ats")

setwd("/Users/katia_santaca/Desktop/--LAVORO--/progetti_github/proiezioni-oncologiche-ats")


#
# 5. Eseguire questo script utilizzando:
#
#       Source
#
#    oppure dalla console R:
#
#       source("main.R")
#
# Oppure selezionando tutta questa pagina e selezionare il tasto Run
# ==============================================================================
# STRUTTURA ATTESA DEL PROGETTO
# ==============================================================================
#
# ATS/
# |
# |-- main.R
# |-- ATS.Rproj
# |
# |-- dati/
# |   `-- registro_tumori_project.RData
# |
# |-- functions/
# |   |-- 00_setup.R
# |   |-- 01_importazione_e_preparazione_dati.R
# |   |-- 02_caratterizzazione_denominatore.R
# |   |-- 03_qc.R
# |   |-- 04_trasformazione_registro.R
# |   |-- 05_creazione_table_one_registro.R
# |   `-- 06_creazione_tabelle_proiezione.R
# |
# |-- 1-IR overall.Rmd
# |-- 2-IR by cancer sites.Rmd
# |-- 3-IR by sex.Rmd
# |-- 4-IR by age classes cat5.Rmd
# |-- 5-IR by age classes macrocat.Rmd
# |-- 6-IR by ASST.Rmd
# |-- 7-IR by district.Rmd
# |
# `-- output/
#
# ==============================================================================
# IMPORTANTE
# ==============================================================================
#
# Non modificare i file contenuti nella cartella "functions" salvo diversa
# indicazione.
#
# Tutti gli output generati dall'analisi vengono salvati nella cartella
# "output".
#
# ==============================================================================


# ==============================================================================
# 0. CONTROLLO DELLA WORKING DIRECTORY
# ==============================================================================

required_files <- c(
  "functions/00_setup.R",
  "functions/01_importazione_e_preparazione_dati.R",
  "functions/02_caratterizzazione_denominatore.R",
  "functions/03_qc.R",
  "functions/04_trasformazione_registro.R",
  "functions/05_creazione_table_one_registro.R",
  "functions/06_creazione_tabelle_proiezione.R"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  
  stop(
    paste0(
      "\nERRORE: la working directory non sembra corrispondere ",
      "alla cartella principale del progetto.\n\n",
      "File mancanti:\n",
      paste0(" - ", missing_files, collapse = "\n"),
      "\n\n",
      "Impostare la working directory nella cartella principale ",
      "del progetto e rieseguire main.R."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 1. CREAZIONE DELLE CARTELLE DI OUTPUT
# ==============================================================================

output_dirs <- c(
  "output",
  "output/qc"
)

for (dir in output_dirs) {
  
  dir.create(
    dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ==============================================================================
# 2. SETUP DELL'AMBIENTE
# ==============================================================================

message("\n[1/7] Configurazione dell'ambiente...")

source(
  "./functions/00_setup.R",
  local = FALSE
)


# ==============================================================================
# 3. IMPORTAZIONE E PREPARAZIONE DEI DATI
# ==============================================================================

message("[2/7] Importazione e preparazione del Registro Tumori...")

source(
  "./functions/01_importazione_e_preparazione_dati.R",
  local = FALSE
)


# ==============================================================================
# 4. PREPARAZIONE DEI DENOMINATORI
# ==============================================================================

message("[3/7] Preparazione dei denominatori...")

source(
  "./functions/02_caratterizzazione_denominatore.R",
  local = FALSE
)


# ==============================================================================
# 5. QUALITY CONTROL DEL REGISTRO TUMORI
# ==============================================================================

message("[4/7] Esecuzione dei controlli di qualità...")

source(
  "./functions/03_qc.R",
  local = FALSE
)


# ==============================================================================
# 6. TRASFORMAZIONE DEL REGISTRO TUMORI
# ==============================================================================

message("[5/7] Preparazione dei dati per le analisi di incidenza...")

source(
  "./functions/04_trasformazione_registro.R",
  local = FALSE
)


# ==============================================================================
# 7. TABELLA DESCRITTIVA DEL REGISTRO TUMORI
# ==============================================================================

message("[6/7] Creazione della tabella descrittiva del Registro Tumori...")

source(
  "./functions/05_creazione_table_one_registro.R",
  local = FALSE
)


# ==============================================================================
# 8. CREAZIONE DELLE TABELLE PER LE PROIEZIONI
# ==============================================================================

message("[7/7] Calcolo delle tabelle per le analisi e le proiezioni...")

source(
  "./functions/06_creazione_tabelle_proiezione.R",
  local = FALSE
)


# ==============================================================================
# 9. GENERAZIONE DEI REPORT
# ==============================================================================

message("\nGenerazione dei report...")

suppressWarnings(
  rmarkdown::render(
    "1-IR overall.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "1-IR overall.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)
suppressWarnings(
  rmarkdown::render(
    "2-IR by cancer sites.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "3-IR by sex.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "4-IR by age classes_cat5.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "5-IR by age classes_macrocat.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "6-IR by ASST.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

suppressWarnings(
  rmarkdown::render(
    "7-IR by district.Rmd",
    output_dir = "output",
    quiet = TRUE
  )
)

# ==============================================================================
# FINE ANALISI
# ==============================================================================

message(
  paste0(
    "\n",
    "============================================================\n",
    " ANALISI COMPLETATA CON SUCCESSO\n",
    "============================================================\n",
    "\n",
    "Gli output sono disponibili nella cartella:\n",
    "\n",
    "  output/\n",
    "\n",
    "Controllare in particolare gli output di Quality Control\n",
    "prima di utilizzare o condividere i risultati finali.\n"
  )
)
 
