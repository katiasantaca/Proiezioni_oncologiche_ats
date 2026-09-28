# ============================================================
# PREPARAZIONE DEL DENOMINATORE DI POPOLAZIONE
# ============================================================
#
# Lo script:
#   1. importa i denominatori annuali e il registro tumori;
#    ---denominatori
#   2. associa a ciascun comune ASST, Distretto e Ambito;
#   3. armonizza la denominazione del distretto Oglio Po;
#   4. aggrega la popolazione per anno, sesso, fascia di età
#      e articolazione territoriale; 
#   5-6-7. costruisce le classi di età utilizzate nelle analisi.
#    ---registro
#   8. aggiunta della sede tumorale come riportato nel paper di riferimento dell'obbiettivo 4.
# ============================================================


# ------------------------------------------------------------
# 1. Importazione dei dati
# ------------------------------------------------------------

import_denominatori <- read.csv(
  "./dati/ats_denominatore_per_anni.csv"
)

import_definizione_comuni <- read.csv(
  "./dati/comuni_ATS_singoli_terreok.csv"
)

registro_tumori_originale<-read_sas("./dati/rt_2009_2021_crypt.sas7bdat")
# ------------------------------------------------------------
# 2. Associazione delle informazioni territoriali
# ------------------------------------------------------------
# A ciascun comune di residenza (COMRES) vengono associate
# le informazioni relative ad ASST, Distretto e Ambito.

denominatore_finale <- import_denominatori %>%
  left_join(
    import_definizione_comuni %>%
      select(
        COMRES,
        ASST,
        DISTRETTO,
        AMBITO
      ),
    by = "COMRES"
  )


# Gli oggetti utilizzati esclusivamente per l'importazione
# non sono più necessari.

rm(
  import_denominatori,
  import_definizione_comuni
)


# ------------------------------------------------------------
# 3. Armonizzazione delle denominazioni territoriali
# ------------------------------------------------------------
# Il distretto "CASALASCO - VIADANESE OGLIO PO" è presente
# in più ASST. Per distinguerlo correttamente nelle analisi,
# la denominazione viene preceduta dal nome dell'ASST.

denominatore_finale <- denominatore_finale %>%
  mutate(
    DISTRETTO = if_else(
      DISTRETTO == "CASALASCO - VIADANESE OGLIO PO",
      paste(ASST, DISTRETTO, sep = " - "),
      DISTRETTO
    )
  )


# ------------------------------------------------------------
# 4. Aggregazione del denominatore
# ------------------------------------------------------------
# La popolazione viene aggregata per:
#   - anno;
#   - sesso;
#   - fascia di età quinquennale originale;
#   - ASST;
#   - Distretto;
#   - Ambito.
#
# POP rappresenta la popolazione complessiva corrispondente
# a ciascuna combinazione delle variabili sopra indicate.

denominatore_finale_aggregato <- denominatore_finale %>%
  group_by(
    anno,
    SESSO,
    FASCIA_ETA_TASSI,
    ASST,
    DISTRETTO,
    AMBITO
  ) %>%
  summarise(
    POP = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )


# Il dataset precedente all'aggregazione non è più necessario.

rm(denominatore_finale)


# ============================================================
# CLASSIFICAZIONE DELLE FASCE DI ETÀ
# ============================================================
#
# Vengono costruite due classificazioni:
#
#   classe_eta:
#       macro-classi utilizzate per le analisi per fascia d'età
#
#   classe_eta_5:
#       classi quinquennali con etichette semplificate
#
# ============================================================

denominatore_finale_aggregato <- denominatore_finale_aggregato %>%
  mutate(
    
    # --------------------------------------------------------
    # 5. Macro-classi di età
    # --------------------------------------------------------
    
    classe_eta = case_when(
      
      FASCIA_ETA_TASSI %in% c(
        "000-004",
        "005-009",
        "010-014",
        "015-019",
        "020-024",
        "025-029",
        "030-034",
        "035-039",
        "040-044",
        "045-049"
      ) ~ "<50",
      
      FASCIA_ETA_TASSI %in% c(
        "050-054",
        "055-059"
      ) ~ "50-59",
      
      FASCIA_ETA_TASSI %in% c(
        "060-064",
        "065-069"
      ) ~ "60-69",
      
      FASCIA_ETA_TASSI %in% c(
        "070-074",
        "075-079"
      ) ~ "70-79",
      
      FASCIA_ETA_TASSI %in% c(
        "080-084",
        "085-999"
      ) ~ "80+",
      
      TRUE ~ NA_character_
    ),
    
    
    # --------------------------------------------------------
    # 6. Classi di età quinquennali
    # --------------------------------------------------------
    
    classe_eta_5 = recode(
      FASCIA_ETA_TASSI,
      
      "000-004" = "0-4",
      "005-009" = "5-9",
      "010-014" = "10-14",
      "015-019" = "15-19",
      "020-024" = "20-24",
      "025-029" = "25-29",
      "030-034" = "30-34",
      "035-039" = "35-39",
      "040-044" = "40-44",
      "045-049" = "45-49",
      "050-054" = "50-54",
      "055-059" = "55-59",
      "060-064" = "60-64",
      "065-069" = "65-69",
      "070-074" = "70-74",
      "075-079" = "75-79",
      "080-084" = "80-84",
      "085-999" = "85+",
      
      .default = NA_character_
    )
  )

# ------------------------------------------------------------
# 7. Controllo delle fasce di età non riconosciute
# ------------------------------------------------------------

fasce_eta_non_classificate <- denominatore_finale_aggregato %>%
  filter(
    is.na(classe_eta) |
      is.na(classe_eta_5)
  ) %>%
  distinct(FASCIA_ETA_TASSI)

if (nrow(fasce_eta_non_classificate) > 0) {
  
  warning(
    "Sono presenti fasce di età non riconosciute nella classificazione."
  )
  
} else {
  
  rm(fasce_eta_non_classificate)
  
}


# ------------------------------------------------------------
# 8. Sede tumorale consistente al paper definito dall'obbiettivo 4
# ------------------------------------------------------------