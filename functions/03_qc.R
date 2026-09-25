# ============================================================
# PREPARAZIONE E CONTROLLO QUALITÀ DEL REGISTRO TUMORI ----
# ============================================================
#
# Lo script:
#   1. prepara le variabili necessarie per le analisi;
#   2. registra la numerosità iniziale del Registro Tumori;
#   3. esclude i tumori della cute con codice ICD-10 C44;
#   4. identifica e rimuove eventuali duplicati riferiti alla
#      stessa persona, sede tumorale e data di incidenza;
#   5. salva nel percorso output/qc i record interessati
#      dai controlli di qualità.
#
# Il Registro Tumori viene assunto come fonte validata.
# Non vengono pertanto effettuati ulteriori controlli
# su sesso e stato in vita/decesso.
# ============================================================


# ------------------------------------------------------------
# 1. Conservazione del Registro Tumori originale ----
# ------------------------------------------------------------

registro_tumori <- registro_tumori_originale

# ------------------------------------------------------------
# 2. Preparazione delle variabili ----
# ------------------------------------------------------------
# La data di incidenza e la data di fine osservazione vengono
# convertite in formato Date.
#
# In assenza dell'anno di nascita, questo viene stimato come:
# anno della diagnosi - età alla diagnosi.
#
# Il codice fiscale viene rinominato come person_id e vengono corretti i distretti omonimi.
# Viene aggiunto il cancer site 
# ============================================================
# 2.1 set date ----
# ============================================================
registro_tumori <- registro_tumori %>%
  mutate(
    dinciden = as.Date(dinciden),
    dof = as.Date(dof),
    anno_di_nascita = year(dinciden) - ETA,
    person_id = codfisc
  ) %>%
  select(-codfisc)
# ============================================================
# 2.1 set distretti  ----
# ============================================================
registro_tumori <- registro_tumori %>%
  mutate(
    DISTRETTO = if_else(
      DISTRETTO == "CASALASCO - VIADANESE OGLIO PO",
      paste(ASST, DISTRETTO, sep = " - "),
      DISTRETTO
    )
  )

# ============================================================
# 2.2 set sede  ----
# ============================================================
#
# La sede tumorale viene classificata a partire dal codice
# ICD-10 a tre caratteri (icd10_3).
#
# Le etichette vengono assegnate direttamente in italiano,
# evitando il successivo utilizzo di una tabella esterna
# di traduzione.
# ============================================================

registro_tumori <- registro_tumori %>%
  mutate(
    cancer_site = case_when(
      
      icd10_3 %in% c(
        "C01", "C02", "C03", "C04", "C05", "C06",
        "C09", "C10", "C11", "C12", "C13", "C14",
        "C32"
      ) ~ "Tratto digestivo superiore",
      
      icd10_3 == "C15" ~ "Esofago",
      
      icd10_3 == "C16" ~ "Stomaco",
      
      icd10_3 %in% c(
        "C18", "C19", "C20", "C21"
      ) ~ "Colon-retto",
      
      icd10_3 == "C22" ~ "Fegato",
      
      icd10_3 %in% c(
        "C23", "C24"
      ) ~ "Vie biliari",
      
      icd10_3 == "C25" ~ "Pancreas",
      
      icd10_3 %in% c(
        "C33", "C34"
      ) ~ "Polmone",
      
      icd10_3 %in% c(
        "C40", "C41"
      ) ~ "Osso",
      
      icd10_3 == "C43" ~ "Melanoma della pelle",
      
      icd10_3 == "C45" ~ "Mesotelioma",
      
      icd10_3 == "C46" ~ "Sarcoma di Kaposi",
      
      icd10_3 %in% c(
        "C47", "C49"
      ) ~ "Tessuto connettivo e tessuti molli",
      
      icd10_3 == "C50" ~ "Mammella",
      
      icd10_3 == "C53" ~ "Cervice uterina",
      
      icd10_3 == "C54" ~ "Corpo dell'utero",
      
      icd10_3 == "C55" ~ "Utero, sede non specificata",
      
      icd10_3 == "C56" ~ "Ovaio",
      
      icd10_3 == "C61" ~ "Prostata",
      
      icd10_3 == "C62" ~ "Testicolo",
      
      icd10_3 %in% c(
        "C64", "C65", "C66", "C68"
      ) ~ "Rene e organi urinari",
      
      icd10_3 %in% c(
        "D41", "D09", "C67") ~ "Vescica urinaria",
      
      icd10_3 %in% c(
        "C70", "C71", "C72"
      ) ~ "Cervello e sistema nervoso",
      
      icd10_3 == "C73" ~ "Tiroide",
      
      icd10_3 == "C81" ~ "Malattia di Hodgkin",
      
      icd10_3 %in% c(
        "C82", "C83", "C84", "C85", "C96"
      ) ~ "Linfomi non-Hodgkin",
      
      icd10_3 %in% c(
        "C88", "C89", "C90"
      ) ~ "Mieloma multiplo",
      
      icd10_3 %in% c(
        "C92", "C93", "C94", "C95"
      ) ~ "Leucemia",
      
      (
        icd10_3 >= "C00" & icd10_3 <= "C43"
      ) |
        (
          icd10_3 >= "C45" & icd10_3 <= "C96"
        ) ~ "Altra sede tumorale specificata",
      
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 2.3 Le sedi tumorali vengono ordinate raggruppando, per quanto  ----
# possibile, sedi anatomicamente o clinicamente affini. ----
# ============================================================

canc_site_levels <- c(
  "Esofago",
  "Stomaco",
  "Tratto digestivo superiore",
  "Colon-retto",
  "Fegato",
  "Vie biliari",
  "Pancreas",
  "Polmone",
  "Mesotelioma",
  "Melanoma della pelle",
  "Sarcoma di Kaposi",
  "Rene e organi urinari",
  "Vescica urinaria",
  "Cervice uterina",
  "Corpo dell'utero",
  "Utero, sede non specificata",
  "Ovaio",
  "Prostata",
  "Testicolo",
  "Tiroide",
  "Mammella",
  "Cervello e sistema nervoso",
  "Malattia di Hodgkin",
  "Linfomi non-Hodgkin",
  "Mieloma multiplo",
  "Leucemia",
  "Osso",
  "Tessuto connettivo e tessuti molli",
  "Altra sede tumorale specificata"
)


# ============================================================
# 3 REPORT DEI CONTROLLI DI QUALITÀ ----
# ============================================================

tabella_report_qc <- tibble(
  controllo = character(),
  numero_righe = character()
)


# Funzione di supporto per aggiungere una riga al report QC  ----

aggiungi_qc <- function(tabella, descrizione, valore) {
  
  bind_rows(
    tabella,
    tibble(
      controllo = descrizione,
      numero_righe = as.character(valore)
    )
  )
}


# ------------------------------------------------------------
# 3.0 Numerosità iniziale del Registro Tumori ----
# CONTROLLO 1  ----
# ------------------------------------------------------------

tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "1_Numero righe nel registro di partenza",
  nrow(registro_tumori)
)

tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "1_Numero distinto soggetti nel registro di partenza",
  n_distinct(registro_tumori$person_id)
)


# ============================================================
# 4 CONTROLLO DUPLICATI  ----
# ============================================================
#
# Vengono ricercate più righe associate alla stessa combinazione:
#
#   person_id + cancer_site + dinciden
#
# Le righe interessate vengono salvate per il controllo QC.
# Per ogni combinazione duplicata viene successivamente
# mantenuta una sola riga.
# ============================================================


# Identificazione delle combinazioni duplicate
err7_tab_piu_linee_per_la_stessa_inci_data_paziente <-
  registro_tumori %>%
  count(
    person_id,
    cancer_site,
    dinciden,
    name = "n"
  ) %>%
  filter(n > 1)


# Estrazione di tutte le righe originali interessate
# dalle combinazioni duplicate
err7_tutte_le_righe <- registro_tumori %>%
  semi_join(
    err7_tab_piu_linee_per_la_stessa_inci_data_paziente,
    by = c(
      "person_id",
      "cancer_site",
      "dinciden"
    )
  ) %>%
  arrange(
    person_id,
    cancer_site,
    dinciden
  )


 
write_csv(
  err7_tutte_le_righe,
  "output/qc/02qc_tutte_le_righe_per_stessa_incidenza_data_paziente.csv"
)

# CONTROLLO 2  ----
# Aggiornamento del report QC
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  paste0(
    "2_Numero combinazioni persona-sede-data con più righe; ",
    "vedere file err7_tab_piu_linee_per_la_stessa_inci_data_paziente.csv"
  ),
  nrow(err7_tab_piu_linee_per_la_stessa_inci_data_paziente)
)


# Rimozione dei duplicati
# Viene mantenuta la prima riga per ciascuna combinazione
# persona-sede-data di incidenza.
registro_tumori <- registro_tumori %>%
  arrange(
    person_id,
    cancer_site,
    dinciden
  ) %>%
  distinct(
    person_id,
    cancer_site,
    dinciden,
    .keep_all = TRUE
  )


# Numerosità dopo il controllo
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "2_Numero distinto soggetti nel registro dopo controllo",
  n_distinct(registro_tumori$person_id)
)

tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "2_Numero righe nel registro dopo controllo",
  nrow(registro_tumori)
)


# Pulizia oggetti temporanei
rm(
  err7_tab_piu_linee_per_la_stessa_inci_data_paziente,
  err7_tutte_le_righe
)


# ============================================================
# 5 ESCLUSIONE DEL CODICE ICD-10 C44----
# ============================================================
#
# Le neoplasie cutanee codificate come C44 vengono escluse
# dalle analisi di incidenza oncologica.
#
# Le righe escluse vengono conservate in un file QC.
# ============================================================


# Identificazione delle righe da escludere
err8_rimossa_sede_c44 <- registro_tumori %>%
  filter(icd10_3 == "C44")


# Esclusione delle sole righe C44.
# Eventuali valori mancanti di icd10_3 vengono mantenuti.
registro_tumori <- registro_tumori %>%
  filter(
    is.na(icd10_3) |
      icd10_3 != "C44"
  )


# Salvataggio delle righe escluse
write_csv(
  err8_rimossa_sede_c44,
  "output/qc/03qc_righe_rimosse_presenza_sede_c44.csv"
)


# Aggiornamento del report QC
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  paste0(
    "3_Numero righe eliminate perché riferite al codice",
    "diagnostico ICD-10 C44; vedere file err8_rimossa_sede_c44.csv"
  ),
  nrow(err8_rimossa_sede_c44)
)


sedi_tumorali_non_classificate <- registro_tumori %>%
  filter(is.na(cancer_site)) %>%
  count(icd10_3, sort = TRUE)

if (nrow(sedi_tumorali_non_classificate) > 0) {
  
  warning(
    "Sono presenti codici ICD-10 non associati a una sede tumorale."
  )
  
} else {
  
  rm(sedi_tumorali_non_classificate)
  
}
check_sede_icd10 <- registro_tumori %>%
  count(sede, icd10_3, cancer_site, name = "n") %>%
  arrange(sede, icd10_3)

write_csv(check_sede_icd10, "output/qc/00qc_controllo_congruenza_numerosita_tumorale_per_sede_ats_vs_sede_progettualita.csv")
rm(check_sede_icd10)


# Numerosità dopo il controllo
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "3_Numero distinto soggetti nel registro dopo controllo",
  n_distinct(registro_tumori$person_id)
)

tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "3_Numero righe nel registro dopo controllo",
  nrow(registro_tumori)
)


# Pulizia dell'oggetto temporaneo
rm(err8_rimossa_sede_c44)

# ------------------------------------------------------------
# Identificazione delle righe duplicate che verranno eliminate
# ------------------------------------------------------------

righe_rimosse_stessa_sede <- registro_tumori %>%
  group_by(person_id, cancer_site) %>%
  arrange(dinciden, .by_group = TRUE) %>%
  filter(
    cancer_site != "Altra sede tumorale specificata",
    row_number() > 1
  ) %>%
  ungroup()


# Salvataggio delle righe eliminate per controllo QC
write_csv(
  righe_rimosse_stessa_sede,
  "output/qc/04qc_righe_rimosse_stessa_sede_tumorale.csv"
)


# ------------------------------------------------------------
# Mantenimento della prima incidenza per persona e sede
# ------------------------------------------------------------
# Per ogni persona e sede tumorale viene mantenuta la prima
# incidenza in ordine cronologico.
#
# Per la categoria "Altra sede tumorale specificata" vengono
# invece mantenute tutte le osservazioni.

registro_tumori <- registro_tumori %>%
  group_by(person_id, cancer_site) %>%
  arrange(dinciden, .by_group = TRUE) %>%
  filter(
    cancer_site == "Altra sede tumorale specificata" |
      row_number() == 1
  ) %>%
  ungroup()
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "4_Numero righe nel registro dopo controllo",
  nrow(registro_tumori)
)
# Numerosità dopo il controllo
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
  "4_Numero distinto soggetti nel registro dopo controllo",
  n_distinct(registro_tumori$person_id)
)
# Aggiornamento del report QC
tabella_report_qc <- aggiungi_qc(
  tabella_report_qc,
    "4_Numero righe più incidenza per stessa sede",
  nrow(righe_rimosse_stessa_sede)
)
rm(righe_rimosse_stessa_sede)

write_csv(tabella_report_qc, "output/qc/tabella_report_qc.csv")
rm(tabella_report_qc)
