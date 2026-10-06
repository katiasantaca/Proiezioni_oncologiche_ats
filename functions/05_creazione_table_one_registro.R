# ============================================================
# CREAZIONE TABLE 1 - REGISTRO TUMORI
# ============================================================

# ============================================================
# CARTELLA OUTPUT
# ============================================================

dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# FUNZIONI DI FORMATTAZIONE
# ============================================================

# Interi con separatore delle migliaia
# es. 30147 -> "30,147"
format_n <- function(x) {
  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}


# Percentuali
# es. 48.6 -> "48.6"
# 4.0 -> "4"
format_perc <- function(x) {
  
  out <- sprintf("%.1f", x)
  
  out <- sub("\\.0$", "", out)
  
  out
}


# N (%)
# es. 30147, 48.6 -> "30,147 (48.6)"
format_n_perc <- function(n, perc) {
  
  paste0(
    format_n(n),
    " (",
    format_perc(perc),
    ")"
  )
}


# ============================================================
# FUNZIONE TABLE 1
# ============================================================

crea_table_one_registro <- function(data) {
  
  # ----------------------------------------------------------
  # Preparazione
  # ----------------------------------------------------------
  
  data <- data %>%
    mutate(
      dinciden = as.Date(dinciden)
    )
  
  
  # ----------------------------------------------------------
  # Numero totale di soggetti
  # ----------------------------------------------------------
  
  n_totale <- n_distinct(data$person_id)
  
  
  # ----------------------------------------------------------
  # Prima incidenza per soggetto
  # ----------------------------------------------------------
  
  prima_incidenza <- data %>%
    arrange(
      person_id,
      dinciden
    ) %>%
    group_by(person_id) %>%
    slice(1) %>%
    ungroup()
  
  
  # ----------------------------------------------------------
  # Pazienti con più di un tumore
  #
  # Tumori distinti definiti da:
  # person_id + cancer_site + dinciden
  # ----------------------------------------------------------
  
  pazienti_piu_tumori <- data %>%
    distinct(
      person_id,
      cancer_site,
      dinciden
    ) %>%
    count(
      person_id,
      name = "n_tumori"
    ) %>%
    filter(n_tumori > 1)
  
  n_piu_tumori <- nrow(pazienti_piu_tumori)
  
  perc_piu_tumori <- n_piu_tumori / n_totale * 100
  
  
  # ==========================================================
  # SESSO
  # ==========================================================
  
  tab_sesso <- prima_incidenza %>%
    count(
      sesso,
      name = "n"
    ) %>%
    mutate(
      perc = n / n_totale * 100,
      voce = case_when(
        sesso == "F" ~ "Femmine",
        sesso == "M" ~ "Maschi",
        TRUE ~ as.character(sesso)
      ),
      valore = format_n_perc(n, perc)
    ) %>%
    select(
      voce,
      valore
    )
  
  
  # ==========================================================
  # ETÀ
  # ==========================================================
  
  eta_media <- mean(
    prima_incidenza$eta,
    na.rm = TRUE
  )
  
  eta_sd <- sd(
    prima_incidenza$eta,
    na.rm = TRUE
  )
  
  eta_mediana <- median(
    prima_incidenza$eta,
    na.rm = TRUE
  )
  
  eta_q1 <- quantile(
    prima_incidenza$eta,
    probs = 0.25,
    na.rm = TRUE,
    names = FALSE
  )
  
  eta_q3 <- quantile(
    prima_incidenza$eta,
    probs = 0.75,
    na.rm = TRUE,
    names = FALSE
  )
  
  
  # ==========================================================
  # CLASSI DI ETÀ
  # ==========================================================
  
  tab_classe_eta <- prima_incidenza %>%
    mutate(
      classe_eta = factor(
        classe_eta,
        levels = c(
          "<50",
          "50-59",
          "60-69",
          "70-79",
          "80+"
        )
      )
    ) %>%
    count(
      classe_eta,
      name = "n"
    ) %>%
    arrange(classe_eta) %>%
    mutate(
      perc = n / n_totale * 100,
      voce = case_when(
        as.character(classe_eta) == "80+" ~ "≥80",
        TRUE ~ as.character(classe_eta)
      ),
      valore = format_n_perc(n, perc)
    ) %>%
    select(
      voce,
      valore
    )
  
  
  # ==========================================================
  # ANNO DI DIAGNOSI
  # ==========================================================
  
  tab_anno <- prima_incidenza %>%
    count(
      anno_incidenza,
      name = "n"
    ) %>%
    arrange(anno_incidenza) %>%
    mutate(
      perc = n / n_totale * 100,
      voce = as.character(anno_incidenza),
      valore = format_n_perc(n, perc)
    ) %>%
    select(
      voce,
      valore
    )
  
  
  # ==========================================================
  # BASE DIAGNOSI
  # ==========================================================
  
  tab_base <- prima_incidenza %>%
    count(
      base,
      name = "n"
    ) %>%
    arrange(base) %>%
    mutate(
      perc = n / n_totale * 100,
      voce = as.character(base),
      valore = format_n_perc(n, perc)
    ) %>%
    select(
      voce,
      valore
    )
  
  
  # ==========================================================
  # SEDE TUMORALE ALLA PRIMA INCIDENZA
  # ==========================================================
  
  tab_sede <- prima_incidenza %>%
    count(
      cancer_site,
      name = "n"
    ) %>%
    mutate(
      perc = n / n_totale * 100,
      
      voce = recode(
        cancer_site,
        
        "Breast" =
          "Mammella",
        
        "Colon rectum" =
          "Colon-retto",
        
        "Lung" =
          "Polmone",
        
        "Prostate" =
          "Prostata",
        
        "Urinary bladder" =
          "Vescica urinaria",
        
        "Other specified cancer site" =
          "Altra sede tumorale specificata",
        
        "Stomach" =
          "Stomaco",
        
        "Pancreas" =
          "Pancreas",
        
        "Kidney and urinary organs" =
          "Rene e organi urinari",
        
        "Non-Hodgkin lymphomas" =
          "Linfomi non-Hodgkin",
        
        "Liver" =
          "Fegato",
        
        "Melanoma of skin" =
          "Melanoma della pelle",
        
        "Thyroid" =
          "Tiroide",
        
        "Upper digestive tract" =
          "Tratto digestivo superiore",
        
        "Corpus uteri" =
          "Corpo dell'utero",
        
        "Multiple myeloma" =
          "Mieloma multiplo",
        
        "Brain and nervous system" =
          "Cervello e sistema nervoso",
        
        "Leukaemia" =
          "Leucemia",
        
        "Ovary" =
          "Ovaio",
        
        "Biliary tract" =
          "Vie biliari",
        
        "Hodgkin disease" =
          "Malattia di Hodgkin",
        
        "Oesophagus" =
          "Esofago",
        
        "Cervix uteri" =
          "Cervice uterina",
        
        "Testis" =
          "Testicolo",
        
        "Connective and soft tissue" =
          "Tessuto connettivo e tessuti molli",
        
        "Mesothelioma" =
          "Mesotelioma",
        
        "Kaposi sarcoma" =
          "Sarcoma di Kaposi",
        
        "Bone" =
          "Osso",
        
        "Uterus unspecified" =
          "Utero",
        
        .default = as.character(cancer_site)
      ),
      
      valore = format_n_perc(
        n,
        perc
      )
    ) %>%
    
    # Ordine decrescente per numerosità,
    # come nella tabella richiesta
    arrange(desc(n)) %>%
    
    select(
      voce,
      valore
    )
  
  
  # ==========================================================
  # TABELLA FINALE
  # ==========================================================
  
  table_one <- bind_rows(
    
    # Sesso
    tibble(
      voce = "Sesso",
      valore = ""
    ),
    
    tab_sesso,
    
    
    # Età
    tibble(
      voce = "Età media (deviazione standard)",
      valore = paste0(
        sprintf("%.1f", eta_media),
        " (",
        sprintf("%.1f", eta_sd),
        ")"
      )
    ),
    
    tibble(
      voce = "Mediana (primo-terzo quartile)",
      valore = paste0(
        format_n(eta_mediana),
        " (",
        format_n(eta_q1),
        "-",
        format_n(eta_q3),
        ")"
      )
    ),
    
    
    # Classe età
    tibble(
      voce = "Classe di età",
      valore = ""
    ),
    
    tab_classe_eta,
    
    
    # Tumori multipli
    tibble(
      voce = "Pazienti con più di un tumore",
      valore = format_n_perc(
        n_piu_tumori,
        perc_piu_tumori
      )
    ),
    
    
    # Anno diagnosi
    tibble(
      voce = "Anno di diagnosi",
      valore = ""
    ),
    
    tab_anno,
    
    
    # Base diagnosi
    tibble(
      voce = "Base diagnosi*",
      valore = ""
    ),
    
    tab_base,
    
    
    # Sede tumorale
    tibble(
      voce = "Sede tumorale",
      valore = ""
    ),
    
    tab_sede
  )
  
  
  # ----------------------------------------------------------
  # Nome colonna dinamico con N totale
  # ----------------------------------------------------------
  
  names(table_one) <- c(
    "",
    paste0(
      "N= ",
      format_n(n_totale),
      " (%)"
    )
  )
  
  
  return(table_one)
}


# ============================================================
# CREAZIONE TABLE 1
# ============================================================

table_one_registro <- crea_table_one_registro(
  table_prediction_registro_assistiti_presenti
)


# ============================================================
# VISUALIZZAZIONE
# ============================================================

print(
  table_one_registro,
  n = Inf
)


# ============================================================
# SALVATAGGIO CSV
# ============================================================

write_csv(
  table_one_registro,
  "output/table_one_registro.csv"
)