# ============================================================
# CARATTERIZZAZIONE DELLA POPOLAZIONE ASSISTITA
# ============================================================
#
# A partire dal dataset aggregato "denominatore_finale_aggregato"
# vengono prodotti i dataset utilizzati per descrivere la
# popolazione assistita secondo:
# 
#   - anno
#   - sesso
#   - classe di età
#   - classe di età quinquennale
#   - ASST
#   - distretto
#
# La variabile POP rappresenta la numerosità della popolazione
# assistita per ciascuna combinazione di caratteristiche.
# ============================================================


# ------------------------------------------------------------
# 1. Popolazione assistita complessiva per anno
# ------------------------------------------------------------

caratterizzazione_assistiti <- denominatore_finale_aggregato %>%
  group_by(anno) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno
  )


# ------------------------------------------------------------
# 2. Popolazione assistita per anno e sesso
# ------------------------------------------------------------

caratterizzazione_assistiti_sesso <- denominatore_finale_aggregato %>%
  group_by(anno, SESSO) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno,
    sesso = SESSO
  )


# ------------------------------------------------------------
# 3. Popolazione assistita per anno e classe di età
# ------------------------------------------------------------

caratterizzazione_assistiti_classe_eta <- denominatore_finale_aggregato %>%
  group_by(anno, classe_eta) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno
  )


# ------------------------------------------------------------
# 4. Popolazione assistita per anno e classe di età quinquennale
# ------------------------------------------------------------

caratterizzazione_assistiti_classe_eta_5 <- denominatore_finale_aggregato %>%
  group_by(anno, classe_eta_5) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno
  )


# ------------------------------------------------------------
# 5. Popolazione assistita per anno e ASST
# ------------------------------------------------------------

caratterizzazione_asst <- denominatore_finale_aggregato %>%
  group_by(anno, ASST) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno
  )


# ------------------------------------------------------------
# 6. Popolazione assistita per anno, ASST e distretto
# ------------------------------------------------------------

caratterizzazione_distretto <- denominatore_finale_aggregato %>%
  group_by(anno, ASST, DISTRETTO) %>%
  summarise(
    n = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    anno_assistenza = anno
  )


# ------------------------------------------------------------
# Pulizia dell'ambiente
# ------------------------------------------------------------
# Il dataset intermedio non è più necessario dopo la creazione
# delle tabelle di caratterizzazione.

rm(denominatore_finale_aggregato)