anni <- 2009:2021

table_prediction_registro_assistiti_presenti <- registro_tumori %>%
  mutate(
    dinciden = as.Date(dinciden),
    anno_incidenza = year(dinciden),
    eta = ETA, sesso= SESSO, asst = ASST, distretto = DISTRETTO
  )

for (aa in anni) {
  table_prediction_registro_assistiti_presenti[[paste0("evento_", aa)]] <-
    as.integer(table_prediction_registro_assistiti_presenti$anno_incidenza == aa)
  
  table_prediction_registro_assistiti_presenti[[paste0("fup_", aa)]] <-
    ifelse(
      table_prediction_registro_assistiti_presenti$anno_incidenza == aa,
      as.integer(
        table_prediction_registro_assistiti_presenti$dinciden -
          as.Date(paste0(aa, "-01-01"))
      ) + 0.5,
      0L
    )
}

table_prediction_registro_assistiti_presenti <- table_prediction_registro_assistiti_presenti %>%
  mutate(
    classe_eta = case_when(
      eta < 50 ~ "<50",
      eta >= 50 & eta < 60 ~ "50-59",
      eta >= 60 & eta < 70 ~ "60-69",
      eta >= 70 & eta < 80 ~ "70-79",
      eta >= 80 ~ "80+",
      TRUE ~ NA_character_
    ),
    classe_eta_5 = case_when(
      eta < 5 ~ "0-4",
      eta < 10 ~ "5-9",
      eta < 15 ~ "10-14",
      eta < 20 ~ "15-19",
      eta < 25 ~ "20-24",
      eta < 30 ~ "25-29",
      eta < 35 ~ "30-34",
      eta < 40 ~ "35-39",
      eta < 45 ~ "40-44",
      eta < 50 ~ "45-49",
      eta < 55 ~ "50-54",
      eta < 60 ~ "55-59",
      eta < 65 ~ "60-64",
      eta < 70 ~ "65-69",
      eta < 75 ~ "70-74",
      eta < 80 ~ "75-79",
      eta < 85 ~ "80-84",
      eta >= 85 ~ "85+",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    person_id,
    sesso,
    anno_di_nascita,
    eta,
    dof,
    #vit_stat_dof,
    #data_decesso,
    dinciden,
    anno_incidenza,
    cancer_site,
    starts_with("evento_"),
    starts_with("fup_"),
    base,
    classe_eta,
    classe_eta_5, asst, distretto
  )
