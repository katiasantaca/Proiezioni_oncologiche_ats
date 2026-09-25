df_incidenti <- table_prediction_registro_assistiti_presenti

rm(table_prediction_registro_assistiti_presenti)
###############################################################
## FUNCTIONS
###############################################################

#####
summary_incidence <- function(data, group = NULL){
  
  if(is.null(group)){
    
    out <- data %>%
      summarise(across(starts_with("evento_"), sum),
                across(starts_with("fup_"), sum))
    
  } else{
    
    out <- data %>%
      group_by(across(all_of(group))) %>%
      summarise(across(starts_with("evento_"), sum), 
                across(starts_with("fup_"), sum), .groups = "drop")
    
  }
  
  out %>%
    pivot_longer(cols = matches("^(evento|fup)_"),
                 names_to = c(".value","anno"),
                 names_sep = "_") %>%
    rename(n_eventi = evento,
           sumfup_days = fup)
}


#####
compute_IR <- function(summary_table,
                       population_table,
                       by){
  
  out <- population_table %>%
    mutate(anno_assistenza = as.character(anno_assistenza)) %>%
    left_join(summary_table, by = by) %>%
    filter(is.na(n_eventi)==FALSE) %>%
    rowwise() %>%
    mutate(PY = ((n - n_eventi) * 365.25 + sumfup_days) / 365.25,
           #calculate IR as n.events per PY
           IR = n_eventi / PY,
           test = list(poisson.test(x=n_eventi, T = PY)),
           lower_IR = test$conf.int[1],
           upper_IR = test$conf.int[2],
           IR_10000 = round(IR * 10000, 2),
           lower_IR_10000 = round(lower_IR * 10000, 2),
           upper_IR_10000 = round(upper_IR * 10000, 2)) %>%
    select(-test) %>%
    ungroup()
  
  
  out
  
}



###############################################################
## OVERALL
###############################################################

summary_all <- summary_incidence(df_incidenti)

table_IR_all <- compute_IR(summary_table = summary_all,
                           population_table = caratterizzazione_assistiti,
                           by = c("anno_assistenza" = "anno")) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  rename(anno = anno_assistenza) %>%
  select(anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc)


###############################################################
## BY CANCER SITE
###############################################################

#definiamo ordine categorie di cancer_site: raggruppate per "organo comune"

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


summary_sede <- summary_incidence(df_incidenti, group = "cancer_site")

table_IR_sede <- compute_IR(summary_table = summary_sede,
                            population_table = caratterizzazione_assistiti,
                            by = c("anno_assistenza" = "anno")) %>%
  rename(anno = anno_assistenza) %>%
  mutate(cancer_site=factor(cancer_site,levels=canc_site_levels)) %>%
  group_by(cancer_site) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(cancer_site,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(cancer_site,anno)



###############################################################
## BY SEX
###############################################################

summary_sex <- summary_incidence(df_incidenti, group = "sesso")

table_IR_sex <- compute_IR(summary_table = summary_sex,
                           population_table = caratterizzazione_assistiti_sesso,
                           by = c("anno_assistenza" = "anno","sesso" = "sesso")) %>%
  rename(anno = anno_assistenza,sex = sesso) %>%
  mutate(sex=factor(sex,levels=c("M","F"))) %>%
  group_by(sex) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(sex,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(sex,anno)



###############################################################
## BY AGE (macro categories)
###############################################################

summary_age <- summary_incidence(df_incidenti,group = "classe_eta")

table_IR_agecat <-compute_IR(summary_table = summary_age,
                             population_table = caratterizzazione_assistiti_classe_eta,
                             by = c("anno_assistenza" = "anno", "classe_eta" = "classe_eta")) %>%
  rename(anno = anno_assistenza,
         age_cat = classe_eta) %>%
  mutate(age_cat = factor(age_cat, levels=c("<50","50-59", "60-69","70-79","80+"))) %>%
  group_by(age_cat) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(age_cat,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(age_cat,anno) 




###############################################################
## BY AGE (5-year classes)
###############################################################

summary_age5 <- summary_incidence(df_incidenti,group = "classe_eta_5")

table_IR_agecat5 <-compute_IR(summary_table = summary_age5,
                              population_table = caratterizzazione_assistiti_classe_eta_5,
                              by = c("anno_assistenza" = "anno", "classe_eta_5" = "classe_eta_5")) %>%
  rename(anno = anno_assistenza,
         age_cat5 = classe_eta_5) %>%
  mutate(age_cat5 = factor(age_cat5,
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+"))) %>%
  group_by(age_cat5) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(age_cat5,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(age_cat5,anno) 


###############################################################
## BY DISTRICT
###############################################################

summary_district <- summary_incidence(df_incidenti,group = "distretto")


table_IR_district <-compute_IR(summary_table = summary_district,
                               population_table = caratterizzazione_distretto,
                               by = c("anno_assistenza" = "anno", "DISTRETTO" = "distretto")) %>%
  rename(anno = anno_assistenza,
         distretto = DISTRETTO) %>%
  mutate(distretto = str_to_sentence(distretto),
         distretto = factor(distretto,
                            levels=c("Cremasco","Cremonese","Mantovano","Alto mantovano","Basso mantovano", "Cremona - casalasco - viadanese oglio po", "Mantova - casalasco - viadanese oglio po"))) %>%
  group_by(distretto) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(distretto,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(distretto,anno) 


###############################################################
## BY ASSST
###############################################################

summary_asst <- summary_incidence(df_incidenti,group = "asst")

table_IR_asst <-compute_IR(summary_table = summary_asst,
                           population_table = caratterizzazione_asst,
                           by = c("anno_assistenza" = "anno", "ASST" = "asst")) %>%
  rename(anno = anno_assistenza,
         asst = ASST) %>%
  mutate(asst = str_to_sentence(asst),
         asst = factor(asst,
                       levels=c("Crema","Cremona","Mantova"))) %>%
  group_by(asst) %>%
  mutate(delta_IR_perc = if_else(is.na(lag(IR_10000)),
                                 NA_character_,
                                 sprintf("%.2f%%", (IR_10000 / lag(IR_10000) - 1) * 100))) %>%
  ungroup() %>%
  select(asst,anno,n_eventi,PY,IR_10000,lower_IR_10000,upper_IR_10000, delta_IR_perc) %>%
  arrange(asst,anno) 



#################################################################
rm(list = setdiff(ls(), ls(pattern = "table_")))

save.image("dati/computed_IRs.RData")

