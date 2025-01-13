#Vi laver FORV's indikator
library(dkstat)
library(tidyr)
library(ggplot2)
library(dplyr)

#4.1

FORV <- list(
  INDIKATOR="*",
  TID="*"
)

FORV <- dst_get_data(table = "FORV1",query = FORV,lang = "da")

# Fjern de første 3 karakterer ("F1 ")
FORV$INDIKATOR <- gsub("^[^ ]+ ", "", FORV$INDIKATOR)

FORV <- pivot_wider(FORV, 
                    names_from = INDIKATOR, 
                    values_from = value, 
                    id_cols = TID)

#Starter for år 1996
FORV <- FORV[256:nrow(FORV),]
FORV <- as.data.frame(FORV)
row.names(FORV) <- FORV[,1]
FORV <- FORV[,-1]


#Laver det til kvartaler
FORV <- ts(FORV, start = c(1996, 1), frequency = 12)
FORV <- aggregate(FORV, nfrequency = 4)/3
FORV <- as.data.frame(FORV)

år <- rep(1996:2024, each = 4)  # 28 år (1996-2024), 4 kvartaler pr. år
kvartal <- rep(1:4, 28)         # 4 kvartaler pr. år i 28 år
række_navn <- paste0(år, "K", kvartal) # Kombiner år og kvartal for at lave labels som "1996K1", "1996K2" osv.
række_navn <- række_navn[1:116] 
rownames(FORV) <- række_navn # Tilføj de nye rækkenavne til din dataframe df_ft


#Vi trækker FTI ud af datasættet, dette skal bruges senere
DST_FTI <- as.data.frame(FORV[,1])
colnames(DST_FTI) <- "Forbrugertillidsindikatoren"
FORV <- FORV[,-1]

FORV$Kvartal <- rownames(FORV)

#Vi vender fortegnene
colnames(FORV)
FORV$`Priser i dag, sammenlignet med for et år siden` <- FORV$`Priser i dag, sammenlignet med for et år siden`*-1
FORV$`Arbejdsløsheden om et år, sammenlignet med i dag` <- FORV$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1



#Opret et nyt dataframe med udvalgte kolonner fra DI_ny
begge_indikatorer <- FORV %>%
    select(Kvartal)

begge_indikatorer <- cbind(begge_indikatorer,DST_FTI)


#Gennemsnittet af de 4 underspørgsmål fra DI
begge_indikatorer$DI_Indikator <- NA
for(i in 1:nrow(FORV)){
  sum <- sum(FORV[i,c(1,3,5,9)]/4)
  begge_indikatorer$DI_Indikator[i] <- sum 
}


ggplot(begge_indikatorer, aes(x = Kvartal)) + 
  # Linje for Forbrugertillidsindikatoren med legend
  geom_line(aes(y = Forbrugertillidsindikatoren, color = "Forbrugertillidsindikatoren"), size = 0.8, group = 1, show.legend = TRUE) +  
  # Sekundær akse for linjerne
  scale_y_continuous(sec.axis = sec_axis(~., name = "Procent")) +  
  # Kun K1 vises på x-aksen
  scale_x_discrete(breaks = begge_indikatorer$Kvartal[grepl("K1", begge_indikatorer$Kvartal)]) +  
  # Definer farverne for linjer
  scale_color_manual(values = c("Forbrugertillidsindikatoren" = "red")) +  # Vælg farve til linjen
  # Labels til grafen med justerede titler
  labs(y = "Procent", x = "", 
       title = "Udsving i Forbrugertillidsindikatoren under økonomisk vækst og kriser (1996-2024)",
       subtitle = "Indikatoren viser en næsten ligelig fordeling mellem højkonjunkturer og lavkonjunkturer i perioden") +
  # Brug minimal stil og roter x-aksen for bedre læsbarhed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom",  # Placer legenden i bunden
        legend.title = element_blank(),  # Fjern titlen fra legenden
        legend.key = element_blank(),    # Fjern kasserne rundt om linjerne
        legend.text = element_text(size = 10),  # Juster tekststørrelsen i legenden
        plot.title = element_text(size = 14, face = "bold"),  # Gør titlen fed
        plot.subtitle = element_text(size = 10),  # Juster underteksten
        legend.background = element_rect(fill = "white", color = "white")) +  # Hvid baggrund for legenden
  # Brug guide_legend() til at vise linjer som streger
  guides(color = guide_legend(override.aes = list(linetype = c(1))))


#Opg. 4.2

# Filtrer kun data fra år 2000 og frem
FORV_filter <- FORV %>%
  filter(as.numeric(sub("K.*", "", Kvartal)) >= 2000)

# Kontrollér, om data nu kun indeholder rækker fra 2000 og frem
head(FORV_filter)


Opg.4.2 <- colMeans(FORV_filter[,5, drop = FALSE])

print(Opg.4.2)

#Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket  = -10.45233 
summary(FORV_filter$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)




#Opg. 4.3

# Hent data og transformér
# For at finde navnene på kolonner i statistikbanken henter jeg metadata.
Forbrug <- dst_meta(table = "NKHC021", lang = "da")  

Forbrugsamlet <- list( 
  FORMAAAL = "*", 
  Tid = "*", 
  PRISENHED = "2020-priser, kædede værdier", 
  SÆSON = "Sæsonkorrigeret" 
) 

Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrugsamlet, lang = "da") 

# Transformér data til bredt format
Forbrug_opdelt <- pivot_wider(
  Forbrugdata,  
  names_from = FORMAAAL, 
  values_from = value,  
  id_cols = TID
) 

# Konverter til data.frame og tilføj TID som kolonne
Forbrug_opdelt <- as.data.frame(Forbrug_opdelt)
rownames(Forbrug_opdelt) <- Forbrug_opdelt[, 1]
Forbrug_opdelt <- Forbrug_opdelt[, -1]
Forbrug_opdelt$TID <- rownames(Forbrug_opdelt)

# Filtrer data for 2020 og 2023
Forbrug_filtered <- Forbrug_opdelt %>%
  filter(substr(TID, 1, 4) =="2020" | substr(TID, 1, 4) =="2023") %>%
  mutate(År = substr(TID, 1, 4))  # Ekstraher årstal

# Beregn gennemsnit for hvert år
Forbrug_gns <- Forbrug_filtered %>%
  group_by(År) %>%
  summarise(across(-TID, mean, na.rm = TRUE)) %>%
  pivot_longer(-År, names_to = "Kategori", values_to = "Værdi")

# Fjern rækken med "CPT i alt"
Forbrug_gns <- Forbrug_gns %>%
  filter(Kategori != "CPT I alt")

# Hvad brugte danskerne flest penge på i 2023?
Forbrug_2023 <- Forbrug_gns %>%
  filter(År == "2023") %>%
  arrange(desc(Værdi))

print("Hvad danskerne brugte flest penge på i 2023:")
print(Forbrug_2023[1, ])  # Topkategori for 2023

# Beregn procentvise ændringer fra 2020 til 2023
Forbrug_ændring <- Forbrug_gns %>%
  pivot_wider(names_from = År, values_from = Værdi) %>%
  mutate(Procentændring = ((`2023` - `2020`) / `2020`) * 100) %>%
  arrange(desc(Procentændring))

print("Hvilken gruppe steg mest fra 2020 til 2023:")
print(Forbrug_ændring[1, ])  # Topkategori med størst procentændring

# Lav graf for 2023 uden "CPT i alt"
ggplot(Forbrug_2023, aes(x = reorder(Kategori, -Værdi), y = Værdi, fill = Kategori)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Danskerne brugte flest penge på Boligudnyttelse i 2023",
    subtitle = "(2023-priser, kvartalsgennemsnit, kædede værdier)",
    x = "Kategori",
    y = "Værdi"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )


#4.4

# Filtrer data for perioden 2000K1 til 2024K2 for begge indikatorer og forbrugsdata
regression_data <- begge_indikatorer %>%
  filter(Kvartal >= "2000K1" & Kvartal <= "2024K2")

Forbrug_opdelt_regr <- Forbrug_opdelt %>%
  filter(TID >= "2000-01-01" & TID < "2024-07-01")

# Fjern eventuelle irrelevante kolonner som "TID" eller "I alt"
forbrugsgrupper <- Forbrug_opdelt_regr %>%
  select(-`CPT I alt`)
  
# Konverter TID til Kvartal
forbrugsgrupper <- forbrugsgrupper %>%
  mutate(
    Kvartal = paste0(
      substr(TID, 1, 4), "K", 
      (as.numeric(substr(TID, 6, 7)) - 1) %/% 3 + 1
    )
  )

# Initialiser en tom liste til at gemme summaries
regressioner_DST <- list()

# Flet datasæt baseret på Kvartal
regression_data_combined <- regression_data %>%
  inner_join(forbrugsgrupper, by = "Kvartal")

# Ryd op i kolonnenavne (fjern mellemrum og specielle tegn)
colnames(regression_data_combined) <- make.names(colnames(regression_data_combined))

# Opdater listen over forbrugsgrupper med de rensede kolonnenavne
forbrugsgrupper <- colnames(regression_data_combined)[4:18]  # De afhængige variabler (forbrugsgrupper)


# Løkke over forbrugsgrupperne
for (gruppe in forbrugsgrupper) {
  # Formuler modellen som y ~ x
  model <- lm(as.formula(paste0("`", gruppe, "` ~ Forbrugertillidsindikatoren")), 
              data = regression_data_combined)
  
  # Gem summary for modellen
  regressioner_DST[[gruppe]] <- summary(model)
}

# Initialiser en tom liste til DI regression summaries
regressioner_DI <- list()

# Løkke over forbrugsgrupperne
for (gruppe in forbrugsgrupper) {
  # Formuler modellen som y ~ x
  model <- lm(as.formula(paste0("`", gruppe, "` ~ DI_Indikator")), 
              data = regression_data_combined)
  
  # Gem summary for modellen
  regressioner_DI[[gruppe]] <- summary(model)
}

# forbrugsgrupper
# Fødevarer mv.

summary(regressioner_DST[1])
summary(regressioner_DST[[1]])
regressioner_DST[[1]]$coefficients
summary(regressioner_DST[[1]]$residuals)
regressioner_DST[[1]]$r.squared

# Drikkevarer og tobak
# summary(regressioner_DST[[2]])

