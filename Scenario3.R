# Scenario 3
# Vis 1
setwd("C:/Users/Youness/Desktop/R project/stage/scenario/etude 1/strategie1")
used_surface_s2 <- read_excel("surfaces_used.xlsx")
plot_management_s2 <- read_excel("plot_management.xlsx", col_names = TRUE )
biotech_s2 <- read_excel("biotech_parameters.xlsx", col_names = FALSE )
colnames(biotech_s2) <- c("parameters", "value")
data2_s2 <- filter(biotech_s2, parameters %in% c('nb_female', 
                                                 'nb_young',
                                                 'nb_male', 'nb_birth'))
# Vis 2

pp_s2 <- as.integer(rowSums(used_surface_s2[1,6:10]))
pt_s2 <- as.integer(rowSums(used_surface_s2[2,6:10]))
a_s2 <- c( pt_s2, pp_s2)
v_s2 <- c("pt", "pp")

rendement_s2 <- data.frame(type = v_s2, valeur = a_s2)
plot_management_s2[is.na(plot_management_s2)] = 0
a_s2 <- select(plot_management_s2, taille, id_mod_expl_comp)
plot_man_s2 <- as.data.frame(by(a_s2$taille, list(a_s2$id_mod_expl_comp), sum))
plot_man_s2 <- cbind(rownames(plot_man_s2), plot_man_s2)
colnames(plot_man_s2 ) <- c("Surface", "taille")

# Vis 3
stadephysio_s2 <- filter(biotech_s2, parameters %in% c('lamb_require', 
                                                       'fattening_require',
                                                       'lamb_ewe_s1_require',
                                                       'lamb_ewe_s2_require',
                                                       'empty_require',
                                                       'end_gest_require',
                                                       'beg_lact_require',
                                                       'male_require'
))

#Paramètres de sortie
Outputs_requirements_s2 <- read_excel("Outputs_requirements.xlsx", col_names = TRUE )
Feed_Available_s2 <- read_excel("Feed_Available.xlsx", col_names = TRUE )

Forage_ingest_s2 <- calculate_mean_by_cycle(Outputs_requirements_s2,  "Forage ingested")
Grain_ingest_s2 <- calculate_mean_by_cycle(Outputs_requirements_s2,  "Grain ingested")
Grassland_ingest_s2 <- calculate_mean_by_cycle(Outputs_requirements_s2,  "Grassland ingested") 
Rangeland_ingest_s2 <- calculate_mean_by_cycle(Outputs_requirements_s2,  "Rangeland ingested") 
cycle_s2 <- calculate_mean_by_cycle(Outputs_requirements_s2,  "cycle") 


Forage_available_s2 <- calculate_mean_by_cycle(Feed_Available_s2,  "Forage_available") 
Grain_available_s2 <- calculate_mean_by_cycle(Feed_Available_s2,  "Grain_Available") 
Grassland_available_s2 <- calculate_mean_by_cycle(Feed_Available_s2,  "Grasslands_Available") 
Rangeland_available_s2 <- calculate_mean_by_cycle(Feed_Available_s2,  "Rangelands_Available") 

data_s2 <- cbind(Forage_ingest_s2, Grain_ingest_s2, Grassland_ingest_s2, Rangeland_ingest_s2,
                 Forage_available_s2, Grain_available_s2, Grassland_available_s2, Rangeland_available_s2, cycle_s2 )

colnames(data_s2) <- c(
  "Forage_ingest",
  "Grain_ingest",
  "Grassland_ingest",
  "Rangeland_ingest",
  "Forage_available",
  "Grain_available",
  "Grassland_available",
  "Rangeland_available",
  "cycle"
)


# Vis a
dates_s2 <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
dates_formattees_s2<- format(dates_s2, "%d %b")
Date_s2 <- data.frame(Date = dates_formattees_s2)
Herd_Requirement_mean_s2 <- as.data.frame(by(Outputs_requirements_s2$'Herd Requirement', list(Outputs_requirements_s2$cycle), mean)) 
Herd_Requirement_min_s2<- as.data.frame(by(Outputs_requirements_s2$'Herd Requirement', list(Outputs_requirements_s2$cycle), min)) 
Herd_Requirement_max_s2 <- as.data.frame(by(Outputs_requirements_s2$'Herd Requirement', list(Outputs_requirements_s2$cycle), max)) 
data_radio_s2 <- cbind(dd_s2 = a <- seq(1, 366, by =1 ) , Date_s2 , Herd_Requirement_mean_s2, Herd_Requirement_min_s2, Herd_Requirement_max_s2 )
colnames(data_radio_s2) <- c("dd" , "Date", "mean", "min", "max")


#Vis b
Rangeland_ingested_mean_s2 <- as.data.frame(by(Outputs_requirements_s2$'Rangeland ingested', list(Outputs_requirements_s2$cycle), mean))
Forage_ingested_mean_s2 <- as.data.frame(by(Outputs_requirements_s2$'Forage ingested', list(Outputs_requirements_s2$cycle), mean))
Grassland_ingested_mean_s2 <- as.data.frame(by(Outputs_requirements_s2$'Grassland ingested', list(Outputs_requirements_s2$cycle), mean))
data3_radio_s2 <- cbind(Date_s2, Rangeland_ingested_mean_s2, Forage_ingested_mean_s2, Grassland_ingested_mean_s2)
colnames(data3_radio_s2) <- c("Date", "Rangeland_ingested", "Forage_ingested", "Grassland_ingested")
data3_radio_s2 <- gather(data3_radio_s1, key="year", value="points", 2:4)

# Indicateur de performance
# Vis c
# Pour calculer la quantité de Grain consommé 
data_grain_s2 <- sum(data_s2$Grain_ingest)
farm_prod_s2 <-  read_excel("Farm_Productions.xlsx", col_names = TRUE )
requirement_herd_s2 <- read_excel("Herd_Requirements.xlsx", col_names = TRUE )
female_s2 <- calculate_mean_by_cycle(requirement_herd_s2,  "Herd size Female")
colnames(female_s2) <- c("herd_size_female")
female_sum_s2 <- sum(female_s2$herd_size_female)
female_sum2_s2 <- round(female_sum_s2/366)
grain_ingest_par_fememlle_s2 <- data_grain_s2/female_sum2_s2

# Pour calculer la quantité de forrage consommé 
data_forrage_s2 <- sum(data_s2$Forage_ingest)
forage_ingest_par_fememlle_s2 <- data_forrage_s2/female_sum2_s2


#Calcul du taux de paturage 
data_grass_s2 <- sum(data_s2$Grassland_ingest)
data_rangela_s2 <- sum(data_s2$Rangeland_ingest )
quantite_ms_s2 <- data_grass_s2 + data_rangela_s2
quantite_ms_totale_s2 <- quantite_ms_s2 + data_forrage_s2

taux_paturage_s2 <- quantite_ms_s2/quantite_ms_totale_s2
# Données pour représenter le produit et le consommé 
grain_produced_s2 <- mean(farm_prod_s2$`Grain produced`)
forage_produced_s2 <- mean(farm_prod_s2$`Forage produced`)
grass_produced_s2 <- 0

a1_s2 <- c(grain_produced_s2, forage_produced_s2, grass_produced_s2, 
           quantite_ms_s2, data_forrage_s2, data_grain_s2)
a2_s2 <- c("Grain", "Forage", "Grass", "Grass", "Forage", "Grain")
a3_s2<- c("Produit" , "Produit" , "Produit", "Consommé", "Consommé", "Consommé")
data_ressource_s2 <- data.frame(Nature = a3_s2, type = a2_s2, Qte = a1_s2)

# Autonomie fourragère 
auto_four_s2 <- forage_produced_s2/data_forrage_s2
# •	Autonomie en concentré 
auto_concen_s2 <- grain_produced_s2/quantite_ms_s2

