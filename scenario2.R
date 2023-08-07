# Scenario 2
# Vis 1
setwd("C:/Users/Youness/Desktop/R project/stage/scenario/etude 1/situ_sousCC")
used_surface_s1 <- read_excel("surfaces_used.xlsx")
plot_management_s1 <- read_excel("plot_management.xlsx", col_names = TRUE )
biotech_s1 <- read_excel("biotech_parameters.xlsx", col_names = FALSE )
colnames(biotech_s1) <- c("parameters", "value")
data2_s1 <- filter(biotech_s1, parameters %in% c('nb_female', 
                                                 'nb_young',
                                                 'nb_male', 'nb_birth'))
# Vis 2

pp_s1 <- as.integer(rowSums(used_surface_s1[1,6:10]))
pt_s1 <- as.integer(rowSums(used_surface_s1[2,6:10]))
a_s1 <- c( pt_s1, pp_s1)
v_s1 <- c("pt", "pp")

rendement_s1 <- data.frame(type = v_s1, valeur = a_s1)
plot_management_s1[is.na(plot_management_s1)] = 0
a_s1 <- select(plot_management_s1, taille, id_mod_expl_comp)
plot_man_s1 <- as.data.frame(by(a_s1$taille, list(a_s1$id_mod_expl_comp), sum))
plot_man_s1 <- cbind(rownames(plot_man_s1), plot_man_s1)
colnames(plot_man_s1 ) <- c("Surface", "taille")

# Vis 3
stadephysio_s1 <- filter(biotech_s1, parameters %in% c('lamb_require', 
                                                       'fattening_require',
                                                       'lamb_ewe_s1_require',
                                                       'lamb_ewe_s2_require',
                                                       'empty_require',
                                                       'end_gest_require',
                                                       'beg_lact_require',
                                                       'male_require'
))

#Paramètres de sortie
Outputs_requirements_s1 <- read_excel("Outputs_requirements.xlsx", col_names = TRUE )
Feed_Available_s1 <- read_excel("Feed_Available.xlsx", col_names = TRUE )

Forage_ingest_s1 <- calculate_mean_by_cycle(Outputs_requirements_s1,  "Forage ingested")
Grain_ingest_s1 <- calculate_mean_by_cycle(Outputs_requirements_s1,  "Grain ingested")
Grassland_ingest_s1 <- calculate_mean_by_cycle(Outputs_requirements_s1,  "Grassland ingested") 
Rangeland_ingest_s1 <- calculate_mean_by_cycle(Outputs_requirements_s1,  "Rangeland ingested") 
cycle_s1 <- calculate_mean_by_cycle(Outputs_requirements_s1,  "cycle") 


Forage_available_s1 <- calculate_mean_by_cycle(Feed_Available_s1,  "Forage_available") 
Grain_available_s1 <- calculate_mean_by_cycle(Feed_Available_s1,  "Grain_Available") 
Grassland_available_s1 <- calculate_mean_by_cycle(Feed_Available_s1,  "Grasslands_Available") 
Rangeland_available_s1 <- calculate_mean_by_cycle(Feed_Available_s1,  "Rangelands_Available") 

data_s1 <- cbind(Forage_ingest_s1, Grain_ingest_s1, Grassland_ingest_s1, Rangeland_ingest_s1,
                 Forage_available_s1, Grain_available_s1, Grassland_available_s1, Rangeland_available_s1, cycle_s1 )

colnames(data_s1) <- c(
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
dates_s1 <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
dates_formattees_s1 <- format(dates_s1, "%d %b")
Date_s1 <- data.frame(Date = dates_formattees_s1)
Herd_Requirement_mean_s1 <- as.data.frame(by(Outputs_requirements_s1$'Herd Requirement', list(Outputs_requirements_s1$cycle), mean)) 
Herd_Requirement_min_s1<- as.data.frame(by(Outputs_requirements_s1$'Herd Requirement', list(Outputs_requirements_s1$cycle), min)) 
Herd_Requirement_max_s1 <- as.data.frame(by(Outputs_requirements_s1$'Herd Requirement', list(Outputs_requirements_s1$cycle), max)) 
data_radio_s1 <- cbind(dd_s1 = a <- seq(1, 366, by =1 ) , Date_s1 , Herd_Requirement_mean_s1, Herd_Requirement_min_s1, Herd_Requirement_max_s1 )
colnames(data_radio_s1) <- c("dd" , "Date", "mean", "min", "max")


#Vis b
Rangeland_ingested_mean_s1 <- as.data.frame(by(Outputs_requirements_s1$'Rangeland ingested', list(Outputs_requirements_s1$cycle), mean))
Forage_ingested_mean_s1 <- as.data.frame(by(Outputs_requirements_s1$'Forage ingested', list(Outputs_requirements_s1$cycle), mean))
Grassland_ingested_mean_s1 <- as.data.frame(by(Outputs_requirements_s1$'Grassland ingested', list(Outputs_requirements_s1$cycle), mean))
data3_radio_s1 <- cbind(Date_s1, Rangeland_ingested_mean_s1, Forage_ingested_mean_s1, Grassland_ingested_mean_s1)
colnames(data3_radio_s1) <- c("Date", "Rangeland_ingested", "Forage_ingested", "Grassland_ingested")
data3_radio_s1 <- gather(data3_radio_s1, key="year", value="points", 2:4)

# Indicateur de performance
# Vis c
# Pour calculer la quantité de Grain consommé 
data_grain_s1 <- sum(data_s1$Grain_ingest)
farm_prod_s1 <-  read_excel("Farm_Productions.xlsx", col_names = TRUE )
requirement_herd_s1 <- read_excel("Herd_Requirements.xlsx", col_names = TRUE )
female_s1 <- calculate_mean_by_cycle(requirement_herd_s1,  "Herd size Female")
colnames(female_s1) <- c("herd_size_female")
female_sum_s1 <- sum(female_s1$herd_size_female)
female_sum2_s1 <- round(female_sum_s1/366)
grain_ingest_par_fememlle_s1 <- data_grain_s1/female_sum2_s1

# Pour calculer la quantité de forrage consommé 
data_forrage_s1 <- sum(data_s1$Forage_ingest)
forage_ingest_par_fememlle_s1 <- data_forrage_s1/female_sum2_s1


#Calcul du taux de paturage 
data_grass_s1 <- sum(data_s1$Grassland_ingest)
data_rangela_s1 <- sum(data_s1$Rangeland_ingest )
quantite_ms_s1 <- data_grass_s1 + data_rangela_s1
quantite_ms_totale_s1 <- quantite_ms_s1 + data_forrage_s1

taux_paturage_s1 <- quantite_ms_s1/quantite_ms_totale_s1
# Données pour représenter le produit et le consommé 
grain_produced_s1 <- mean(farm_prod_s1$`Grain produced`)
forage_produced_s1 <- mean(farm_prod_s1$`Forage produced`)
grass_produced_s1 <- 0

a1_s1 <- c(grain_produced_s1, forage_produced_s1, grass_produced_s1, 
           quantite_ms_s1, data_forrage_s1, data_grain_s1)
a2_s1 <- c("Grain", "Forage", "Grass", "Grass", "Forage", "Grain")
a3_s1<- c("Produit" , "Produit" , "Produit", "Consommé", "Consommé", "Consommé")
data_ressource_s1 <- data.frame(Nature = a3_s1, type = a2_s1, Qte = a1_s1)

# Autonomie fourragère 
auto_four_s1 <- forage_produced_s1/data_forrage_s1
# •	Autonomie en concentré 
auto_concen_s1 <- grain_produced_s1/quantite_ms_s1