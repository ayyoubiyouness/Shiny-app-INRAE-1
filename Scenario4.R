# Scenario 3
# Vis 1
setwd("C:/Users/Youness/Desktop/R project/stage/scenario/etude 1/strategie2")
used_surface_s3 <- read_excel("surfaces_used.xlsx")
plot_management_s3 <- read_excel("plot_management.xlsx", col_names = TRUE )
biotech_s3 <- read_excel("biotech_parameters.xlsx", col_names = FALSE )
colnames(biotech_s3) <- c("parameters", "value")
data2_s3 <- filter(biotech_s3, parameters %in% c('nb_female', 
                                                 'nb_young',
                                                 'nb_male', 'nb_birth'))
# Vis 2

pp_s3 <- as.integer(rowSums(used_surface_s3[1,6:10]))
pt_s3 <- as.integer(rowSums(used_surface_s3[2,6:10]))
a_s3 <- c( pt_s3, pp_s3)
v_s3 <- c("pt", "pp")

rendement_s3 <- data.frame(type = v_s3, valeur = a_s3)
plot_management_s3[is.na(plot_management_s3)] = 0
a_s3 <- select(plot_management_s3, taille, id_mod_expl_comp)
plot_man_s3 <- as.data.frame(by(a_s3$taille, list(a_s3$id_mod_expl_comp), sum))
plot_man_s3 <- cbind(rownames(plot_man_s3), plot_man_s3)
colnames(plot_man_s3 ) <- c("Surface", "taille")

# Vis 3
stadephysio_s3 <- filter(biotech_s3, parameters %in% c('lamb_require', 
                                                       'fattening_require',
                                                       'lamb_ewe_s1_require',
                                                       'lamb_ewe_s2_require',
                                                       'empty_require',
                                                       'end_gest_require',
                                                       'beg_lact_require',
                                                       'male_require'
))

#Paramètres de sortie
Outputs_requirements_s3 <- read_excel("Outputs_requirements.xlsx", col_names = TRUE )
Feed_Available_s3 <- read_excel("Feed_Available.xlsx", col_names = TRUE )

Forage_ingest_s3 <- calculate_mean_by_cycle(Outputs_requirements_s3,  "Forage ingested")
Grain_ingest_s3 <- calculate_mean_by_cycle(Outputs_requirements_s3,  "Grain ingested")
Grassland_ingest_s3 <- calculate_mean_by_cycle(Outputs_requirements_s3,  "Grassland ingested") 
Rangeland_ingest_s3 <- calculate_mean_by_cycle(Outputs_requirements_s3,  "Rangeland ingested") 
cycle_s3 <- calculate_mean_by_cycle(Outputs_requirements_s3,  "cycle") 


Forage_available_s3 <- calculate_mean_by_cycle(Feed_Available_s3,  "Forage_available") 
Grain_available_s3 <- calculate_mean_by_cycle(Feed_Available_s3,  "Grain_Available") 
Grassland_available_s3 <- calculate_mean_by_cycle(Feed_Available_s3,  "Grasslands_Available") 
Rangeland_available_s3 <- calculate_mean_by_cycle(Feed_Available_s3,  "Rangelands_Available") 

data_s3 <- cbind(Forage_ingest_s3, Grain_ingest_s3, Grassland_ingest_s3, Rangeland_ingest_s3,
                 Forage_available_s3, Grain_available_s3, Grassland_available_s3, Rangeland_available_s3, cycle_s3 )

colnames(data_s3) <- c(
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
dates_s3 <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
dates_formattees_s3<- format(dates_s3, "%d %b")
Date_s3 <- data.frame(Date = dates_formattees_s3)
Herd_Requirement_mean_s3 <- as.data.frame(by(Outputs_requirements_s3$'Herd Requirement', list(Outputs_requirements_s3$cycle), mean)) 
Herd_Requirement_min_s3<- as.data.frame(by(Outputs_requirements_s3$'Herd Requirement', list(Outputs_requirements_s3$cycle), min)) 
Herd_Requirement_max_s3 <- as.data.frame(by(Outputs_requirements_s3$'Herd Requirement', list(Outputs_requirements_s3$cycle), max)) 
data_radio_s3 <- cbind(dd_s3 = a <- seq(1, 366, by =1 ) , Date_s3 , Herd_Requirement_mean_s3, Herd_Requirement_min_s3, Herd_Requirement_max_s3 )
colnames(data_radio_s3) <- c("dd" , "Date", "mean", "min", "max")


#Vis b
Rangeland_ingested_mean_s3 <- as.data.frame(by(Outputs_requirements_s3$'Rangeland ingested', list(Outputs_requirements_s3$cycle), mean))
Forage_ingested_mean_s3 <- as.data.frame(by(Outputs_requirements_s3$'Forage ingested', list(Outputs_requirements_s3$cycle), mean))
Grassland_ingested_mean_s3 <- as.data.frame(by(Outputs_requirements_s3$'Grassland ingested', list(Outputs_requirements_s3$cycle), mean))
data3_radio_s3 <- cbind(Date_s3, Rangeland_ingested_mean_s3, Forage_ingested_mean_s3, Grassland_ingested_mean_s3)
colnames(data3_radio_s3) <- c("Date", "Rangeland_ingested", "Forage_ingested", "Grassland_ingested")
data3_radio_s3 <- gather(data3_radio_s1, key="year", value="points", 2:4)

# Indicateur de performance
# Vis c
# Pour calculer la quantité de Grain consommé 
data_grain_s3 <- sum(data_s3$Grain_ingest)
farm_prod_s3 <-  read_excel("Farm_Productions.xlsx", col_names = TRUE )
requirement_herd_s3 <- read_excel("Herd_Requirements.xlsx", col_names = TRUE )
female_s3 <- calculate_mean_by_cycle(requirement_herd_s3,  "Herd size Female")
colnames(female_s3) <- c("herd_size_female")
female_sum_s3 <- sum(female_s3$herd_size_female)
female_sum2_s3 <- round(female_sum_s3/366)
grain_ingest_par_fememlle_s3 <- data_grain_s3/female_sum2_s3

# Pour calculer la quantité de forrage consommé 
data_forrage_s3 <- sum(data_s3$Forage_ingest)
forage_ingest_par_fememlle_s3 <- data_forrage_s3/female_sum2_s3


#Calcul du taux de paturage 
data_grass_s3 <- sum(data_s3$Grassland_ingest)
data_rangela_s3 <- sum(data_s3$Rangeland_ingest )
quantite_ms_s3 <- data_grass_s3 + data_rangela_s3
quantite_ms_totale_s3 <- quantite_ms_s3 + data_forrage_s3

taux_paturage_s3 <- quantite_ms_s3/quantite_ms_totale_s3
# Données pour représenter le produit et le consommé 
grain_produced_s3 <- mean(farm_prod_s3$`Grain produced`)
forage_produced_s3 <- mean(farm_prod_s3$`Forage produced`)
grass_produced_s3 <- 0

a1_s3 <- c(grain_produced_s3, forage_produced_s3, grass_produced_s3, 
           quantite_ms_s3, data_forrage_s3, data_grain_s3)
a2_s3 <- c("Grain", "Forage", "Grass", "Grass", "Forage", "Grain")
a3_s3<- c("Produit" , "Produit" , "Produit", "Consommé", "Consommé", "Consommé")
data_ressource_s3 <- data.frame(Nature = a3_s3, type = a2_s3, Qte = a1_s3)

# Autonomie fourragère 
auto_four_s3 <- forage_produced_s3/data_forrage_s3
# •	Autonomie en concentré 
auto_concen_s3 <- grain_produced_s3/quantite_ms_s3

