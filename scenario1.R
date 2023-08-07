

# Scenario 1 
# Vis 1
setwd("C:/Users/Youness/Desktop/R project/stage/scenario/etude 1/situ_initial")
used_surface <- read_excel("surfaces_used.xlsx")
plot_management <- read_excel("plot_management.xlsx", col_names = TRUE )
biotech <- read_excel("biotech_parameters.xlsx", col_names = FALSE )
colnames(biotech) <- c("parameters", "value")
data2 <- filter(biotech, parameters %in% c('nb_female', 
                                           'nb_young',
                                           'nb_male', 'nb_birth'))
# Vis 2

pp <- as.integer(rowSums(used_surface[1,6:10]))
pt <- as.integer(rowSums(used_surface[2,6:10]))
a <- c( pt, pp)
v <- c("pt", "pp")

rendement <- data.frame(type = v, valeur = a)
plot_management[is.na(plot_management)] = 0
a <- select(plot_management, taille, id_mod_expl_comp)
plot_man <- as.data.frame(by(a$taille, list(a$id_mod_expl_comp), sum))
plot_man <- cbind(rownames(plot_man), plot_man)
colnames(plot_man ) <- c("Surface", "taille")

# Vis 3
stadephysio <- filter(biotech, parameters %in% c('lamb_require', 
                                                 'fattening_require',
                                                 'lamb_ewe_s1_require',
                                                 'lamb_ewe_s2_require',
                                                 'empty_require',
                                                 'end_gest_require',
                                                 'beg_lact_require',
                                                 'male_require'
))
#Paramètres de sortie
Outputs_requirements <- read_excel("Outputs_requirements.xlsx", col_names = TRUE )
Feed_Available <- read_excel("Feed_Available.xlsx", col_names = TRUE )

Forage_ingest <- calculate_mean_by_cycle(Outputs_requirements,  "Forage ingested")
Grain_ingest <- calculate_mean_by_cycle(Outputs_requirements,  "Grain ingested")
Grassland_ingest <- calculate_mean_by_cycle(Outputs_requirements,  "Grassland ingested") 
Rangeland_ingest <- calculate_mean_by_cycle(Outputs_requirements,  "Rangeland ingested") 
cycle <- calculate_mean_by_cycle(Outputs_requirements,  "cycle") 


Forage_available <- calculate_mean_by_cycle(Feed_Available,  "Forage_available") 
Grain_available <- calculate_mean_by_cycle(Feed_Available,  "Grain_Available") 
Grassland_available <- calculate_mean_by_cycle(Feed_Available,  "Grasslands_Available") 
Rangeland_available <- calculate_mean_by_cycle(Feed_Available,  "Rangelands_Available") 

data <- cbind(Forage_ingest, Grain_ingest, Grassland_ingest, Rangeland_ingest,
              Forage_available, Grain_available, Grassland_available, Rangeland_available, cycle )

colnames(data) <- c(
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
dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
dates_formattees <- format(dates, "%d %b")
Date <- data.frame(Date = dates_formattees)
Herd_Requirement_mean <- as.data.frame(by(Outputs_requirements$'Herd Requirement', list(Outputs_requirements$cycle), mean)) 
Herd_Requirement_min<- as.data.frame(by(Outputs_requirements$'Herd Requirement', list(Outputs_requirements$cycle), min)) 
Herd_Requirement_max <- as.data.frame(by(Outputs_requirements$'Herd Requirement', list(Outputs_requirements$cycle), max)) 
data_radio <- cbind(dd = a <- seq(1, 366, by =1 ) , Date , Herd_Requirement_mean, Herd_Requirement_min, Herd_Requirement_max )
colnames(data_radio) <- c("dd" , "Date", "mean", "min", "max")

#Vis b
Rangeland_ingested_mean <- as.data.frame(by(Outputs_requirements$'Rangeland ingested', list(Outputs_requirements$cycle), mean))
Forage_ingested_mean <- as.data.frame(by(Outputs_requirements$'Forage ingested', list(Outputs_requirements$cycle), mean))
Grassland_ingested_mean <- as.data.frame(by(Outputs_requirements$'Grassland ingested', list(Outputs_requirements$cycle), mean))
data3_radio <- cbind(Date, Rangeland_ingested_mean, Forage_ingested_mean, Grassland_ingested_mean)
colnames(data3_radio) <- c("Date", "Rangeland_ingested", "Forage_ingested", "Grassland_ingested")
data3_radio <- gather(data3_radio, key="year", value="points", 2:4)


# Indicateur de performance
# Vis c
# Pour calculer la quantité de Grain consommé 
data_grain <- sum(data$Grain_ingest)
farm_prod <-  read_excel("Farm_Productions.xlsx", col_names = TRUE )
requirement_herd <- read_excel("Herd_Requirements.xlsx", col_names = TRUE )
female <- calculate_mean_by_cycle(requirement_herd,  "Herd size Female")
colnames(female) <- c("herd_size_female")
female_sum <- sum(female$herd_size_female)
female_sum2 <- round(female_sum/366)
grain_ingest_par_fememlle <- data_grain/female_sum2

# Pour calculer la quantité de forrage consommé 
data_forrage <- sum(data$Forage_ingest)
forage_ingest_par_fememlle <- data_forrage/female_sum2




taux_paturage <- quantite_ms/quantite_ms_totale
# Données pour représenter le produit et le consommé 
grain_produced <- mean(farm_prod$`Grain produced`)
forage_produced <- mean(farm_prod$`Forage produced`)
grass_produced <- 0

a1 <- c(grain_produced, forage_produced, grass_produced, 
        quantite_ms, data_forrage, data_grain )
a2 <- c("Grain", "Forage", "Grass", "Grass", "Forage", "Grain")
a3 <- c("Produit" , "Produit" , "Produit", "Consommé", "Consommé", "Consommé")
data_ressource <- data.frame(Nature = a3, type = a2, Qte = a1)


#Fonction pour déterminer la valeur par simulation
somme_par_scenario <- function(nbr, parameter) {
  data_par <- data.frame()
  valeur <- 0
  for (i in seq(nbr , nrow(Outputs_requirements), by = 10)) {
    jj <- Outputs_requirements[[parameter]][i]
    data_par <- rbind(data_par, jj)
  }
  colnames(data_par) <- c(parameter)
  valeur <- sum(data_par)
  
  return(valeur)
}
#Calcul du taux de paturage, de l'autonomie fourragère et de l'autonomie en concentré 
taux_patu<- data.frame()
autono_four <- data.frame()
autono_concen <- data.frame()
for (i in 1:10) {
  
  taux1 <- ( somme_par_scenario(i, "Grassland ingested") + somme_par_scenario(i, "Rangeland ingested") ) /( somme_par_scenario(i, "Grassland ingested") + somme_par_scenario(i, "Rangeland ingested") + somme_par_scenario(i,  "Forage ingested") )
  taux2 <- ( farm_prod$`Forage produced`[i] + somme_par_scenario(i, "Grassland ingested") + somme_par_scenario(i, "Rangeland ingested")  )/( somme_par_scenario(i, "Grassland ingested") + somme_par_scenario(i, "Rangeland ingested") + somme_par_scenario(i,  "Forage ingested") )
  taux3 <- ( farm_prod$`Grain produced`[1] )/(somme_par_scenario(1, "Grain ingested"))
  taux_patu <- rbind(taux_patu, taux1)
  autono_four <- rbind(autono_four, taux2)
  autono_concen <- rbind(autono_concen, taux3)
  
}
colnames(taux_patu) <- c("taux")
colnames(autono_four) <- c("taux")
colnames(autono_concen) <- c("taux")
taux_paturage <- mean(taux_patu$taux)
auto_four <- mean(autono_four$taux)
auto_concen <- mean(autono_concen$taux)





