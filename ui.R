ui <- dashboardPage(
  
  dashboardHeader(title = "Projet Adapt-Herd",
                  titleWidth = 250
                  
  ),
  dashboardSidebar(
    width = 280, 
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Etude de cas 1",  icon = tags$i(class = "fa-solid fa-building-wheat"),
               menuSubItem("Situation Initiale", tabName = "subitem11"),
               menuSubItem("Situation sous changement climatique", tabName = "subitem12"),
               menuSubItem("Stratégie d'adaptation 1", tabName = "subitem13"),
               menuSubItem("Stratégie d'adaptation 2", tabName = "subitem14"),
               menuSubItem("Stratégie d'adaptation 3", tabName = "subitem15")
      ),
      menuItem("Etude de cas 2", icon = tags$i(class = "fa-solid fa-building-wheat"),
               menuSubItem("Situation Initiale", tabName = "subitem21"),
               menuSubItem("Situation sous changement climatique", tabName = "subitem22"),
               menuSubItem("Stratégie d'adaptation 1", tabName = "subitem23"),
               menuSubItem("Stratégie d'adaptation 2", tabName = "subitem24"),
               menuSubItem("Stratégie d'adaptation 3", tabName = "subitem25")
      ),
      menuItem("Etude de cas 3", icon = tags$i(class = "fa-solid fa-building-wheat"),
               menuSubItem("Situation Initiale", tabName = "subitem31"),
               menuSubItem("Situation sous changement climatique", tabName = "subitem32"),
               menuSubItem("Stratégie d'adaptation 1", tabName = "subitem33"),
               menuSubItem("Stratégie d'adaptation 2", tabName = "subitem34"),
               menuSubItem("Stratégie d'adaptation 3", tabName = "subitem35")
      )
    )
  ),
  
  dashboardBody(
    includeCSS("C:/Users/Youness/Desktop/R project/stage/shiny/withjs/www/dark_mode.css"),
    useShinyjs(), 
    tags$script(src = "https://kit.fontawesome.com/3a3a5f75cd.js"),
    tabItems(
      # ------------------------------ about
      tabItem("about", 
              h2("Interface et visualisation de données de simulation : application aux stratégies d’adaptation des systèmes d’élevage aux changements climatiques",style="text-align:center"),
              imageOutput("home_img"),
              # includeHTML("C:/Users/Youness/Desktop/R project/stage/shiny/test/R/www/index.html"),
              box(width=12,height="80px",
                  p(style="font-size:20px",strong("L'objectif"),"de cette application est de développer des prototypes de visualisations permettant de rendre compte des résultats obtenus des simulations 
et facilitant la comparaison entre eux. "),
              ),
      ),
      # ------------------------------ scénario 0
      tabItem("subitem11",
              div(id = "scenario0",
                  
                  div(class="column left-column",
                      id = "par_entree",
                      h4("Situation initiale :Paramètres d'entrées", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width=12,
                          title = " Effectif des animaux par catégorie ",
                          status = "primary",
                          
                          #height = 3,
                          radioButtons("item_choice", label = NULL, inline = TRUE, choices = c("rectangle", "parliment", "circle")),
                          
                          highchartOutput("animated", height = 240),
                          height = 450
                        )
                        
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Répartition des surfaces par type de ressources et par type d'usage",
                          status = "primary",
                          highchartOutput("ressource_surface")
                        ),
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Needs according to physiological stages",
                          status = "primary",
                          DTOutput("stade_physio"))
                        
                      )
                  ),
                  div(class = "column right-column",
                      id = "par_sortie",
                      h3("Situation initiale : Paramètres de sortie", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width = 12,
                          title = "Ressources que les animaux doivent consommer",
                          status = "primary",
                          highchartOutput('plot'),
                          dropdown(
                            awesomeRadio(
                              inputId = "radio",
                              label = "Choix de la période",
                              
                              choices = list("Par mois" = 1, "Par saison" = 2, "Par quinzine" = 3, "Toute l'année" = 4),
                              selected = 1,
                              
                              status = "warning",
                            ),
                            style = "unite", icon = icon("gear"),
                            status = "danger", width = "200px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ),
                            up = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Ressources en parcours consommées par les animaux",
                          status = "primary",
                          highchartOutput("ressour_consom")
                        )
                      ),
                      
                      br(),
                      h3("Situation initiale : Indicateur de performance", align = "center"),
                      br(),
                      
                      infoBoxOutput("forage_ingestt", width = 8),
                      
                      infoBoxOutput("grain_ingestt", width = 8),
                      
                      br(),
                      
                      box(title = "Quantité de ressource consommées et produit",
                          width=12,
                          status = "primary",
                          highchartOutput("ressour_consom_produit")
                      ),
                      
                      box(
                        width=12,
                        height = 370,
                        title = "Taux de paturage",
                        status = "primary",
                        highchartOutput("gauge1")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie fourragère ",
                        status = "primary",
                        highchartOutput("gauge2")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie en concentré",
                        status = "primary",
                        highchartOutput("gauge3")
                      )
                      
                  )
                  
                  
              )
      ),
      # ------------------------------ scénario 1
      tabItem("subitem12",
              div(id = "scenario1",
                  
                  div(class="column left-column",
                      id = "par_entree_s1",
                      h3("Situation sous changement climatique : Paramètres d'entrées", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width=12,
                          title = " Effectif des animaux par catégorie ",
                          status = "primary",
                          
                          #height = 3,
                          radioButtons("item_choice_s1", label = NULL, inline = TRUE, choices = c("rectangle", "parliment", "circle")),
                          
                          highchartOutput("animated_s1", height = 240),
                          height = 450
                        )
                        
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Répartition des surfaces par type de ressources et par type d'usage",
                          status = "primary",
                          highchartOutput("ressource_surface_s1")
                        ),
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Besoin en fonction des stades physiologiques ",
                          status = "primary",
                          highchartOutput("stade_physio_s1"))
                        
                      )
                  ),
                  div(class = "column right-column",
                      id = "par_sortie_s1",
                      h3("Situation sous changement climatique : Paramètres de sortie", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width = 12,
                          title = "Ressources que les animaux doivent consommer",
                          status = "primary",
                          highchartOutput('plot_s1'),
                          dropdown(
                            awesomeRadio(
                              inputId = "radio",
                              label = "Choix de la période", 
                              
                              choices = list("Par mois" = 1, "Par saison" = 2, "Par quinzine" = 3, "Toute l'année" = 4), 
                              selected = 1,
                              
                              status = "warning",
                            ),
                            style = "unite", icon = icon("gear"),
                            status = "danger", width = "200px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ),
                            up = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Ressources en parcours consommées par les animaux",
                          status = "primary",
                          highchartOutput("ressour_consom_s1")
                        )
                      ),
                      
                      br(),
                      h3("Situation sous changement climatique : Indicateur de performance", align = "center"),
                      br(),
                      
                      infoBoxOutput("forage_ingestt_s1", width = 8),
                      
                      infoBoxOutput("grain_ingestt_s1", width = 8),
                      
                      br(),
                      
                      box(title = "Quantité de ressource consommées et produit",
                          width=12,
                          status = "primary",
                          highchartOutput("ressour_consom_produit_s1")
                      ),
                      
                      box(
                        width=12,
                        height = 370,
                        title = "Taux de paturage",
                        status = "primary",
                        highchartOutput("gauge1_s1")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie fourragère ",
                        status = "primary",
                        highchartOutput("gauge2_s1")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie en concentré",
                        status = "primary",
                        highchartOutput("gauge3_s1")
                      )
                  )
              )
      ),
      # ------------------------------ scénario 2
      tabItem("subitem13",
              div(id = "scenario2",
                  
                  div(class="column left-column",
                      id = "par_entree_s2",
                      h3("Situation sous changement climatique : Paramètres d'entrées", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width=12,
                          title = " Effectif des animaux par catégorie ",
                          status = "primary",
                          
                          #height = 3,
                          radioButtons("item_choice_s2", label = NULL, inline = TRUE, choices = c("rectangle", "parliment", "circle")),
                          
                          highchartOutput("animated_s2", height = 240),
                          height = 450
                        )
                        
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Répartition des surfaces par type de ressources et par type d'usage",
                          status = "primary",
                          highchartOutput("ressource_surface_s2")
                        ),
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Besoin en fonction des stades physiologiques ",
                          status = "primary",
                          highchartOutput("stade_physio_s2"))
                        
                      )
                  ),
                  div(class = "column right-column",
                      id = "par_sortie_s2",
                      h3("Situation sous changement climatique : Paramètres de sortie", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width = 12,
                          title = "Ressources que les animaux doivent consommer",
                          status = "primary",
                          highchartOutput('plot_s2'),
                          dropdown(
                            awesomeRadio(
                              inputId = "radio_s2",
                              label = "Choix de la période", 
                              
                              choices = list("Par mois" = 1, "Par saison" = 2, "Par quinzine" = 3, "Toute l'année" = 4), 
                              selected = 1,
                              
                              status = "warning",
                            ),
                            style = "unite", icon = icon("gear"),
                            status = "danger", width = "200px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ),
                            up = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Ressources en parcours consommées par les animaux",
                          status = "primary",
                          highchartOutput("ressour_consom_s2")
                        )
                      ),
                      
                      br(),
                      h3("Situation sous changement climatique : Indicateur de performance", align = "center"),
                      br(),
                      
                      infoBoxOutput("forage_ingestt_s2", width = 8),
                      
                      infoBoxOutput("grain_ingestt_s2", width = 8),
                      
                      br(),
                      
                      box(title = "Quantité de ressource consommées et produit",
                          width=12,
                          status = "primary",
                          highchartOutput("ressour_consom_produit_s2")
                      ),
                      
                      box(
                        width=12,
                        height = 370,
                        title = "Taux de paturage",
                        status = "primary",
                        highchartOutput("gauge1_s2")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie fourragère ",
                        status = "primary",
                        highchartOutput("gauge2_s2")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie en concentré",
                        status = "primary",
                        highchartOutput("gauge3_s2")
                      )
                  )
              ),
      ),
      tabItem("subitem14",
              # ------------------------------ scénario 3
              div(id = "scenario3",
                  
                  div(class="column left-column",
                      id = "par_entree_s3",
                      h3("Situation sous changement climatique : Paramètres d'entrées", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width=12,
                          title = " Effectif des animaux par catégorie ",
                          status = "primary",
                          
                          #height = 3,
                          radioButtons("item_choice_s3", label = NULL, inline = TRUE, choices = c("rectangle", "parliment", "circle")),
                          
                          highchartOutput("animated_s3", height = 240),
                          height = 450
                        )
                        
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Répartition des surfaces par type de ressources et par type d'usage",
                          status = "primary",
                          highchartOutput("ressource_surface_s3")
                        ),
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Besoin en fonction des stades physiologiques ",
                          status = "primary",
                          highchartOutput("stade_physio_s3"))
                        
                      )
                  ),
                  div(class = "column right-column",
                      id = "par_sortie_s3",
                      h3("Situation sous changement climatique : Paramètres de sortie", align = "center"),
                      br(),
                      fluidRow(
                        box(
                          width = 12,
                          title = "Ressources que les animaux doivent consommer",
                          status = "primary",
                          highchartOutput('plot_s3'),
                          dropdown(
                            awesomeRadio(
                              inputId = "radio_s3",
                              label = "Choix de la période", 
                              
                              choices = list("Par mois" = 1, "Par saison" = 2, "Par quinzine" = 3, "Toute l'année" = 4), 
                              selected = 1,
                              
                              status = "warning",
                            ),
                            style = "unite", icon = icon("gear"),
                            status = "danger", width = "200px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ),
                            up = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          width=12,
                          title = "Ressources en parcours consommées par les animaux",
                          status = "primary",
                          highchartOutput("ressour_consom_s3")
                        )
                      ),
                      
                      br(),
                      h3("Situation sous changement climatique : Indicateur de performance", align = "center"),
                      br(),
                      
                      infoBoxOutput("forage_ingestt_s3", width = 8),
                      
                      infoBoxOutput("grain_ingestt_s3", width = 8),
                      
                      br(),
                      
                      box(title = "Quantité de ressource consommées et produit",
                          width=12,
                          status = "primary",
                          highchartOutput("ressour_consom_produit_s3")
                      ),
                      
                      box(
                        width=12,
                        height = 370,
                        title = "Taux de paturage",
                        status = "primary",
                        highchartOutput("gauge1_s3")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie fourragère ",
                        status = "primary",
                        highchartOutput("gauge2_s3")
                      ),
                      box(
                        width=12,
                        height = 370,
                        title = "Autonomie en concentré",
                        status = "primary",
                        highchartOutput("gauge3_s3")
                      )
                      
                  )
                  
                  
              ),
      )
              
    )
  )
)

