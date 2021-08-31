#pacotes

library(shiny)
library(leaflet)
library(rgdal)
library(RColorBrewer)


#Adicionar dados e shapefile
regiao <- readOGR("~/Regioes de saude.shp", verbose = FALSE)

dados <- read.table("~/dados.csv",
                    header = TRUE,
                    sep = ";")

dados <- dados[order(match(dados$GERES, regiao$GERES2)),]

names(dados)
#rótulos
labase <- sprintf("<strong>%s</strong><br/>%s ", regiao$GERES2, regiao$REGIAO3) %>% lapply(htmltools::HTML)


    # Choices for drop-downs
    vars <- c("Unidades de Saúde da Família (USF)" = "dados$USF",
               "Equipes NASF" = "dados$NASF",
               "Academias da Saúde" = "dados$ACAD",
               "Equipe de Saúde no Domicílio (SAD)" = "dados$SAD")
    
    vars2 <- c( "UPA" = "dados$UPA",
                "UPAE" = "dados$UPAE",
                "Centros de Especialidades"= "dados$ESPECIAL",
                "Policlínicas" = "dados$POLICLIN",
                "Hospitais Regionais"= "dados$HOSP_REG",
                "Hospitais Gerais"= "dados$HOSP_GERAIS",
                "Hospitais OSS" = "dados$HOSP_OSS",
                "Hospitais de Pequeno Porte" = "dados$HPP",
                "Hospitais Contratualizados" = "dados$HOSP_CONTRAT",
                "Unidades Mistas" = "dados$UNID_MISTAS")
    #Cores
    pal <- colorFactor("Set3",
                        dados$ï..REGIAO,
                        na.color = "#808080",
                        alpha = FALSE,
                        reverse = FALSE)
    
   #Shiny
    
    ui <- fluidPage(
        
        navbarPage("PERNAMBUCO", id="nav",
                   
                   tabPanel("REGIÕES DE SAÚDE",
                            div(class="outer",
                                tags$head(),
                                tags$h1("Regiões de Saúde"),
                                tags$h4("Aqui são apresentadas as regiões de saúde de Pernambuco"),
                                tags$h5("Mova o ponteiro do mouse sobre elas para ver detalhes"),
                                leafletOutput("map1"))),
                   tabPanel("ATENÇÃO PRIMÁRIA",
                            div(class="outer",
                                tags$head(),
                                leafletOutput("map2", width="100%", height="100%"),
                                sidebarPanel(id = "controls", class = "panel panel-default",
                                              fixed = TRUE,
                                              draggable = TRUE, top = 60,
                                              left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              h4("  Pontos de atenção primária  "),
                                              selectInput("esc", "Escolha:", vars),
                                      conditionalPanel("input.esc == 'dados'")))),
                   tabPanel("ATENÇÃO SECUNDÁRIA",
                            div(class="outer",
                                tags$head(),
                                leafletOutput("map3", width="100%", height="100%"),
                                sidebarPanel(id = "controls", class = "panel panel-default",
                                              fixed = TRUE, draggable = TRUE, top = 60,
                                              left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              h4("Pontos de atenção secundária"),
                                              selectInput("esc2", "Escolha:", vars2),
                                              conditionalPanel("input.esc2 == 'dados'"))))))


# Define server logic required to draw a map
server <- function(input, output) {
    
     output$map1 <-renderLeaflet({
        leaflet() %>%
            setView(-37.8, -8.4, 7) %>%
            addProviderTiles(providers$OpenStreetMap) %>% 
            addPolygons(data = regiao,
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1,
                        color = ~pal(dados$ï..REGIAO),
                        dashArray = "",
                        fillOpacity = 0.8,
                        fillColor = ~pal(dados$ï..REGIAO),
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = labase,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "14px",
                            direction = "auto"))%>%
            addScaleBar(position = "topright", options = scaleBarOptions(maxWidth = 150, metric = TRUE,
                                                                         imperial = FALSE , updateWhenIdle = TRUE))
            
        
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
