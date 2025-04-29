# Projeto: Mapa Interativo Brasil População Oficial
# Autor: Paulo Rebelo
# Descrição: Representação cartográfica dos estados brasileiros contendo elementos interativos na forma de pop-ups, os quais exibem informações oficiais e gráficos ilustrativos em formato reduzido, além da funcionalidade de exportação de dados para CSV.

# Bibliotecas 
library(geobr)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(plotly)
library(htmltools)

# 

# Mapa
estados <- read_state()

# População Oficial IBGE 2022
regioes_estados <- tibble::tibble(
  abbrev_state = c(
    "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
    "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
    "RS", "RO", "RR", "SC", "SP", "SE", "TO"
  ),
  region = c(
    "Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste", "Centro Oeste", "Sudeste", "Centro Oeste", "Nordeste",
    "Centro Oeste", "Centro Oeste", "Sudeste", "Norte", "Nordeste", "Sul", "Nordeste", "Nordeste", "Sudeste", "Nordeste",
    "Sul", "Norte", "Norte", "Sul", "Sudeste", "Nordeste", "Norte"
  ),
  populacao = c(
    830026,  3272391, 733759, 3807926, 14812617, 8456121, 2962995, 3973697, 6555260, 6824426,
    3526220, 2809394, 20524866, 8602865, 3766528, 11466630, 9137282, 3194253, 16054524, 3560902,
    10882979, 1815278, 636707, 7345692, 44606336, 2298696, 1607363
  )
)

# Informações
estados <- estados %>%
  left_join(regioes_estados, by = "abbrev_state")

# Cores Estados
set.seed(123)
cores_estados <- colorRampPalette(colors())(nrow(estados))
estados$cor_estado <- cores_estados

# Mini Gráfico
mini_grafico <- function(nome_estado, populacao) {
  paste0(
    "<div><strong>População Visualizada:</strong></div>",
    "<div style='height:100px;width:200px;'>",
    "<img src='https://quickchart.io/chart?c={type:%22bar%22,data:{labels:[%22", nome_estado, "%22],datasets:[{data:[", populacao, "]}]}}' style='width:100%;height:100%;'>",
    "</div>"
  )
}

# Vetor Mini Gráfico
graficos_html <- sapply(1:nrow(estados), function(i) {
  mini_grafico(estados$name_state[i], estados$populacao[i])
})

# Gráfico Dataframe
estados$grafico_popup <- graficos_html

# Mapa Interativo
leaflet_map <- leaflet(estados) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~cor_estado,
    weight = 1,
    opacity = 1,
    color = "black",
    fillOpacity = 0.7,
    group = ~region,
    label = ~paste0(name_state),
    popup = ~paste0(
      "<strong>Estado:</strong> ", name_state, "<br>",
      "<strong>Sigla:</strong> ", abbrev_state, "<br>",
      "<strong>Região:</strong> ", region, "<br>",
      "<strong>População Estimada (IBGE 2022):</strong> ", format(populacao, big.mark = "."), "<br><br>",
      grafico_popup
    ),
    highlight = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLayersControl(
    overlayGroups = c("Norte", "Nordeste", "Centro Oeste", "Sudeste", "Sul"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Exibir Mapa
leaflet_map

# Mapa HTML
saveWidget(leaflet_map, "mapa_brasil_interativo.html", selfcontained = TRUE)

# CSV
dados_exportar <- estados %>%
  select(name_state, abbrev_state, region, populacao)

write.csv(dados_exportar, "dados_estados_brasil.csv", row.names = FALSE)

