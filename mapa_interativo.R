# Projeto: Mapa Interativo Brasil População Oficial
# Autor: Paulo Rebelo

# Bibliotecas 
library(geobr)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Paleta de Cores
cores_estados <- c(
  "#2b6cb0", "#3182ce", "#4299e1", "#63b3ed", "#90cdf4", # Azuis
  "#38a169", "#48bb78", "#68d391", "#9ae6b4",             # Verdes
  "#805ad5", "#9f7aea", "#b794f4",                         # Roxos
  "#d53f8c", "#ed64a6", "#f687b3"                          # Rosas
)

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
    830026, 3272391, 733759, 3807926, 14812617, 8456121, 2962995, 3973697, 6555260, 6824426,
    3526220, 2809394, 20524866, 8602865, 3766528, 11466630, 9137282, 3194253, 16054524, 3560902,
    10882979, 1815278, 636707, 7345692, 44606336, 2298696, 1607363
  )
)

# Juntar com shapefile
estados <- estados %>%
  left_join(regioes_estados, by = "abbrev_state")

# Cores por Região
cores_regiao <- c(
  "Norte" = "#2b6cb0",
  "Nordeste" = "#38a169",
  "Centro Oeste" = "#805ad5",
  "Sudeste" = "#d53f8c",
  "Sul" = "#3182ce"
)

estados$cor_regiao <- cores_regiao[estados$region]

# Mapa Interativo sem mini gráfico
leaflet_map <- leaflet(estados) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~cor_regiao,
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    group = ~region,
    label = ~paste0(name_state),
    popup = ~paste0(
      "<div style='font-family: Roboto, sans-serif;'>",
      "<h3 style='color: #2d3748; margin-bottom: 10px;'>", name_state, "</h3>",
      "<div style='background-color: #f7fafc; padding: 10px; border-radius: 5px;'>",
      "<p><strong>Sigla:</strong> ", abbrev_state, "</p>",
      "<p><strong>Região:</strong> ", region, "</p>",
      "<p><strong>População Estimada (IBGE 2022):</strong> ", format(populacao, big.mark = "."), "</p>",
      "</div>",
      "</div>"
    ),
    highlight = highlightOptions(
      weight = 3,
      color = "#2d3748",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLayersControl(
    overlayGroups = c("Norte", "Nordeste", "Centro Oeste", "Sudeste", "Sul"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = cores_regiao,
    labels = names(cores_regiao),
    title = "Regiões do Brasil",
    opacity = 0.7
  )

# Exibir Mapa
leaflet_map

# Salvar HTML
saveWidget(leaflet_map, "mapa_brasil_interativo.html", selfcontained = TRUE)

# Exportar CSV
dados_exportar <- estados %>%
  select(name_state, abbrev_state, region, populacao)

write.csv(dados_exportar, "dados_estados_brasil.csv", row.names = FALSE)



