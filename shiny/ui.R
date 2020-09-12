require(tidyverse)
require(plotly)
require(shiny)

ui <- fluidPage("", titlePanel("Evoluation of line length in 21 years of R packages on CRAN"), h4("Chai-yi Yen, Mia Chang & Chung-hong Chan (2020)"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("year", "Year", min = 1998, max = 2019, value = 2019, step = 1, animate = TRUE)
                    ),
                    mainPanel(
                        plotlyOutput("linechart")
                    )
                )
)
