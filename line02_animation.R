require(tidyverse)
require(plotly)
require(modules)
require(shiny)

cfg <- modules::use("config.R")

comment_dist <- readRDS(cfg$PATH_COMMENT_DIST)


## comment_dist %>% mutate(comment = ifelse(comment == 1, "Yes", "No")) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline)) %>% 
##     select(pub_year, n_chars, prob, comment) %>% filter(pub_year %in% c(2010, 2019)) %>%
##     filter(n_chars > 40 & n_chars < 100 & pub_year <= 2019) %>% 
##     mutate(prob = prob * 100) %>%
##     ggplot(aes(x = n_chars, y = prob, color = comment)) + 
##     geom_line(stat = 'identity') +
##     geom_vline(xintercept = 80, alpha = 0.3) + facet_wrap(~ pub_year, nrow = 3) +
##     xlab('Number of characters') + ylab('Share of all lines (%)') + scale_color_brewer(palette="Dark2") +
##     theme(plot.title = element_text(size = 24, face = "bold"), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 15), axis.title=element_text(size=14,face="bold")) + 
##     theme(rect = element_rect(fill = "transparent")) +
##     theme(legend.position = "none") + theme(strip.text.x = element_text(size = 15, face = "bold")) -> line_compare
## ggsave('visualization_line/line_length_2010_2019.png', line_compare, width = 6, height = 5, units = 'in', bg = "transparent")

## comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 150 & pub_year <= cfg$INCLUDE_YR) %>% 
##   ggplot(aes(x = n_chars, y = prob, color = comment)) + 
##   geom_line(stat = 'identity') +
##   geom_vline(xintercept = 80, alpha = 0.1) + facet_wrap(~ pub_year, nrow = 3)

comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 150 & pub_year <= cfg$INCLUDE_YR) %>% ungroup -> line_prob

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

server <- function(input, output) {
    output$linechart <- renderPlotly({
        pal <- c("#1B9E77", "#D95F02")
        pal <- setNames(pal, c("Code", "Comment"))
        line_prob %>% filter(pub_year == input$year & n_chars < 150 & n_chars >= 2) %>% mutate(comment = ifelse(comment, "Code", "Comment")) %>% plot_ly(x = ~n_chars, y = ~ prob, color = ~ comment, type = "scatter", mode = "lines", colors = pal) %>% layout(yaxis = list(range = c(0, 11), title = "Share of lines (%)"), xaxis = list(range = c(2, 150), title = "Number of characters"))
    })
}

shinyApp(ui, server)

## ## information entropy

## comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline), plogp = prob * log(prob)) %>% summarise(entropy = - sum(plogp)) %>% ungroup %>% mutate(type = paste0('linelength_', ifelse(comment, "comment", "code"))) %>% select(pub_year, entropy, type) %>% saveRDS('entropy_linelength.RDS')
