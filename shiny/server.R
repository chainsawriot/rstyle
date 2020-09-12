require(dplyr)
require(plotly)
require(shiny)

comment_dist <- readRDS("comment_dist.RDS")

comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 150 & pub_year <= 2019) %>% ungroup -> line_prob

server <- function(input, output) {
    output$linechart <- renderPlotly({
        pal <- c("#1B9E77", "#D95F02")
        pal <- setNames(pal, c("Code", "Comment"))
        line_prob %>% filter(pub_year == input$year & n_chars < 150 & n_chars >= 2) %>% mutate(comment = ifelse(comment, "Code", "Comment")) %>% plot_ly(x = ~n_chars, y = ~ prob, color = ~ comment, type = "scatter", mode = "lines", colors = pal) %>% layout(yaxis = list(range = c(0, 11), title = "Share of lines (%)"), xaxis = list(range = c(2, 150), title = "Number of characters"))
    })
}
