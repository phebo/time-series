library(shiny)
library(tidyverse)
library(stabledist)

ui <- fluidPage(
  titlePanel("Simulations of basic stochastic processes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("process", "Stochastic process",
                  c("Brownian/Lévy" = "brown",
                    "Geometric" = "geo",
                    "Ornstein–Uhlenbeck" = "ou")),
      sliderInput("nSim", "# Simulated paths", min=10, max=100, step=10, value=10),
      sliderInput("Tt", "Total time", min=1, max=25, value=10),
      sliderInput("mu", "Mu", min=-1, max=1, value=0, step=0.1),
      sliderInput("sigma", "Sigma", min=0.1, max=1, value=0.2, step=0.1),
      sliderInput("alpha", "Alpha (Lévy process)", min=0.1, max=2, value=2, step=0.1),
      sliderInput("beta", "Beta (Lévy process)", min=-1, max=1, value=0, step=0.1),
      sliderInput("theta", "Theta (OU process)", min=0.1, max=1, value=0.2, step=0.1),
    ),
    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    nT <- 301
    dt <- input$Tt / (nT-1)
    if(input$process %in% c("brown", "geo")) df <-
      expand_grid(i=1:input$nSim, t=seq(dt,input$Tt,length.out = nT-1)) %>%
      mutate(dy = rstable(input$nSim * (nT-1), alpha=input$alpha, beta=input$beta,
                          gamma=input$sigma*dt^(1/input$alpha), delta=input$mu*dt)) %>%
      bind_rows(tibble(i=1:input$nSim, t=0, dy=0)) %>% arrange(i,t) %>%
      group_by(i) %>% mutate(y = cumsum(dy)) %>% ungroup()
    if(input$process == "geo") df <- df %>% mutate(y = exp(y))
    if(input$process == "ou") {
      y = rep(0, input$nSim)
      df = tibble(i=1:input$nSim, t=0, y=y)
      tt = dt
      while(tt <= input$Tt) {
        dz <- rnorm(input$nSim, sd = sqrt(dt))
        y <- y + input$theta * (input$mu - y) * dt + input$sigma * dz
        df <- bind_rows(df, tibble(i=1:input$nSim, t=tt, y=y))
        tt <- tt + dt
      }}
    df %>% ggplot(aes(x=t, y=y, group=i)) + geom_line()
    }, height=700)
}

shinyApp(ui = ui, server = server)
