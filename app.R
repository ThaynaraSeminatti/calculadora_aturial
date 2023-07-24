#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(lifecontingencies)
library(tidyverse)
library(plotly)
library(shinydashboard)

cabecalho <- dashboardHeader(title = 'Calculadora Atuarial')

barra_lateral <- dashboardSidebar(width = '250px')

painel_principal <- dashboardBody(
  tabItem(tabName = 'dashboard',
          fluidRow(
            valueBox(subtitle =  "Valor mensal de contribuição",
                     value = textOutput("value_month"), icon = icon("usd", lib = "glyphicon"),color = "purple"),
            valueBox(subtitle =  "Esperança de vida na idade de aposentadoria", 
                    value = textOutput('esperanca2'), icon = icon("heart", lib = "glyphicon"),color = "purple"),
            valueBox(subtitle =  "Valor acumulado na idade de aposentadoria", 
                     value = textOutput('bnf'), icon = icon("usd", lib = "glyphicon"),color = "purple")
            
          )
    
  ),
  fluidRow(solidHeader = TRUE,
    column(width = 12,
            box(width = '100%',
                column(width = 6,
                       box(width = '100%',
                           numericInput("year_act", "Qual sua idade:", value = 22,min = 0)
                          )
                       ),
                column(width = 6,
                       box(width = '100%',
                           numericInput("year_ret", "Qual idade você pretende aposentar:", value = 70,min = 0)
                       )
                ),
                column(width = 6,
                       box(width = '100%',
                           numericInput("month_income", "Qual renda mensal você pretende ter:", value = 2000,min = 0)
                       )
                ),
                column(width = 6,
                       box(width = '100%', color = "primary",
                           "",
                           sliderInput("interest", "Juros anual:", value = 0.05,min = 0.01,max = 1)
                       )
                )
                )# fim box
            ),# fim colunas
          ),
  fluidRow(
          column(width = 12,
                 box(title = 'Valor guardado mensamente dado que você aposenta-se na idade x',
                     width = '100%',
                     # background = 'purple',
                     plotlyOutput("var_value_month")
                     )
                 ),
           column(width = 6,
                  box(title = 'Valor acumulado do investimento',
                      width = '100%',
                      # background = 'purple',
                      plotlyOutput("value_ac")
                      )
                  ),
          column(width = 6,
                 box(title = 'Esperança de vida até a idade de aposentadoria',
                     width = '100%',
                     # background = 'purple',
                     plotlyOutput("esperanca")
                 )
          )
      
    )
  )



ui <- dashboardPage(header = cabecalho, sidebar = barra_lateral, body = painel_principal, skin = "purple") 
# 
# 
# ui2 <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("year_act", "Qual sua idade:", value = 22),
#       numericInput("year_ret", "Qual idade você pretende aposentar:", value = 70),
#       numericInput("month_income", "Qual renda mensal você pretende ter:", value = 2000),
#       numericInput("interest", "Juros anual:", value = 0.05,max = 1)
#     ),
#     mainPanel(
#       textOutput("value_month"),
#       textOutput('esperanca2'),
#       textOutput('bnf'),
#       plotlyOutput("var_value_month"),
#       plotlyOutput("value_ac"),
#       plotlyOutput("esperanca")
#     )
#   )
# )

server <- function(input, output) {
  
  
  cor4 = '#e6e5e4'
  cor3 = '#1e1e5d'
  cor2 = '#d42f7d'
  cor1 = '#8c8ca4'
  
  data(soa08Act)
  parcelas = 12
  
  P1 <- reactive({
    B = input$month_income
    idade_atual = input$year_act
    idade_aposentadoria = input$year_ret
    t = idade_aposentadoria  - idade_atual
    i = input$interest
    
    
    BENEFICIO = parcelas*B*axn(soa08Act, x = idade_atual, k=parcelas, m = t,i = i)
    P = BENEFICIO / (12*axn(soa08Act, x = idade_atual, k=parcelas, n = t,i = i))
    P
  })
  
  data1 <- reactive({
    B = input$month_income
    idade_atual = input$year_act
    idade_aposentadoria = input$year_ret
    t = idade_aposentadoria  - idade_atual
    i = input$interest
    idada_ap_var = (idade_aposentadoria - 5):(idade_aposentadoria + 5)
    t2 = idada_ap_var  - idade_atual
    BENEFICIO2 = parcelas*B*axn(soa08Act, x = idade_atual, k=parcelas, m = t2,i = i)
    
    P2 = BENEFICIO2 / (parcelas*axn(soa08Act, x = idade_atual, k=parcelas, n = t2,i = i))
    data = data.frame(idada_ap_var = idada_ap_var , P = P2)
      })
  
  data2 <- reactive({
    B = input$month_income
    idade_atual = input$year_act
    idade_aposentadoria = input$year_ret
    t = idade_aposentadoria  - idade_atual
    i = input$interest
    
    
    BENEFICIO = parcelas*B*axn(soa08Act, x = idade_atual, k=parcelas, m = t,i = i)
    P = BENEFICIO / (12*axn(soa08Act, x = idade_atual, k=parcelas, n = t,i = i))
    # Função para calcular o valor futuro após o segundo mês com aportes mensais
    valor_futuro_segundo_mes <- function(aporte, taxa_juros_anual, periodo){
      taxa_juros_mensal <- taxa_juros_anual / 12
      valor_futuro <- aporte * ((1 + taxa_juros_mensal)^periodo - 1) / taxa_juros_mensal
      return(valor_futuro)
    }
    
    # Dados do exemplo
    aporte_mensal <- 64.07
    taxa_juros_anual <- 0.05
    meses <- 1:(parcelas*t)
    
    # Chamando a função para calcular o valor futuro após o segundo mês
    valor_futuro <- valor_futuro_segundo_mes(P, i, meses)
    
    data2 = data.frame(meses = meses, valor_futuro = valor_futuro)

    
  })
  
  data3 <- reactive({
    idade_aposentadoria = input$year_ret
    esperaca = c()
    for (i in 1:idade_aposentadoria) {
      esperaca[i] = exn(soa08Act, x = i)
      
    }
    data3 = data.frame(esperaca = esperaca, idade = 1:idade_aposentadoria)
  })
  
  output$value_month <- renderText({
    valor_formatado <- format(round(P1(),2), nsmall = 2, decimal.mark = ",", big.mark = ".", prefix = "R$")
    paste('R$: ',valor_formatado)
    })
  
  
  
  output$esperanca2 <- renderText({
    idade_aposentadoria = input$year_ret
    ex = exn(soa08Act, x = idade_aposentadoria)
    paste("",
          round(ex,2), ' anos.', sep = '')
  })
  
  output$bnf <- renderText({
    B = input$month_income
    idade_atual = input$year_act
    idade_aposentadoria = input$year_ret
    t = idade_aposentadoria  - idade_atual
    i = input$interest
    
    
    BENEFICIO = parcelas*B*axn(soa08Act, x = idade_atual, k=parcelas, m = t,i = i)
    P = BENEFICIO / (12*axn(soa08Act, x = idade_atual, k=parcelas, n = t,i = i))
    # Função para calcular o valor futuro após o segundo mês com aportes mensais
    valor_futuro_segundo_mes <- function(aporte, taxa_juros_anual, periodo){
      taxa_juros_mensal <- taxa_juros_anual / 12
      valor_futuro <- aporte * ((1 + taxa_juros_mensal)^periodo - 1) / taxa_juros_mensal
      return(valor_futuro)
    }
    
    # Dados do exemplo
    aporte_mensal <- 64.07
    taxa_juros_anual <- 0.05
    meses <- (parcelas*t)
    
    # Chamando a função para calcular o valor futuro após o segundo mês
    valor_futuro <- valor_futuro_segundo_mes(P, i, meses)
    valor_formatado <- format(valor_futuro, nsmall = 2, decimal.mark = ",", big.mark = ".", prefix = "R$")
    paste('R$: ',valor_formatado)
  })
  
  output$var_value_month <- renderPlotly({
    ggplotly(
      ggplot(data = data1(), aes(x = factor(idada_ap_var), y = P, fill = factor(idada_ap_var)))+
      geom_col(show.legend = FALSE)+
      scale_y_continuous(labels = scales::dollar_format(prefix = "R$"))+
      labs(x = 'idade', y = '')+
      theme_bw()+
      scale_fill_manual(values = c(rep(cor1,5),cor2,rep(cor1,5)))
    )
  })

  output$value_ac <- renderPlotly({
    ggplotly(
      data2() %>%
        ggplot(aes(x = meses, y = valor_futuro))+
        geom_col(fill = cor1)+
        scale_y_continuous(labels = scales::dollar_format(prefix = "R$"))+
        labs(y = '', x = 'meses')+
        theme_bw()
    )
  })
  output$esperanca <- renderPlotly({
    ggplotly(
      data3() %>% 
        ggplot(aes(x = idade , y = esperaca))+
        geom_line(size = 1, color = cor2)+ 
        labs(x = 'idade', y = 'anos')+
        theme_bw()
    )
  })

}

shinyApp(ui, server)
