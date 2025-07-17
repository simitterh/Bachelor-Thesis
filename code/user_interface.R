OICD_UI <- dashboardPage(
  dashboardHeader(title = "Portfolio Choice"),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Task", tabName = "Task",
               icon=icon("list")),
      menuItem("Scenario 1", tabName = "Risky_Visualisierung",
               icon=icon("chart-line")),
      menuItem("Scenario 2", tabName = "Riskless_Visualisierung",
               icon=icon("chart-line"))
      )
  ),
  
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$style(
        HTML(
          ".MathJax {
            font-size: 5pt !important;
          }"
        )
      )
    ),
    tabItems(
      
# Aufgabenstellung -------------------------------------------------------------
      tabItem(tabName = "Task",
              h2("Portfolio-Choice: Optimal Investment-Consumption Decision"),
              fluidRow(width=12,
                       box(width=12,title="Risky investment",
                           p("A risk averse Investor is endowed with capital
                             \\(K\\). He/she must now decide on immediate 
                             consumption, \\(c_0\\), and on how much should 
                             optimally be invested in an investment opportunity 
                             that provides a risky dividend which can be 
                             consumed tomorrow. Thus, consumption \\(c_1\\) 
                             at \\(t=1\\) is stochastic."),
                           p("The investor seeks to maximize expected utility
                             \\(E[U(c_0,c_1)]\\)."),
                           p("More specific: The investor has time-seperable 
                             utility with constant relative risk aversion 
                             (CRRA)"),
                           p("\\(U(c_0, c_1) = u(c_0) + \\beta \\, 
                             E[u(c_1)],\\)"),
                           tags$ul(
                             tags$li("\\(c_0, c_1 > 0\\),"),
                             tags$li("\\(\\beta \\leq 1\\),"),
                             tags$li("\\(\\gamma > 0\\).")
                           ),
                           
                           p("There is no income at \\(t=1\\), so investment
                             today is the only way to get consumption
                             tomorrow."),
                           p("The risky investment has a price \\(p_0\\). The
                             payoff (dividend) of the investment, however, is 
                             not deterministic."),
                           p("We assume that at \\(t=1\\), \\(N\\) different
                             states may occur \\(\\Rightarrow\\) 
                             \\(\\pi=[\\pi_1, \\pi_2, ..., \\pi_N]\\),
                             \\(d_1=[d_{1,1}, d_{1,2}, ..., d_{1,N}]\\)
                             with \\(d_{1,i}>0\\) and 
                             \\(\\sum_{i=1}^{N}\\pi_i=1.\\)"),
                           p("Buying \\(x\\) units of the risky investment then
                             yields a dividend payment of \\(xd_{1,i}\\) in
                             state \\(i\\)."),
                           p("The investment problem is then"),
                           p("\\(E[U(c_0, c_1)] = u(c_0) + \\beta 
                             \\sum_{i=1}^{N}\\pi_i u(c_{1,i}) \\rightarrow
                             max_{c_0, c_{1,i}}\\)"),
                           tags$ul(
                             tags$li("\\(c_0 = K - xp_0\\),"),
                             tags$li("\\(c_{1,i} = xd_{1,i}\\),"),
                             tags$li("\\(x \\geq 0\\).")
                           )
                       ),
                       
                       box(width=12,title="Risky and riskless Investment",
                           p("Now we assume that investors have in addition to
                           the risky investment also a riskless investment."),
                           p("Paying a price of \\(b_0\\) at \\(t=0\\), the
                           investor receives a fixed payment of \\(1\\) in each
                           state at \\(t=1\\)."),
                           p("The new optimization problem, which is to maximize
                           by deciding the magnitude of the risky investment,
                           \\(x\\), and the riskless investment, \\(y\\), is
                           then"),
                           p("\\(E[U(c_0, c_1)] = u(c_0) + \\beta 
                             \\sum_{i=1}^{N}\\pi_i u(c_{1,i}) \\rightarrow
                             max_{c_0, c_{1,i}}\\)"),
                           tags$ul(
                             tags$li("\\(c_0 = K - xp_0 - yb_0\\),"),
                             tags$li("\\(c_{1,i} = xd_{1,i} + y \\cdot 1\\),"),
                             tags$li("\\(x \\geq 0\\),"),
                             tags$li("\\(y \\geq 0\\).")
                                  )
                           )
              )
      ),
      

# Risky Visualisierung ---------------------------------------------------------
      tabItem(tabName = "Risky_Visualisierung",
              h2("Plot of the total expected utility:"),
              fluidRow(
                column(width=3,
                       box(width=NULL,title= "Parameters",
                           sliderInput(inputId= "capital_x", 
                                       label="capital",
                                       min = 1, 
                                       max = 100,
                                       step = 1,
                                       value = 20),
                           sliderInput(inputId = "price_x", 
                                       label= "price",
                                       min = 1, 
                                       max = 100,
                                       step = 1,
                                       value = 19),
                           sliderInput(inputId = "beta_x", 
                                       label= "discount factor",
                                       min = 0, 
                                       max = 1,
                                       step = 0.01,
                                       value = 0.9),
                           sliderInput(inputId = "gamma_x",
                                       label= "constant relative risk aversion",
                                       min = 0.01, 
                                       max = 5,
                                       step = 0.01,
                                       value = 2),
                           ),
                       
                       box(inputId = "Isoquant_delta",  width=NULL,
                           
                           sliderInput(inputId = "delta_x", 
                                       label= "isoquant delta",
                                       min = -0.8,
                                       max = 0.8,
                                       step = 0.1,
                                       value = 0)
                           ),
                       
                       box(width=NULL,
                           actionButton(inputId= "setToDefault_Parameter_x",
                                        label= "reset", width = '40%'), 
                                        align = "center",
                           ),
                       
                       box(inputId = "Investment_box_x",
                           width=NULL, title= "Investment details",
                           
                           sliderInput(inputId = "probability1_x", 
                                       label= "probability1",
                                       min = 0,
                                       max = 1,
                                       step = 0.01,
                                       value = 0.2),
                           
                           sliderInput(inputId= "dividend1_x", 
                                       label="dividend1",
                                       min = 1, 
                                       max = 100,
                                       step= 1,
                                       value = 5),
                           
                           sliderInput(inputId = "probability2_x", 
                                       label= "probability2",
                                       min = 0,
                                       max = 1,
                                       step = 0.01,
                                       value = 0.5),
                           
                           sliderInput(inputId= "dividend2_x", 
                                       label="dividend2",
                                       min = 1, 
                                       max = 100,
                                       step= 1,
                                       value = 15),
                           
                           sliderInput(inputId = "probability3_x", 
                                       label= "probability3",
                                       min = 0,
                                       max = 1,
                                       step = 0.01,
                                       value = 0.3),
                           
                           sliderInput(inputId= "dividend3_x", 
                                       label="dividend3",
                                       min = 1, 
                                       max = 100,
                                       step= 1,
                                       value = 25)
                       ),
                       box(width=NULL,
                           actionButton(inputId= "setToDefault_investment_x",
                                        label= "reset", width = '40%'), 
                                        align = "center",
                       ),
                       box(width=NULL,title="results",
                           htmlOutput("max_x"),
                           htmlOutput("maxZf_x")
                           )
                ),
                column(width=9,
                       box(width=NULL,
                           plotOutput(outputId = "PortfolioChoice_x",
                                      height="55vh"))
                ),
                column(width=9,
                       box(width=NULL,
                           plotOutput(outputId = "Isoquants_x",height="55vh"))
                ),
                column(width=9,
                       box(width=NULL,
                           plotOutput(outputId = "MarginalUtility_x",
                                      height="55vh"))
                )
              )
      ),
      
      
# Riskless Visualisierung ------------------------------------------------------

tabItem(tabName = "Riskless_Visualisierung",
        h2("Plot of the total expected utility:"),
        fluidRow(
          column(width=3,
                 
                 box(width=NULL,title= "Parameters",
                     sliderInput(inputId = "capital_xy", 
                                 label="capital",
                                 min = 1, 
                                 max = 100,
                                 step = 1,
                                 value = 20),
                     sliderInput(inputId = "price_p0_xy", 
                                 label= "price risky investment",
                                 min = 1, 
                                 max = 100,
                                 step = 1,
                                 value = 19),
                     sliderInput(inputId = "price_b0_xy", 
                                 label= "price riskless investment",
                                 min = 0.05, 
                                 max = 5,
                                 step = 0.05,
                                 value = 0.95),
                     sliderInput(inputId = "beta_xy", 
                                 label= "discount factor",
                                 min = 0, 
                                 max = 1,
                                 step = 0.01,
                                 value = 0.9),
                     sliderInput(inputId = "gamma_xy",
                                 label= "constant relative risk aversion",
                                 min = 0.01, 
                                 max = 5,
                                 step = 0.01,
                                 value = 2),
                 ),
                 
                 box(inputId = "Isoquant_delta_xy",  width=NULL,
                     
                     sliderInput(inputId = "delta_xy", 
                                 label= "isoquant delta y",
                                 min = -1,
                                 max = 1,
                                 step = 0.05,
                                 value = 0)
                 ),
                 
                 box(width=NULL,
                     actionButton(inputId= "setToDefault_Parameter_xy",
                                  label= "reset", width = '40%'), 
                     align = "center",
                 ),
                 
                 box(inputId = "Investment_box_xy",  width=NULL, 
                     title= "Investment details",
                     
                     sliderInput(inputId = "probability1_xy", 
                                 label= "probability1",
                                 min = 0,
                                 max = 1,
                                 step = 0.01,
                                 value = 0.2),
                     
                     sliderInput(inputId= "dividend1_xy", 
                                 label="dividend1",
                                 min = 1, 
                                 max = 100,
                                 step= 1,
                                 value = 5),
                     
                     sliderInput(inputId = "probability2_xy", 
                                 label= "probability2",
                                 min = 0,
                                 max = 1,
                                 step = 0.01,
                                 value = 0.5),
                     
                     sliderInput(inputId= "dividend2_xy", 
                                 label="dividend2",
                                 min = 1, 
                                 max = 100,
                                 step= 1,
                                 value = 15),
                     
                     sliderInput(inputId = "probability3_xy", 
                                 label= "probability3",
                                 min = 0,
                                 max = 1,
                                 step = 0.01,
                                 value = 0.3),
                     
                     sliderInput(inputId= "dividend3_xy", 
                                 label="dividend3",
                                 min = 1, 
                                 max = 100,
                                 step= 1,
                                 value = 25)
                     ),
                 box(width=NULL,
                     actionButton(inputId= "setToDefault_investment_xy",
                                  label= "reset", width = '40%'), 
                     align = "center",
                 ),
                 box(width=NULL,title="results",
                     htmlOutput("max_xy_x"),
                     htmlOutput("max_xy_y"),
                     htmlOutput("maxZf_xy")
                     )
          ),
          
          column(width=9,
                 box(width=NULL,
                     plotOutput(outputId = "PortfolioChoice_xy",
                                height="55vh"))
          ),
          
          column(width=9,
                 box(width=NULL,
                     plotOutput(outputId = "Isoquants_xy", height="55vh"))
          ),
          
          column(width=9,
                 box(width=NULL,
                     plotOutput(outputId = "MarginalUtility_xy",
                                height="55vh"))
          )
        )
      )
    )
  )
)
