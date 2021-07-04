# Define UI
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="Electric vehicles Newyork"),
    dashboardSidebar(sidebarMenu(
      menuItem("Car Models and OwnerShip",tabName="carmodels",icon=icon("car")),
      menuItem("Emissions",tabName = "Emissions",icon = icon("gas-pump")),
      menuItem("Rebate Amount Brand",tabName="RebateAmount",icon=icon("money")),
      menuItem("Car Density",tabName = "CarDenisty",icon = icon("globe")),
      menuItem("Rebate Amount County",tabName = "RebateCounty",icon=icon("money"))
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "carmodels",
                fluidRow(
                  box(column(width=6,selectInput("Brand","Brand",choices=sort(unique(EV$Make))))),
                  box(column(width= 6, offset = 1,radioButtons("Year","Year",sort(unique(EV$Year))))),
                  box(width=6,collapsible=TRUE,uiOutput(outputId = "UIout2")),
                  box(width=6,collapsible=TRUE,collapsed = TRUE,uiOutput(outputId = "UIout1"))
                  
                )),
        tabItem(tabName = "Emissions",
                fluidRow(
                  box(column(width =6,selectInput("Emissions_Year","Year",choices=sort(unique(EV$Year))))),
                  box(column(width= 6, offset = 1,radioButtons("Emissions_Choice","Emissions",choices=c("CO2","Petrol")))),
                  box(width = 12,collapsible=TRUE,uiOutput(outputId = "UIout3"))
                )),
        tabItem(tabName = "RebateAmount",
                fluidRow(
                  box(column(width=4,selectInput("R_Brand","Brand",choices=sort(unique(EV$Make))))),
                  #box(column(width=4,selectInput("R_County","County",choices=sort(unique(EV$County))))),
                  box(column(width=4,offset =1,radioButtons("R_Year","Year",sort(unique(EV$Year))))),
                  box(width=12,collapsible=TRUE, uiOutput(outputId = "UIout4"))
                )),
        tabItem(tabName = "CarDenisty",
                fluidRow(
                  box(column(width=4,selectInput("Map_Brand","Brand",choices=sort(unique(EV$Make))))),
                  box(column(width=4,offset = 1,radioButtons("Map_Year","Year",sort(unique(EV$Year))))),
                  box(width = 12,collapsible = TRUE, uiOutput(outputId = "UIout5"))
                )),
        tabItem(tabName = "RebateCounty",
                fluidRow(
                  box(column(width=4,selectInput("County_R_Brand","Brand",choices=sort(unique(EV$Make))))),
                  box(column(width=4,offset = 1,radioButtons("County_R_Year","Year",sort(unique(EV$Year))))),
                  box(width = 12,collapsible = TRUE, uiOutput(outputId = "UIout6"))
                ))
      )))
  
)  

