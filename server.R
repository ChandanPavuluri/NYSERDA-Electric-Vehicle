# Define server function
server <- function(input, output) {
  
  brands_df <- reactive({ EV %>%
      filter(
        Make == input$Brand,
        Year == input$Year)%>%
      group_by(Model)})
  
  counts_df <- reactive({brands_df()%>%
      group_by(Make,Transaction.Type)%>%
      summarise(count =n())%>%
      mutate(prop = (count/sum(count))*100)%>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop)})
  
  CO2_df <- reactive({ EV %>%
      group_by(Make,Model,EV.Type,Year)%>%
      summarise(Emissions = sum(CO2))%>%
      arrange(desc(Emissions))%>%
      filter(Year == input$Emissions_Year)})
  
  Petrol_df <- reactive({ EV %>%
      group_by(Make,Model,EV.Type,Year)%>%
      summarise(Emissions = sum(Petrol))%>%
      arrange(desc(Emissions))%>%
      filter(Year == input$Emissions_Year)})
  
  
  Rebate_df <- reactive({EV%>%
      select(Make,Model,Year,County,EV.Type,Rebate.Amount..USD.)%>%
      filter(Year == input$R_Year,Make == input$R_Brand)%>%
      group_by(Model,EV.Type)%>%
      summarise(count = sum(Rebate.Amount..USD.))})
  
  Map_df <- reactive({EV %>%
      group_by(Make,Year,County)%>%
      filter(Make == input$Map_Brand,Year == input$Map_Year)%>%
      summarise(count = n())})
  
  Map_County <- reactive({inner_join(ny_county, Map_df(), by = "County")})
  
  County_Rebate_df <- reactive({EV%>%
      group_by(Make,Year,County,Rebate.Amount..USD.)%>%
      filter(Year == input$County_R_Year,Make == input$County_R_Brand)%>%
      summarise(Value = sum(Rebate.Amount..USD.))})
  
  Map_County_Rebate <- reactive({inner_join(ny_county, County_Rebate_df(), by = "County")})
  
  
  
  # Creating barplot for brands and year
  output$barplot <- renderPlot({
    barplot <- ggplot(data = brands_df() ) + geom_bar(mapping = aes(x = Model, fill = Transaction.Type), position = "dodge")+
      facet_wrap(~EV.Type,scales="free_x")+
      labs(x= paste(input$Brand,input$Year),y="Total Count",fill="OwnerShip")+
      ggtitle(paste(input$Brand,"Models and EV Type","For The",input$Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text.x =element_text(angle =45,hjust = 1))
    #barplot <- ggplotly(barplot)
    barplot
  })
  
  output$piechart <- renderPlot({ggplot(counts_df(), aes(x="", y=count, fill=Transaction.Type)) + geom_bar(stat="identity", width=1)+ coord_polar(theta = "y")+ 
      geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5))+
      ggtitle(paste(input$Brand,"Ownership Style Percentages For The",input$Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill="OwnerShip")+
      theme_void()})
  
  output$scatterplot_Emissions <- renderPlotly({if(input$Emissions_Choice == "CO2"){
    scatterplot<-ggplot(data = CO2_df(), mapping = aes(label = Model)) + 
      geom_point(mapping = aes(x = Make, y = Emissions , color = EV.Type))+
      ggtitle(paste("Reduction of Metric tons of CO2 For The Year",input$Emissions_Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x=paste("Brands",input$Emissions_Year),y=paste(input$Emissions_Choice,"MTCO2e"))+
      theme(axis.text.x =element_text(angle =45,hjust = 1))
  } else {scatterplot<-ggplot(data = Petrol_df(), mapping = aes(label = Model)) + 
    geom_point(mapping = aes(x = Make, y = Emissions , color = EV.Type))+
    ggtitle(paste("Usage Reduction of Petrol in Gallons For The Year",input$Emissions_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x=paste("Brands",input$Emissions_Year),y=paste(input$Emissions_Choice,"(Gallons)"))+
    theme(axis.text.x =element_text(angle =45,hjust = 1))}
    scatterplot <- ggplotly(scatterplot)
    scatterplot})
  
  
  output$barplot_Rebate <- renderPlotly({barplot <- ggplot(data= Rebate_df(), aes(x=Model, y=count)) +
    geom_bar(fill="brown2",stat="identity")+facet_wrap(~EV.Type,scales="free_x")+
    ggtitle(paste("Rebate Amount of",input$R_Brand,"Cars For The Year",input$R_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x= paste(input$R_Brand,input$R_Year),y=("USD"))
  barplot <- ggplotly(barplot)
  barplot})
  
  output$County_Map <- renderPlotly({map <- NY_County_map + 
    geom_polygon(data = Map_County() , aes(fill = count,label = County), color = "black") +
    geom_polygon(color = "black", fill = NA)+
    scale_fill_continuous(
      low = "#56B1F7", high = "#132B43",na.value = "red",
      guide=guide_colorbar(barwidth = 2,barheight = 10))+
    ggtitle(paste("NY County wise",input$Map_Brand,"cars count for the year",input$Map_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(fill = "Density")})
  
  output$County_Map_Rebate <- renderPlotly({map <- NY_County_map + 
    geom_polygon(data = Map_County_Rebate() , aes(fill = Value,label = County), color = "black") +
    geom_polygon(color = "black", fill = NA)+
    scale_fill_continuous(
      low = "#56B1F7", high = "#132B43",na.value = "red",
      guide=guide_colorbar(barwidth = 2,barheight = 10))+
    ggtitle(paste("NY County Wise",input$County_R_Brand,"Cars Total Rebate Amount For The Year",input$County_R_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(fill = "USD")})
  
  output$UIout1 <- renderUI({
    if(nrow(brands_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotOutput("barplot")
    }
  })
  
  output$UIout2 <- renderUI({
    if(nrow(counts_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotOutput("piechart")
    }
  })
  
  output$UIout3 <- renderUI({
    plotlyOutput("scatterplot_Emissions")
  })
  
  
  output$UIout4 <- renderUI({
    if(nrow(Rebate_df()) == 0){
      p(strong(paste("No"),input$R_Brand, paste("Cars for the year"),input$R_Year))
    } else {
      plotlyOutput("barplot_Rebate")
    }
    
  })
  
  output$UIout5 <- renderUI({
    
    if(nrow(Map_df()) == 0){
      p(strong(paste("No"),input$Map_Brand, paste("Cars for the year"),input$Map_Year))
    } else {
      plotlyOutput("County_Map")
    }
    
  })
  
  output$UIout6 <- renderUI({
    
    if(nrow(County_Rebate_df()) == 0){
      p(strong(paste("No"),input$County_R_Brand, paste("Cars for the year"),input$County_R_Year))
    } else {
      plotlyOutput("County_Map_Rebate")
    }
    
  })
}




