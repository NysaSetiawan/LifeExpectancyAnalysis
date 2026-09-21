library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(maps)
library(stringr)

df<-read.csv("Life_Expectancy_Data.csv")

num_cols<-sapply(df,is.numeric)
df[,num_cols]<-lapply(df[,num_cols],function(x){
  x[is.na(x)]<-median(x,na.rm=TRUE)
  return(x)
})

names(df)<-make.names(names(df),unique=TRUE)
if("Infant.deaths"%in%names(df)){
  names(df)[names(df)=="Infant.deaths"]<-"infant.deaths"
}
if("Under.five.deaths"%in%names(df)){
  names(df)[names(df)=="Under.five.deaths"]<-"under.five.deaths"
}

df$Status<-as.factor(df$Status)

ui<-dashboardPage(
  dashboardHeader(title="Life Expectancy Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName="overview",icon=icon("database"),
               menuSubItem("Life Expectancy Overview",tabName="heart",icon=icon("heartbeat")),
               menuSubItem("Sustainable Development Goals",tabName="sdgs",icon=icon("globe")),
               menuSubItem("Search Data",tabName="search",icon=icon("search")),
               menuSubItem("Summary Statistics",tabName="summary",icon=icon("list"))
      ),
      menuItem("Exploration",tabName="exploratory_parent",icon=icon("chart-bar"),
               menuSubItem("Exploratory Data Analysis",tabName="eda",icon=icon("line-chart")),
               menuSubItem("Country Insights",tabName="country_insights",icon=icon("map"))
      ),
      menuItem("Linear Regression Model",tabName="regression_parent",icon=icon("chart-line"),
               menuSubItem("Interactive Regression",tabName="regression",icon=icon("chart-line")),
               menuSubItem("Proof",tabName="proof",icon=icon("flask"))
      ),
      menuItem("Conclusion",tabName="conclusion",icon=icon("clipboard-check"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="heart",
              fluidRow(
                column(6,
                       box(width=12,height="400px",title="Life Expectancy Overview",
                           status="primary",solidHeader=TRUE,
                           div(style="text-align:center;padding-top:20px;",
                               tags$img(src="le.jpeg",width="90%",style="max-width: 300px;height: auto;")
                           ),
                           div(style="text-align:center;font-style:italic;",
                               "A visual representation of life expectancy.")
                       )
                ),
                column(6,
                       h3("Understanding Life Expectancy"),
                       p("Life expectancy refers to the average number of years a person can expect to live, based on current mortality trends. It is a key indicator of a population’s overall health and quality of life. Influenced by a range of factors such as healthcare access, nutrition, sanitation, lifestyle, and socioeconomic status, life expectancy varies significantly between countries and demographic groups."),
                       p("Globally, life expectancy has shown a steady increase over the past decades, reflecting improvements in medical care, disease prevention, and living conditions. However, disparities still exist. Developed countries tend to have higher life expectancies due to advanced healthcare systems and healthier environments, while developing regions may face challenges such as poverty, malnutrition, and limited healthcare access, which can reduce life expectancy.")
                )
              )
      ),
      tabItem(tabName="sdgs",
              h2("Sustainable Development Goals Overview"),
              fluidRow(
                column(6,
                       box(width=12,height="400px",title="SDG 3: Good Health and Well-being",
                           status="primary",solidHeader=TRUE,
                           div(style="text-align:center;padding-top:30px;",
                               tags$img(src="sdgs3.png",
                                        width="90%",
                                        style="max-width: 250px;height: auto;display: block;margin-left: auto;margin-right: auto;")
                           ),
                           div(style="text-align:center;font-style:italic;",
                               "A visual representation of SDG 3.")
                       )
                ),
                column(6,
                       h3("SDG 3: Good Health and Well-being"),
                       p("Sustainable Development Goal 3 aims to ensure healthy lives and promote well-being for all at all ages. One of its key indicators is life expectancy, which reflects the overall effectiveness of health systems, disease prevention, and access to care. Improving life expectancy aligns with SDG 3 targets, such as reducing maternal and child mortality, combating diseases, and strengthening healthcare services—especially in low-resource settings.")
                )
              )
      ),
      tabItem(tabName="search",
              h2("Search Data"),
              DT::dataTableOutput("dataTable")
      ),
      tabItem(tabName="summary",
              h2("Summary Statistics"),
              verbatimTextOutput("summaryStats"),
              br()
      ),
      tabItem(tabName="eda",
              h2("Data Exploration"),
              fluidRow(
                column(width=12,
                       actionButton("btn_developed_country","Developed Countries",icon=icon("globe-americas")),
                       actionButton("btn_developing_country","Developing Countries",icon=icon("globe-asia")),
                       actionButton("btn_disease_comparison","Disease Trends",icon=icon("virus")),
                       actionButton("btn_mortality_indicators","Mortality Indicators",icon=icon("procedures")),
                       actionButton("btn_schooling_trends","Schooling Trends",icon=icon("graduation-cap")),
                       actionButton("btn_bmi_distribution","BMI Distribution",icon=icon("weight")),
                       actionButton("btn_income_trends","Income Trends",icon=icon("money-bill-alt")),
                       actionButton("btn_gdp_trends","GDP Trends",icon=icon("chart-line"))
                )
              ),
              hr(),
              uiOutput("eda_main_content")
      ),
      
      tabItem(tabName="country_insights",
              h2("Life Expectancy Across Countries"),
              fluidRow(
                box(width=12,title="Life Expectancy Map",status="primary",solidHeader=TRUE,
                    plotlyOutput("lifeExpMap",height="500px")
                )
              ),
              fluidRow(
                box(width=12,title="Map Description",status="info",solidHeader=TRUE,
                    textOutput("lifeExpMapDescription")
                )
              ),
              fluidRow(
                box(width=12,title="World Life Expectancy Trend",status="success",solidHeader=TRUE,
                    plotlyOutput("overallLifeExpTrend",height="400px")
                )
              ),
              fluidRow(
                box(width=12,title="Indonesia Life Expectancy Trend",status="success",solidHeader=TRUE,
                    plotlyOutput("indoLifeExpTrend",height="400px")
                )
              ),
              hr(),
              h2("Life Expectancy Trends for Top/Bottom Countries"),
              fluidRow(
                box(width=4,
                    numericInput("num_countries_facet","Number of Countries (N):",value=5,min=1,max=20),
                    radioButtons("country_rank_type","Show:",
                                 choices=c("Top N by Average Life Expectancy"="top",
                                           "Bottom N by Average Life Expectancy"="bottom"),
                                 selected="top")
                ),
                box(width=8,
                    plotlyOutput("lifeExpFacetTrend",height="600px")
                )
              )
      ),
      tabItem(tabName="regression",
              h2("Correlation Matrix of Key Numerical Health Indicators",align="center"),
              plotlyOutput("corrMatrix",height="600px"),
              hr(),
              h2("Linear Regression Model by Development Status"),
              fluidRow(
                valueBoxOutput("regressionCountryBox",width=3),
                valueBoxOutput("regressionDataBox",width=3),
                valueBoxOutput("regressionStatusBox",width=3),
                valueBoxOutput("regressionCorrelationBox",width=3)
              ),
              fluidRow(
                box(width=4,selectInput("devStatus","Select Development Status:",
                                        choices=unique(df$Status),selected=unique(df$Status)[1])),
                box(width=4,selectInput("xvar_reg","Select X:",
                                        choices=names(df)[sapply(df,is.numeric)],selected="Schooling")),
                box(width=4,selectInput("yvar_reg","Select Y:",
                                        choices=names(df)[sapply(df,is.numeric)],selected="Life.expectancy"))
              ),
              plotlyOutput("regByStatusPlot")
      ),
      
      tabItem(tabName="proof",
              h2("Life Expectancy Regression Model",align="center"),
              p("Five regression model that correlates with Life Expectancy.",
                style="text-align: center;font-size: 16px;margin-bottom: 30px;"),
              fluidRow(
                box(width=6,title=HTML("Life Expectancy vs Adult Mortality <i class='fa fa-procedures'></i>"),status="primary",solidHeader=TRUE,
                    plotlyOutput("proofPlot1",height="400px")),
                box(width=6,title=HTML("Life Expectancy vs Schooling <i class='fa fa-graduation-cap'></i>"),status="success",solidHeader=TRUE,
                    plotlyOutput("proofPlot2",height="400px"))
              ),
              fluidRow(
                box(width=6,title=HTML("Life Expectancy vs Income Composition of Resources <i class='fa fa-money-bill-alt'></i>"),status="info",solidHeader=TRUE,
                    plotlyOutput("proofPlot3",height="400px")),
                box(width=6,title=HTML("Life Expectancy vs HIV/AIDS <i class='fa fa-virus'></i>"),status="warning",solidHeader=TRUE,
                    plotlyOutput("proofPlot4",height="400px"))
              ),
              fluidRow(
                column(3),
                box(width=6,title=HTML("Life Expectancy vs BMI <i class='fa fa-weight'></i>"),status="danger",solidHeader=TRUE,
                    plotlyOutput("proofPlot5",height="400px")),
                column(3)
              )
      ),
      tabItem(tabName="conclusion",
              fluidRow(
                box(width=12,title="Conclusion",status="info",solidHeader=TRUE,
                    textOutput("conclude")
                )
              ),
      )
    )
  )
)

server<-function(input,output,session){
  active_eda_status<-reactiveVal("")
  active_eda_mode<-reactiveVal("") # Initialize to an empty string
  
  observeEvent(input$btn_developed_country,{
    active_eda_mode("country_status")
    active_eda_status("Developed")
  })
  
  observeEvent(input$btn_developing_country,{
    active_eda_mode("country_status")
    active_eda_status("Developing")
  })
  
  observeEvent(input$btn_disease_comparison,{
    active_eda_mode("disease_comparison")
    active_eda_status("")
  })
  
  observeEvent(input$btn_mortality_indicators,{
    active_eda_mode("mortality_indicators")
    active_eda_status("")
  })
  
  observeEvent(input$btn_schooling_trends,{
    active_eda_mode("schooling_trends")
    active_eda_status("")
  })
  
  observeEvent(input$btn_bmi_distribution,{
    active_eda_mode("bmi_distribution")
    active_eda_status("")
  })
  
  observeEvent(input$btn_income_trends,{
    active_eda_mode("income_trends")
    active_eda_status("")
  })
  
  observeEvent(input$btn_gdp_trends,{
    active_eda_mode("gdp_trends")
    active_eda_status("")
  })
  
  output$eda_main_content<-renderUI({
    if(active_eda_mode()=="country_status"){
      tagList(
        h3(textOutput("eda_analysis_title")),
        
        if(active_eda_status()!=""){
          tagList(
            fluidRow(
              box(width=4,
                  radioButtons("visualize_by_country","Visualize by Country:",
                               choices=c("No (Overall Factor Distribution)"="no",
                                         "Yes (Select a Country)"="yes"),
                               selected="no",
                               inline=TRUE)
              ),
              box(width=4,
                  selectInput("factor_eda","Select a factor to visualize:",
                              choices=NULL)
              ),
              conditionalPanel(
                condition="input.visualize_by_country=='yes'",
                box(width=4,
                    selectInput("selected_country_eda","Select Country:",
                                choices=NULL)
                )
              )
            ),
            fluidRow(
              box(width=12,plotlyOutput("factor_distribution_eda"))
            )
          )
        }else{
          h4("Please select 'Developed Countries' or 'Developing Countries' to start the analysis.")
        }
      )
    }else if(active_eda_mode()=="disease_comparison"){
      tagList(
        h3("Disease Trends Comparison"),
        fluidRow(
          column(width=4,
                 box(width=12,
                     radioButtons("disease_country_selection","Compare by:",
                                  choices=c("All Countries"="all",
                                            "Select Countries"="selected"),
                                  selected="all",
                                  inline=TRUE)
                 )
          ),
          column(width=4,
                 box(width=12,
                     selectInput("selected_diseases_factors","Select Disease(s) to Compare:",
                                 choices=c("Hepatitis B"="Hepatitis.B",
                                           "Measles"="Measles",
                                           "Polio"="Polio",
                                           "Diphtheria"="Diphtheria",
                                           "HIV/AIDS"="HIV.AIDS"),
                                 multiple=TRUE,
                                 selected=c("Hepatitis.B","Measles"))
                 )
          ),
          column(width=4,
                 conditionalPanel(
                   condition="input.disease_country_selection=='selected'",
                   box(width=12,
                       selectInput("selected_diseases_countries","Select Countries:",
                                   choices=NULL,
                                   multiple=TRUE)
                   )
                 )
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("diseaseComparisonPlot",height="400px"))
        )
      )
    }else if(active_eda_mode()=="mortality_indicators"){
      tagList(
        h3("Distribution of Key Mortality Indicators"),
        fluidRow(
          box(width=4,plotlyOutput("adultMortalityHist")),
          box(width=4,plotlyOutput("infantDeathHist")),
          box(width=4,plotlyOutput("underFiveDeathHist"))
        ),
        hr(),
        h3("Mortality Trends by Year and Development Status"),
        fluidRow(
          box(width=6,
              radioButtons("mortality_trend_status","Group by Development Status:",
                           choices=c("Overall"="All",
                                     "Developed"="Developed",
                                     "Developing"="Developing"),
                           selected="All",
                           inline=TRUE)
          ),
          box(width=6,
              sliderInput("year_range",
                          "Select Year Range:",
                          min=min(df$Year),
                          max=max(df$Year),
                          value=c(min(df$Year),max(df$Year)),
                          step=1,
                          sep=""
              )
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("mortalityTrendPlot",height="500px"))
        )
      )
    }else if(active_eda_mode()=="schooling_trends"){
      tagList(
        h3("Schooling Trends by Year and Development Status"),
        fluidRow(
          box(width=12,
              radioButtons("schooling_trend_status","Group by Development Status:",
                           choices=c("Overall"="All",
                                     "Developed"="Developed",
                                     "Developing"="Developing"),
                           selected="All",
                           inline=TRUE)
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("schoolingTrendPlot",height="500px"))
        )
      )
    }else if(active_eda_mode()=="bmi_distribution"){
      tagList(
        h3("BMI Distribution by Category"),
        fluidRow(
          column(width=6,
                 box(width=12,
                     radioButtons("bmi_country_selection","Display for:",
                                  choices=c("All Countries"="all",
                                            "Select Countries"="selected"),
                                  selected="all",
                                  inline=TRUE)
                 )
          ),
          column(width=6,
                 conditionalPanel(
                   condition="input.bmi_country_selection=='selected'",
                   box(width=12,
                       selectInput("selected_bmi_countries","Select Country(s):",
                                   choices=NULL,
                                   multiple=TRUE)
                   )
                 )
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("bmiDistributionPlot",height="400px"))
        )
      )
    }else if(active_eda_mode()=="income_trends"){
      tagList(
        h3("Income Composition of Resources Trends"),
        fluidRow(
          box(width=12,
              radioButtons("income_trend_status","Group by Development Status:",
                           choices=c("Overall"="All",
                                     "Developed"="Developed",
                                     "Developing"="Developing"),
                           selected="All",
                           inline=TRUE)
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("incomeTrendPlot",height="500px"))
        )
      )
    }else if(active_eda_mode()=="gdp_trends"){
      tagList(
        h3("GDP Trends"),
        fluidRow(
          box(width=4,
              radioButtons("gdp_display_mode","Display Mode:",
                           choices=c("By Development Status"="status_group",
                                     "Top/Bottom N Countries"="top_bottom_countries"),
                           selected="status_group",
                           inline=TRUE)
          ),
          conditionalPanel(
            condition="input.gdp_display_mode=='status_group'",
            box(width=8,
                radioButtons("gdp_trend_status","Group by Development Status:",
                             choices=c("Overall"="All",
                                       "Developed"="Developed",
                                       "Developing"="Developing"),
                             selected="All",
                             inline=TRUE)
            )
          ),
          conditionalPanel(
            condition="input.gdp_display_mode=='top_bottom_countries'",
            box(width=4,
                numericInput("num_countries_gdp","Number of Countries (N):",value=5,min=1,max=20)
            ),
            box(width=4,
                radioButtons("gdp_rank_type","Show:",
                             choices=c("Top N by Average GDP"="top",
                                       "Bottom N by Average GDP"="bottom"),
                             selected="top")
            )
          )
        ),
        fluidRow(
          box(width=12,plotlyOutput("gdpTrendPlot",height="500px"))
        )
      )
    }
  })
  
  output$eda_analysis_title<-renderText({
    req(active_eda_mode()=="country_status")
    
    if(active_eda_status()=="Developed"){
      "Analysis of Developed Countries"
    }else if(active_eda_status()=="Developing"){
      "Analysis of Developing Countries"
    }else{
      "Please select a country status for analysis."
    }
  })
  
  observe({
    req(active_eda_mode()=="country_status")
    excluded_cols<-c("Country","Year","Status")
    available_factors<-names(df)[!names(df)%in%excluded_cols]
    # Set a default selected value
    default_selected_factor <- if("Life.expectancy"%in%available_factors){
      "Life.expectancy"
    }else if(length(available_factors)>0){
      available_factors[1]
    }else{
      NULL
    }
    updateSelectInput(session,"factor_eda",choices=available_factors, selected = default_selected_factor)
  })
  
  observe({
    req(active_eda_mode()=="country_status",active_eda_status()!="")
    filtered_countries<-df%>%
      filter(Status==active_eda_status())%>%
      pull(Country)%>%
      unique()%>%
      sort()
    updateSelectInput(session,"selected_country_eda",choices=filtered_countries)
  })
  
  output$factor_distribution_eda<-renderPlotly({
    req(active_eda_mode()=="country_status",active_eda_status()!="",input$factor_eda)
    filtered_data<-df%>%filter(Status==active_eda_status())
    plot_title_suffix<-""
    
    if(input$visualize_by_country=="yes"&&!is.null(input$selected_country_eda)&&input$selected_country_eda!=""){
      filtered_data<-filtered_data%>%filter(Country==input$selected_country_eda)
      plot_title_suffix<-paste0(" (Country: ",input$selected_country_eda,")")
      if(nrow(filtered_data)==0){
        return(plotly_empty()%>%layout(title=paste0("No data for ",input$selected_country_eda," for the selected factor.")))
      }
    }
    
    selected_factor<-input$factor_eda
    if(!is.null(selected_factor)&&selected_factor!=""){
      if(is.numeric(filtered_data[[selected_factor]])){
        p<-ggplot(filtered_data,aes(x=.data[[selected_factor]])) +
          geom_histogram(binwidth=diff(range(filtered_data[[selected_factor]],na.rm=TRUE))/30,fill="lightblue",color="black") +
          labs(title=paste("Distribution of",str_to_title(gsub("\\."," ",selected_factor)),plot_title_suffix),
               x=str_to_title(gsub("\\."," ",selected_factor)),
               y="Count") +
          theme_minimal()
      }else{
        p<-ggplot(filtered_data,aes(x=.data[[selected_factor]])) +
          geom_bar(fill="coral",color="black") +
          labs(title=paste("Distribution of",str_to_title(gsub("\\."," ",selected_factor)),plot_title_suffix),
               x=str_to_title(gsub("\\."," ",selected_factor)),
               y="Count") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=45,hjust=1))
      }
      ggplotly(p)
    }else{
      plotly_empty()%>%layout(title="Please select a factor to visualize.")
    }
  })
  output$dataTable<-DT::renderDataTable({
    datatable(df,options=list(pageLength=10,scrollX=TRUE))
  })
  
  output$summaryStats<-renderPrint({summary(df)})
  output$structure<-renderPrint({str(df)})
  
  output$lifeExpMap<-renderPlotly({
    df_agg<-df%>%
      group_by(Country)%>%
      summarise(Avg_Life_Expectancy=mean(Life.expectancy,na.rm=TRUE))
    
    world_map<-map_data("world")
    country_mapping<-data.frame(
      dataset_name=c(
        "United States of America","United Kingdom of Great Britain and Northern Ireland",
        "Russian Federation","Iran (Islamic Republic of)","Republic of Korea",
        "Democratic People's Republic of Korea","Republic of Moldova","The former Yugoslav republic of Macedonia",
        "Bolivia (Plurinational State of)","Venezuela (Bolivarian Republic of)","Democratic Republic of the Congo",
        "Lao People's Democratic Republic","Syrian Arab Republic","Côte d'Ivoire","Cabo Verde"
      ),
      map_name=c(
        "USA","UK","Russia","Iran","South Korea","North Korea","Moldova","Macedonia", "Bolivia","Venezuela","Democratic Republic of the Congo","Laos","Syria","Ivory Coast","Cape Verde"
      ),
      stringsAsFactors=FALSE
    )
    
    df_agg_mapped<-df_agg%>%
      mutate(
        Country_Mapped=case_when(
          Country%in%country_mapping$dataset_name~
            country_mapping$map_name[match(Country,country_mapping$dataset_name)],
          TRUE~Country
        )
      )
    
    world_data<-left_join(world_map,df_agg_mapped,by=c("region"="Country_Mapped"))
    map_plot<-ggplot(data=world_data,aes(x=long,y=lat,group=group,fill=Avg_Life_Expectancy,
                                         text=paste("Country:",region,"<br>Avg. Life Expectancy:",
                                                    ifelse(is.na(Avg_Life_Expectancy),"No Data",
                                                           round(Avg_Life_Expectancy,2))))) +
      geom_polygon(color="white",size=0.2) +
      scale_fill_viridis_c(option="plasma",name="Avg. Life Expectancy",na.value="grey90") +
      labs(title="Average Life Expectancy Across Countries") +
      theme_void() +
      theme(
        plot.title=element_text(hjust=0.5,size=16),
        legend.position="bottom",
        legend.key.width=unit(1.5,"cm")
      )
    ggplotly(map_plot,tooltip="text")%>%
      layout(
        geo=list(showframe=FALSE,showcoastlines=FALSE,projection=list(type='equirectangular')),
        title=list(text="Average Life Expectancy Across Countries",font=list(size=16))
      )
  })
  
  output$lifeExpMapDescription<-renderText({
    "The map shows that North America, Western Europe, Australia, and East Asia (especially Japan and South Korea) have higher life expectancy. In contrast, Sub-Saharan Africa has the lowest life expectancy (dark purple areas). Regions like South America, Southeast Asia, and parts of Central Asia fall in the moderate range."
  })
  
  output$conclude<-renderText({
    "European tends to have longer Life Expetancy than Africa and Asia.
     Life Expectancy tends to increase each year.
    Adult Mortality and HIV have negative correlation while Schooling, Income and BMI have positive correlation."
  })
  
  output$overallLifeExpTrend<-renderPlotly({
    overall_data<-df%>%
      group_by(Year)%>%
      summarise(Avg_Life_Expectancy=mean(Life.expectancy,na.rm=TRUE))
    
    plot_ly(overall_data,x=~Year,y=~Avg_Life_Expectancy,type='scatter',mode='lines+markers',
            line=list(color='blue',width=3),
            marker=list(color='darkblue',size=6),
            hovertemplate=paste(
              "<b>Year:</b> %{x}<br>",
              "<b>Average Global Life Expectancy:</b> %{y:.2f}<br>",
              "<extra></extra>"
            )) %>%
      layout(title="Average Life Expectancy Across All Countries Over Years",
             xaxis=list(title="Year"),
             yaxis=list(title="Life Expectancy (years)"))
  })
  
  output$indoLifeExpTrend<-renderPlotly({
    indo_data<-df%>%
      filter(Country=="Indonesia")%>%
      group_by(Year)%>%
      summarise(Avg_Life_Expectancy=mean(Life.expectancy,na.rm=TRUE))
    
    plot_ly(indo_data,x=~Year,y=~Avg_Life_Expectancy,type='scatter',mode='lines+markers',
            line=list(color='green',width=3),
            marker=list(color='darkgreen',size=6),
            hovertemplate=paste(
              "<b>Year:</b> %{x}<br>",
              "<b>Life Expectancy:</b> %{y:.2f}<br>",
              "<extra></extra>"
            )) %>%
      layout(title="Life Expectancy in Indonesia Over Years",
             xaxis=list(title="Year"),
             yaxis=list(title="Life Expectancy (years)"))
  })
  
  output$lifeExpFacetTrend<-renderPlotly({
    req(input$num_countries_facet,input$country_rank_type)
    
    avg_life_exp_by_country<-df%>%
      group_by(Country)%>%
      summarise(Avg_Life_Expectancy=mean(Life.expectancy,na.rm=TRUE))%>%
      ungroup()
    
    if(input$country_rank_type=="top"){
      selected_countries<-avg_life_exp_by_country%>%
        arrange(desc(Avg_Life_Expectancy))%>%
        head(input$num_countries_facet)%>%
        pull(Country)
    }else{
      selected_countries<-avg_life_exp_by_country%>%
        arrange(Avg_Life_Expectancy)%>%
        head(input$num_countries_facet)%>%
        pull(Country)
    }
    plot_data<-df%>%
      filter(Country%in%selected_countries)%>%
      group_by(Country,Year)%>%
      summarise(Life.expectancy=mean(Life.expectancy,na.rm=TRUE),.groups='drop')%>%
      ungroup()
    
    p<-ggplot(plot_data,aes(x=Year,y=Life.expectancy,color=Country,group=Country)) +
      geom_line(size=1) +
      geom_point(size=2) +
      facet_wrap(~Country,scales="free_y",ncol=2) +
      labs(title=paste("Life Expectancy Trends for",input$country_rank_type,input$num_countries_facet,"Countries"),
           x="Year",
           y="Life Expectancy (years)") +
      theme_minimal() +
      theme(legend.position="none",
            strip.text=element_text(size=10,face="bold"),
            plot.title=element_text(hjust=0.5,size=16),
            axis.title=element_text(size=12),
            axis.text=element_text(size=10))
    ggplotly(p,tooltip=c("x","y","color"))%>%
      layout(hovermode="x unified")
  })
  
  observe({
    req(active_eda_mode()=="disease_comparison")
    all_countries<-unique(df$Country)%>%sort()
    updateSelectInput(session,"selected_diseases_countries",choices=all_countries)
  })
  
  observe({
    req(active_eda_mode()=="bmi_distribution")
    all_countries<-unique(df$Country)%>%sort()
    updateSelectInput(session,"selected_bmi_countries",choices=all_countries)
  })
  
  output$diseaseComparisonPlot<-renderPlotly({
    req(active_eda_mode()=="disease_comparison",input$selected_diseases_factors)
    plot_data<-df
    if(input$disease_country_selection=="selected"){
      req(input$selected_diseases_countries)
      plot_data<-plot_data%>%filter(Country%in%input$selected_diseases_countries)
      req(nrow(plot_data)>0)
    }
    cols_to_select<-c("Year",input$selected_diseases_factors)
    if(input$disease_country_selection=="selected"){
      cols_to_select<-unique(c("Country",cols_to_select))
    }
    req(all(cols_to_select%in%names(plot_data)))
    plot_data_long<-plot_data%>%
      select(all_of(cols_to_select))%>%
      pivot_longer(
        cols=all_of(input$selected_diseases_factors),
        names_to="Disease",
        values_to="Value"
      )
    if(input$disease_country_selection=="all"){
      plot_data_final<-plot_data_long%>%
        group_by(Year,Disease)%>%
        summarise(Avg_Value=mean(Value,na.rm=TRUE),.groups='drop')
    }else{
      plot_data_final<-plot_data_long%>%
        group_by(Year,Country,Disease)%>%
        summarise(Avg_Value=mean(Value,na.rm=TRUE),.groups='drop')
    }
    plot_data_final<-plot_data_final%>%
      filter(!is.na(Avg_Value)&!is.nan(Avg_Value))
    p<-ggplot(plot_data_final,aes(x=Year,y=Avg_Value,color=Disease,group=Disease)) +
      geom_line(size=1) +
      geom_point(size=2) +
      labs(title=NULL,
           x="Year",
           y="Disease Value",
           color="Disease") +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5,size=16),
            axis.title=element_text(size=12),
            axis.text=element_text(size=10),
            legend.position="bottom")
    
    if(input$disease_country_selection=="selected"&&length(unique(plot_data_final$Country))>1){
      p<-p+facet_wrap(~Country,scales="free_y",ncol=2)
    }
    tooltip_cols<-c("x","y","color")
    if("Country"%in%names(plot_data_final)&&input$disease_country_selection=="selected"){
      tooltip_cols<-c(tooltip_cols,"Country")
    }
    ggplotly(p,tooltip=tooltip_cols)%>%
      layout(hovermode="x unified")
  })
  
  output$adultMortalityHist<-renderPlotly({
    p<-ggplot(df,aes(x=Adult.Mortality)) +
      geom_histogram(binwidth=50,fill="lightblue",color="black") +
      labs(title="Adult Mortality Distribution",x="Adult Mortality",y="Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$infantDeathHist<-renderPlotly({
    p<-ggplot(df,aes(x=infant.deaths)) +
      geom_histogram(binwidth=50,fill="lightcoral",color="black") +
      labs(title="Infant Deaths Distribution",x="Infant Deaths",y="Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$underFiveDeathHist<-renderPlotly({
    p<-ggplot(df,aes(x=under.five.deaths)) +
      geom_histogram(binwidth=50,fill="lightgreen",color="black") +
      labs(title="Under-five Deaths Distribution",x="Under-five Deaths",y="Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$mortalityTrendPlot<-renderPlotly({
    plot_data<-df%>%
      filter(Year>=input$year_range[1]&Year<=input$year_range[2])
    if(input$mortality_trend_status!="All"){
      plot_data<-plot_data%>%filter(Status==input$mortality_trend_status)
    }
    if(input$mortality_trend_status=="All"){
      aggregated_data<-plot_data%>%
        group_by(Year,Status)%>%
        summarise(
          Adult.Mortality=mean(Adult.Mortality,na.rm=TRUE),
          infant.deaths=mean(infant.deaths,na.rm=TRUE),
          under.five.deaths=mean(under.five.deaths,na.rm=TRUE),
          .groups='drop'
        )%>%
        pivot_longer(
          cols=c("Adult.Mortality","infant.deaths","under.five.deaths"),
          names_to="Mortality_Type",
          values_to="Average_Value"
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Value,color=Mortality_Type,group=Mortality_Type)) +
        geom_line(size=1) +
        geom_point(size=2) +
        facet_wrap(~Status,scales="free_y",ncol=2) +
        labs(title=paste("Average Mortality Trends by Year and Development Status (",input$year_range[1],"-",input$year_range[2],")"),
             x="Year",
             y="Average Mortality Value",
             color="Mortality Type") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="bottom",
              strip.text=element_text(size=12,face="bold"))
      
    }else{
      aggregated_data<-plot_data%>%
        group_by(Year)%>%
        summarise(
          Adult.Mortality=mean(Adult.Mortality,na.rm=TRUE),
          infant.deaths=mean(infant.deaths,na.rm=TRUE),
          under.five.deaths=mean(under.five.deaths,na.rm=TRUE),
          .groups='drop'
        )%>%
        pivot_longer(
          cols=c("Adult.Mortality","infant.deaths","under.five.deaths"),
          names_to="Mortality_Type",
          values_to="Average_Value"
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Value,color=Mortality_Type,group=Mortality_Type)) +
        geom_line(size=1) +
        geom_point(size=2) +
        labs(title=paste("Average Mortality Trends by Year for",input$mortality_trend_status,"Countries (",input$year_range[1],"-",input$year_range[2],")"),
             x="Year",
             y="Average Mortality Value",
             color="Mortality Type") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="bottom")
    }
    
    ggplotly(p,tooltip=c("x","y","color"))%>%
      layout(hovermode="x unified")
  })
  
  output$schoolingTrendPlot<-renderPlotly({
    plot_data<-df
    if(input$schooling_trend_status!="All"){
      plot_data<-plot_data%>%filter(Status==input$schooling_trend_status)
    }
    
    if(input$schooling_trend_status=="All"){
      aggregated_data<-plot_data%>%
        group_by(Year,Status)%>%
        summarise(
          Average_Schooling=mean(Schooling,na.rm=TRUE),
          .groups='drop'
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Schooling,color=Status,group=Status)) +
        geom_line(size=1) +
        geom_point(size=2) +
        facet_wrap(~Status,scales="free_y",ncol=2) +
        labs(title="Average Schooling Trends by Year and Development Status",
             x="Year",
             y="Average Schooling (years)",
             color="Development Status") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="bottom",
              strip.text=element_text(size=12,face="bold"))
    }else{
      aggregated_data<-plot_data%>%
        group_by(Year)%>%
        summarise(
          Average_Schooling=mean(Schooling,na.rm=TRUE),
          .groups='drop'
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Schooling,group=1)) +
        geom_line(size=1,color="purple") +
        geom_point(size=2,color="darkred") +
        labs(title=paste("Average Schooling Trends by Year for",input$schooling_trend_status,"Countries"),
             x="Year",
             y="Average Schooling (years)") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="none")
    }
    ggplotly(p,tooltip=c("x","y","color"))%>%
      layout(hovermode="x unified")
  })
  
  output$bmiDistributionPlot<-renderPlotly({
    req(active_eda_mode()=="bmi_distribution")
    plot_data<-df
    plot_title_suffix<-""
    grouping_cols<-c("BMI_Category")
    
    if(input$bmi_country_selection=="selected"){
      req(input$selected_bmi_countries)
      
      if(length(input$selected_bmi_countries)>2){
        return(plotly_empty()%>%layout(title="Please select a maximum of two countries for comparison in BMI distribution.",
                                       annotations=list(
                                         x=0.5,
                                         y=0.5,
                                         text="Please select a maximum of two countries for comparison.",
                                         xref="paper",
                                         yref="paper",
                                         showarrow=FALSE,
                                         font=list(size=16)
                                       )
        ))
      }
      plot_data<-plot_data%>%filter(Country%in%input$selected_bmi_countries)
      req(nrow(plot_data)>0)
      
      if(length(input$selected_bmi_countries)==1){
        plot_title_suffix<-paste0(" for ",input$selected_bmi_countries)
      }else{
        plot_title_suffix<-paste0(" for ",paste(input$selected_bmi_countries,collapse=" and "))
        grouping_cols<-c("Country",grouping_cols)
      }
    }else{
      plot_title_suffix<-" for All Countries"
    }
    df_bmi_categorized<-plot_data%>%
      mutate(
        BMI_Category=cut(BMI,
                         breaks=c(0,18.5,24.9,29.9,Inf),
                         labels=c("Underweight (<18.5)","Normal Weight (18.5-24.9)","Overweight (25-29.9)","Obesity (>=30)"),
                         right=TRUE,
                         include.lowest=TRUE)
      )%>%
      filter(!is.na(BMI_Category))
    
    bmi_counts<-df_bmi_categorized%>%
      group_by(!!!syms(grouping_cols))%>%
      summarise(Count=n(),.groups='drop')%>%
      arrange(factor(BMI_Category,levels=c("Underweight (<18.5)","Normal Weight (18.5-24.9)","Overweight (25-29.9)","Obesity (>=30)")))
    
    if(input$bmi_country_selection=="selected"&&length(input$selected_bmi_countries)==2){
      p<-ggplot(bmi_counts,aes(x=BMI_Category,y=Count,fill=Country,
                               text=paste("Category:",BMI_Category,"<br>Country:",Country,"<br>Count:",Count))) +
        geom_bar(stat="identity",color="black",position="dodge") +
        labs(title=paste("Distribution of BMI Categories",plot_title_suffix),
             x="BMI Category",
             y="Number of Entries",
             fill="Country") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text.x=element_text(angle=45,hjust=1,size=10)) +
        scale_fill_brewer(palette="Paired")
    }else{
      p<-ggplot(bmi_counts,aes(x=BMI_Category,y=Count,fill=BMI_Category,
                               text=paste("Category:",BMI_Category,"<br>Count:",Count))) +
        geom_bar(stat="identity",color="black") +
        labs(title=paste("Distribution of BMI Categories",plot_title_suffix),
             x="BMI Category",
             y="Number of Entries") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text.x=element_text(angle=45,hjust=1,size=10),
              legend.position="none") +
        scale_fill_brewer(palette="Set2")
    }
    ggplotly(p,tooltip="text")%>%
      layout(hovermode="x unified")
  })
  
  output$incomeTrendPlot<-renderPlotly({
    plot_data<-df
    if(input$income_trend_status!="All"){
      plot_data<-plot_data%>%filter(Status==input$income_trend_status)
    }
    if(input$income_trend_status=="All"){
      aggregated_data<-plot_data%>%
        group_by(Year,Status)%>%
        summarise(
          Average_Income_Composition=mean(Income.composition.of.resources,na.rm=TRUE),
          .groups='drop'
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Income_Composition,color=Status,group=Status)) +
        geom_line(size=1) +
        geom_point(size=2) +
        facet_wrap(~Status,scales="free_y",ncol=2) +
        labs(title="Average Income Composition of Resources Trends by Year and Development Status",
             x="Year",
             y="Average Income Composition",
             color="Development Status") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="bottom",
              strip.text=element_text(size=12,face="bold"))
      
    }else{
      aggregated_data<-plot_data%>%
        group_by(Year)%>%
        summarise(
          Average_Income_Composition=mean(Income.composition.of.resources,na.rm=TRUE),
          .groups='drop'
        )
      p<-ggplot(aggregated_data,aes(x=Year,y=Average_Income_Composition,group=1)) +
        geom_line(size=1,color="darkblue") +
        geom_point(size=2,color="blue") +
        labs(title=paste("Average Income Composition of Resources Trends by Year for",input$income_trend_status,"Countries"),
             x="Year",
             y="Average Income Composition") +
        theme_minimal() +
        theme(plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10),
              legend.position="none")
    }
    ggplotly(p,tooltip=c("x","y","color"))%>%
      layout(hovermode="x unified")
  })
  
  output$gdpTrendPlot<-renderPlotly({
    plot_data<-df
    if(input$gdp_display_mode=="status_group"){
      if(input$gdp_trend_status!="All"){
        plot_data<-plot_data%>%filter(Status==input$gdp_trend_status)
      }
      
      if(input$gdp_trend_status=="All"){
        aggregated_data<-plot_data%>%
          group_by(Year,Status)%>%
          summarise(
            Average_GDP=mean(GDP,na.rm=TRUE),
            .groups='drop'
          )
        
        p<-ggplot(aggregated_data,aes(x=Year,y=Average_GDP,color=Status,group=Status)) +
          geom_line(size=1) +
          geom_point(size=2) +
          facet_wrap(~Status,scales="free_y",ncol=2) +
          labs(title="Average GDP Trends by Year and Development Status",
               x="Year",
               y="Average GDP",
               color="Development Status") +
          theme_minimal() +
          theme(plot.title=element_text(hjust=0.5,size=16),
                axis.title=element_text(size=12),
                axis.text=element_text(size=10),
                legend.position="bottom",
                strip.text=element_text(size=12,face="bold"))
        
      }else{
        aggregated_data<-plot_data%>%
          group_by(Year)%>%
          summarise(
            Average_GDP=mean(GDP,na.rm=TRUE),
            .groups='drop'
          )
        
        p<-ggplot(aggregated_data,aes(x=Year,y=Average_GDP,group=1)) +
          geom_line(size=1,color="darkgreen") +
          geom_point(size=2,color="green") +
          labs(title=paste("Average GDP Trends by Year for",input$gdp_trend_status,"Countries"),
               x="Year",
               y="Average GDP") +
          theme_minimal() +
          theme(plot.title=element_text(hjust=0.5,size=16),
                axis.title=element_text(size=12),
                axis.text=element_text(size=10),
                legend.position="none")
      }
    }else{
      req(input$num_countries_gdp,input$gdp_rank_type)
      
      avg_gdp_by_country<-df%>%
        group_by(Country)%>%
        summarise(Avg_GDP=mean(GDP,na.rm=TRUE))%>%
        ungroup()
      
      if(input$gdp_rank_type=="top"){
        selected_countries<-avg_gdp_by_country%>%
          arrange(desc(Avg_GDP))%>%
          head(input$num_countries_gdp)%>%
          pull(Country)
      }else{
        selected_countries<-avg_gdp_by_country%>%
          arrange(Avg_GDP)%>%
          head(input$num_countries_gdp)%>%
          pull(Country)
      }
      
      plot_data_countries<-df%>%
        filter(Country%in%selected_countries)%>%
        group_by(Country,Year)%>%
        summarise(GDP=mean(GDP,na.rm=TRUE),.groups='drop')%>%
        ungroup()
      
      p<-ggplot(plot_data_countries,aes(x=Year,y=GDP,color=Country,group=Country)) +
        geom_line(size=1) +
        geom_point(size=2) +
        facet_wrap(~Country,scales="free_y",ncol=2) +
        labs(title=paste("GDP Trends for",input$gdp_rank_type,input$num_countries_gdp,"Countries"),
             x="Year",
             y="GDP") +
        theme_minimal() +
        theme(legend.position="none",
              strip.text=element_text(size=10,face="bold"),
              plot.title=element_text(hjust=0.5,size=16),
              axis.title=element_text(size=12),
              axis.text=element_text(size=10))
    }
    ggplotly(p,tooltip=c("x","y","color"))%>%
      layout(hovermode="x unified")
  })
  
  output$corrMatrix<-renderPlotly({
    numeric_vars<-df%>%select(where(is.numeric))
    corr_matrix<-cor(numeric_vars,use="complete.obs")
    
    plot_ly(
      z=corr_matrix,
      x=colnames(corr_matrix),
      y=rownames(corr_matrix),
      type="heatmap",
      colorscale=list(
        c(0,"darkblue"),c(0.25,"blue"),c(0.5,"white"),c(0.75,"red"),c(1,"darkred")
      ),
      zmin=-1,
      zmax=1,
      showscale=TRUE,
      hovertemplate="X: %{x}<br>Y: %{y}<br>Correlation: %{z:.3f}<extra></extra>"
    )%>%layout(
      title=list(text="Correlation Matrix",x=0.5,font=list(size=16)),
      xaxis=list(
        title="",
        tickangle=-45,
        tickfont=list(size=10),
        side="bottom"
      ),
      yaxis=list(
        title="",
        tickfont=list(size=10)
      ),
      margin=list(l=200,r=50,b=150,t=80),
      plot_bgcolor="white",
      paper_bgcolor="white"
    )
  })
  
  output$regressionCountryBox<-renderValueBox({
    req(input$devStatus)
    filtered<-df%>%filter(Status==input$devStatus)
    total_countries<-length(unique(filtered$Country))
    valueBox(
      total_countries,
      "NUMBER OF COUNTRIES",
      icon=icon("flag"),
      color="purple"
    )
  })
  
  output$regressionDataBox<-renderValueBox({
    req(input$devStatus,input$xvar_reg,input$yvar_reg)
    filtered<-df%>%filter(Status==input$devStatus)
    valid_data<-filtered%>%
      filter(!is.na(.data[[input$xvar_reg]])&!is.na(.data[[input$yvar_reg]]))%>%
      nrow()
    
    valueBox(
      valid_data,
      "NUMBER OF ROWS",
      icon=icon("table"),
      color="teal"
    )
  })
  
  output$regressionStatusBox<-renderValueBox({
    req(input$devStatus)
    
    valueBox(
      input$devStatus,
      "DEVELOPMENT STATUS",
      icon=icon("chart-line"),
      color="blue"
    )
  })
  
  output$regressionCorrelationBox<-renderValueBox({
    req(input$devStatus,input$xvar_reg,input$yvar_reg)
    filtered<-df%>%filter(Status==input$devStatus)
    
    valid_rows<-filtered%>%
      filter(!is.na(.data[[input$xvar_reg]])&!is.na(.data[[input$yvar_reg]]))
    
    correlation_val<-NA
    if(nrow(valid_rows)>1){
      correlation_val<-cor(valid_rows[[input$xvar_reg]],valid_rows[[input$yvar_reg]],use="complete.obs")
    }
    
    valueBox(
      ifelse(is.na(correlation_val),"N/A",round(correlation_val,3)),
      "CORRELATION",
      icon=icon("link"),
      color="maroon"
    )
  })
  
  output$regByStatusPlot<-renderPlotly({
    req(input$devStatus,input$xvar_reg,input$yvar_reg)
    filtered<-df%>%filter(Status==input$devStatus)
    
    valid_rows<-filtered%>%
      filter(!is.na(.data[[input$xvar_reg]])&!is.na(.data[[input$yvar_reg]]))
    
    if(nrow(valid_rows)>1){
      model<-lm(as.formula(paste(input$yvar_reg,"~",input$xvar_reg)),data=valid_rows)
      valid_rows$pred<-predict(model)
      
      plot_ly(data=valid_rows,x=~get(input$xvar_reg),y=~get(input$yvar_reg),
              type='scatter',mode='markers',name="Actual",
              marker=list(color='blue',size=6))%>%
        add_lines(x=~get(input$xvar_reg),y=~pred,name="Fitted",
                  line=list(color='red',width=2))%>%
        layout(
          title=list(
            text=paste0("Regression of ",input$yvar_reg," on ",input$xvar_reg,
                        "<br><sub>for ",input$devStatus," Countries</sub>"),
            font=list(size=14)
          ),
          xaxis=list(title=input$xvar_reg),
          yaxis=list(title=input$yvar_reg)
        )
    }else{
      plotly_empty()%>%layout(title="No sufficient data available for the selected development status and variables to perform regression.")
    }
  })
  
  create_regression_plot<-function(predictor_col,predictor_name,color='rgba(31, 119, 180, 0.6)'){
    data<-df%>%
      filter(!is.na(Life.expectancy)&!is.na(.data[[predictor_col]]))
    
    if(nrow(data)<2){
      return(plotly_empty()%>%layout(title="Insufficient data for regression analysis"))
    }
    
    formula_str<-paste("Life.expectancy~",predictor_col)
    model<-lm(as.formula(formula_str),data=data)
    data$predicted<-predict(model)
    
    correlation<-round(cor(data$Life.expectancy,data[[predictor_col]],use="complete.obs"),3)
    
    plot_ly(data,x=~get(predictor_col),y=~Life.expectancy,
            type='scatter',mode='markers',name='Observed',
            marker=list(color=color,size=6,opacity=0.7),
            hovertemplate=paste0(
              "<b>",predictor_name,":</b> %{x}<br>",
              "<b>Life Expectancy:</b> %{y:.2f}<br>",
              "<extra></extra>"
            ))%>%
      add_lines(x=~get(predictor_col),y=~predicted,name='Regression Line',
                line=list(color='red',width=2))%>%
      layout(
        title=list(
          text=paste0(predictor_name,"<br><sub>Correlation = ",correlation,"</sub>"),
          font=list(size=14)
        ),
        xaxis=list(title=predictor_name,titlefont=list(size=12)),
        yaxis=list(title="Life Expectancy (years)",titlefont=list(size=12)),
        showlegend=FALSE,
        hovermode='closest',
        margin=list(t=60)
      )
  }
  
  output$proofPlot1<-renderPlotly({
    create_regression_plot("Adult.Mortality","Adult Mortality",'rgba(255, 99, 132, 0.6)')
  })
  
  output$proofPlot2<-renderPlotly({
    create_regression_plot("Schooling","Schooling",'rgba(54, 162, 235, 0.6)')
  })
  
  output$proofPlot3<-renderPlotly({
    create_regression_plot("Income.composition.of.resources","Income Composition of Resources",'rgba(75, 192, 192, 0.6)')
  })
  
  output$proofPlot4<-renderPlotly({
    create_regression_plot("HIV.AIDS","HIV/AIDS",'rgba(255, 206, 86, 0.6)')
  })
  
  output$proofPlot5<-renderPlotly({
    create_regression_plot("BMI","BMI",'rgba(153, 102, 255, 0.6)')
  })
  
}

shinyApp(ui,server)