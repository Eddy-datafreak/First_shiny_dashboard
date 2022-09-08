###############################################################################
###############################################################################
###############################################################################
#This is a dashboard which can be used as a template for health care research
#and is only a simulated version. And the data that is used in this demo is purely
#fictional and should be cited wherever necessary. It is also subject to changes
#and any constructive suggestions/comments are welcome. Overall I hope its useful
#and reaches the right audience.
###############################################################################
###############################################################################
##############################THANK YOU########################################
###############################################################################
###############################################################################




#clear all
rm(list=ls())

#setting directory
#setwd("....//")

#Importing libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)
library(hrbrthemes)
library(DT)
library(DataExplorer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

#Before import the data make sure that the data is in the working directory.
#A good practice when creating a shiny dashboard would be to have a separate folder that includes the script and the data

#Importing data
df1<-read.csv("df.csv")

#The variables are purely fictional and can be replaced by any other variable
#Also we need to create the variables listed below and will provide the same but if you already have your varslist, then go ahead with the same :p
#Re-coding and changing the class of variables wherever necessary
df1$age_grp<-case_when(df1$age >=0 & df1$age <=10 ~ "0-10",
                       df1$age >=11 & df1$age <=20 ~ "11-20",
                       df1$age >=21 & df1$age <=30 ~ "21-30",
                       df1$age >=31 & df1$age <=40 ~ "31-40",
                       df1$age >=41 & df1$age <=50 ~ "41-50",
                       df1$age >=51 & df1$age <=60 ~ "51-60",
                       df1$age >=61 & df1$age <=70 ~ "61-70",
                       df1$age >=71 & df1$age <=80 ~ "71-80",
                       df1$age >=81 & df1$age <=90 ~ "81-90",
                       df1$age >=91 & df1$age <=100 ~ "91-100")
df1$age_grp<-factor(df1$age_grp)
df1$gender<-c("Male","Female")
df1$gender<-factor(df1$gender)
df1$week<-factor(df1$week)
df1$forms<-factor(df1$forms)
df1$token<-factor(df1$token)
df1$blood_draw<-factor(df1$blood_draw)
df1$fever<-factor(df1$fever)
df1$date_screened<-as.Date(df1$date_screened,format = "%d-%m-%Y")
df1$contact_form<-factor(df1$contact_form)
df1$bd_outcome<-factor(df1$bd_outcome)
df1$case_reporting<-factor(df1$case_reporting)
df1$vaccinated<-factor(df1$vaccinated)
df1$consent_for_vaccination<-factor(df1$consent_for_vaccination)
df1$vac_token<-factor(df1$vac_token)
df1$contraindi<-factor(df1$contraindi)
df1$SAE_1st3omins<-factor(df1$SAE_1st3omins)
df1$field_team<-factor(df1$field_team)
df1$clin_team<-factor(df1$clin_team)


#for word cloud
df1$reason_for_call<-factor(df1$reason_for_call,labels = c(1,2,3,4))
df1$reason_n<-as.numeric(df1$reason_for_call)
#for word count
words<-aggregate(df1$reason_n, by=list(Category=df1$reason_for_call), FUN=sum)
words$Category<-c("Acute illness","Non-related illness","Other illness","Emergency")
df2<-data.frame(words$Category,words$x)

#
uxc.colors=c("#fefefe","#f4f2a8","#030303")
uxc.background="#00ccff"




#This builds the html interface
title<-tags$a(href="www.123abc.com",
              "Project title")###This points to the home page incase the client needs more information.

ui <- navbarPage(title = title,collapsible = T,theme=shinytheme("journal"),
                 tabPanel("About",
                          fluidPage(
                            setBackgroundColor(color = c("#F0D3D3","#CCC9FF"),
                                               gradient = "linear",
                                               direction = "bottom",shinydashboard = F),
                            tabsetPanel(
                              tabPanel("Brief overview",
                                       br(),
                                       br(),
                                       h4("1st Paragraph"),
                                       br(),
                                       h3("2nd Paragraph"),
                                       br(),
                                       h2("3rd Paragraph")),
                              tabPanel("Forms",
                                       h1("Study forms"),
                                       br(),
                                       br(),
                                       h3("Screening form"),
                                       br(),
                                       h4("Description..."),
                                       br(),
                                       br(),
                                       h3("Recruitment form"),
                                       br(),
                                       h4("Description..."),
                                       br(),
                                       br(),
                                       h3("Followup form"),
                                       br(),
                                       h4("Description..."),
                                       br(),
                                       br(),
                                       h3("Other details/notes"),
                                       br(),
                                       h4("Description..."),
                                       br(),
                                       br()
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Main Dashboard",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(
                                inputId = "study_fac1",
                                label = "Choose a facility to explore",
                                choices = c("Facility 1","Facility 2","Facility 3")),
                              br(),
                              radioButtons(
                                inputId = "form_status1",
                                label = "Select status",
                                choices = c("Due for screening","Screened",
                                            "Recruited")),
                              br(),
                              radioButtons(
                                inputId = "fever1",
                                label = "Select fever status",
                                choices = c("Yes","No")),
                              br(),
                              radioButtons(
                                inputId = "token",
                                label = "Select token status",
                                choices = c("Issued","Not issued"))
                            ),
                            
                            mainPanel(
                              box(plotOutput(outputId = "pt_status",width = 300,height = 250),title = "Patient status"),
                              box(plotlyOutput(outputId = "week",width = 300,height = 250),title = "Weekly distribution"),
                              dataTableOutput(outputId = "Table1"),height = "100%",
                              downloadButton(outputId = "download1", label = "Download")
                            )
                          )
                 ),
                 
                 tabPanel("Clinical team",
                          sidebarLayout(
                            sidebarPanel(
                              br(),
                              br(),
                              br(),
                              br(),
                              selectInput(inputId = "study_fac2",
                                          label = "Study Facility",
                                          choices = list("Facility 1","Facility 2","Facility 3"),
                                          width=300,selectize = T),
                              br(),
                              br(),
                              br(),
                              dateRangeInput(inputId = "date_range1",
                                             label = "Select date",
                                             start = "2021-01-01",
                                             min = min(df1$date_screened),
                                             max = Sys.Date(),
                                             weekstart = 1,
                                             format = "dd-mm-yyyy",
                                             separator = "to",
                                             autoclose = T)
                            ),
                            
                            mainPanel(
                              box(plotOutput(outputId = "gender"),title = "Gender"),
                              box(plotlyOutput(outputId = "age"),title = "Age distribution vs Form status"),
                              br(),
                              box(plotlyOutput(outputId = "agegrp"),title = "Age distribution by groups"),
                            )
                          )
                 ),
                 
                 tabPanel("Field team",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "study_fac3",
                                          label = "Study Facility",
                                          choices = list("Facility 1","Facility 2","Facility 3"),
                                          width=300,selectize = T),
                              br(),
                              radioButtons(inputId = "form_status2",
                                           label = "Select status",
                                           choices = c("Due for screening","Screened","Recruited")),
                              br(),
                              dateRangeInput(inputId = "date_range2",
                                             label = "Select date",
                                             start = "2021-01-01",
                                             min = min(df1$date_screened),
                                             max = Sys.Date(),
                                             weekstart = 1,
                                             format = "dd-mm-yyyy",
                                             separator = "to",
                                             autoclose = T)
                            ),
                            
                            mainPanel(
                              box(plotlyOutput(outputId = "fever",width = 350,height = 250),title = "Fever"),
                              box(plotlyOutput(outputId = "blood_draw1",width = 350,height = 250),title = "Blood test status"),
                              box(plotlyOutput(outputId = "ent_crf_status",width = 350,height = 250),title = "Form status"),
                              box(plotlyOutput(outputId = "con_form",width = 350,height = 250),title = "Contact status")
                            )
                          )
                 ),
                 
                 tabPanel("Lab team",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "study_fac4",
                                          label = "Study Facility",
                                          choices = list("Facility 1","Facility 2","Facility 3"),
                                          width=250,selectize = T),
                              br(),
                              radioButtons(
                                inputId = "fever2",
                                label = "Select fever option",
                                choices = c("Yes","No")),
                              br(),
                              dateRangeInput(inputId = "date_range3",
                                             label = "Select date",
                                             start = "2021-01-01",
                                             min = min(df1$date_screened),
                                             max = Sys.Date(),
                                             weekstart = 1,
                                             format = "dd-mm-yyyy",
                                             separator = "to",
                                             autoclose = T),
                              br(),
                              textOutput(outputId = "selected_var")
                            ),
                            
                            mainPanel(
                              box(plotlyOutput(outputId = "blood_draw2",width = 400,height = 350),title = "Blood test status"),
                              box(plotlyOutput(outputId = "bd_outcome",width = 400,height = 350),title = "Blood test result"),
                              br(),
                              dataTableOutput(outputId = "Table2"), height = "100%",
                              downloadButton(outputId = "download2", label = "Download")
                            )
                          )
                 ),
                 
                 tabPanel("Central information center",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "study_fac5",
                                          label = "Study Facility",
                                          choices = list("Facility 1","Facility 2","Facility 3"),
                                          width=250,selectize = T),
                              br(),
                              radioButtons(inputId = "fever3",
                                           label = "Select fever option",
                                           choices = c("Yes","No")),
                              br(),
                              dateRangeInput(inputId = "date_range4",
                                             label = "Select date",
                                             start = "2021-01-01",
                                             min = min(df1$date_screened),
                                             max = Sys.Date(),
                                             weekstart = 1,
                                             format = "dd-mm-yyyy",
                                             separator = "to",
                                             autoclose = T)
                            ),
                            
                            mainPanel(
                              box(wordcloud2Output(outputId = "reason_call"),title = "Reasons for calling"),
                              box(plotlyOutput(outputId = "call_gend"),title = "Reasons for calling vs Gender")
                            )
                          )
                 ),
                 
                 
                 tabPanel("Vaccine team",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "study_fac6",
                                          label = "Study Facility",
                                          choices = list("Facility 1","Facility 2","Facility 3"),
                                          width=250,selectize = T),
                              br(),
                              radioButtons(
                                inputId = "fever4",
                                label = "Select fever status",
                                choices = c("Yes","No")),
                              br(),
                              dateRangeInput(inputId = "date_range5",
                                             label = "Select date",
                                             start = "2021-01-01",
                                             min = min(df1$date_screened),
                                             max = Sys.Date(),
                                             weekstart = 1,
                                             format = "dd-mm-yyyy",
                                             separator = "to",
                                             autoclose = T),
                              br(),
                              radioButtons(inputId = "vac_cons",
                                           label = "Select consent status",
                                           choices = c("Yes","No"))
                              
                            ),
                            
                            mainPanel(
                              box(plotlyOutput(outputId = "vac_consent"),title = "Consent for vaccination"),
                              box(plotlyOutput(outputId = "vac_token"),title = "Token status"),
                              box(plotlyOutput(outputId = "contrindi"),title = "Contraindication"),
                              box(plotlyOutput(outputId = "sae"),title = "Any adverse events in 1st 30mins")
                            )
                          )
                 ),
                 
                 tabPanel("Data summary",
                          mainPanel(
                            print("Your data summary here")
                          )
                 )
                 
)




#This help render the output and respond to inputs such as filters
server<- function(input,output,session) {
  
  df<-reactive({
    df1
  })
  
  df_2<-reactive({
    df2
  })
  
  #Main dashboard outputs
  
  output$pt_status<-renderPlot({
    df() %>% 
      filter(study_fac==input$study_fac1) %>% 
      filter(fever==input$fever1) %>% 
      filter(token==input$token) %>% 
      ggplot(mapping = aes(x = forms))+
      geom_bar(fill="#69b3a2")+xlab("Forms")
  })
  
  output$week<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac1) %>% 
      filter(forms==input$form_status1) %>% 
      filter(fever==input$fever1) %>% 
      filter(token==input$token) %>% 
      ggplot(mapping = aes(x = week))+
      geom_bar(stat = "count")
  })
  
  
  #Clinical team outputs
  
  output$gender<-renderPlot({
    df() %>% 
      filter(study_fac==input$study_fac2) %>% 
      filter(date_screened==input$date_range1) %>% 
      ggplot(mapping = aes(x=week,y=gender,fill=gender))+
      geom_bar(position="stack",stat = "identity")+
      scale_color_viridis(discrete = T)+
      ylab("Gender")+xlab("Week")+
      theme_bw()+theme(strip.background.y = element_rect(inherit.blank = T),
                       axis.text.y = element_blank()) 
  })
  
  output$age<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac2) %>% 
      filter(date_screened==input$date_range1) %>% 
      ggplot(mapping = aes(x = age,fill=forms))+
      geom_histogram(bins = 10,alpha=.2,position="identity")+xlab("Age")
  })
  
  output$agegrp<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac2) %>% 
      filter(date_screened==input$date_range1) %>% 
      ggplot(mapping = aes(x=age_grp,fill=gender))+geom_bar(data=subset(df1,gender=="Male"))+
      geom_bar(data=subset(df1,gender=="Female"),aes(y=..count..*(-1)))+
      xlab(label ="Age groups")+ylab(label = "Month")+coord_flip() 
  })
  
  
  
  # Main Dashboard 
  
  output$Table1<-renderDataTable({
    df() %>% 
      filter(study_fac==input$study_fac1) %>% 
      filter(forms==input$form_status1) %>% 
      filter(fever==input$fever1) %>% 
      filter(token==input$token) %>% 
      datatable(df)
  })
  
  # Lab team
  
  output$Table2<-renderDataTable({
    df() %>% 
      filter(study_fac==input$study_fac4) %>% 
      filter(fever==input$fever2) %>% 
      datatable(df)
  })
  
  #For downloading the dataset with selected options on the sidebar layout
  output$download1 <- downloadHandler(
    filename = function() { 
      paste("dataset_", Sys.Date(), ".csv", sep="_")
    },
    content = function(file) {
      write.csv(df(), file)
    }
  )
  #For downloading the dataset with selected options on the sidebar layout
  output$download2 <- downloadHandler(
    filename = function() { 
      paste("dataset_", Sys.Date(), ".csv", sep="_")
    },
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
  
  #Field team
  
  output$fever<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac3) %>% 
      filter(forms==input$form_status2) %>% 
      filter(date_screened==input$date_range2) %>% 
      ggplot(mapping = aes(x=fever,group=forms))+geom_bar()+xlab("Fever")
    
  })
  
  output$blood_draw1<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac3) %>% 
      filter(forms==input$form_status2) %>% 
      filter(date_screened==input$date_range2) %>% 
      ggplot(mapping = aes(x=blood_draw))+geom_bar()+
      xlab("Blood test status")
  })
  
  output$ent_crf_status<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac3) %>% 
      filter(forms==input$form_status2) %>% 
      filter(date_screened==input$date_range2) %>% 
      ggplot(mapping = aes(x=case_reporting))+
      geom_bar()+
      xlab("Form status")
  })
  
  output$con_form<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac3) %>% 
      filter(forms==input$form_status2) %>% 
      filter(date_screened==input$date_range2) %>% 
      ggplot(mapping = aes(x=contact_form))+
      geom_bar(fill=c("#8DB6CD","#4F94CD"))+
      xlab("Contact form status")+
      theme_light()
  })
  
  #Lab team 
  
  output$blood_draw2<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac4) %>% 
      filter(fever==input$fever2) %>% 
      filter(date_screened==input$date_range3) %>% 
      ggplot(mapping = aes(x=blood_draw))+geom_bar()+
      xlab("Blood test status")
  })
  
  
  output$bd_outcome<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac4) %>% 
      filter(fever==input$fever2) %>% 
      filter(date_screened==input$date_range3) %>% 
      ggplot(mapping = aes(x=bd_outcome))+
      geom_bar(fill=c("#8DA6CD","#4F74CD"))+
      xlab("Test result")+
      theme_light()
  })
  
  
  #Central information team
  
  output$reason_call<-renderWordcloud2({
    df_2() %>% 
      wordcloud2(color = rep_len(uxc.colors,nrow(df2)),
                 backgroundColor = uxc.background,size=0.3,minSize = 10,
                 rotateRatio = 0)     
  })
  
  output$call_gend<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac5) %>% 
      filter(fever==input$fever3) %>% 
      filter(date_screened==input$date_range4) %>% 
      ggplot(mapping = aes(x=reason_for_call,fill=gender))+
      geom_bar()+
      coord_flip()+
      xlab("Reasons for calling")
    
  })
  
  #Vaccine team
  
  output$vac_consent<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac6) %>% 
      filter(fever==input$fever4) %>% 
      filter(date_screened==input$date_range5) %>% 
      filter(consent_for_vaccination==input$vac_cons) %>% 
      ggplot(mapping = aes(x=consent_for_vaccination,fill=gender))+
      geom_bar()+
      xlab("Consent for vaccination")
  })
  
  output$vac_token<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac6) %>% 
      filter(fever==input$fever4) %>% 
      filter(date_screened==input$date_range5) %>% 
      filter(consent_for_vaccination==input$vac_cons) %>% 
      ggplot(df1,mapping = aes(x=vac_token))+
      geom_bar(fill=c("#96CDCD","#6CA6CD"))+
      xlab("Token status")
  })
  
  output$contrindi<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac6) %>% 
      filter(fever==input$fever4) %>% 
      filter(date_screened==input$date_range5) %>% 
      filter(consent_for_vaccination==input$vac_cons) %>% 
      ggplot(df1,mapping = aes(x=contraindi,fill=consent_for_vaccination))+
      geom_bar(position = "fill")+
      xlab("Token status")
  })
  
  output$sae<-renderPlotly({
    df() %>% 
      filter(study_fac==input$study_fac6) %>% 
      filter(fever==input$fever4) %>% 
      filter(date_screened==input$date_range5) %>% 
      filter(consent_for_vaccination==input$vac_cons) %>% 
      ggplot(df1,mapping = aes(x=age_grp,fill=SAE_1st3omins))+
      geom_bar(position = "fill")+
      xlab("Age group")
  })
  
  #Data summary
  output$skimr<- renderPrint({
    print("Your data summary here")
  })
  
  
}



shinyApp(ui,server)

