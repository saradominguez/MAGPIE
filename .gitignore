library(shiny)
library(shinyBS)
library(shinymanager)
library(shinyjs)
library(ROAuth)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyLP)
library(zscorer)
library(caret)
library(randomForest)
#rsconnect::showLogs()


# outputDir <- "EARTH APP"
# drop_auth(rdstoken = "droptoken.rds")

rfFit<-readRDS('data/rfFit.rds')
load("data/train.data12_imputed_2020-05-24.Rda")

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


credentials <- data.frame(
    user = c("user"),
    password = c("0000"),
    #comment = c("name of the study"), 
    stringsAsFactors = FALSE
)


    

    
    fluidRow(
        column(12,
    cover<-fluidPage(
        tags$img(src='cover.png', height=300, width=600)
    )))
    
    
    boxTag1 <- widgetUserBox(
        title = tags$h4("Please include the date", style="font-family: 'Segoe UI Light'"),
        src='calendar3.png',
        closable = F,
        boxToolSize = "xs",
        background  = TRUE,
        backgroundUrl = "header.png",
        enable_label = TRUE,
        collapsible = TRUE,
        footer=
            airDatepickerInput(inputId = "date", label=tags$img(height=40,width=120,src='Text_today.png')),
            airDatepickerInput(inputId = "dob", label=tags$img(height=40,width=120,src='Text_birth.png')),
            airDatepickerInput(inputId = "ddx", label=tags$img(height=40,width=120,src='Text_dx.png'))
    )
    
    boxTag3 <- widgetUserBox(
        title = tags$h4("Please include the information about ART", style="font-family: 'Segoe UI Light'"),
        src='ART.png',
        closable = F,
        boxToolSize = "xs",
        background  = TRUE,
        backgroundUrl = "header.png",
        enable_label = TRUE,
        collapsible = TRUE,
        footer=p(
            airDatepickerInput(inputId = "ART_date", label=tags$img(height=40,width=250,src='Text_ART.png'), todayButton = T),
            prettyRadioButtons(inputId = "ART", label=tags$img(height=40,width=190,src='Text_ARTreg.png'),
                               choices = c("3TC+ABC+LPVr", "3TC+ABC+NVP", "3TC+AZT+LPVr", "3TC+AZT+NVP"))
            
        )
    )
    
    boxTag2 <- widgetUserBox(
        title = tags$h4("Please include the information about the infant", style="font-family: 'Segoe UI Light'"),
        src='baby.png',
        closable = F,
        boxToolSize = "xs",
        background  = TRUE,
        backgroundUrl = "header.png",
        enable_label = TRUE,
        collapsible = TRUE,
        footer=p(
            radioGroupButtons(inputId = "gender", label=tags$img(height=40,width=80,src='Text_gender.png'),
                              checkIcon = list(yes=icon("check")),choices = c("Female", "Male"), status="primary",size="normal"),
            numericInput(inputId = "weight", label=tags$img(height=60,width=250,src='Text_grams.png'), min=1200, max=10000, value = ""),
            radioGroupButtons(inputId = "preterm", label=tags$img(height=40,width=270,src='Text_premature.png'),
                              checkIcon = list(yes=icon("check")),choices=c("No", "Yes"), status="primary",size="normal"),
            tags$script("$(\"input:radio[name='gender'][value='Male']\").parent().css('background-color', '#009999');"),
            tags$script("$(\"input:radio[name='gender'][value='Female']\").parent().css('background-color', '#FF9999');"),
            tags$script("$(\"input:radio[name='preterm'][value='Yes']\").parent().css('background-color', '#009999');"),
            tags$script("$(\"input:radio[name='preterm'][value='No']\").parent().css('background-color', '#FF9999');"),
            tags$style("#weight {border: 1px solid #009999;}")
            
        )
    )
    
    boxTag4 <- widgetUserBox(
        title = tags$h4("Please include the information about the infant's mother", style="font-family: 'Segoe UI Light'"),
        src='mother.png',
        closable = F,
        boxToolSize = "xs",
        background  = TRUE,
        backgroundUrl = "header.png",
        enable_label = TRUE,
        collapsible = TRUE,
        footer=p(
            radioGroupButtons(inputId = "SLEHI", label=tags$img(height=40,width=320,src='Text_SLEHI2.png'),
                              checkIcon = list(yes=icon("check")),choiceNames=c("Yes", "No"), 
                              choiceValues=c(1,0),status="primary",size="normal"),
            tags$script("$(\"input:radio[name='SLEHI'][value='1']\").parent().css('background-color', '#009999');"),
            tags$script("$(\"input:radio[name='SLEHI'][value='0']\").parent().css('background-color', '#FF9999');"),
            prettyRadioButtons(inputId = "motart", label=tags$img(height=40,width=320,src='Text_motart.png'),
                              choiceNames = c("Poor (less than 10%)","Intermediate low (10-50%)", "Intermediate high (50-90%)", "Optimal (higher than 90)"),
                              choiceValues=c(0,1,2,3))

        )
    )
    
    boxTag5 <- widgetUserBox(
        title = tags$h4("Please include the infant viral load and % CD4", style="font-family: 'Segoe UI Light'"),
        src='lab.png',
        closable = F,
        boxToolSize = "xs",
        background  = TRUE,
        backgroundUrl = "header.png",
        enable_label = TRUE,
        collapsible = TRUE,
        footer=p(
            numericInput(inputId = "vl", label=tags$img(height=70,width=290,src='Text_VL.png'), min=20, value = ""),
            numericInput(inputId = "cd4", label=tags$img(height=40,width=250,src='Text_CD4.png'), min=2, max=100, value = ""),
            tags$style("#vl {border: 1px solid #009999;}"),
            tags$style("#cd4 {border: 1px solid #009999;}")
        )

    )
    
    button<-fluidPage(
        p(
            div(actionBttn("Submit", size="lg",style="jelly",icon("fas fa-crow"), label=tags$h1("Go", style="font-family: 'Segoe UI Light'; color:#009999"))),
            div(bsModal("largeModalID", "Results", "Submit", size="large", uiOutput("my_ui"),textOutput("textR")))
            
        ) 
    )
    
    
    shinyApp(
        ui = secure_app(
            head_auth=tags$script(inactivity), 
            theme=("bootstrap.min.css"),
            tags_top=tags$img(src='cover.png', width = 330),
            dashboardPagePlus(
            header = dashboardHeaderPlus(
                enable_rightsidebar = F,
                title=tags$img(height=50,width=320,src='navbar1.png'),
                titleWidth = 1900,
                fixed=TRUE,
                disable=T
            ),
            sidebar = dashboardSidebar(
                sidebarMenu(
                    menuItem(" ", icon = icon("id-badge"), tabName = "About")), disable = TRUE
            ),
            body = dashboardBody(
                setShadow(class = "box"),
                setShadow(id = "my-progress"),
                fluidRow(
                    column(12, cover),
                    column(12,boxTag1),
                    column(12, boxTag2),
                    column(12,boxTag3),
                    column(12,boxTag4),
                    column(12,boxTag5),
                    column(12, button))
            ),
            title = "DashboardPage"
        )),
        server = function(input, output) { 
            result_auth <- secure_server(check_credentials = check_credentials(credentials))
            
            myval1<-eventReactive(input$Submit, 
                                    {
                                        age_at_recruitment<-c(as.double(as.Date(input$date,format="%Y/%m/%d") - as.Date(input$dob, format="%Y/%m/%d"))/30.5)
                                        birth_sex<-input$gender
                                        birth_sexz<-as.double(ifelse(input$gender=="Male", 1, 2))
                                        age_at_recruitmentz<-round(age_at_recruitment,0)
                                        weightz<-round(as.double(input$weight/1000),1)
                                        zscore<-getAllWGS(sex=birth_sexz, age=age_at_recruitmentz, weight=weightz, index="wfa")
                                        WAZ<-as.double(zscore)
                                        preterm_birth<-input$preterm
                                        age_at_diagnosis<-as.double(as.Date(input$ddx,format="%Y/%m/%d") - as.Date(input$dob, format="%Y/%m/%d"))
                                        age_at_art<-as.double(as.Date(input$ART_date,format="%Y/%m/%d") - as.Date(input$dob, format="%Y/%m/%d"))
                                        children_baseline_ART<-input$ART
                                        vl_rna_0<-as.numeric(input$vl)
                                        cd4_p_0<-as.numeric(input$cd4)
                                        mother_SLEHI<-input$SLEHI
                                        motart_adherence<-input$motart

                                        
                                        mydata<-cbind(age_at_recruitment, birth_sex, WAZ, 
                                                      preterm_birth, age_at_diagnosis, age_at_art,
                                                      children_baseline_ART, vl_rna_0, cd4_p_0,
                                                      mother_SLEHI, motart_adherence)
                                        
                                        mydata2<-rbind(train.data12_i[,2:12], mydata)
                                        mydata2<-mydata2[72,]
                                        mydata2$age_at_recruitment<-as.numeric(mydata2$age_at_recruitment)
                                        mydata2$WAZ<-as.numeric(mydata2$WAZ)
                                        mydata2$age_at_diagnosis<-as.numeric(mydata2$age_at_diagnosis)
                                        mydata2$age_at_art<-as.numeric(mydata2$age_at_art)
                                        mydata2$vl_rna_0<-as.numeric(mydata2$vl_rna_0)
                                        mydata2$cd4_p_0<-as.numeric(mydata2$cd4_p_0)
                                        #model
                                        
                                        prediction<-predict(rfFit, newdata = data.frame(mydata2), type="prob")
                                        result<-paste(as.numeric(prediction$Yes) * 100, "%")
                                        return(result)
    
                                    })
            
            
            myval2<-eventReactive(input$Submit, 
                                  {
                                      results<-paste("This patient has a", myval1(), "1-year probability to death or clinical progress to AIDS")
                                      tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                      )
                                      )
                                      return(results)
                                      
                                  })
            
            
            output$textR<-renderPrint({
                print(myval2())
                tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                )
                )
            })
            
            output$my_ui<-renderUI({
                if(myval1()<=10)
                    img(src='vlowrisk.png', height='300px')
                
                else
                    if(myval1()>10 & myval1()<=20)
                        img(src='lowrisk1.png', heigh='300px')
                
                else
                    if(myval1()>20 & myval1()<=50)
                        img(src='mildrisk.png', heigh='300px')
                
                else
                    if(myval1()>50 & myval1()<=70)
                        img(src='highrisk1.png', heigh='300px')
                
                else
                    if(myval1()>70)
                        img(src='vhighrisk.png', heigh='300px')
            })
            
            
            
            
            }
    )
    
    
    
