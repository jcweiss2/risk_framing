libs = list("tidyverse", "shiny","pROC","DT","shinyWidgets","shinyAce","shinythemes","ggpubr")
jload = function(l) {
  for(i in l) {
    if(!do.call("require",list(i))) install.packages(i)
    do.call("require",list(i))
  }
}
jload(libs)

# library(tidyverse)
# library(shiny)
# library(pROC)
# library(DT)
# library(shinyWidgets)

ui = function(request) {
  fluidPage(theme=shinytheme("yeti"),
    titlePanel('Optimal risk estimation but low AUC'),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(width=9,
                 sliderInput('nsamples',"Log 10 samples", min=0, max=8, step=0.5, value=3),
          ),
          column(width=3,
                 uiOutput("ui1"),               
          )
        ),
        selectInput('risk', 'Risk distribution', c("Normal [mu, sigma]","Uniform [lb, ub]", "Custom")),
        conditionalPanel("input.risk=='Custom'", #ns=ns,
                         {require(shinyAce)
                           tagList(
                             shinyAce::aceEditor("editor", mode = "r",
                                                 value="c(rep(0.01, 100000), rep(0.02, 100000))",
                                                 placeholder="runif(100,0,1)",
                                                 height = "100px",
                                                 hotkeys = list(
                                                   help_key = "F1",
                                                   run_key = list(
                                                     win="Ctrl-Enter",
                                                     mac="CMD-ENTER"
                                                   )
                                                 )
                             ),
                             actionButton("eval", "Evaluate")
                           )
                         }
        ),
        conditionalPanel("input.risk!='Custom'",
                         splitLayout(
                           numericInput('riskPar1', 'Parameter 1', value=0.4),
                           numericInput('riskPar2', 'Parameter 2', value=0.2)
                         )
        ),
        fluidRow(
          column(width=6,
                 radioGroupButtons('clip_remove', label="Invalid risk", choices=c("Remove","Clip"), selected="Remove"),
          ),
          column(width=3,
                 radioGroupButtons("plotcis", "CIs", choices=c("T","F"), selected="F", size="sm")
          ),
          column(width=3,
                 radioGroupButtons("xlims", "x limits", choices=c("T","F"), selected="T", size="sm")
          )
        ),
        selectInput('errortype', 'Error distribution', c("Epsilon ~ Normal(0, sigma^2)","Epsilon ~ Uniform[-sigma,sigma]", "Custom")),
        conditionalPanel("input.errortype=='Custom'", #ns=ns,
                         {require(shinyAce)
                           tagList(
                             shinyAce::aceEditor("erroreditor", mode = "r",
                                                 value="data()$truth*rnorm(nrow(data()))",
                                                 placeholder="0",
                                                 height = "100px",
                                                 hotkeys = list(
                                                   help_key = "F1",
                                                   run_key = list(
                                                     win="Ctrl-Enter",
                                                     mac="CMD-ENTER"
                                                   )
                                                 )
                             ),
                             actionButton("erroreval", "Evaluate")
                           )
                         }
        ),
        conditionalPanel("input.errortype!='Custom'",
                         numericInput('epsilonsigma', 'Sigma', value=0.01),
        )
        
      ),
      mainPanel(
        HTML("When monitoring risk, e.g., in health care, accurately estimating risk is important,
         so that risks associated with intervention can be balanced with risks of usual care.
         In prediction problems, discriminative ability, often measured with the AUC
         or C-statistic, provides a way to compare relative predictive ability of one or more
         algorithms."),
        h5("However, accurate estimates of risk do not imply high predictive performance.  In fact,
         even Bayes optimal risk estimation can lead to discriminative ability no better 
         than chance."),
        HTML("<b>For example, accurately estimating probabilities of 0.01 and 0.02 corresponds to an AUC
         of just 0.58, simply because there is uncertainty in the binary outcome
         </b> (Bernoullis with p=0.01 and p=0.02); use the custom distribution option for
         this result)."),
        h5("This tool lets you explore the best an algorithm can do, given
         the population risk distribution.  You can also adjusting the error distribution to 
         assess the level of AUC degradation when making suboptimal predictions."),
        h5(" "),
        fluidRow(
          column(width=3,
                 plotOutput('riskhist')
          ),
          column(width=3,
                 plotOutput('riskest')
          ),
          column(width=6,
                 plotOutput('plotroc')
          )
        )
        # DT::dataTableOutput('table1')
        # textOutput('text1')
      )
    )
  )
}
server = function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  data = reactive({
    nsamples = 10**input$nsamples
    riskdist = input$risk
    risk1 = input$riskPar1
    risk2 = input$riskPar2
    clip_remove = input$clip_remove
    input$eval
    input$editor_run_key
    input$erroreval
    input$erroreditor_run_key
    
    if(riskdist=="Uniform [lb, ub]") {
      s = runif(nsamples, risk1, risk2)
    } else if (riskdist == "Normal [mu, sigma]") {
      validate(need(risk2>0, "Sigma must be positive"))
      s = rnorm(nsamples, mean=risk1, sd=risk2)
    } else if (riskdist =="Custom") {
      s = eval(parse(text = isolate(input$editor)))
    } else {
      validate("Error in risk distribution selection")
    }
    if(clip_remove=="Clip") {
      s[s<0] = 0
      s[s>1] = 1
    } else if (clip_remove=="Remove") {
      s = s[s>=0 & s<=1]
    }
    dat = data.frame(truth = s) %>% 
      mutate(observed = s > runif(nrow(.))) %>% as_tibble()
    dat
  })
  
  data_and_error = reactive({
    data() %>% mutate(corrupted=truth+error())
  })
  
  error = reactive({
    dat = data()
    errortype = input$errortype
    epsilonsigma = input$epsilonsigma
    
    if(errortype=="Epsilon ~ Normal(0, sigma^2)") {
      rnorm(nrow(dat),0,epsilonsigma)
    } else if(errortype=="Epsilon ~ Uniform[-sigma,sigma]") {
      runif(nrow(dat),-epsilonsigma, epsilonsigma)
    } else if(errortype=="Custom") {
      s = eval(parse(text = isolate(input$erroreditor)))
    }
  })
  
  observeEvent(input$editor_run_key, {
    print(str(input$editor_run_key))
  })
  
  observeEvent(input$editor_help_key, {
    print(str(input$editor_help_key))
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$riskhist <- renderPlot({
    dat = data() %>% select(`Bayes optimal prediction`=truth)
    h = gghistogram(dat, x = "Bayes optimal prediction", y="..density..") + ylab("Density")
    if(input$xlims == "T") {
      h = h + xlim(-0.1,1.1)
    }
    plot(h)
  })
  
  output$riskest <- renderPlot({
    dat = data_and_error() %>% select(Prediction=corrupted, `Bayes optimal prediction`=truth) %>% head(1e3)
    h = ggplot(dat) + geom_point(aes(x=Prediction,y=`Bayes optimal prediction`)) + theme_bw()
    if(input$xlims == "T") {
      h = h + xlim(-0.1,1.1) + ylim(-0.1,1.1)
    }
    plot(h)
  })
  
  output$ui1 = renderUI({
    tagList(
      htmlOutput("t1"),
      textOutput("t2")
    )
  })
  output$t1 = renderText({"<b>Samples</b>"})
  output$t2 = renderText({nrow(data())})
  
  output$plotroc <- renderPlot({
    # palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    dat = data_and_error()
    require(pROC)
    r = plot.roc(dat$observed, dat$corrupted, percent=T, print.auc=T, ci=T)
    if(input$plotcis=="T") {
      cir = ci.se(r, specificities = seq(0, 100, 5))
      plot(cir, type = "shape", col = "grey")
    } else {
      plot.roc(r, percent=T, print.auc=T, ci=T, col="#000000")
    }
    lines.roc(dat$observed, dat$truth, percent=T, col="#00AAAA")
    legend("bottomright", legend=c("Predictions", "Bayes optimal"), col=c("#000000", "#00AAAA"), lwd=2)
  })
  
  # output$table1 = DT::renderDataTable({
  #   datatable(data() %>% select(`Bayes optimal`=truth, Observed=observed))
  # })
}

shinyApp(ui=ui, server=server,
         enableBookmarking = "server"
)
