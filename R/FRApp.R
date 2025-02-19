#' FRApp
#' @description
#' A Shiny app to fit the nonlinear mixed effects model to FRAP data. By default,
#' R opens the Shiny app in the default browser.
#'
#' @param ... shinyApp function options
#'
#' @return A list of objects that defines the Shiny app.
#' @importFrom shiny fluidPage
#' @importFrom shiny titlePanel
#' @importFrom shiny h4
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny fileInput
#' @importFrom shiny div
#' @importFrom shiny actionButton
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny hr
#' @importFrom shiny sidebarPanel
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny numericInput
#' @importFrom shiny p
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny textInput
#' @importFrom shiny textOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny plotOutput
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tableOutput
#' @importFrom shiny reactiveValues
#' @importFrom shiny observeEvent
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateCheckboxGroupInput
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny observe
#' @importFrom shiny renderPrint
#' @importFrom shiny eventReactive
#' @importFrom shiny showModal
#' @importFrom shiny showNotification
#' @importFrom shiny modalDialog
#' @importFrom shiny removeModal
#' @importFrom shiny renderPlot
#' @importFrom shiny downloadHandler
#' @importFrom shiny req
#' @importFrom shiny renderTable
#' @importFrom shiny renderText
#' @importFrom shiny updateTextInput
#' @importFrom shiny updateNumericInput
#' @importFrom shiny shinyApp
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom gplots textplot
#' @importFrom methods is
#' @importFrom nlme intervals
#' @importFrom nlme pdMat
#' @importFrom nlme nlme
#' @importFrom nlme nlmeControl
#' @importFrom nlme corAR1
#' @importFrom nlme corARMA
#' @importFrom nlme ranef
#' @importFrom nlme varIdent
#' @importFrom nlme varExp
#' @importFrom nlme varPower
#' @importFrom nlme varConstPower
#' @importFrom stats getInitial
#' @importFrom stats qqnorm
#' @importFrom stats formula
#' @importFrom stats predict
#' @importFrom stats residuals
#' @importFrom stats residuals
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils capture.output
#' @importFrom graphics par
#'
#' @export
#'
#' @details
#' FRApp is shiny app that provides a
#' user-friendly interactive interface to streamline
#' the data analysis derived by fitting nonlinear mixed-effects
#' regression models with an asymptotic exponential functional
#' relationship on data with a hierarchical structure.
#'
#' The application accepts the csv files format. By default,
#' it uses the semicolon as the field separator and the period as
#' the decimal separator, but you can select different separators
#' from the drop-down menu.
#'
#' The first line of the file must contain the variable names.
#' The *Browse…* button allows you to load your own data file to be analyzed.
#'
#' Via the application you can: estimate and compare exponential mixed-effects models;
#' print a model report; export the data and the model corresponding objects.
#'
#' @return None
#'
#' @examples
#' # Run the app
#'
#' FRApp()
#'

FRApp <- function(...) {
  ui <- fluidPage(
    titlePanel("FRApp - Data analysis using exponential mixed-effects models"),
    hr(),
    ## ui ---------------------------------
      h4("Data loading"),
      fluidRow(
        column(4,
        div(style = "margin-left: 15px",
      div(style="display:inline-block; margin-right: 10px;", selectInput("sep", "Field separator", choices=c(";",",","tab","space"),
                                                    selected = c(";"), width = "100%")),
      div(style="display:inline-block", selectInput("dec", "Decimal separator", choices=c(".",","),
                                                    selected = c("."), width = "100%")),
      fileInput("data", NULL, accept = ".csv", placeholder = "Select a .csv file"),
      actionButton("example.click", "Load example data"))
      ),
      column(8, verbatimTextOutput("contents"))),
      hr(),
      h4("Fit exponential mixed effects model"),
      sidebarPanel(
        selectInput("response", "Response variable", multiple = F, choices = NULL),
        selectInput("explanatory", "Explanatory variable", multiple = F, choices = NULL),
        hr(),
        h4("Hierarchical structure"),
        selectInput("lvl1", "Level 1", multiple = F, choices = NULL),
        checkboxGroupInput("rndEff1", "Random Effects - Lvl 1", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        hr(style = "height:2px"),
        selectInput("lvl2", "Level 2 (nested in Lvl 1)", multiple = F, choices = NULL),
        checkboxGroupInput("rndEff2", "Random Effects - Lvl 2 ", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        hr(style = "height:2px"),
        selectInput("lvl3", "Level 3 (nested in Lvl 2)", multiple = F, choices = NULL),
        checkboxGroupInput("rndEff3", "Random Effects - Lvl 3", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        hr(),
        h4("Variance function"),
        selectInput("variance", "Variance", multiple = F, choices = c("Power", "ConstantPower", "Exponential", "Constant"), selected = "Constant"),
        hr(),
        h4("Temporal dependence"),
        numericInput("corAR", "Autocorrelation order", 0, min = 0, step = 1),
        hr(),
        h4("Interactions"),
        selectInput("experimentalCond", "Experimental condition", multiple = F, choices = NULL),
        p("interacting with"),
        checkboxGroupInput("fxExpCond", "Fixed effects", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        checkboxGroupInput("rndExpCond1", "Lvl 1 - Random effects", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        checkboxGroupInput("rndExpCond2", "Lvl 2 - Random effects", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        checkboxGroupInput("rndExpCond3", "Lvl 3 - Random effects", choices = c("Asym", "R0", "lrc"), inline = TRUE),
        hr(style = "height:2px"),
        checkboxInput("varInt", "Variance"),
        # checkboxInput("corARint", "Correlation"),
        hr(),
        actionButton("fit.click", "Fit the model"),
        div(style = "margin-top: 5px"),
        hr(),
        h4("Model list"),
        textInput("modelname", NULL, placeholder = "Model name"),
        #fluidRow(
          div(style="display:inline-block; margin-right: 10px;", actionButton("savefit.click", "Add to model list")),
          div(style="display:inline-block", actionButton("resetfit.click", "Reset model list")),
        #),
        div(style = "margin-top: 5px"),
        hr(),
        h4("Download"),
        selectInput("modeldownload", "Select a model", choices = NULL, multiple = F),
        #fluidRow(
          div(style="display:inline-block; margin-right: 10px;", downloadButton("outputButtonReport", "Report")),
          div(style="display:inline-block", downloadButton("outputButtonRData", "RData")),
        #),
        hr(),
      ),
      mainPanel(
        verbatimTextOutput("fitSummary"),
        fluidRow(column(width = 6, plotOutput("fitPlotRes", height="auto")),
        column(width = 6, plotOutput("fitPlotQQ", height="auto")),
        verbatimTextOutput("fitInterv")),
        hr(),
        conditionalPanel(
          condition = "req(fitList)",
          tableOutput("fitListTable")
        )
      )
    )


  server <- function(input, output, session) {
    rv <- reactiveValues()

    observe({
      print(str(rv$fitList))
    })

    observeEvent(input$data, {
      file <- input$data
      req(file)
      if(input$sep==";")  sep <- ";"
      if(input$sep==",")  sep <- ","
      if(input$sep=="tab")  sep <- "\t"
      if(input$sep=="space")  sep <- " "

      if(input$dec==".")  dec <- "."
      if(input$dec==",")  dec <- ","

      loaddata <- try(read.csv(file$datapath, header = T, stringsAsFactors = T,
                                sep = sep, dec = dec)
                      )

      if(inherits(loaddata, "try-error")) {
        showModal(modalDialog(paste0("Verify the field and the decimal separators"), easyClose = TRUE, footer = NULL))
        rv$mydata <- NULL
        }else{
        rv$mydata <- loaddata
        }

      if(length(rv$mydata)<4){
        showModal(modalDialog(paste0("Number of imported variables is too low, verify the data"), easyClose = TRUE, footer = NULL))
      }

      updateSelectInput(session, "response", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")
      updateSelectInput(session, "explanatory", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")
      updateSelectInput(session, "lvl1", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")
      updateSelectInput(session, "lvl2", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")
      updateSelectInput(session, "lvl3", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")
      updateSelectInput(session, "experimentalCond", choices = c(Choose=" ", colnames(rv$mydata)), selected = " ")

      updateCheckboxGroupInput(session, "rndEff1", choices = c("Asym", "R0", "lrc"), selected = NULL, inline = T)
      updateCheckboxGroupInput(session, "rndEff2", choices = c("Asym", "R0", "lrc"), selected = NULL, inline = T)
      updateCheckboxGroupInput(session, "rndEff3", choices = c("Asym", "R0", "lrc"), selected = NULL, inline = T)

      updateSelectInput(session, "variance", choices = c("Power", "ConstantPower", "Exponential", "Constant"), selected = "Constant")
      updateNumericInput(session, "corAR", value = 0)

      updateCheckboxInput(session, "varInt", value = F)
    })

    observe({
      rv$response <- rv$mydata[, colnames(rv$mydata) %in% input$response]
      rv$explanatory <- rv$mydata[, colnames(rv$mydata) %in% input$explanatory]
      rv$lvl1 <- rv$mydata[, colnames(rv$mydata) %in% input$lvl1]
      rv$lvl2 <- rv$mydata[, colnames(rv$mydata) %in% input$lvl2]
      rv$lvl3 <- rv$mydata[, colnames(rv$mydata) %in% input$lvl3]

      rv$experimentalCond <- rv$mydata[, colnames(rv$mydata) %in% input$experimentalCond]

      rv$responseName <- input$response
      rv$explanatoryName <- input$explanatory
      rv$lvl1Name <- input$lvl1
      rv$lvl2Name <- input$lvl2
      rv$lvl3Name <- input$lvl3

      rv$rndEff1 <- input$rndEff1
      rv$rndEff2 <- input$rndEff2
      rv$rndEff3 <- input$rndEff3

      rv$variance <- input$variance
      rv$corAR <- input$corAR

      rv$experimentalCondName <- input$experimentalCond
      rv$fxExpCond <- input$fxExpCond
      rv$rndExpCond1 <- input$rndExpCond1
      rv$rndExpCond2 <- input$rndExpCond2
      rv$rndExpCond3 <- input$rndExpCond3

      rv$varInt <- input$varInt

      rv$modelname <- input$modelname
    })

    observeEvent(input$example.click, {
      rv$mydata <- FRAPdata
      rv$responseName <- "y"
      rv$explanatoryName <- "time"
      rv$lvl1Name <- "Culture.id"
      rv$lvl2Name <- "neuron"
      rv$lvl3Name <- "spine.ID"
      rv$experimentalCondName <- "genetic.id"

      rv$response <- rv$mydata[, rv$responseName]
      rv$explanatory <- rv$mydata[, rv$explanatoryName]
      rv$lvl1 <- rv$mydata[, rv$lvl1Name]
      rv$lvl2 <- rv$mydata[, rv$lvl2Name]
      rv$lvl3 <- rv$mydata[, rv$lvl3Name]
      rv$experimentalCond <- rv$mydata[, rv$experimentalCondName]

      rv$variance <- "Exponential"

      updateSelectInput(session, "response", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$responseName)
      updateSelectInput(session, "explanatory", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$explanatoryName)
      updateSelectInput(session, "lvl1", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl1Name)
      updateSelectInput(session, "lvl2", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl2Name)
      updateSelectInput(session, "lvl3", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl3Name)
      updateSelectInput(session, "experimentalCond", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$experimentalCondName)

      updateCheckboxGroupInput(session, "rndEff1", selected = c("Asym", "R0", "lrc"))
      updateCheckboxGroupInput(session, "rndEff2", selected = c("R0"))
      updateCheckboxGroupInput(session, "rndEff3", selected = c("Asym", "R0", "lrc"))

      updateSelectInput(session, "variance", selected = "Exponential")
      updateNumericInput(session, "corAR", value = 1)

      updateCheckboxInput(session, "varInt", value = T)
      #   }
    })

    observe({
      updateSelectInput(session, "response", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$responseName)
      updateSelectInput(session, "explanatory", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$explanatoryName)
      updateSelectInput(session, "lvl1", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl1Name)
      updateSelectInput(session, "lvl2", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl2Name)
      updateSelectInput(session, "lvl3", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$lvl3Name)
      updateSelectInput(session, "experimentalCond", choices = c(Choose=" ", colnames(rv$mydata)), selected = rv$experimentalCondName)
    })


    ## output model tab ---------------
    output$contents <- renderPrint({
      req(rv$mydata)
      str(rv$mydata)
    })

    fit <- eventReactive(input$fit.click, {
      if (rv$responseName == " " | !is.numeric(rv$responseName)) {
        showModal(modalDialog(paste0("Response variable is missing or it is not numerical"), easyClose = TRUE, footer = NULL))
      }

      if (rv$explanatoryName == " " | !is.numeric(rv$explanatoryName)) {
        showModal(modalDialog(paste0("Explanatory variable is missing or it is not numerical"), easyClose = TRUE, footer = NULL))
      }
      if (rv$lvl1Name == " " & rv$lvl2Name == " " & rv$lvl3Name == " ") {
        showModal(modalDialog(paste0("Select at least one hierarchical level"), easyClose = TRUE, footer = NULL))
      } else {
        if ((rv$lvl1Name != " " & length(rv$rndEff1) == 0) | (rv$lvl2Name != " " & length(rv$rndEff2) == 0) | (rv$lvl3Name != " " & length(rv$rndEff3) == 0)) {
          showModal(modalDialog(paste0("Choose at least one random effect for the selected hierarchical levels"), easyClose = TRUE, footer = NULL))
        } else {
          if (rv$experimentalCondName == " " & any(c(
            !is.null(rv$fxExpCond), !is.null(rv$rndExpCond1), !is.null(rv$rndExpCond2), !is.null(rv$rndExpCond3), rv$varInt # ,rv$corARint
          ))) {
            showModal(modalDialog(paste0("Experimental condition is missing but at least one interaction is choosen"), easyClose = TRUE, footer = NULL))
          } else {
            # Hierarchical structure
            nlme.formula <- paste(rv$responseName, "~", paste("SSasymp(", rv$explanatoryName, ", Asym, R0, lrc)", collapse = ""))

            # Define groups and create id
            hier <- c(rv$lvl1Name, rv$lvl2Name, rv$lvl3Name)
            dataHier <- as.data.frame(rv$mydata[, colnames(rv$mydata) %in% hier])
            id <- as.data.frame(as.factor(dataHier[, 1]))

            if (ncol(dataHier) == 1) {
              colnames(dataHier) <- hier[which(hier != " ")]
              colnames(id) <- colnames(dataHier)
            }
            if (ncol(dataHier) > 1) {
              idx <- sapply(1:ncol(dataHier), seq)
              id.names <- c()
              for (i in 2:ncol(dataHier)) {
                id <- cbind.data.frame(id, as.factor(apply(dataHier[, idx[[i]]], 1, paste, collapse = ".")))
                id.names <- c(id.names, paste(colnames(dataHier)[idx[[i]]], collapse = "."))
              }
              colnames(id) <- c(paste0(rv$lvl1Name, 1), id.names)
            }
            rv$nhier <- ncol(id)
            varFit <- c(rv$responseName, rv$explanatoryName, rv$experimentalCondName)
            rv$mydataFit <- cbind.data.frame(rv$mydata[, colnames(rv$mydata) %in% varFit], id)

            groups <- formula(paste0("~", paste(colnames(id), collapse = "/")))

            pd1 <- pd2 <- pd3 <- NULL
            # Random effects and interaction
            if (rv$lvl1Name != " ") {
              if (is.null(rv$rndExpCond1)) {
                pd1 <- pdMat(formula(paste0(paste(rv$rndEff1, collapse = "+"), "~ 1")), pdClass = "pdDiag")
              } else {
                if (all(rv$rndEff1 == rv$rndExpCond1)) {
                  pd1 <- pdMat(formula(paste(paste0(rv$rndEff1, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                } else {
                  pd1 <- pdMat(
                    list(
                      pdMat(formula(paste0(paste0(setdiff(rv$rndEff1, rv$rndExpCond1), collapse = "+"), "~ 1")), pdClass = "pdDiag"),
                      pdMat(formula(paste(paste0(rv$rndExpCond1, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                    ),
                    pdClass = "pdBlocked"
                  )
                }
              }
            }
            if (rv$lvl2Name != " ") {
              if (is.null(rv$rndExpCond2)) {
                pd2 <- pdMat(formula(paste0(paste(rv$rndEff2, collapse = "+"), "~ 1")), pdClass = "pdDiag")
              } else {
                if (all(rv$rndEff2 == rv$rndExpCond2)) {
                  pd2 <- pdMat(formula(paste(paste0(rv$rndEff2, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                } else {
                  pd2 <- pdMat(
                    list(
                      pdMat(formula(paste0(paste0(setdiff(rv$rndEff2, rv$rndExpCond2), collapse = "+"), "~ 1")), pdClass = "pdDiag"),
                      pdMat(formula(paste(paste0(rv$rndExpCond2, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                    ),
                    pdClass = "pdBlocked"
                  )
                }
              }
            }
            if (rv$lvl3Name != " ") {
              if (is.null(rv$rndExpCond3)) {
                pd3 <- pdMat(formula(paste0(paste(rv$rndEff3, collapse = "+"), "~ 1")), pdClass = "pdDiag")
              } else {
                if (all(rv$rndEff3 == rv$rndExpCond3)) {
                  pd3 <- pdMat(formula(paste(paste0(rv$rndEff3, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                } else {
                  pd3 <- pdMat(
                    list(
                      pdMat(formula(paste0(paste0(setdiff(rv$rndEff3, rv$rndExpCond3), collapse = "+"), "~ 1")), pdClass = "pdDiag"),
                      pdMat(formula(paste(paste0(rv$rndExpCond3, collapse = "+"), rv$experimentalCondName, sep = "~")), pdClass = "pdDiag")
                    ),
                    pdClass = "pdBlocked"
                  )
                }
              }
            }

            rnd <- list(pd1, pd2, pd3)
            rnd <- rnd[!sapply(rnd, is.null)]

            if (is.null(rv$fxExpCond)) {
              fixed.formula <- formula("Asym + R0 + lrc ~ 1")
              start <- getInitial(formula(nlme.formula), data = rv$mydataFit)
            } else {
              if (length(rv$fxExpCond) == 3) {
                fixed.formula <- formula(paste("Asym + R0 + lrc ~", rv$experimentalCondName, collapse = ""))
                start <- rep(getInitial(formula(nlme.formula), data = rv$mydataFit), 2)
              } else {
                fixed.formula <- list(
                  formula(paste(paste(rv$fxExpCond, collapse = "+"), "~", rv$experimentalCondName, collapse = "")),
                  formula(paste(paste(c("Asym", "R0", "lrc")[!c("Asym", "R0", "lrc") %in% rv$fxExpCond], collapse = "+"), "~ 1", collapse = ""))
                )
                start <- c(getInitial(formula(nlme.formula), data = rv$mydataFit), getInitial(formula(nlme.formula), data = rv$mydataFit)[rv$fxExpCond])
              }
            }

            showModal(modalDialog("Fitting the model... \n Please wait patiently, this may take several minutes", footer = NULL))

            # Variance and AR
            # constant variance and no AR
            if (rv$variance == "Constant" & !rv$varInt) {
              if (rv$corAR == 0) {
                modelFit <- tryCatch(nlme(formula(nlme.formula),
                  data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                  start = start, control = nlmeControl(pnlsMaxIter = 200)),
                  error=function(e) e, warning=function(w) w)
              } else {
                #  if(!rv$corARint){
                modelFit <- tryCatch(nlme(formula(nlme.formula),
                  data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                  start = start, correlation = corARMA(p = rv$corAR, q = 0), control = nlmeControl(pnlsMaxIter = 200)),
                  error=function(e) e, warning=function(w) w)
                # }else{
                #  modelFit <- try(nlme(formula(nlme.formula), data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                #                       start = start, correlation = corARMA(form = ~rv$explanatoryName|rv$experimentalCondName, p=rv$corAR, q = 0),
                #                       control = nlmeControl(pnlsMaxIter = 200)))
                # }
              }
            } else {
              varForm <- formula(~ fitted(.))
              if (rv$varInt) {
                varForm <- formula(paste0("~ fitted(.)|", rv$experimentalCondName))
              }
              if (rv$variance == "Constant") varFun <- get("varIdent")
              if (rv$variance == "Power") varFun <- get("varPower")
              if (rv$variance == "ConstantPower") varFun <- get("varConstPower")
              if (rv$variance == "Exponential") varFun <- get("varExp")
              if (rv$corAR == 0) {
                modelFit <- tryCatch(nlme(formula(nlme.formula),
                  data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                  start = start, weights = varFun(form = varForm), control = nlmeControl(pnlsMaxIter = 200)),
                  error=function(e) e, warning=function(w) w)
              } else {
                if(rv$corAR==1){
                  modelFit <- tryCatch(nlme(formula(nlme.formula),
                                       data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                                       start = start, weights = varFun(form = varForm), correlation = corAR1(), control = nlmeControl(pnlsMaxIter = 200, maxIter = 200)),
                                       error=function(e) e, warning=function(w) w)
                } else {
                #  if(!rv$corARint){
                modelFit <- tryCatch(nlme(formula(nlme.formula),
                  data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                  start = start, weights = varFun(form = varForm), correlation = corARMA(p = rv$corAR, q = 0), control = nlmeControl(pnlsMaxIter = 200, maxIter = 200)),
                  error=function(e) e, warning=function(w) w)
                }
                #  }else{
                #  modelFit <- try(nlme(formula(nlme.formula), data = rv$mydataFit, fixed = fixed.formula, groups = groups, random = rnd,
                #                       start = start, weights = varFun(form = varForm), correlation = corARMA(form = ~1|rv$experimentalCondName, p=rv$corAR, q = 0), control = nlmeControl(pnlsMaxIter = 200)))
                # }
              }
            }
            removeModal()
            if(is(modelFit,"warning")){
              showModal(modalDialog(paste0(conditionMessage(modelFit)), easyClose = TRUE, footer = NULL))
              modelFit <- NULL
            }
            if(is(modelFit,"error")){
              showModal(modalDialog(paste0(conditionMessage(modelFit)), easyClose = TRUE, footer = NULL))
              modelFit <- NULL
            }
            # if (is(modelFit) == "try-error") {
            #   showModal(modalDialog(attr(modelFit, which = "condition"), easyClose = TRUE, footer = NULL))
            #   modelFit <- NULL
            # }
            modelFit
          }
        }
      }
    })

    # output fitted model
    output$fitSummary <- renderPrint({
      req(fit())
      rv$fitSumm <- summary(fit())
      summary(fit())
    })
    output$fitPlotRes <- renderPlot({
      req(fit())
      rv$fitPlotRes <- nlme::plot.lme(fit(), pch = 21, idResType = "n")
      nlme::plot.lme(fit(), pch = 21, idResType = "n")
    }, height = function() {
      session$clientData$output_fitPlotRes_width
    })

    output$fitPlotQQ <- renderPlot({
      req(fit())
      rv$fitPloQQ <- qqnorm(fit(), ~ resid(., type = "n"), abline = c(0, 1))
      qqnorm(fit(), ~ resid(., type = "n"), abline = c(0, 1))
    }, height = function() {
      session$clientData$output_fitPlotRes_width
    })
    output$fitInterv <- renderPrint({
      req(fit())
      rv$fitInterv <- intervals(fit())
      intervals(fit())
    })

    # save the fitted model
    rv$counter <- 0

    observeEvent(input$savefit.click, {
      req(fit())
      if (rv$modelname == " ") {
        showModal(modalDialog(paste0("The name of the model is missing"), easyClose = TRUE, footer = NULL))
      }else{
      if (rv$modelname %in% names(rv$fitList)) {
        showModal(modalDialog(paste0("Duplicated name, change the name of the model"), easyClose = TRUE, footer = NULL))
      } else {
        rv$counter <- rv$counter + 1
        rv$fitList[[rv$counter]] <<- fit()
        names(rv$fitList)[[rv$counter]] <<- rv$modelname

        if(rv$counter==1){
          output$fitListTable <- renderTable({
          cbind.data.frame(
            "Model" = c(1:rv$counter),
            "Model name" = names(rv$fitList)
          )
        })
        }
        if(rv$counter>1){
          fitListSel <- rv$fitList
          output$fitListTable <- renderTable({
            cbind.data.frame(
              "Name" = names(fitListSel),
              eval(parse(text = paste("anova(", paste("fitListSel[[", 1:length(fitListSel), "]]", sep = "", collapse = ","), ")")))[, -1]
            )
          })
        }
        showNotification(paste0(rv$modelname, " added to model list"),
                         type = "message", duration = 5)
      }

        rv$modelname <- " "
        updateTextInput(session, "modelname", value = " ")
      }
    })

    # save and reset the fitted model
    observeEvent(input$resetfit.click, {
      rv$fitList <- NULL
      output$fitListTable <- renderTable({})
      showNotification("Model list is empty", type = "message", duration = 5)
      rv$counter <- 0
    })

    ## Download tab ---------------------
    observe({
      updateSelectInput(session, "modeldownload", choices = c(" ", names(rv$fitList)))
    })

    output$outputButtonReport <- downloadHandler(
      filename = function() {
        paste("FRAppReport", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep = "")
      },
      content = function(file) {
        req(names(rv$fitList) %in% input$modeldownload)
        selectedModel <- which(names(rv$fitList) %in% input$modeldownload)
        fitListSel <- rv$fitList[[selectedModel]]

        plotRes <- plot(fitListSel, pch = 21, idResType = "n")
        plotQQ <- qqnorm(fitListSel, ~ resid(., type = "n"), abline = c(0, 1))
        pdf(file, paper = "a4")
        textplot(capture.output(summary(fitListSel)), valign = "top", fixed.width = F, cex = 0.7)
        textplot(capture.output(intervals(fitListSel)), valign = "top", fixed.width = F, cex = 0.7)
        par(mfrow = c(1, 2))
        print(plotRes)
        print(plotQQ)
        dev.off()
      }
    )

    output$outputButtonRData <- downloadHandler(
      filename = function() {
        paste("FRAppRData", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RData", sep = "")
      },
      content = function(file) {
        req(names(rv$fitList) %in% input$modeldownload)
        selectedModel <- which(names(rv$fitList) %in% input$modeldownload)
        fitListSel <- rv$fitList[[selectedModel]]

        fit <- fitListSel
        dataAug <- cbind.data.frame(rv$mydata,fit$groups)
        CI <- intervals(fitListSel)
        resid <- residuals(fitListSel, levels = ncol(fitListSel$groups))
        pred <- predict(fitListSel, level = 0:ncol(fitListSel$groups))
        raneff <- ranef(fitListSel, level = 0:ncol(fitListSel$groups))
        save(fit, dataAug, pred, CI, resid, raneff, file = file)
      }
    )

  }

  # Run the application
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, ...))
}
