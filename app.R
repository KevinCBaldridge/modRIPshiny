#created by Kevin C. Baldridge, Ph.D.
#all rights reserved
#https://kevincbaldridge.github.io

#work in progress! not yet available, git repo README will be updated with link
#I'll also put it on my above git page for easy access

library(shiny)
library(tximport)
#library(modRIPseq)
library(dplyr)

#to do:
###finish input buttons
###transition to shinydashboard?
###update modRIPseq package repo that this will use for proper dependency upload

#######
ui <- fluidPage(

    # Application title
    titlePanel("modRIPseq R workflow"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(

          #have modalnotification for instructions, file req etc
          actionButton("instructions","Instructions"),

          #checkbox to use paper data or not
          #make this a select choice for upload, mine/juans, or juan/marks?
           checkboxInput(inputId="usePubData",
                         label="Use data from Gonzalez-Rivera, Baldridge, et al (2020)"),

          #data upload
          fileInput(inputId="upload",
                    label="Load your own rsem count files",
                    multiple=TRUE,
                    buttonLabel = "Upload",
                    placeholder="select files..."),

          #have buttons for building DESeqDataSets & running DEseq
          actionButton("buildDDS","Create DESeqDataSet from RSEM count files"),
          actionButton("runDESeq","Run DESeq analysis according to modRIPseq package"),

          #have select dropdown for transform dataset or not
          selectInput("tfm",
                      label="Transform working dataset?",
                      choices=c("None",
                                "rlog",
                                "VarianceStabilizingTransformation")),

          #have action button for PCA plot
          actionButton("PCA","PCA plot"),

          #have check box for combo of venn diagram
          #have secondary download data option for overlap groups?




          #use selectInput with multiple=TRUE to choose biomaRt attribs
          #populate list with the biomaRt fetched?


          #have text input to filter based on padj user defined cutoff
          #include active dataframe download


        ),
          #
#maybe I should have the UI add tabs for each stage as it goes, building input objects, building dds, then results, etc displayed on each tab
        # This is largely placeholder for now
        mainPanel(
          verbatimTextOutput("random"),
          verbatimTextOutput("tmp"),
          tableOutput("factorTbl"),
          #ideally make this fit reactive to change label for the current filtered set info?
          downloadButton('downloadMe',label="Download current filtered set"),

        )

)
)
##############
#if user uploads file, use theirs, otherwise load
#


# Define server logic required to draw a histogram
server <- function(input, output) {
#after I update modRIPseq package update, these will be obsolete and instead use those same functions from modRIPseq package
  loadFactors <- function(filepath=system.file(package='modRIPseq',"extdata","factorsAll.tbl")){
    dir <- dirname(filepath)
    factorTbl <- readr::read_table(filepath)
    for (i in seq_along(factorTbl)) factorTbl[[i]] <- forcats::as_factor(factorTbl[[i]])
    factorTbl$file <- file.path(dir,factorTbl$file)
    return(factorTbl)
  }

  #after I update modRIPseq package update, these will be obsolete and instead use those same functions from modRIPseq package
  reorderFactorsByFile <- function(factortibble=factorTbl,filecol=file,filelist=fileList){
    factortibble <- factortibble %>% dplyr::arrange(factor({{filecol}},levels=filelist))
    return(factortibble)
  }

###NOTE that i think i should just go ahead and see if I can use a reactive wrapper to set these data at the front end, then all other renderings and such can just use those earlier set variables?
#after this function, the filelist should be instead just factorTbl$file, ordered right way as such
    output$factorTbl <- renderTable({
      if (input$usePubData){load("data/data_padjSubset.Rdata");"using previously published data"}
      else {
      factorTbl <- loadFactors(input$upload %>%
                                            dplyr::filter(grepl("factor",name))%>%
                                            dplyr::select(datapath) %>% as.character())
      factorTbl <- reorderFactorsByFile(factortibble = factorTbl,
                                                   filecol = file,
                                                   filelist = input$upload)
      }
    })

    infoModal <- modalDialog(includeHTML("Instructions.html"),
  #this instructions doc should be edited though, probably don't need file list? - or need to ref datapath from filename to ensure ordering...
      title="Instructions")
    showModal(infoModal)
    observeEvent(input$instructions, {
      showModal(infoModal)
    })
    observeEvent(input$buildDDS,{
      output$tmp <- renderTable(
        dataset <- tximport(input$upload$datapath,type = 'rsem',txOut = TRUE)
      )
    })
    output$random <- renderText({
      if (input$usePubData){load("data/data_padjSubset.Rdata");"using previously published data"}
      else {
        paste0("using your uploaded data:\n",
               paste(unlist(input$upload$name),collapse="\n"))
        }
    })
    #ideally
    output$downloadMe <- downloadHandler(filename = "data.csv",
                                         content=function(file){
                                           write.csv(dataset,file)
                                         })
    # output$result <- renderDataTable({
    #   if (input$buildDDS){
    #     #need to figure out filename inputs here...
    #     ddsObjList <- modRIPseq::splitAndBuildDEobjs()
    #     counts(ddsObjList[1])
    #   }
    # })
    output$p1 <- renderPlot({
        # generate bins based on input$bins from ui.R
      if (input$PCA){
      modRIPseq::makePCA(data)
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
