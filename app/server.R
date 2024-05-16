library(shiny)
library(shinyBS)
library(RColorBrewer)
library(ggplot2)
library(biomaRt)
library(png)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(shinyRGL)
library(rgl)
library(ggrepel)
library(readxl)
library(biomaRt)
library(data.table)
library(Seurat)
library(scExtras)
library(patchwork)
library(cowplot)
library(BPCells)
library(SCP)

#Specify color palette for the tSNE and UMAP plots
cpallette=c("#64B2CE", "#DA5724", "#74D944", "#CE50CA", "#C0717C", "#CBD588", "#5F7FC7",
            "#8B4484", "#D3D93E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
            "#8A7C64", "#599861","#000099","#FFCC66","#99CC33","#CC99CC","#666666", "#695F74","#0447F9",
            "#89134F","#2CF7F0","#F72C35","#A5B617","#B05927","#B78ED8","yellow","orange","purple")



server <- function(input, output, session) {
  
  #### Project Description ####
  output$desc <- renderText({
    text="Functional regeneration of the lung’s gas exchange surface following injury requires the coordination of a complex series of cell behaviors within the alveolar niche. Using a multi-modal approach, we have mapped the temporal sequencing of mouse lung regeneration after acute viral injury, demonstrating that this response is asynchronously phased across different cellular compartments. This longitudinal atlas of regeneration has produced a catalogue of new cell states that reflect transient and persistent transcriptional alterations in daughter cells as they transit across axes of differentiation. These new cell states include an injury-induced capillary endothelial cell (iCAP) that arises after injury, persists indefinitely, and shares transcriptional hallmarks with both developing lung endothelium and the endothelial aberrations found in degenerative human lung diseases. This comprehensive atlas of lung regeneration provides a foundational resource to understand the complexity of the cellular and molecular responses to injury, reveals the critical importance of capillary endothelium in maintaining and rebuilding the alveolar niche after injury, and correlates these responses to those found in development and human lung diseases."
    return(text)
  })
  output$geo<- renderText({
    text="The dataset can be found in NCBI GEO using the accession id GSE262927"
    return(text)
  })
  ####### LOAD EXCEL AND POPULATE DROP DOWN FOR PROJECTS #########
  
  readexcel = reactive({
    file = read.csv("data/param.csv")
    return(file)
  })
  
  #Get Project list and populate drop-down
  output$projectlist = renderUI({
    excel=readexcel()
    prj=as.character(excel$projects)
    selectInput("projectlist","Select a project",as.list(sort(as.character(prj))))
  })
 
  #### Load data ####
  
  #Load Rdata
  fileload <- reactive({
    withProgress(session = session, message = 'Loading Dataset...',{
        inFile = paste('data/',as.character(input$projectlist),'.RDS',sep = '')
        scrna=readRDS(inFile)
    })
    return(scrna)
  })
  
  ###################################################
  ###################################################
  ####### Compare Tsne plot with controls  ##########
  ###################################################
  ###################################################
  
  #generate variable list for left plot
  output$tsnea2 = renderUI({
    scrna=fileload()
    metadata=as.data.frame(scrna@meta.data) 
    metadata = metadata %>% select(-orig.ident)
    var=colnames(metadata)
    selectInput("tsnea2","Select a Category",var,"Select a category")
  })
  
  #generate variable list for right plot
  output$tsneb2 = renderUI({
    scrna=fileload()
    metadata=as.data.frame(scrna@meta.data) 
    metadata = metadata %>% select(-orig.ident)
    var=colnames(metadata)
    selectInput("tsneb2","Select a Variable",var,"Select a category")
  })
  

  
  #Genelist for tsne plot A
  output$gene1aui = renderUI({
    scrna=fileload()
    options=sort(rownames(scrna))
    withProgress(session = session, message = 'Generating gene list...',detail = 'Please Wait...',{
      selectizeInput('gene1a', label='Gene Name',choices=options,multiple=FALSE,selected=options[1])})
  })
  
  #Genelist for tsne plot B
  output$gene2aui = renderUI({
    scrna=fileload()
    options=sort(rownames(scrna))
    withProgress(session = session, message = 'Generating gene list...',detail = 'Please Wait...',{
      selectInput('gene2a', label='Gene Name',options,multiple=FALSE, selectize=TRUE,selected=options[1])})
  }) 
  
  #Based on all use input, generate plots using the right category and dimensionality reduction methods
  comptsne2 = reactive({
    scrna=fileload()
    metadata=as.data.frame(scrna@meta.data)
    met= sapply(metadata,is.numeric)
    tsnea=input$tsnea2
    tsneb=input$tsneb2
    feature=names(met[met==TRUE])
    tsne=names(met[met==FALSE])
    
    if(input$categorya2 =="clust"){
      plot1=CellDimPlot(srt = scrna,reduction="umap",group.by = "celltype",label = input$checklabel1,  pt.size = input$pointa2,label.size = 7, theme_use = "theme_blank")
    }else if(input$categorya2=="geneexp"){
      validate(need(input$gene1a %in% rownames(GetAssayData(object=scrna)),"Incorrect Gene name.Gene names are case-sensitive.Please check for typos."))
      plot1=FeatureDimPlot(srt = scrna,reduction="umap", label_repel=T,features = input$gene1a,pt.size = input$pointa2, theme_use = "theme_blank")
      #plot1=eval(parse(text=paste("plot1$`",input$gene1a,"`",sep="")))
    }else if(input$categorya2 =="var" & input$tsnea2 %in% tsne){
      plot1=CellDimPlot(srt = scrna,reduction="umap",group.by = tsnea,label = input$checklabel1, pt.size = input$pointa2,label.size = 7, theme_use = "theme_blank")
    }else if(input$categorya2 =="var" & input$tsnea2 %in% feature){
      plot1=FeatureDimPlot(srt = scrna,reduction="umap",label_repel=T, features = tsnea ,pt.size = input$pointa2, theme_use = "theme_blank")
      #plot1=eval(parse(text=paste("plot1$`",tsnea,"`",sep="")))
    }
    
    if(input$categoryb2 =="clust"){
      plot2=CellDimPlot(srt = scrna,reduction="umap",group.by = "celltype",label = input$checklabel2, pt.size = input$pointa2,label.size = 7, theme_use = "theme_blank")
    }else if(input$categoryb2=="geneexp"){
      validate(need(input$gene2a %in% rownames(GetAssayData(object=scrna)),"Incorrect Gene name.Gene names are case-sensitive.Please check for typos."))
      plot2=FeatureDimPlot(srt = scrna,reduction="umap",label_repel=T, features = input$gene2a,pt.size = input$pointa2, theme_use = "theme_blank")
      #plot2=eval(parse(text=paste("plot2$`",input$gene2a,"`",sep="")))
    }else if(input$categoryb2 =="var" & input$tsneb2 %in% tsne){
      plot2=CellDimPlot(srt = scrna,reduction="umap",group.by = tsneb,label = input$checklabel2, pt.size = input$pointa2,label.size = 7, theme_use = "theme_blank")
    }else if(input$categoryb2 =="var" & input$tsneb2 %in% feature){
      plot2=FeatureDimPlot(srt = scrna,reduction="umap",label_repel=T, features = tsneb,pt.size = input$pointa2, theme_use = "theme_blank")
      #plot2=eval(parse(text=paste("plot2$`",tsneb,"`",sep="")))
    }
    
    p=plot1+plot2
    return(p)
    #p2 <- add_sub(p, paste(input$projectlist,"CompareTsne",sep=""), x = 0.87,vpadding = grid::unit(1, "lines"),size=11)
    #ggdraw(p2)
  })
  
  #render final plot
  output$comptsne2 = renderPlot({
    input$load
    withProgress(session = session, message = 'Generating plot...',detail = 'Please Wait...',{
      comptsne2()
    })
  })
  
  #Handler to download plot
  output$downloadtsneplot <- downloadHandler(
    filename = function() {
      paste0(projectname(),"_CompareTsne.pdf",sep="")
    },
    content = function(file){
      pdf(file,width=14,height = 8,useDingbats=FALSE)
      plot(comptsne2())
      dev.off()
    })
  
  ############################################################
  ############################################################
  ###################### Violin plot #########################
  ############################################################
  ############################################################
  output$vlngene = renderUI({
    scrna=fileload()
    options=sort(rownames(scrna))
    withProgress(session = session, message = 'Generating gene list...',detail = 'Please Wait...',{
      selectInput('vlngene', label='Gene Name',options,multiple=FALSE, selectize=TRUE,selected=options[1])})
  }) 
  
  output$stackvlngene = renderUI({
    scrna=fileload()
    options=sort(rownames(scrna))
    withProgress(session = session, message = 'Generating gene list...',detail = 'Please Wait...',{
      selectInput('stackvlngene', label='Gene Name',options,multiple=TRUE, selectize=TRUE)})
  }) 
  
  output$groupvln = renderUI({
    scrna=fileload()
    #options=scrna@meta.data %>% select()
    options=c("experimental_group","lineage","celltype","subtype")
      selectInput('groupvln', label='Group by',options,multiple=FALSE, selectize=TRUE,selected=options[1])
  }) 
  
  output$vlnsplitby = renderUI({
    scrna=fileload()
    options=c("experimental_group","lineage","tamoxifen_start_day","sacrifice_day")
    selectInput('vlnsplitby', label='Split by',options,multiple=FALSE, selectize=TRUE,selected=options[1])
  })
  
  plotvlnplot = reactive({
    scrna=fileload()
    if(input$stacked ==TRUE){
      p= FeatureStatPlot(scrna, stat.by=input$stackvlngene,pt.size=0,split.by =input$vlnsplitby ,group.by=input$groupvln,theme_use = "theme_blank",stack = T) 
    }else{
    p= FeatureStatPlot(scrna, stat.by=input$vlngene,pt.size=0,split.by =input$vlnsplitby ,group.by=input$groupvln,theme_use = "theme_blank") 
    }
    return(p)
  })
  
  
  #render final plot
  output$plotvlnplot = renderPlot({
    withProgress(session = session, message = 'Generating...',detail = 'Please Wait...',{
      plotvlnplot()
    })
  })
  
  #Handler to download plot
  output$downloadvlnplot <- downloadHandler(
    filename = function() {
      paste0(input$projectlist(),"_VlnPlot.pdf",sep="")
    },
    content = function(file){
      pdf(file,width=14,height = 8,useDingbats=FALSE)
      plot(plotvlnplot())
      dev.off()
    })
  
}#end of server
