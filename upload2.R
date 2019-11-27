library(shiny)
library(shinyjs)
library(phonTools)
library(DBI)
library(stringr)

source("common.R")

# Single page app to upload wave file
# 

ui <- fluidPage(
  titlePanel("Nahrávanie rečových súborov"),
  useShinyjs(),
  bootstrapPage(
    div(id = "divId",
      textAreaInput("email", "Váš email:", width = "100%"),
      hidden(div(id = "errorEmail", HTML("Email nemá správny formát"), style = "color:red;")),
      textAreaInput("nick", "Vaša prezývka:"),
      hidden(div(id = "errorNick", HTML("Zlý nick - má byť 3-10 znakov"), style = "color:red;")),
      actionButton("idButton", "OK")
      ),
    
    hidden(div(id = "divFile", 
      sidebarLayout(
        sidebarPanel(
          HTML("<H4>Najviac nahratých samohlások</H4"),
          tags$br(),
          tableOutput("leaders"),
          HTML("<H4>Najmenej nahratých samohlások</H4>"),
          tableOutput("laggers")
        ),
        
        mainPanel(
          fileInput("file1", label = "Vyber audio súbor", accept = "audio/*", multiple = T),
          tableOutput("recorded")           
        )
      )
    
      )),
    hidden(div(id = "divWords",
          fixedRow(
            column(4, "Prehraj nahrávku",   
              uiOutput(outputId = "play")),
            column(8, "Pridaj textový popis", 
              textAreaInput("words", "Čo bolo povedané: ", width = "100%"),
              fixedRow(column(3,
                    actionButton("submit", "Nahraj")),
                    column(3, 
                    actionButton("cancel", "Vynechaj"))))
        ) #fixedRow
      ) #div
    ) # hidden
  ) # bootstrapPage
) # fluidPage



server <- function(input, output, session) {
  
  valNewId <- reactiveVal(NULL)
  valIdFile <- reactiveVal(0)
  valTable <- reactiveVal(0)
  
  # handle user info
  # 
  observeEvent(input$idButton, {
                 if (input$idButton > 0) {
                   
                   email_regex = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"
                   good_email = str_detect(input$email, email_regex)
                   
                   nick_regex = "^[a-zA-Z0-9.-_]{3,10}$"
                   good_nick = str_detect(input$nick, nick_regex)
                   
                   if (good_nick) {
                     shinyjs::hide(id = "errorNick")
                   } else {
                     shinyjs::show(id = "errorNick")
                   }
                   
                   if (good_email)  {
                     shinyjs::hide(id = "errorEmail")
                   } else {
                     shinyjs::show(id = "errorEmail")
                   }
                   
                   if (good_nick & good_email) {
                     print(sprintf("logon %s", input$email))
                     hide("divId")
                     shinyjs::show(id = "divFile")
                   }
                 }
               })
  
  observeEvent(input$file1, {
     if (is.null(input$file1)) {
       # do nothing
       return()
     }
     
     shinyjs::show(id = "divWords")
     shinyjs::hide(id = "divFile")
     valIdFile(1)
     
     newId <- list()
     con <- dbConnect(RSQLite::SQLite(), dbFile)
     for (r in 1:nrow(input$file1)) {
       row <- input$file1[r, ]
       # print(sprintf("Name: %s, path %s", 
                     # row$name, row$datapath))
       
       if (tolower(substrRight(row$datapath,3)) == "wav") {
         datapath = row$datapath
       } else {
         datapath <- tempfile(fileext =  ".wav")
         # print(sprintf("converting audio to wav file %s", datapath))
         result = system2(command = "ffmpeg", 
		args = sprintf("-i %s %s", row$datapath,  datapath))
         # print(result)
       }
       snd = loadsnd(datapath)
       
       push_wav <- function(file_name) {
         ins1 <- dbSendStatement(con, 
          "INSERT INTO WavFiles 
          (OriginalName,  Email, NickName, Frequency, Duration) 
          VALUES (?,?,?,?,?)")
         
         dbBind(ins1, list(file_name, input$email, input$nick, snd$fs, snd$duration))
         # print(sprintf("Rows added = %d", dbGetRowsAffected(ins1)))
         lrid <- dbSendQuery(con, "select last_insert_rowid()")
         f2 <- dbFetch(lrid)
         dbClearResult(lrid)
         # print(f2)
         return(f2[1,1])
       }
       
       MAX_DURATION = 5000# in milliseconds
       
       if (snd$duration <= MAX_DURATION) {
         ni <- push_wav(row$name)
         newId <- c(newId, ni)
         newFile <- sprintf(sound_file_format, ni)
         file.copy(from = datapath, to = newFile, overwrite = T)
       } else {
         n <- ceiling(snd$duration / MAX_DURATION)
         ns1 <- (MAX_DURATION / 1000) * snd$fs
         starts <- round(seq(1, length(snd$sound) - ns1 + 1, length.out  = n))
         for (si in 1:n) {
           s = starts[si]
           ni <- push_wav(sprintf("%s (%d. časť)", row$name, si))
           newId <- c(newId, ni)
           newFile <- sprintf(sound_file_format, ni)
           writesound(snd$sound[s:(s + ns1 - 1)], 
                      filename = newFile, 
                      fs = snd$fs)
         }
       }
     }
     
     dbDisconnect(con)
     # print("done copying files")
     # print(newId)
     valNewId(newId)
   })
  
  nextFile <- function(id) {
    if (id + 1 > length(valNewId())) {
      valIdFile(0)
      shinyjs::hide(id = "divWords")
      shinyjs::reset(id = "words")
      removeUI(selector = "#divFile", 
               immediate = T,
               multiple = T)
      
      insertUI("#divId", "afterEnd", 
               ui = div(id = "divFile", 
                        sidebarLayout(
                          sidebarPanel(
                            HTML("<H4>Najviac nahratých</H4"),
                            tags$br(),
                            tableOutput("leaders"),
                            HTML("<H4>Najmenej nahratých</H4>"),
                            tableOutput("laggers")
                          ),
                          
                          mainPanel(
                            fileInput("file1", label = "Vyber ďalší audio súbor", 
                                      accept = "audio/wav", multiple = T),
                            tableOutput("recorded")           
                          )
                        )
               )
      )
    } else {
      valIdFile(id + 1)
      reset("words")
    }
  }
  
  observeEvent(input$submit, {
     #print("in submit")
     if (input$submit > 0) {
       #print("in submit 0 ")
       
       tabcounter <- valTable()
       valTable(tabcounter + 1)
       print(sprintf("tabcounter %d", tabcounter))
       
       id <- valIdFile()
       
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       
       ins2 <- dbSendStatement(con, 
          "UPDATE WavFiles SET Words = ? WHERE rowid = ?")
       
       wavId = valNewId()[[id]]
       dbBind(ins2, list(input$words, wavId))
      
       #print(sprintf("Rows changed = %d", dbGetRowsAffected(ins2)))
       dbClearResult(ins2)
    

       words1 = str_split(input$words, "[ ,.!?;]")[[1]] 
       words2 = words1[words1 != ""]
       
       for (iwd in 1:length(words2)) {
          wd = words2[iwd]
          pos = str_locate_all(wd, "[aeiouyáéíóúýäô]+")[[1]]
          
          if (nrow(pos) > 0) {
            for (ivoc in 1:nrow(pos)) {
              ins2 <- dbSendStatement(con, "INSERT INTO Segments (
                                      WavFile, WordPosition, VocalPosition, 
                                      PreVocal, Vocal, PostVocal) VALUES
                                      (?,?,?,?,?,?)")
              
              dbBind(ins2, list(wavId, iwd, ivoc, 
                                substr(wd, 1, pos[ivoc,1] - 1),
                                substr(wd, pos[ivoc,1], pos[ivoc,2]),
                                substr(wd, pos[ivoc,2] + 1, str_length(wd))
              )
              )
              dbClearResult(ins2)
            }
          }
          
       }
       dbDisconnect(con)
       
       nextFile(id)
     }
   })
  
  observeEvent(input$cancel, {
    if (input$cancel > 0) {
      tabcounter <- valTable()
      valTable(tabcounter + 1)
      print(sprintf("tabcounter %d", tabcounter))
      id <- valIdFile()
      wavId = valNewId()[[id]]
      
      
      con <- dbConnect(RSQLite::SQLite(), dbFile)
      ins2 <- dbSendStatement(con, "DELETE FROM WavFiles WHERE rowid = ?")
      dbBind(ins2, wavId)
      dbClearResult(ins2)
      dbDisconnect(con)
      
      nextFile(id)
    }
  })
    
    output$play <- renderUI({
        id <- valIdFile()
        if (id > 0) {
          print("rendering audio tag")
          print(valNewId())
          print(id)
          newFile <- sprintf(short_sound_file_format, valNewId()[[id]])
          div(id = "audioDiv", 
              tags$audio(src = newFile, type = "audio/wav",  controls = T)
          )
        }
      }
    )
    
    output$recorded <- renderTable({
      f1 <- valTable()
      con <- dbConnect(RSQLite::SQLite(), dbFile)
      ins2 <- dbSendQuery(con, 
                 "SELECT OriginalName AS [Súbor], 
			COUNT(*) AS [Počet samohlások],
			Words AS [Obsah]  
                 FROM WavFiles W JOIN Segments  S
                 ON W.rowid = S.WavFile
                 WHERE  email = ?
                 GROUP BY W.OriginalName, W.RowId, W.Words")
      dbBind(ins2, input$email)
      res <- dbFetch(ins2)
      dbClearResult(ins2)
      dbDisconnect(con)
      res
    })
    
    queryDb <- function(query) {
      f1 <- valTable()
      con <- dbConnect(RSQLite::SQLite(), dbFile)
      ins2 <- dbSendQuery(con, query)
      res <- dbFetch(ins2)
      dbClearResult(ins2)
      dbDisconnect(con)
      res
    }
    
    
    output$leaders <- renderTable({
     queryDb("SELECT NickName as [prezývka], COUNT(*) AS [nahratých] FROM WavFiles W 
             JOIN Segments S ON S.WavFile = W.rowid
             GROUP BY NickName ORDER BY COUNT(*) DESC LIMIT 3
             ")
    })
    
    output$laggers <- renderTable({
      queryDb("SELECT NickName as [prezývka], COUNT(*) AS [nahratých] FROM WavFiles W 
             JOIN Segments S ON S.WavFile = W.rowid
             GROUP BY NickName ORDER BY COUNT(*) ASC LIMIT 3
             ")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
