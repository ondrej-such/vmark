#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(future)
library(promises)
source("common.R")

plan(multicore)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "annotate.css")
   ),
   titlePanel("Označovanie samohlások"),
   includeScript("www/message.js"),
   #tags$script(src = "annotate.js"),
   useShinyjs(),
   extendShinyjs("www/annotate.js"),
  
   div(id = "divId",
      textAreaInput("email", "Váš email:", width = "100%"),
      textAreaInput("nick", "Vaša prezývka:"),
      hidden(div(id = "logonError", HTML("Zlé prihlásenie - neexistujú dáta"), style = "color:red;")),
      tags$br(),
      actionButton("idButton", "OK")
   ),
   # tags$script(HTML(
   #   "document.body.style.backgroundColor = 'skyblue';"
   # ))
   hidden(div(id = "divSummary",
       sidebarLayout(
         
         sidebarPanel(
           HTML("<H4>Najviac označených</H4"),
           tags$br(),
           tableOutput("leaders"),
           HTML("<H4>Najmenej označených</H4>"),
           tableOutput("laggers")
         ),
         mainPanel(
            uiOutput("summary")))
   )),
  
   hidden(div(id = "divSpectra",
      sidebarLayout(
        sidebarPanel(class="wrapper", id = "divDetail",
                     actionButton("detailDone", "Návrat späť", style="margin:5px;"),
                     tags$br(),
                     tags$table(tags$tbody(tags$tr(tags$td(
                     selectInput("det_size", label = "Dĺžka výseku", 
                                 choices = c( "300ms" = 150, "400ms" = 200, "500ms"= 250),
                                 selected = "200")
			), tags$td(
                     selectInput("win_size", label = "Dĺžka okna", 
                                 choices = c("8ms" = 8, "16ms" = 16, "24ms" = 24, "32ms" = 32),
                                 selected = "24")
			)))),
                     tags$table(
                       tags$tbody(tags$tr(tags$td("Samohláska"),tags$td(uiOutput("phones"))), 
                                  tags$tr(tags$td("Začiatok"),tags$td(id = "phoneStart")), 
                                  tags$tr(tags$td("..zaznačený"),tags$td(id = "phoneStartDB")),
                                  tags$tr( tags$td("Koniec"),tags$td(id = "phoneEnd")),
                                  tags$tr(tags$td("..zaznačený"),tags$td(id = "phoneEndDB"))
                       )),
                     tags$br(),
                     shinyjs::disabled(actionButton("detailNext", "Zaznač")), 
                     shinyjs::disabled(actionButton("detailShow", "Zobraz")),
                     # shinyjs::disabled(actionButton("detailRight", "Posuň")),
                     tags$br(),
                     uiOutput("markedAudio")),
    mainPanel( div(class="wrapper",
                   uiOutput("allSpectrum", click = "plot1_click")
      ), width = 6),  position = "right"
    ),

     div(id = "details",
         imageOutput("detailSpectrum"),
         HTML("<canvas id='spectrum2' class='spectrum' width=600 height=400 
              onclick='spectrum2_click(event)'></canvas>")
     )
   ) # spectra
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   specVal <- reactiveValues(x3 = -1, x4 = -1)
   selVal <- reactiveVal(-1)
   
   do_annotation <- function(con) {
     q3 <- dbSendQuery(con, 
                "SELECT Vocal, Start, Finish FROM Segments 
                WHERE wavFile = ? AND Finish is NOT NULL")
     dbBind(q3, input$wavId)
     res <- dbFetch(q3)
     
     js$erase_spectrum2_ann()
     for (i in 1:nrow(res)) {
       js$draw_label(vocal = res[i,1], 
                     start = usr2dev(res[i,2]), 
                     finish = usr2dev(res[i,3]))
     }
     dbClearResult(q3)
   }
   
   output$summary <- renderUI({
     if (!(input$idButton > 0) || (!is.null(input$wavId) && (input$wavId > 0))) {
       return()
     }
     con <- dbConnect(RSQLite::SQLite(), dbFile)
     ins2 <- dbSendQuery(con, "SELECT W.rowid, 
                                  words, 
                                  sum(CASE  WHEN Start IS NULL THEN 0 ELSE 1 END) as C1, 
                                  sum(1) as C2
                                  FROM WavFiles  AS W 
                                  JOIN Segments AS S on W.rowid = S.wavFile
                                  WHERE email = ? and nickName = ?
                                  GROUP BY W.rowid, words ORDER BY C2 - C1 DESC, W.Rowid DESC")
     dbBind(ins2, list(input$email, input$nick))
     res <- dbFetch(ins2)
     dbClearResult(ins2)
     dbDisconnect(con)
    
     tags$table(
       tags$thead(tags$tr(tags$td("Slovný prepis"), 
                          tags$td(HTML("Označené <br> samohlásky")),
                          tags$td(HTML("Celkovo <br> samohlások")))),
       tags$tbody(
         tagList(
            lapply(1:nrow(res), function(x) {
                  trtext = sprintf("<tr onclick='draw_spec(%d)' >
                               <td> %s</td>
                               <td> %d</td>
                               <td> %d</td>
                               </tr>", res[x,1],  res[x,2], res[x,3], res[x,4]);
                  shiny::HTML(trtext)
                }
              )
          )
       )
     )
    }
   )
   
   observeEvent(input$idButton, {
     if (input$idButton > 0) {
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       q2 <- dbSendQuery(con, "SELECT * FROM WavFiles WHERE Email = ? AND NickName = ? LIMIT 1")
       dbBind(q2, list(input$email, input$nick))
       rs <- dbFetch(q2)
       if (nrow(rs) > 0) {
         shinyjs::hide(id = "divId")
         shinyjs::show(id = "divSummary")
       } else {
         shinyjs::show(id = "logonError")
       }
     }
   })
   
   output$phones <- renderUI({
     if (!is.null(input$wavId) && (input$wavId > 0)) {
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       q2 <- dbSendQuery(con, "SELECT 
        WordPosition ||  ' ' || PreVocal || ' ' ||
         upper(Vocal) || ' ' || lower(PostVocal) AS Label, Rowid, Start, Finish
                                    From Segments
                                    WHERE wavfile = ? ORDER BY Rowid")
       dbBind(q2, input$wavId)
       res <- dbFetch(q2)
       # print(sprintf("changing selVal to value %d", res[1,2]))
       selVal(res[1,2])
       # print(res[1,])
       # js$write_boundaryDB(time = if (is.null(res[1,3])) "" else res[1,3], end = 0)
       # js$write_boundaryDB(time = if (is.null(res[1,4])) "" else res[1,4], end = 1)
       dbDisconnect(con)
       tags$select(id = "select1", 
                   style = "margin-bottom: 2px;margin-top:2px;font-size:14px;",
                   onChange = "Shiny.setInputValue('sel2Id', this.options[this.selectedIndex].value)",
                   
         tagList(
           lapply(1:nrow(res), function(x) {
             # print(res[x,])
             stext <- sprintf("<option value='%d'>%s</option>", res[x,2], res[x,1]);
             shiny::HTML(stext)
           })
         )
       )
     }
   })
   
   observeEvent(input$wavId, {
      if (!is.null(input$wavId)) {
        if(input$wavId > 0) {
          shinyjs::show("divSpectra")
          shinyjs::hide("divSummary")
        }  else {
          shinyjs::show("divSummary")
          shinyjs::hide("divSpectra")
        }
      }
   })
   
   output$allSpectrum <- renderUI({
     if (!is.null(input$wavId) && (input$wavId > 0)) {
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       ins2 <- dbSendQuery(con, "SELECT deviceXmax,
                                    deviceXmin,
                                    userXmin,
                                    userXmax,
                                    userPlotXmin,
                                    userPlotXmax
                           FROM Spectrograms WHERE wavid = ?")
       
       dbBind(ins2, list(input$wavId))
       res <- dbFetch(ins2)
       dbClearResult(ins2)
       dbDisconnect(con)
       specVal$deviceXmax <- res[1,1]
       specVal$userXmin <- res[1,3]
       specVal$userXmax <- res[1,4]
       specVal$userPlotXmin <- res[1,5]
       specVal$userPlotXmax <- res[1,6]
       
       div(
          HTML(sprintf("<img src='spec/spec%d-24.png' />", input$wavId)),
          HTML(sprintf("<canvas id='spectrum2_ann' class='spectrum' height=400 width=%d'></canvas>",
                       res[1,1])),
          HTML(sprintf("<canvas id='spectrum1' class='spectrum' height=400 width=%d onclick='spectrum1_click(event)'></canvas>",
                       res[1,1]))
       )
     }
   })
   
   usr2dev <- function(x) {
     return ((x - specVal$userXmin) / (specVal$userXmax - specVal$userXmin) *
              specVal$deviceXmax)
   }
   
   dev2usr <- function(x) {
     return ((x / specVal$deviceXmax) *
               (specVal$userXmax - specVal$userXmin) + specVal$userXmin)
   }
   
   # input$det_size <- 150
   det_size <- function() {
     as.numeric(input$det_size)
   }
   
   observeEvent(input$x1, {
      # print("handling changed input$x1")
      mid <- dev2usr(input$x1)
      
      if ((mid >= specVal$userPlotXmin + det_size()) && (mid <= specVal$userPlotXmax - det_size())) {
        low = mid - det_size()
        high = mid + det_size()
        
        # these two if's are not needed due to above check
        if (low < specVal$userXmin) {
          low = specVal$userXmin
          high = low + 2 * det_size()
        }
        if (high > specVal$userXmax) {
          high = specVal$userXmax
          low = high - 2 * det_size()
        }
        js$spectrum1_draw2(usr2dev(low),
                           usr2dev(high))
        # shinyjs::enable(id = "detailRight")
      }
      
      
      con <- dbConnect(RSQLite::SQLite(), dbFile)
      do_annotation(con)
      dbDisconnect(con)
      #shinyjs::show(id = "divDetail")
      specVal$x3 = -1
      specVal$x4 = -1
      js$erase_detail()
    })
   
   output$markedAudio <- renderUI({
     if (!is.null(specVal$x3) && (specVal$x3 > 0) 
         && !is.null(specVal$x4)  && (specVal$x4 > 0) &&
         !is.null(input$wavId) && (input$wavId > 0)) {
       # print("starting markedAudio")
       x1 = dev2usr(input$x1)
       x3 = dev2usr2(specVal$x3)
       x4 = dev2usr2(specVal$x4)
       isolate(if (!is.null(specVal$markedAudio)) {
         file.remove(specVal$markedAudio)
       })
       
       fname = tempfile("marked", "www/tmp", ".wav")
       specVal$markedAudio = fname
       snd = loadsnd(sprintf(sound_file_format, input$wavId))
       ns = length(snd$sound)
       
       ts = x1 - det_size() + x3
       te = x1 - det_size() + x4

       writesound(snd$sound[(ns * (ts) / snd$duration):
                              (ns * (te) / snd$duration)],
                  filename = fname, fs = snd$fs)
       tags$audio(src = substring(fname, 4), controls = T, autoplay = T)
     }
   })

   output$allAudio <- renderUI({
      if(!is.null(input$wavId) && (input$wavId > 0)) {
        tags$audio(src = sprintf(short_sound_file_format, input$wavId), controls = T)
      }
    })
   
   output$detailSpectrum <- renderImage({
     if (!is.null(input$x1) && (input$x1  > 0) && !is.null(input$wavId) && (input$wavId > 0)) {

       snd = loadsnd(sprintf(sound_file_format, input$wavId))
       x1 = dev2usr(input$x1)
       ns = length(snd$sound) # number of samples
       
       s1 = round(ns * (x1 - det_size()) / snd$duration)
       s2 = round(ns * (x1 + det_size()) / snd$duration)

       if ((s1 < 1) || (s2 > ns)) {
         return(list(src = "www/tmp/detim774774eb8435.png")) #BUGBUG
       }
       snd2 = list(filename = "cut.wav", fs = snd$fs, duration = 2 * det_size(), 
                   sound = snd$sound[s1:s2])
       class(snd2) <- "sound"

       plot_detail <- function(snd3, win_size) {
         fname <- tempfile(pattern = "detim", tmpdir = "www/tmp", fileext = ".png")
         # print(sprintf("plot_detail %s", fname))
         png(fname, height = 400, width = 600)
         plot.sk(spectrogram(snd3, windowlength = win_size, timestep = -300, show = F))
         # print("plot_detail wrote spectrogram")
         detailUserXmin = grconvertX(0, "ndc", "user")
         detailUserXmax = grconvertX(1, "ndc", "user")
         detailPlotXmin = grconvertX(0, "npc", "user")
         detailPlotXmax = grconvertX(1, "npc", "user")
         dev.off()
         list(fname, detailUserXmin, detailUserXmax, detailPlotXmin, detailPlotXmax)
       }
       
       sp1 = future(plot_detail(snd3, win_size),
                    globals = list(snd3 = snd2,
                                   win_size = as.numeric(input$win_size)))
       # print(sp1)
       

       v = then(sp1, ~{
         # print(.$fs)
         specVal$detailUserXmin = .[[2]] #grconvertX(0, "ndc", "user")
         specVal$detailUserXmax = .[[3]] #grconvertX(1, "ndc", "user")
         specVal$detailPlotXmin = .[[4]] #grconvertX(0, "npc", "user")
         specVal$detailPlotXmax = .[[5]] #grconvertX(1, "npc", "user")
         list(src = substring(.[[1]],1), width = 600, height = 400)
         #stopifnot(grconvertX(1, "ndc", "device") == 600)
       }) # %...T>% print  %...>%  {.}
       
       return(v)
     }
     # print("returning default image")
     return(list(src = "www/tmp/detim774774eb8435.png"))
    }, deleteFile = T
   )
   
   usr2dev2 <- function(x) {
     # print(sprintf("detailUserXmin %f", specVal$detailUserXmin))
     return ((x - specVal$detailUserXmin) / (specVal$detailUserXmax - specVal$detailUserXmin) *
               600)
   }
   
   dev2usr2 <- function(x) {
     # print(sprintf("detailUserXmin %f", specVal$detailUserXmin))
     return ((x / 600) *
               (specVal$detailUserXmax - specVal$detailUserXmin) + specVal$detailUserXmin)
   }
   
   observeEvent(input$x2, {
     
       if (is.null(input$x1) || (input$x1 < 0))
         return()
       x2 <- input$x2;
       x2u <- dev2usr2(x2);
       
       x1 = dev2usr(input$x1);
       
       change4 = F;
       # print(c(x2u, specVal$detailPlotXmin))
       if ((x2u >= specVal$detailPlotXmin) && (x2u <= specVal$detailPlotXmax)) {
         if (specVal$x3 < 0) {
           specVal$x3 = x2
         } else if (specVal$x3 > x2) {
           specVal$x3 = x2
           specVal$x4 = -1
         } else if (specVal$x4 < 0) {
           specVal$x4 = x2
           change4 = T;
         } else {
           specVal$x3 = x2
           specVal$x4 = -1
         }
       }
       
       if (change4) {
         x4 = dev2usr2(specVal$x4)
         js$write_boundary(time = round(x1 - det_size() + x4), end = 1)
       } else {
         x3 = dev2usr2(specVal$x3)
         js$write_boundary(time = round(x1 - det_size() + x3), end = 0)
         js$write_boundary(time = "", end = 1)
       }
     }
   )
   
   observeEvent(specVal$x3, {
     if (specVal$x3 > 0) {
       js$erase_detail()
       js$draw_detail(specVal$x3, 0)
       shinyjs::disable("detailNext")
     }
   })
   
   observeEvent(specVal$x4, {
     if (specVal$x4 > 0) {
       js$draw_detail(specVal$x4, 1)
       shinyjs::enable("detailNext")
     }
   })
   
   observeEvent(input$detailDone, {
     selVal(-1)
     specVal$x3 = -1
     specVal$x4 = -1
     js$reset_spec()
     # shinyjs::disable(id = "detailRight")
     #shinyjs::hide(id = "divDetail")
     js$erase_detail()
     js$write_boundary(time = "", end = 0)
     js$write_boundary(time = "", end = 1)
     #shinyjs::hide(id = "divSpectra")
     #shinyjs::show(id = "divSummary")
   })
   
   observeEvent(input$detailNext, {
     session$sendCustomMessage("handler1", 1)
   })
   
   observeEvent(input$selId, {
     if (!is.null(input$selId) && (input$selId >= 0)) {
       if (!is.null(specVal$x3) && (specVal$x3 > 0) && !is.null(specVal$x4)  && (specVal$x4 > 0)) {
         x1 = dev2usr(input$x1)
         x3 = dev2usr2(specVal$x3)
         x4 = dev2usr2(specVal$x4)
         ts = round(x1 - det_size() + x3)
         te = round(x1 - det_size() + x4)
         
         # print(input$selId)
         # print(class(input$selId))
         con <- dbConnect(RSQLite::SQLite(), dbFile)
         q2 <- dbSendStatement(con, 
                               "UPDATE Segments SET Start = ?, Finish = ? WHERE rowid = ?")
         dbBind(q2, list(ts, te, input$selId))
         dbClearResult(q2)
         do_annotation(con)
         if (input$selId == input$sel2Id) {
           # print("apparently there's only one vocal segment")
           js$write_boundaryDB(time = ts, end = 0)
           js$write_boundaryDB(time = te, end = 1)
         }
         dbDisconnect(con)
         js$reset_selId()  # maybe move outside if ???
       }
     }
   })
   
   observeEvent(input$detailShow,
      {
        con <- dbConnect(RSQLite::SQLite(), dbFile)
        ins2 <- dbSendQuery(con, "SELECT (Start + Finish) / 2 FROM Segments WHERE rowid = ?")
        dbBind(ins2, selVal())
        rs <- dbFetch(ins2)
        dbClearResult(ins2)
        dbDisconnect(con)
        new_x1 <- rs[1,1]
        if (is.null(new_x1) || is.na(new_x1))
          return()
        # print(sprintf("new_x1 = %f", new_x1))
        if (new_x1 <= det_size() + specVal$userPlotXmin) {
          print("bumping off low")
          new_x1 = det_size() + specVal$userPlotXmin + 1
        }
        if (new_x1 > specVal$userPlotXmax - det_size() ) {
          # print("bumping off high")
          new_x1 = specVal$userPlotXmax - det_size() - 1
        }
        # print(sprintf("new_x1 = %f", new_x1))
        js$set_x1(x1 = usr2dev(new_x1))
      }
   )
   
   # observeEvent(input$detailRight,
    # {
       # if(!is.null(input$x1) && (input$x1 > 0)) {
         # new_x1 <- dev2usr(input$x1) + det_size() - as.numeric(input$win_size)
         # if (is.null(new_x1) || is.na(new_x1))
           # return()
         # print(sprintf("new_x1 = %f", new_x1))
         # if (new_x1 > specVal$userPlotXmax - det_size() ) {
           # print("bumping off high")
           # new_x1 = specVal$userPlotXmax - det_size() - 1
         # }
         # print(sprintf("new_x1 = %f", new_x1))
         # js$set_x1(x1 = usr2dev(new_x1))
       # }
    # }
   # )
  
   observeEvent(selVal(), {
     if ((selVal() > 0)) {
       idn <- input$detailNext
       # print(sprintf("observeEvent selVal: selVal %d input$detailNext %d", selVal(), idn))
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       ins2 <- dbSendQuery(con, "SELECT Start, Finish FROM Segments WHERE rowid = ?")
       dbBind(ins2, selVal())
       rs <- dbFetch(ins2)
       dbClearResult(ins2)
       dbDisconnect(con)
       if (!is.null(rs[1,1]) && !is.na(rs[1,1])) {
         shinyjs::enable(id = "detailShow")
         js$write_boundaryDB(time = rs[1,1], end = 0)
         js$write_boundaryDB(time = rs[1,2], end = 1)
       } else {
         shinyjs::disable(id = "detailShow")
         js$write_boundaryDB(time = "", end = 0)
         js$write_boundaryDB(time = "", end = 1)
       }

     } else {
       shinyjs::disable("detailShow")
       js$write_boundaryDB(time = "", end = 0)
       js$write_boundaryDB(time = "", end = 1)
     }
   })
   
   output$phoneEndDB <-renderText({
     if ((selVal() > 0)) {
       input$detailNext
       idn <- input$detailNext
       # print(sprintf("end %d", idn))
       con <- dbConnect(RSQLite::SQLite(), dbFile)
       ins2 <- dbSendQuery(con, "SELECT Finish FROM Segments WHERE rowid = ?")
       dbBind(ins2, selVal())
       rs <- dbFetch(ins2)
       dbClearResult(ins2)
       dbDisconnect(con)
       return(rs[1,1])
     }
     ""
   })
   
   observeEvent(input$sel2Id, {
     newVal = as.numeric(input$sel2Id)
     # print(sprintf("sel2Id %d", newVal))
     selVal(newVal)
   })
   
   queryDb <- function(query) {
     f1 <- input$detailDone
     con <- dbConnect(RSQLite::SQLite(), dbFile)
     ins2 <- dbSendQuery(con, query)
     res <- dbFetch(ins2)
     dbClearResult(ins2)
     dbDisconnect(con)
     res
   }
   
   output$leaders <- renderTable({
     queryDb("SELECT NickName as [prezývka], COUNT(*) AS [označených] FROM WavFiles W 
             JOIN Segments S ON S.WavFile = W.rowid
             WHERE S.Finish IS NOT NULL
             GROUP BY NickName ORDER BY COUNT(*) DESC LIMIT 3
             ")
   })
   
   output$laggers <- renderTable({
     queryDb("SELECT NickName as [prezývka], COUNT(*) AS [označených] FROM WavFiles W 
             JOIN Segments S ON S.WavFile = W.rowid
             WHERE S.Finish IS NOT NULL
             GROUP BY NickName ORDER BY COUNT(*) ASC LIMIT 3
             ")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

