library(shiny)

#funkce pro vypocet koncentrace redeni dna - c1*V1=c2*V2 - vysledek musi byt cislo (numeric(double))
calc_V1 <- function(C1, C2, V2) {
  if(any(is.na(c(C1, C2, V2))) || any(c(C1, C2, V2) <= 0)) return(NA_real_) #jsou vsechny hodnoty kladne a zadane
  if(C2 >= C1) return(NA_real_) # koncentrace nesmí byt nizsi nez na tu kterou redim
  (C2 * V2) / C1
}

# funkce pro zaokrouhleni na zadana desetinna mista
round_to <- function(x, d) {
  if (is.null(d) || d < 0) return(x) #s NA hodnotami nepracuju a mensima nez 0
  round(x, digits = d)
}

#userinterface - vzhled, tlacitka, vkladani hodnot
ui <- fluidPage(
  titlePanel("DNA ředění"),
  sidebarLayout(
    sidebarPanel(
      numericInput("C2", "Cílová koncentrace (ng/µl)", value = 5, min = 0.0001),
      numericInput("V2", "Výsledný objem na vzorek (µl)", value = 50, min = 0.01),
      numericInput("round", "Zaokrouhlit na (počet desetinných míst)", value = 0, min = 0, step = 1),
      hr(),
      h5("Zadání vstupních koncentrací (stock, ng/µl)"),
      tags$small("Vložte jednu koncentraci na řádek, používejte desetinnou tečku."),
      textAreaInput("bulk", label = NULL, value = "", rows = 6, placeholder = "např.\n120\n55\n27.5"),
      actionButton("calculate", "Vypočítej"),
      hr(),
      h5("Interaktivní přidávání"),
      numericInput("single_in", "Koncentrace nového vzorku (ng/µl)", value = 200, min = 0.0001),
      actionButton("add_sample", "Přidat vzorek"),
      actionButton("clear_all", "Vymazat vše", style = "color: white; background-color: #d9534f"),
      hr(),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      h4("Výsledné ředění"),
      tableOutput("results_table"),
      hr(),
      verbatimTextOutput("warnings")
    )
  )
)


server <- function(input, output, session) {
  # ulozeni vzorku jako reaktivni vektory
  samples <- reactiveVal(numeric(0))
  # vlozeni vice vzorku zaroven kazdy na novy radek
  observeEvent(input$calculate, {
    vals <- as.numeric(strsplit(input$bulk, "\n")[[1]]) #rozdeleni textu co se vlozil a prevod na ciselne hodnoty oddelovac radek a list vraci
    samples(c(samples(), vals)) # prevod na ciselny vektor
    updateTextAreaInput(session, "bulk", value = "") #cleanup pole 
  })
  # pridani jednoho vzorku navic
  observeEvent(input$add_sample, {
    samples(c(samples(), input$single_in)) # nove hodnoty k stavajicim do vektoru
  })
  # vymazani vseho
  observeEvent(input$clear_all, {
    samples(numeric(0)) #vycisteni vseho - prepsani vzorku na prazdny vektor
  })
  #vysledky redeni
  results <- reactive({
    
    s <- samples()
    if (length(s) == 0) return(NULL) #nepocitat dokud nemam hodnoty zadane
    
    # vypocet objemu DNA pomoci prvni funkce pro kazdy prvek vektoru
    V1 <- sapply(s, calc_V1,
                 C2 = input$C2,
                 V2 = input$V2)
    
    # vypocet vody 
    water <- input$V2 - V1
    
    #tvorba tabulky a zaokrouhlovani, korekce nizkych koncentraci vstupnich
    data.frame(
      vzorek = seq_along(s), #indexovani vzorku
      vzorek_ng_ul = s,
      vzorek_ul = round_to(V1, input$round),
      voda_ul = round_to(water, input$round),
      poznamka = ifelse(is.na(V1),
                        "Nelze ředit (C1 < C2)",
                        "")
    )
  })
  #zobrazit ve forme tabulky a nez se spocita ukazat hlasku
  output$results_table <- renderTable({
    df <- results() #nacteni vysledku
    if (is.null(df)) {
      data.frame(info = "Zatím nebyly zadány žádné vzorky")
    } else {
      df
    }
  })
  #varovani vzorky co maji nizsi koncentraci nez chci ziskat jeste pod tabulkou
  output$warnings <- renderText({ #renderText - textové varovani
    df <- results() #nacteni vysledku
    if (is.null(df)) return("") #kdyz nejsou data nezobrazovat
    if (any(is.na(df$vzorek_ul))) { 
      "Některé vzorky nelze ředit, protože jejich koncentrace je nižší než cílová."
    } else {
      ""
    }
  }) #pouze pokud to plati aspon pro jeden (existuje NA)
  #stazeni tabulky do csv - nemusi byt
  output$download_csv <- downloadHandler(
    filename = function() "dna_dilution.csv",
    content = function(file) { #docasna cesta k souboru
      write.csv(results(), file, row.names = FALSE) #false uz je mam jako cisla vzorku
    }
  )
}

shinyApp(ui = ui, server = server) #spusteni aplikace