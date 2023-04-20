# Author: Rahul Gupta
# Date: 2023-03-14

library(dplyr)
library(tm)
library(topicdoc)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(plotly)
library(textstem)
library(ggwordcloud)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(gridExtra)
library(DT)

# User Inputs
data_location = "/Users/tiana/Documents/1.\ Projects/NUS\ -\ CS5340/Project/NIPS\ Papers"
papers_name = "papers.csv"
authors_name = "authors.csv"
paper_author_name = "paper_authors.csv"


# 1. Text Cleaning
func_basic_text_cleaning = function(paper) {
    paper$paper_text_cln = paper$paper_text
    
    # Removing very small text
    paper = paper[nchar(paper$paper_text) > 500, ]
    
    # Adding a word to each text to avoid DTM with no words for paper 
    paper$paper_text_cln = paste0(paper$paper_text_cln, 
                                  " Paper Acknowledged")
    
    paper$paper_text_cln = gsub("\\n", " ", paper$paper_text_cln)                # Removing \n before any step
    paper$paper_text_cln = tolower(paper$paper_text_cln)                         # Converting to lower case
    paper$paper_text_cln = trimws(paper$paper_text_cln)                          # Removing leading and trailing white spaces
    paper$paper_text_cln = gsub("[^a-z ]", "", paper$paper_text_cln)             # Only retaining letters and some symbols
    paper$paper_text_cln_b4_lemma = paper$paper_text_cln
    paper$paper_text_cln = lemmatize_strings(paper$paper_text_cln,
                                             dictionary = lexicon::hash_lemmas)  # Lemmatization to consider similar words
    
    return(paper)
}

# 2. Create DTM
func_create_dtm = function(d_dtm, dtm_sparcity) {
    
    row.names(d_dtm) = NULL
    d_dtm$names_index = row.names(d_dtm)
    
    docs = VCorpus(VectorSource(d_dtm$paper_text_cln))
    
    docs_index = names(docs)
    docs_text = sapply(docs, function(x) x[["content"]])
    
    doc_index_df = data.frame("docs_index" = docs_index, "docs_text" = docs_text)
    
    # Getting date and other columns
    doc_index_df = merge(x = d_dtm, 
                         y = doc_index_df,
                         by.x = "names_index",
                         by.y = "docs_index",
                         all.x = T
    )
    if(nrow(d_dtm) != nrow(doc_index_df)) {
        warning("Data size issue observed ...")
        
    }
    
    # Further data cleaning 
    cat("\nStart Cleaning corpus: ", Sys.time())
    docs = tm_map(docs, removePunctuation)
    docs = tm_map(docs, removeNumbers)
    docs = tm_map(docs, stripWhitespace)
    docs = tm_map(docs, content_transformer(tolower))
    docs = tm_map(docs, removeWords, c(stopwords("english")))
    cat("\nEND Cleaning corpus: ", Sys.time())
    
    # Create DTM
    
    # A lot of run time is because of DTM; adding bounds to control the same
    # ?DocumentTermMatrix
    # https://search.r-project.org/CRAN/refmans/tm/html/termFreq.html
    # https://stackoverflow.com/questions/13366897/r-documenttermmatrix-control-list-not-working-silently-ignores-unknown-paramete
    
    # dtm = DocumentTermMatrix(docs)
    cat("\nStart DTM: ", Sys.time())
    dtm = DocumentTermMatrix(docs, control=list(bounds = list(global = c(200,Inf))))
    cat("\nEnd DTM: ", Sys.time())
    
    cat("\nDTM Shape: ", dim(dtm))
    
    # Added: Removing sparse words to reduce size (Another way of reducing size)
    # https://stackoverflow.com/questions/28763389/how-does-the-removesparseterms-in-r-work
    # dtm = removeSparseTerms(dtm, dtm_sparcity)
    
    # Create TDM
    tdm = "" # TermDocumentMatrix(docs)
    
    return(list("dtm" = dtm, "doc_index_df" = doc_index_df, "tdm" = tdm))
}

# 3. Generate LDA plots
func_lda_plots = function(dtm, 
                          dtm_index_docs, 
                          k_topics, 
                          topic_term_cnt){
    # 1. Running LDA
    cat("\nStart Topic modeling: ", Sys.time())
    lda = LDA(dtm,
              k = k_topics,
              control = list(seed = 1234))
    cat("\nEND Topic modeling: ", Sys.time())
    
    # Running topic diagnostics
    # print(topic_diagnostics(lda, dtm))
    
    # 2. Topic - term betas
    topic_term = tidy(lda, 
                      matrix = "beta")
    
    # 3. plotting word for each topic
    topic_term_plts = list()
    for(t in 1:length(unique(topic_term$topic))) {
        wc_t = topic_term[which(topic_term$topic == t),]
        wc_t = arrange(wc_t, desc(beta))
        wc_t = head(wc_t, topic_term_cnt)
    
    topic_term_plts[[paste0("Topic_", t)]] = ggplot(wc_t,
                                                    aes(label = term,
                                                        size  = beta,
                                                        col   = as.character(beta))) + 
        geom_text_wordcloud(rm_outside = TRUE) + 
        labs(title = paste0("Topic ", t)) + 
        theme(text = element_text(size=15))
    }
    
    # 4. Topic assignment to each feedback
    topic_doc = tidy(lda, matrix = "gamma")
    topic_doc = topic_doc %>% group_by(document) %>% slice(which.max(gamma))
    
    
    # 5. Feedback - Topic Pie Plot
    topic_count = topic_doc %>% group_by(topic) %>% dplyr::summarise('# Feedback' = n())
    topic_count$Topic = paste0("Topic ", topic_count$topic)
    topic_pie_plt = plot_ly(data=topic_count,
                            labels=~Topic, 
                            values=~`# Feedback`,
                            type="pie"
                            #marker = list(colors = hue_pal() (max (nrow(topic_count), 1)))
                            ) %>%
        layout (title = list(text = ' ', #' Feedback Classification by Topic
                             side = "left"),
    xaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    vaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE) )
    
    # 6. Feedback topic examples
    topic_doc = merge(x = topic_doc,
                      y = dtm_index_docs, 
                      by.x = "document",
                      by.y = "names_index",
                      all.x = T)
    
    if (nrow(topic_doc) != nrow(topic_doc))
        stop ("Row expansion in func Ida plots ...")

    topic_doc_tables = list()
    for (t in unique(topic_doc$topic)) {
        # topic_doc_tables[[paste ("topic", t)]] = data.frame(topic_doc %>%
        #                                                     filter(topic == t) %>%
        #                                                     arrange(desc(gamma)) %>%
        #                                                     select(topic, docs_text, gamma) %>%
        #                                                     dplyr::rename(Topic = topic, 
        #                                                            `Topic Contribution` = gamma,
        #                                                            Feedback = docs_text) %>%
        #                                                     mutate(`Topic Contribution` = round(`Topic Contribution`, 3))
                                                            
        topic_doc_tables[[paste ("topic", t)]] = data.frame(topic_doc %>%
                                                                filter(topic == t) %>%
                                                                arrange(desc(gamma)) %>%
                                                                select(topic, title, abstract, gamma) %>%
                                                                dplyr::rename(Topic = topic, 
                                                                              `Topic Contribution` = gamma) %>%
                                                                mutate(`Topic Contribution` = round(`Topic Contribution`, 3))                                                    
        )}

    # 7. Topic Distribution by month
    topic_doc$topic = as.character(topic_doc$topic)
    topic_across_time = topic_doc %>% group_by (year, topic) %>% dplyr::summarize(`# Feedback` = n())
    topic_across_time = topic_across_time %>% dplyr::rename (Year= year, Topic = topic)
    topic_across_time_plt = ggplot(topic_across_time,
                                   aes(fill=Topic,
                                   y =`# Feedback`,
                                   x = Year)) +
                                   geom_bar (position="stack", stat="identity") +
                                   scale_fill_discrete () +
                                   labs (title= " ") # "Topic Distribution Across Time"
    
    return(list("topic_term_plts" = topic_term_plts,
                "topic_pie_plt" = topic_pie_plt,
                "topic_doc_tables" = topic_doc_tables,
                "topic_across_time_plt" = topic_across_time_plt,
                "lda" = lda
    ))
}




# Main

# TO RUN IN SESSION
# paper_df = read.csv(file.path(data_location, papers_name))
#  
# paper_df       = func_basic_text_cleaning(paper = paper_df)
# dtm_res        = func_create_dtm(d_dtm = paper_df, dtm_sparcity=0.01)
# dtm            = dtm_res$dtm
# dtm_index_docs = dtm_res$doc_index_df
# 
# topic_plts = func_lda_plots(dtm=dtm,
#                             dtm_index_docs=dtm_index_docs,
#                             k_topics=5,
#                             topic_term_cnt=20)
# 
# #do.call("grid.arrange", c(topic_plts$topic_term_plts, nco1=2))
# 
# grid.arrange(grobs=topic_plts$topic_term_plts, ncol=2,top="Main Title")


# Server

server = function(input, output, session) { 
    
    # Reading data
    paper_df = read.csv(file.path(data_location, papers_name))
    print("Completed reading data ...")
                         
    papers_eventreactive <- eventReactive(input$topic_run_button, {
        
        # Filtering as per input
        paper_df = paper_df[paper_df$year >= input$year_range[1],]
        paper_df = paper_df[paper_df$year <= input$year_range[2],]
        print("Completed filtering data ...")
        
        # Running DTM creation function
        paper_df       = func_basic_text_cleaning(paper = paper_df)
        dtm_res        = func_create_dtm(d_dtm = paper_df, dtm_sparcity= input$dtm_sparcity) #0.01)
        dtm            = dtm_res$dtm
        dtm_index_docs = dtm_res$doc_index_df
        
        print("Completed Running DTM function ...")
        
        # Running LDA
        print("Started Running topic model function ...")
        topic_plts = func_lda_plots(dtm=dtm, 
                                    dtm_index_docs=dtm_index_docs, 
                                    k_topics=input$num_topics, # 5, 
                                    topic_term_cnt=20)
        print("Completed Running topic model function ...")
        
        # Creating a dynamic height div to store topic term word clouds
        output$paper_plots_dynamic <- renderUI({
            return(fluidRow(box(title = "Topic WordClouds",
                   plotOutput(outputId = "adjusted_output_paper_plots_dynamic",
                               height=paste0((ceiling(input$num_topics/2)*200), "px")),
                   width = 12 )))
    
        })
        
        return(list("res" = topic_plts))
    })
    
    wc_plot_height = eventReactive(input$topic_run_button, {
        return(ceiling(input$num_topics/2)*200)
    })
    
    # Plotting paper pie chart
    output$paper_pie_plt <- renderPlotly({
        result = papers_eventreactive()
        result = result[["res"]]
        result$topic_pie_plt
        })
    # Plotting topic across time plot
    output$paper_across_time_plt <- renderPlotly({
        result = papers_eventreactive()
        result = result[["res"]]
        result$topic_across_time_plt 
        })
    
    # Plotting topic word clouds
    output$adjusted_output_paper_plots_dynamic <- renderPlot({
        result = papers_eventreactive()
        result = result[["res"]]
        grid.arrange(grobs=result$topic_term_plts,
                     ncol=2)
        }, height=function() {wc_plot_height()})
    
    # Plotting table
    output$paper_table <- renderDT ( {
        result = papers_eventreactive()
        result = result[["res"]]
        
        res_table = do.call("rbind", result$topic_doc_tables)
        res_table$Topic = as.character(res_table$Topic)
        res_table = res_table %>% rename(`Topic Contribution` = Topic.Contribution) %>% arrange (Topic)
        data.frame(res_table, check.names = F)
        }, filter = "top", rownames= FALSE, options = list(pageLength = 5,
                                                           lengthChange = FALSE,
                                                           dom = 'ltipr'))
    
}    

# UI ####
ui <- dashboardPage(
    dashboardHeader(title = "Topic Modeling"),
    dashboardSidebar(),
    dashboardBody(
        # All user inputs
        fluidRow(
            
            # column(2, 
            #        style="max-width : 250px",
            #        title = "Select Date Range",
            #        dateRangeInput(inputId = "year_range", 
            #                       label = "Select Date Range", 
            #                       start = 1987,
            #                       end   = 2017,
            #                       format = "yyyy"
            #        ),
            # ),
            
            column(2, 
                   style="max-width : 850px",
                   title = "Select Year Range",
                   sliderInput(inputId = "year_range", 
                               label = "Select Year Range", 
                               value = c(2010,2017),
                               min = 1987,
                               max = 2017,
                               step = 1)
            ),
                   
            column(2, 
                   style="max-width : 650px",
                   title = "Select # Topics",
                   sliderInput(inputId = "num_topics", 
                               label = "Select # Topics", 
                               value = 5,
                               min = 1,
                               max = 20,
                               step = 1,
                               ticks = F)
            ),
            
            column(2, 
                   style="max-width : 650px",
                   title = "Select DTM Sparcity",
                   sliderInput(inputId = "dtm_sparcity", 
                               label = "Select DTM Sparcity", 
                               value = 0.01,
                               min = 0.01,
                               max = 0.99,
                               step = 0.01,
                               ticks = F)
            ),
            
            column(1, 
                   style="max-width : 350px",
                   actionButton(inputId = "topic_run_button",
                                label = "Submit",
                                icon = icon("paper-plane"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                   )
            ),
        ),
        
        # All Plots
        fluidRow(box(title = "Topic Assignment Distribution",
                     shinycssloaders::withSpinner(plotlyOutput("paper_pie_plt"), type=6), 
                     width=6),
                 
                 box(title = "Topic Across time",
                     shinycssloaders::withSpinner(plotlyOutput("paper_across_time_plt"), type=6), 
                     width=6),
        ),
        
        fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("paper_plots_dynamic"), type=6))),
        
        fluidRow(box(title = "Topic Assignment Table",
                     shinycssloaders::withSpinner(DT::DTOutput("paper_table"), type=6), 
                     width=12)
        ),
    )
)
    
shinyApp(ui, server)    


    
    
