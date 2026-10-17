
# OTT Movie Ratings Analyzer
# Group 7

library(shiny)
library(shinythemes)
library(ggplot2)

# -------------------------------
# 1. Create Sample Dataset
# -------------------------------

set.seed(123)

n <- 200

movies <- data.frame(
  rating = round(
    pmax(1, pmin(10, rnorm(n, mean = 7.1, sd = 1.1))),
    1
  ),
  genre = sample(
    c("Action", "Comedy", "Drama", "Thriller", "Romance"),
    n, replace = TRUE
  ),
  release_year = sample(2015:2026, n, replace = TRUE),
  duration = sample(80:200, n, replace = TRUE),
  language = sample(
    c("Hindi", "English", "Tamil", "Telugu", "Korean"),
    n, replace = TRUE
  )
)

# -------------------------------
# 2. User Interface
# -------------------------------

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  titlePanel("OTT Movie Ratings Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dataset Controls"),
      
      fileInput(
        "file",
        "Upload CSV Dataset",
        accept = ".csv"
      ),
      
      helpText(
        "CSV columns required: rating, genre, release_year, duration, language"
      ),
      
      hr(),
      
      h4("P9: Movie Age Comparison"),
      
      sliderInput(
        inputId = "cutoff",
        label = "New Movie Release-Year Cutoff",
        min = 2015,
        max = 2026,
        value = 2022,
        step = 1,
        sep = "",
        ticks = TRUE,
        animate = FALSE
      ),
      
      helpText(
        "New movies: release year >= cutoff. Old movies: release year < cutoff."
      ),
      
      hr(),
      
      downloadButton(
        "download",
        "Download Current Dataset"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Dataset",
          h3("Movie Dataset"),
          textOutput("rowCount"),
          tableOutput("dataTable")
        ),
        
        tabPanel(
          "P1: Normal Probability",
          h3("Probability that Rating > 8"),
          verbatimTextOutput("normalResult"),
          plotOutput("ratingPlot")
        ),
        
        tabPanel(
          "P4/P5: One-Sample t-Test",
          h3("Test Whether Average Rating > 7"),
          verbatimTextOutput("ttestResult")
        ),
        
        tabPanel(
          "P6: Linear Regression",
          h3("Predict Rating from Duration"),
          verbatimTextOutput("regressionResult"),
          plotOutput("regressionPlot")
        ),
        
        tabPanel(
          "P7: Chi-Square Test",
          h3("Genre and Language Independence"),
          verbatimTextOutput("chiResult")
        ),
        
        tabPanel(
          "P9: Mann-Whitney U Test",
          h3("Compare New and Old Movie Ratings"),
          
          textOutput("cutoffText"),
          
          verbatimTextOutput("groupSummary"),
          
          verbatimTextOutput("mwResult"),
          
          plotOutput("comparePlot")
        )
      )
    )
  )
)

# -------------------------------
# 3. Server Logic
# -------------------------------

server <- function(input, output, session) {
  
  # Reactive dataset
  data_source <- reactiveVal(movies)
  
  # Read uploaded CSV
  observeEvent(input$file, {
    req(input$file)
    
    uploaded_data <- tryCatch(
      read.csv(
        input$file$datapath,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
    
    required_columns <- c(
      "rating", "genre", "release_year",
      "duration", "language"
    )
    
    if (
      is.null(uploaded_data) ||
      !all(required_columns %in% names(uploaded_data))
    ) {
      showNotification(
        paste(
          "Invalid CSV. Required columns:",
          paste(required_columns, collapse = ", ")
        ),
        type = "error",
        duration = 8
      )
      return()
    }
    
    uploaded_data$rating <- suppressWarnings(
      as.numeric(uploaded_data$rating)
    )
    
    uploaded_data$release_year <- suppressWarnings(
      as.numeric(uploaded_data$release_year)
    )
    
    uploaded_data$duration <- suppressWarnings(
      as.numeric(uploaded_data$duration)
    )
    
    uploaded_data <- uploaded_data[
      complete.cases(
        uploaded_data[, required_columns]
      ),
    ]
    
    if (
      nrow(uploaded_data) == 0 ||
      any(!is.finite(uploaded_data$rating)) ||
      any(!is.finite(uploaded_data$release_year)) ||
      any(!is.finite(uploaded_data$duration)) ||
      any(uploaded_data$rating < 1 | uploaded_data$rating > 10) ||
      any(uploaded_data$duration <= 0)
    ) {
      showNotification(
        "CSV contains no valid rows or has invalid numeric values.",
        type = "error"
      )
      return()
    }
    
    data_source(uploaded_data)
    
    # Update slider based on uploaded release years
    years <- uploaded_data$release_year
    min_year <- floor(min(years))
    max_year <- ceiling(max(years))
    
    if (min_year == max_year) {
      min_year <- min_year - 1
      max_year <- max_year + 1
    }
    
    updateSliderInput(
      session,
      "cutoff",
      min = min_year,
      max = max_year,
      value = min(max(2022, min_year), max_year),
      step = 1
    )
    
    showNotification(
      "CSV dataset uploaded successfully.",
      type = "message"
    )
  })
  
  # -------------------------------
  # Dataset Tab
  # -------------------------------
  
  output$rowCount <- renderText({
    paste("Total valid movie records:", nrow(data_source()))
  })
  
  output$dataTable <- renderTable({
    head(data_source(), 20)
  }, striped = TRUE, bordered = TRUE)
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("ott_movie_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_source(), file, row.names = FALSE)
    }
  )
  
  # -------------------------------
  # P1: Normal Probability
  # -------------------------------
  
  output$normalResult <- renderPrint({
    df <- data_source()
    ratings <- df$rating
    
    mu <- mean(ratings)
    sigma <- sd(ratings)
    
    if (sigma == 0) {
      cat("Standard deviation is zero; normal probability cannot be estimated.")
    } else {
      probability <- 1 - pnorm(
        8,
        mean = mu,
        sd = sigma
      )
      
      cat("Mean Rating:", round(mu, 3), "\n")
      cat("Standard Deviation:", round(sigma, 3), "\n")
      cat("P(Rating > 8):", round(probability, 4), "\n")
      cat("Percentage:", round(probability * 100, 2), "%\n")
    }
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(data_source(), aes(x = rating)) +
      geom_histogram(
        binwidth = 0.5,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Distribution of Movie Ratings",
        x = "Rating",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  # -------------------------------
  # P4/P5: One-Sample Right-Tailed t-Test
  # H0: Mean rating = 7
  # H1: Mean rating > 7
  # -------------------------------
  
  output$ttestResult <- renderPrint({
    ratings <- data_source()$rating
    
    if (length(ratings) < 2 || sd(ratings) == 0) {
      cat("Not enough variation or observations to perform the t-test.")
    } else {
      result <- t.test(
        ratings,
        mu = 7,
        alternative = "greater"
      )
      
      print(result)
      
      cat("\nConclusion at 5% significance level:\n")
      
      if (result$p.value < 0.05) {
        cat("Reject H0. There is evidence that the average rating is greater than 7.")
      } else {
        cat("Fail to reject H0. There is insufficient evidence that the average rating is greater than 7.")
      }
    }
  })
  
  # -------------------------------
  # P6: Linear Regression
  # rating predicted by duration
  # -------------------------------
  
  output$regressionResult <- renderPrint({
    df <- data_source()
    
    if (
      nrow(df) < 3 ||
      length(unique(df$duration)) < 2
    ) {
      cat("Insufficient data to fit the regression model.")
    } else {
      model <- lm(rating ~ duration, data = df)
      
      print(summary(model))
      
      coefficients <- coef(model)
      
      cat("\nRegression Equation:\n")
      cat(
        "Rating =",
        round(coefficients[1], 3),
        "+",
        round(coefficients[2], 4),
        "* Duration\n"
      )
      
      cat(
        "\nExample prediction for a 120-minute movie:\n",
        round(
          predict(
            model,
            newdata = data.frame(duration = 120)
          ),
          2
        )
      )
    }
  })
  
  output$regressionPlot <- renderPlot({
    df <- data_source()
    
    ggplot(df, aes(x = duration, y = rating)) +
      geom_point(color = "steelblue", alpha = 0.7) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "darkred"
      ) +
      labs(
        title = "Movie Duration vs Rating",
        x = "Duration (minutes)",
        y = "Rating"
      ) +
      theme_minimal()
  })
  
  # -------------------------------
  # P7: Chi-Square Independence Test
  # Genre vs Language
  # -------------------------------
  
  output$chiResult <- renderPrint({
    df <- data_source()
    
    contingency_table <- table(
      df$genre,
      df$language
    )
    
    print(contingency_table)
    
    if (
      nrow(contingency_table) < 2 ||
      ncol(contingency_table) < 2
    ) {
      cat("\nInsufficient categories for the Chi-square test.")
    } else {
      result <- tryCatch(
        chisq.test(contingency_table),
        error = function(e) NULL
      )
      
      if (is.null(result)) {
        cat("\nUnable to perform the Chi-square test.")
      } else {
        cat("\nChi-Square Test Result:\n")
        print(result)
        
        cat("\nConclusion at 5% significance level:\n")
        
        if (result$p.value < 0.05) {
          cat("Reject H0. Genre and language appear to be associated.")
        } else {
          cat("Fail to reject H0. There is insufficient evidence of an association between genre and language.")
        }
        
        if (any(result$expected < 5)) {
          cat(
            "\nWarning: Some expected cell counts are below 5; interpret the result cautiously."
          )
        }
      }
    }
  })
  
  # -------------------------------
  # P9: Mann-Whitney U Test
  # Reactive to slider cutoff
  # -------------------------------
  
  # This reactive expression runs again when
  # either the dataset or cutoff slider changes.
  movieGroups <- reactive({
    df <- data_source()
    cutoff <- input$cutoff
    
    list(
      cutoff = cutoff,
      new = df$rating[df$release_year >= cutoff],
      old = df$rating[df$release_year < cutoff]
    )
  })
  
  output$cutoffText <- renderText({
    groups <- movieGroups()
    
    paste(
      "Cutoff year:", groups$cutoff,
      "| New movies: year >=", groups$cutoff,
      "| Old movies: year <", groups$cutoff
    )
  })
  
  output$groupSummary <- renderPrint({
    groups <- movieGroups()
    
    cat("New Movies:", length(groups$new), "\n")
    cat("Old Movies:", length(groups$old), "\n")
    
    if (length(groups$new) > 0) {
      cat(
        "New Movie Mean Rating:",
        round(mean(groups$new), 2),
        "\n"
      )
    }
    
    if (length(groups$old) > 0) {
      cat(
        "Old Movie Mean Rating:",
        round(mean(groups$old), 2),
        "\n"
      )
    }
  })
  
  output$mwResult <- renderPrint({
    groups <- movieGroups()
    
    new_ratings <- groups$new
    old_ratings <- groups$old
    
    if (
      length(new_ratings) < 1 ||
      length(old_ratings) < 1
    ) {
      cat(
        "Cannot perform the test. Both new and old movie groups must contain at least one movie."
      )
    } else {
      result <- wilcox.test(
        new_ratings,
        old_ratings,
        alternative = "greater",
        exact = FALSE
      )
      
      cat("Mann-Whitney U Test\n")
      cat("H0: New movies do not have higher ratings than old movies.\n")
      cat("H1: New movies have higher ratings than old movies.\n\n")
      
      print(result)
      
      cat("\nConclusion at 5% significance level:\n")
      
      if (result$p.value < 0.05) {
        cat("Reject H0. There is evidence that new movies have higher ratings.")
      } else {
        cat("Fail to reject H0. There is insufficient evidence that new movies have higher ratings.")
      }
    }
  })
  
  output$comparePlot <- renderPlot({
    groups <- movieGroups()
    
    df <- data_source()
    cutoff <- groups$cutoff
    
    plot_data <- data.frame(
      rating = c(groups$new, groups$old),
      movie_age = c(
        rep("New Movies", length(groups$new)),
        rep("Old Movies", length(groups$old))
      )
    )
    
    validate(
      need(nrow(plot_data) > 0, "No movies available for comparison."),
      need(
        length(groups$new) > 0 && length(groups$old) > 0,
        "Both groups need at least one movie. Please change the cutoff year."
      )
    )
    
    ggplot(
      plot_data,
      aes(x = movie_age, y = rating, fill = movie_age)
    ) +
      geom_boxplot(alpha = 0.8) +
      labs(
        title = paste("New vs Old Movie Ratings | Cutoff:", cutoff),
        x = "Movie Group",
        y = "Rating"
      ) +
      scale_fill_manual(
        values = c(
          "New Movies" = "steelblue",
          "Old Movies" = "orange"
        )
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# -------------------------------
# 4. Run Application
# -------------------------------

shinyApp(ui = ui, server = server)
