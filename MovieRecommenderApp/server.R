## server.R

# load functions
source('functions/recommenders.R') # recommenders

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in movie data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# read in movie images
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# read in rating data; read local version because scan() failed sporadically for remote reading
# local version: 
# ratings = read.csv('data/ratings.dat', 
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
# clean up rating data
ratings = ratings[(!is.na(ratings$Rating))&(ratings$Rating<=5),]


shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)   # list
      
      # run UBCF
      recom_res = rateRecommender(ratings, user_ratings)
      
      # extract MovieIDs and ratings from recommendation results
      recom_res = recom_res[order(-recom_res$rating), , drop=F]
      
      recom_res
      
    }) # still busy
    
  }) # clicked on button for UBCF tab
  
  
  # display the UBCF recommendation
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies[movies$MovieID==as.integer(sub('m','',recom_result$item[(i-1)*num_movies+j])), 'image_url'], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies[movies$MovieID==as.integer(sub('m','',recom_result$item[(i-1)*num_movies+j])), 'Title'], height = 150))
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function for UBCF
  
  
  # Calculate recommendations based on the selected genre
  df_genre <- eventReactive(input$genrebtn, {
    withBusyIndicatorServer("genrebtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's genre selection
      user_genre <- input$user_genre
      
      # run the genre-based recommender
      recom_pop <- get_recommendations_by_genre_by_popularity(movies, ratings, user_genre)
      
    }) # still busy
    
  }) # clicked on button for genre-based recommendation
  
  
  # display the genre-based recommendations
  output$rec_genre_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_pop <- df_genre()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_pop$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_pop$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function for genre-based recommender
  
}) # server function