## server.R



# load functions
recommend_by_num_of_ratings <- function(genre){
  
  tmp = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')
  
  
  #filter by genre
  tmp_selected = tmp[grepl(genre, tmp$Genres),]
  
  #sort by num of ratings
  recommended = arrange(tmp_selected, desc(ratings_per_movie))
  
  recommended
}

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


recommend_by_user_rating <- function(user_ratings) {
  set.seed(12345)
  
  rating_no_timestamp = subset(ratings, select=-c(Timestamp))
  
  new_user_id = 0
  new_user = data.frame(UserID = new_user_id, user_ratings)
  train = rbind(rating_no_timestamp, new_user)
  
  i = paste0('u', train$UserID)
  j = paste0('m', train$MovieID)
  x = train$Rating
  
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(normalize = 'Z-score', 
                                          method = 'Cosine', 
                                          nn = 25))
  
  search_user = paste0('u', new_user_id)
  
  recom_UBCF = predict(rec_UBCF, Rmat[search_user], type = 'ratings')

  recom_res = as(recom_UBCF, 'data.frame')
  recom_res = recom_res[order(-recom_res$rating),]
  recom_res
  
  
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

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


#read in ratings data
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')





shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # System I
  
  # Calculate recommendations based on the selected genre
  df_genre <- eventReactive(input$genre_btn, {
    withBusyIndicatorServer("genre_btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's genre selection
      user_selected_genre <- input$selected_genre
      
      # run the genre-based recommender
      recom_pop <- recommend_by_num_of_ratings(user_selected_genre)
      
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
    
  }) # renderUI function recommender based on selected genre
  
  
  
  
  # #System II
  # # Calculate recommendations when the sbumbutton is clicked
   df <- eventReactive(input$btn, {
     withBusyIndicatorServer("btn", { # showing the busy indicator
       # hide the rating container
       useShinyjs()
       jsCode <- "document.querySelector('[data-widget=collapse]').click();"
       runjs(jsCode)
       
       # get the user's rating data
       value_list <- reactiveValuesToList(input)
       user_ratings <- get_user_ratings(value_list)
       
       #start
       recom_res = recommend_by_user_rating(user_ratings)
       
       
     }) # still busy
     
   }) # clicked on button
   
   
   # display the recommendations
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
     
  }) # renderUI function
  
  
}) # server function