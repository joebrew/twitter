library(tidyverse)
library(readr)

follow_and_unfollow <- function(users){
  followed <- unfollowed <- c()
 
  # Wrap the whole attempt in a try statement, so that it doesn't fail even if webscraping goes wrong
  tried <- try({
    # Read credentials
    credentials <- yaml.load_file('credentials.yaml')
    # start a chrome browser
    system('docker run -d -p 4445:4444 selenium/standalone-chrome')
    # system('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
    
    message('Started docker. Waiting 3 seconds')
    Sys.sleep(3)
    
    log_in <- function(){
      remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
      remDr$open()
      message('**sleeping for 1 sec')
      Sys.sleep(1)
      # navigate to twitter
      remDr$navigate("https://twitter.com/login")
      
      # Identify where to enter user/pass and submit button
      username_entry <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "js-initial-focus", " " ))]')
      password_entry <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "js-password-field", " " ))]') 
      submit <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "EdgeButtom--medium", " " ))]')
      
      username_entry$sendKeysToElement(list(credentials$user))
      password_entry$sendKeysToElement(list(credentials$password))
      submit$clickElement()
      message('Logged in. Sleeping for 5 seconds')
      Sys.sleep(5)
      assign('remDr',
             remDr,
             envir = .GlobalEnv
      )
    }
    
    follow_function <- function(user){
      # Go to page of user
      Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
      remDr$navigate(paste0("https://twitter.com/", user))
      Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
      # remDr$screenshot(file = 'test.png')
      
      # Find the following/unfollow button
      button <- remDr$findElement('class name', 'not-following')
      # Click the button
      button$clickElement()
    }
    unfollow_function <- function(user){
      # Go to page of user
      Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
      remDr$navigate(paste0("https://twitter.com/", user))
      Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
      # remDr$screenshot(file = 'test.png')
      
      # Find the following/unfollow button
      button <- remDr$findElement('class name', 'following')
      # Click the button
      button$clickElement()
    }
    
    # LOG IN
    log_in()
    
    # remDr$screenshot(file = 'test.png')
    # Go through each user and unfollow
    for(i in 1:length(users)){
      
      if(i %% 10 == 0){
        message('Sleeping an extra 20 seconds due to multiple of 10')
        Sys.sleep(10)
      }
      
      message('Following ', i, ' of ', length(users))
    
      try_follow <- try({
        user <- users[i]
        follow_function(user = user)
      })
      
      if(class(try_follow) == 'try-error'){
        message('Was not able to follow ', user, '----------------')
        message('Will log in again')
        log_in()
        
      } else {
        message('Successfully followed ', user, '----------------')
        followed <- c(followed, user)
      }
      sleeper <- sample(seq(sleep[1], sleep[2], length = 1000), 1)
      message('SLEEPING FOR ', round(sleeper, digits = 1), ' SECONDS')
      Sys.sleep(sleeper)
    }
    
    # DONE FOLLOWING, NOW SLEEP FOR 15 MINUTES
    message('----------------------------------')
    message('----------------------------------')
    message('DONE FOLLOWING -------------------')
    message('----------------------------------')
    message('----------------------------------')
    message('GOING TO SLEEP FOR 15 MINUTES-----')
    print(Sys.time())
    Sys.sleep(15 * 60)
    
    # UNFOLLOW
    for(i in 1:length(followed)){
      if(i %% 10 == 0){
        message('Sleeping an extra 20 seconds due to multiple of 10')
        Sys.sleep(10)
      }
      message('Unfollowing ', i, ' of ', length(users))
      
      try_unfollow <- try({
        user <- users[i]
        unfollow_function(user = user)
      })
      
      if(class(try_unfollow) == 'try-error'){
        message('Was not able to unfollow ', user, '----------------')
        message('Will log in again')
        log_in()
      } else {
        message('Successfully unfollowed ', user, '----------------')
      }
      sleeper <- sample(seq(sleep[1], sleep[2], length = 1000), 1)
      message('SLEEPING FOR ', round(sleeper, digits = 1), ' SECONDS')
      Sys.sleep(sleeper)
    }
    
    remDr$close()
    remDr$closeall()
    remDr$closeServer()
    # stop the selenium server
    # remDr$closeServer()
  })
  
  
  if(class(tried) == 'try-error'){
    warning('---There was an error with the webserver.')
  } else {
    message('done')
  }
  
}
