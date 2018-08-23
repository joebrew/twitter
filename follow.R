library(RSelenium)
library(yaml)
library(tidyverse)
library(XML)

# Define function for following
follow <- function(users = 'Tabarniaenserio',
                     sleep = c(0,0)){
  
  # Wrap the whole attempt in a try statemnet, so that it doesn't fail even if webscraping goes wrong
  tried <- try({
    # Read credentials
    credentials <- yaml.load_file('credentials.yaml')
    # start a chrome browser
    system('docker run -d -p 4445:4444 selenium/standalone-chrome')
    # system('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
    
    message('Started docker. Waiting 3 seconds')
    Sys.sleep(3)
    
    remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
    
    # remDr <- remoteDriver(remoteServerAddr = "localhost" 
    #                       , port = 4445L
    #                       , browserName = "firefox"
    # )
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
    # remDr$screenshot(file = 'test.png')
    # Go through each user and unfollow
    for(i in 1:length(users)){
      message('Following ', i, ' of ', length(users))
      
      
      try_follow <- try({
        user <- users[i]
        
        # Go to page of user
        Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
        remDr$navigate(paste0("https://twitter.com/", user))
        Sys.sleep(sample(seq(0.5, 1.5, length = 20), 1))
        # remDr$screenshot(file = 'test.png')
        
        # Find the following/unfollow button
        # button <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "unfollow-text", " " ))]')
        # buttons <- remDr$findElements('class', 'EdgeButton')
        # button <- remDr$findElements('partial link text', 'nfoll')
        button <- remDr$findElement('class name', 'not-following')
        # Click the button
        # button[[3]]$clickElement()
        button$clickElement()
        # button$sendKeysToElement(list('\uE007'))
        
        
      })
      
      if(class(try_follow) == 'try-error'){
        warning('Was not able to follow ', user, '----------------')
      } else {
        message('Successfully followed ', user, '----------------')
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

