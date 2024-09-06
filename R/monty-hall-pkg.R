#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   `create_game()`
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'     Contestant selects a door in the Monty Hall Problem game.
#'    
#' @description
#'    `select_door()` generates a random contestant selection of
#'    one of the three available game doors. 
#' 
#' @details
#'   This is the contestants first selection of one of the three
#'   available game doors and the first door selection in the Monty
#'   Hall problem game. 
#'
#' @param ... A numeric value
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the position of the door they chose.
#'   
#' @examples
#'   `select_door()`
#'   [1]
#'   
#'   `select_door()`
#'   [2]
#'   
#'   `select_door()`
#'   [3]
#'   
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a remaining goat door. 
#' 
#' @description
#' `open_goat_door()` generates the host's selection
#'  of a  remaining goat door and opens it
#' for the Contestant to see. 
#'   
#' @details
#' If the contestant selected the car on their first 
#' door selection, the host randomly selects one of
#' the two remaining goat doors. If the contestant selected
#' a goat door for their initial selection, the host
#' selects the remaining unopened goat door. 
#'   
#' @param ... A numeric value
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the position of the door they chose.
#'   
#' @examples
#' `open_goat_door ()`
#' [1]
#' 
#' `open_goat_door ()`
#' [2]
#' 
#' `open_goat_door ()`
#' [3]
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant stay or switch doors.
#' 
#' @description
#' `change_door()` randomly generates the contestant's choice
#' to either switch to the remaining door or 
#' remain with their initial selection.
#'   
#' @details
#' The Contestant now chooses whether they would like to
#' switch to the remaining unopened door or to stay
#' with their initial selection. Their are aiming to
#' open the door with a car behind it. 
#'   
#' @param ... A numeric value
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the position of the door they chose.
#'   
#' @examples
#' `change_door ()`
#' [1]
#' 
#' `change_door ()`
#' [2]
#' 
#' `change_door ()`
#' [3]
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner.
#' 
#' @description
#' `determine_winner()` determines if the contestant 
#' won the game by making a final selection of a 
#' door with a car behind it. 
#'   
#' @details
#' If the contestant's final selection is a door with
#' a car behind it, then the contestant won the game.
#'  If the contestant's final selection is a door with
#'  a goat behind it, then the contestant lost the
#'  game. 
#'   
#' @param ... A Character value
#' 
#' @return A single character vector indicating "WIN" or "LOSE"
#'   
#' @examples
#' 
#' `determine_winner()`
#' [LOSE]
#' 
#' `determine_winner()``
#' [WIN]
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Complete Monty Hall Problem Game.
#' 
#' @description
#' `play_game()` allows the user to play the entire Monty
#' Hall Problem Game.
#' 
#' @details
#' A wrapped version of all the applicable Monty Hall Problem
#' Game functions: `create_game()`, `select_door()`, `open_goat_door()`,
#' `change_door()`, and `determine_winner()`. Finally, all
#' of these game results are stored as a data frame. 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 2 x 2 data frame of character values: strategy
#' (i.e., "stay" or "switch") and outcome (i.e, "WIN" or "LOSE")
#' 
#' @examples
#' `play_game()`

#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Simulation of Multiple Monty Hall Problem Games
#' 
#' @description
#' `play_n_games()` simulates n number of Monty Hall Problem Games.
#' 
#' @details
#' The user can choose the number of times they would like the
#' Monty Hall Problem Game to be simulated and this loop will run 
#' the game n number of times. 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return A 2 x 2 table displaying the proportion of lost and 
#' won outcomes based on the strategy of "switch" or the strategy 
#' of "stay."
#' 
#' @examples
#' `play_n_games <- function (n = 50)`
#' `play_n_games <- function (n = 125)`
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
