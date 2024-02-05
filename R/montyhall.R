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
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}
#' @title
#' Select an initial door.
#'
#' @description
#' `select_door()` retuns a door selection for the initial pick by the contestant.
#'
#' @details
#' This is the first choice of the three doors. The contestant can pick any one.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' The function returns a number between 1 and 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}
#' @title
#' Host opens a goat door.
#'
#' @description
#' `select_door()` selects one of the the goat doors that hasn't been
#' chosen by the contestant.
#'
#' @details
#' This function chooses one of the remaining goat doors. As the contestant has
#' already chosen one door, this function chooses between 2 goat doors if the
#' initial pick was the car door, or it chooses the only goat door available if
#' the contestant originally picks a goat door.
#'
#' @param game length 3 character vector
#' @param a.pick number between 1 and 3
#'
#' @return
#' The function returns a number between 1 and 3
#'
#' @examples
#' open_goat_door()
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
#' Contestant decides to stay or switch.
#'
#' @description
#' `change_door()` reveals the final door choice depending if the contestant
#' stays or switches doors.
#'
#' @details
#' This function chooses the initial pick as the final pick if the contestant
#' decides to stay and switches to the remaining door if they decide to switch.
#'
#' @param stay a true or false value
#' @param opened.door a number between 1 and 3
#' @param a.pick a number between 1 and 3
#'
#' @return
#' The function returns a value between 1 and 3
#'
#' @examples
#' change_door()
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
#' Determine if the contestant has won.
#'
#' @description
#' `determine_winner()` reveals if the final pick results in a win or lose.
#'
#' @details
#' If the final pick is a car door, then the contestant wins. If the final pick
#' is a goat door, then the contestant loses.
#'
#' @param final.pick a number between 1 and 3
#' @param game length 3 character vector
#'
#' @return
#' This function returns a length one character vector
#'
#' @examples
#' determine_winner()
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
#' Play the game.
#'
#' @description
#' `play_game()` combines each step of the game to run as one function and show
#' the conditions and results of a game
#'
#' @details
#' This function includes every step of the game to show the door selection,
#' the first pick, the opened door, if the contestant stays or switches, the
#' final pick, and the result of the game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' This function returns a vector of the game set up, numbers between 1 and 3
#' for the initial pick, opened door, and final door, "Stay"/"Switch", and
#' "Win"/"Lose"
#'
#' @examples
#' play_game()
#'
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
#' Play the game 100 times
#'
#' @description
#' `play_n_games()` replays the game "n" times
#'
#' @details
#' This function replays the game with every step mentioned previously and runs
#' through it "n" number of times
#'
#' @param n a value indicating the number of games
#'
#' @return
#' This function returns a table of the probabilities of winning or losing
#' depending on if the player stays or switches doors.
#'
#' @examples
#' play_n_games()
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

