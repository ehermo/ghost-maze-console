

#Ghost Maze
# Naming conventions
# http://adv-r.had.co.nz/Style.html
# https://www.datanovia.com/en/blog/r-coding-style-best-practices/#function-naming-convention


#install.packages("collections")
require(collections)
require(knitr)
require(beepr)


mapping <- dict()
mapping$set(0, "🏾 " )#⛰  ◾■🔳
mapping$set(1, "🏻 ")# □ ◻ ◽
mapping$set(2, "👻 ")#🎃🧟🕷
mapping$set(9, "⛩ ") 
mapping$set(5, "🚹 ")

CORRIDOR <- 1
EXIT <- 9
GHOST <- 2
PLAYER <- 5
DIRECTIONS <- c("N", "E", "S", "W")
GHOST_SPEED <- 3


#https://stackoverflow.com/questions/27112370/make-readline-wait-for-input-in-r

user.input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n=1))
  }
}


get_random_position <- function (maze) {
  row_lower_limit <- 2
  row_upper_limit <- nrow(maze) - 1
  col_lower_limit <- 1
  col_upper_limit <- ncol(maze)

  repeat {
    row <- round(runif(1, row_lower_limit, row_upper_limit))
    col <- round(runif(1, col_lower_limit, col_upper_limit))
    if (maze[row, col] == CORRIDOR) {
      return(list("row" = row, "col" = col))
    }
  }
}


render_bye <- function() {
  
  cat("🎉 🎊 👏 👏 👏")
  
}


render_ghost <- function() {
  
  cat(paste(("________________________________________________________________________________________"),
  ("_______________________UUUUHHHHHHH UHHHHHHHH GOT YOU!___________________________________"),
  ("________________________________________________________________________________________"),
  ("░░______░░______░░____________░░__░░__░░__░░██______░░__░░░░______________░░______░░__░░"),
  ("______________________░░__________________██████__________________░░____________________"),
  ("__________________________________________██████________________________________________"),
  ("________________________________________██████████______________________________________"),
  ("________________________________________██████████______________________________________"),
  ("______________________________________██████████████____________________________________"),
  ("______________________________________██████████████____________________________________"),
  ("__________________________________████__██__██__██__████________________________________"),
  ("____________________________██████████████████████████████████__________________________"),
  ("____________________________██████████████████████████████████__________________________"),
  ("________________________________██████████████████████████______________________________"),
  ("______________________________██__________________________██____________________________"),
  ("______________________________██______██________██________██____________________________"),
  ("______________________________██____████________████______██____________________________"),
  ("______________________________██____████________████______██____________________________"),
  ("________░░______░░____________██__________________________██______________░░______░░____"),
  ("______________________░░____██____________████______________██____░░____________________"),
  ("____________________________██______________________________██__________________________"),
  ("______________________________████______________________████____________________________"),
  ("__________________________________██__________________██__░░____________________________"),
  ("__________________________________██__________________██________________________________"),
  ("____________________________________████__________████__________________________________"),
  ("________________________________________██████████______________________________________"),
  ("________________________________________________________________________________________"),
  ("________________________________________________________________________________________"),
  ("__░░░░░░__░░░░__░░░░░░__░░░░░░__░░░░__░░░░░░__░░░░░░__░░░░░░░░░░░░__░░░░░░__░░░░__░░░░░░"),
  sep="\n"
  ))

}


get_random_direction <- function() {
  lower_limit <- 1
  upper_limit <- length(DIRECTIONS) # length of a vector
  idx <- round(runif(1,lower_limit,upper_limit))
  DIRECTIONS[idx]
}

can_move_to <- function(maze, destination) {
  return (maze[destination$row,destination$col] == CORRIDOR ||
            maze[destination$row,destination$col] == EXIT)
}

is_exit <- function(maze,position) {
  return (maze[position$row,position$col] == EXIT)
}

get_position_forward <- function(current_position, direction) {
  
  position_forward <- current_position
  if(direction == "N") {
    position_forward$row <- position_forward$row - 1
  } 
  else if (direction == "S") {
    position_forward$row <- position_forward$row + 1
  }
  else if (direction == "W") {
    position_forward$col <- position_forward$col - 1
  }
  else if (direction == "E") {
    position_forward$col <- position_forward$col + 1 
  }
  position_forward
}


#  FTF
#  TXT
#  FTF
is_next_to <- function(position_1, position_2) {
  
  row_distance <- abs(position_1$row - position_2$row)
  col_distance <- abs(position_1$col - position_2$col)
  
  distance <- row_distance + col_distance
  
  if(distance > 1) {
    return(FALSE)
  }
  TRUE
}

get_graphics <- function(maze_view,mapping) {
  
  nrow <- nrow(maze_view)
  ncol <- ncol(maze_view)
  matrix(lapply(c(maze_view),mapping$get), nrow,ncol)
  
}

rotate_clockwise <- function(x) {t( apply(x, 2, rev))}

what_player_can_see <- function (maze, player_position, ghost_position, direction, distance = 3) {
  
  padding <- distance
  number_rot <- 0
  meta_maze <- matrix(0,nrow(maze) + (2 * padding), ncol(maze) + (2 * padding))
  meta_maze[(1 + padding):(nrow(maze) + padding ), (1 + padding):(ncol(maze) + padding)] <- maze
  meta_maze[ghost_position$row + padding,ghost_position$col + padding] <- GHOST
  #print(meta_maze)
  if(direction == "N") {
    start_row <- player_position$row  -3
    end_row <- player_position$row 
    start_col <- player_position$col - 1 
    end_col <- player_position$col + 1 
    number_rot <- 0
    
  }
  else if (direction == "W") {
    start_row <- player_position$row - 1
    end_row <- player_position$row + 1 
    start_col <- player_position$col - 3 
    end_col <- player_position$col 
    number_rot <- 1
  }
  else if (direction == "S") {
    start_row <- player_position$row 
    end_row <- player_position$row + 3 
    start_col <- player_position$col - 1 
    end_col <- player_position$col + 1 
    number_rot <- 2
  }
  else if (direction == "E") {
    start_row <- player_position$row - 1 
    end_row <- player_position$row + 1 
    start_col <- player_position$col
    end_col <- player_position$col + 3 
    number_rot <- 3
  }
  start_row <- start_row + padding
  end_row <- end_row + padding
  start_col <- start_col + padding
  end_col <- end_col + padding
  maze_view <- meta_maze[start_row:end_row, start_col:end_col]
  
  count_rot <- 0
  while(count_rot < number_rot)
  {
    maze_view <- rotate_clockwise(maze_view)
    count_rot <- count_rot + 1
  }

  maze_view[nrow(maze_view),2] <- PLAYER
  maze_view
}


render_view <- function(maze, direction) {
  

  #cat("\014") 
  cat("\f")
  cat("\t**** GHOST MAZE ****\n")
  cat("\t  FORWARD    👆: x\n")
  cat("\t  TURN LEFT  👈: n\n")
  cat("\t  TURN RIGHT 👉: m\n")
  cat("\t  QUIT       👋: q\n")
  cat("\n")
  #print(sprintf("Player direction: %s", player_direction))
  #print(sprintf("Player position: %d,%d", player_position$row, player_position$col))
  #print(sprintf("Ghost position: %d,%d", ghost_position$row, ghost_position$col))
  #print(kable(maze, "simple", align = "ccc"))
  
  cat("\t🏿 🏿 🏿 🏿 🏿 🏿\n")
  cat(paste("\t🏿 ",apply(maze, 1, paste, collapse = ""),"🏿 ", collapse = "\n"))
  cat("\n\t🏿 🏿 🏿 🏿 🏿 🏿\n")
  cat("\n")

  
}

turn <- function(direction, towards) {

  curr_direction_idx <- match(direction,DIRECTIONS) # 1:4
  next_direction_idx <- switch(towards,
                              "LEFT" = curr_direction_idx - 1,
                              "RIGHT" = curr_direction_idx + 1)
  
  if (next_direction_idx == 0) {
    next_direction_idx <- 4
  }  else if (next_direction_idx == 5){
    next_direction_idx <- 1
  }
  DIRECTIONS[next_direction_idx]
}

##
maze.data <- c(0,0,0,0,0,0,0,0,0,0)
maze.data <- c(maze.data,0,1,1,1,1,0,0,1,1,0)
maze.data <- c(maze.data,0,0,1,0,0,1,1,1,0,0)
maze.data <- c(maze.data,0,0,1,1,0,1,0,1,1,0)
maze.data <- c(maze.data,0,1,1,0,1,0,0,1,0,0)
maze.data <- c(maze.data,0,0,1,1,1,1,1,1,0,0)
maze.data <- c(maze.data,0,0,0,0,0,0,9,0,0,0)
maze = matrix(maze.data,nrow=7,ncol=10,byrow=TRUE);

#maze.data <- c(1,1,1,1,1)
#maze.data <- c(maze.data,0,0,0,0,0)
#maze.data <- c(maze.data,0,1,1,1,0)
#maze.data <- c(maze.data,0,1,1,1,0)
#maze.data <- c(maze.data,0,1,9,1,0)
#maze = matrix(maze.data,nrow=5,ncol=5,byrow=TRUE);


player_position <- get_random_position(maze)
ghost_position <- get_random_position(maze)
#maze <- move_to(maze, GHOST, ghost_position)
ghost_moves <- 0
player_direction <- get_random_direction()
player_moves_since_last_ghost_move <- 0

game <- TRUE

while(game) {

  #ghost_moves
  if (player_moves_since_last_ghost_move == GHOST_SPEED) {
    ghost_position <- get_random_position(maze)
    ghost_moves <- ghost_moves +  1
    player_moves_since_last_ghost_move <- 0
  }

  #ghost player collision
  repeat{
    if (is_next_to(player_position, ghost_position)) {
      if(ghost_moves > 1) {
        render_ghost()
        beep(9)
        Sys.sleep(3)
      }
      ghost_position <- get_random_position(maze)
      player_position <- get_random_position(maze)
      player_direction <- get_random_direction()
    }
    else {
      break
    }
  }
  
  maze_view <- what_player_can_see(maze = maze,player_position = player_position, ghost_position = ghost_position, direction = player_direction)
  render_view(get_graphics(maze_view,mapping))
  
  repeat {
    
    action <- user.input("Choose your next move (x,n,m,q) and press enter:")
    
    if( action %in% c("N","n")) {
      cat(sprintf("%s: turning left 👈\n", action))
      beep(1)
      Sys.sleep(1)
      player_direction <- turn(player_direction,"LEFT")
      player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
      break
    } else if (action %in% c("M","m")) {
      cat(sprintf("%s: turning right 👉\n", action))
      beep(1)
      Sys.sleep(1)
      player_direction <- turn(player_direction,"RIGHT")
      player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
      break
    } else if (action %in% c("X","x")) {
      cat(sprintf("%s: moving forward 👆\n", action))
      beep(1)
      Sys.sleep(1)
      next_position <- get_position_forward(player_position, player_direction)
      # Wall collision
      if(can_move_to(maze,next_position)) {
        player_position <- next_position
        player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
        if(is_exit(maze,player_position)) {
          cat(sprintf("You have escaped in %d moves\n", ghost_moves * GHOST_SPEED + player_moves_since_last_ghost_move))
          render_bye()
          Sys.sleep(2)
          game <- FALSE
        }
        break
      }
      else {
        cat("You are a muggle, you cannot walk through the wall!!\n")
      }
      
    } else if (action %in% c("Q","q")) {
      cat(sprintf("%s: leaving...\n", action))
      beep(6)
      Sys.sleep(1)
      cat("We miss you already\n")
      game <- FALSE
      break
    }
  }
}




