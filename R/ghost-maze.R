# Ghost Maze
# Naming conventions
# http://adv-r.had.co.nz/Style.html
# https://www.datanovia.com/en/blog/r-coding-style-best-practices/#function-naming-convention

#install.packages("collections")
#install.packages("invctr")
# suppressMessages(library(collections))
# suppressMessages(library(knitr))
# suppressMessages(library(beepr))
# suppressMessages(library(dplyr))
# suppressMessages(library(invctr))
# suppressMessages(library(stringr))
require(collections)
require(knitr)
require(beepr)
require(dplyr)
require(invctr)
require(stringr)

# Constants
WALL <- 0
CORRIDOR <- 1
EXIT <- 9
GHOST <- 2
ZOMBIE <- 3
PLAYER <- 5
SIGHT <- 100
DIRECTIONS <- c("N", "E", "S", "W")


# Action map
# * key (name):""
# * value (action): list
# * * desc:""
# * * keys (keyboard):c()
# * * echo:""
action_map <- dict()
action_map$set("walk" ,
               list(
                 "desc"="walk forward  👆",
                 "keys"=c("w","W"),
                 "echo"="%s: moving forward 👆"))
action_map$set("turnr",
               list(
                 "desc"="turn right    👉", 
                 "keys"=c("d","D"),
                 "echo"="%s: turning right 👉"))
action_map$set("turnl",
               list(
                 "desc"="turn left     👈",
                 "keys"=c("a","A"),
                 "echo"="%s: turning left 👈"))
action_map$set("quit" ,
               list(
                 "desc"="quit the game 👋", 
                 "keys"=c("q","Q"),
                 "echo"="%s: leaving..."))
sound_map <- dict()
sound_map$set("move",list("beep"=10, "duration" = 0))
sound_map$set("quit",list("beep"=6, "duration" = 1))
sound_map$set("ghost",list("beep"=9, "duration" = 3))
sound_map$set("zombie",list("beep"=9, "duration" = 3))
sound_map$set("finish",list("beep"=3, "duration" = 3))
sound_map$set("intro",list("beep"=8, "duration" = 6))

graph_map <- dict()
graph_map$set(WALL, list("block"="🏾 ","desc"="wall")) #⛰  ◾■🔳
#graph_map$set(WALL, list("block"="⬛ ","desc"="wall")) 
graph_map$set(CORRIDOR, list("block"="🏻 ","desc"="corridor"))# □ ◻ ◽
#graph_map$set(CORRIDOR, list("block"="⬜ ","desc"="corridor"))
graph_map$set(GHOST,  list("block"="👻 ", "desc"="ghost" ))#🎃🧟🕷 🧛 🧟
#graph_map$set(EXIT,   list("block"="⛩ "  ,"desc"="exit"))
graph_map$set(EXIT,   list("block"="🏆 "  ,"desc"="exit"))
#graph_map$set(EXIT,   list("block"="🏁 "  ,"desc"="exit"))
graph_map$set(PLAYER, list("block"="🚹 ","desc"="player")) #🚺
#graph_map$set(SIGHT, list("block"="🏿 ","desc"="visual field limit"))
#graph_map$set(SIGHT, list("block"="🌑 ", "desc"="visual field limit"))
graph_map$set(ZOMBIE, list("block"="🧟 ","desc"="zombie"))


#https://stackoverflow.com/questions/27112370/make-readline-wait-for-input-in-r
user_input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n=1))
  }
}

# @misc{ wiki:xxx,
#   author = "Rosetta Code",
#   title = "Terminal control/Clear the screen --- Rosetta Code{,} ",
#   year = "2022",
#   url = "https://rosettacode.org/w/index.php?title=Terminal_control/Clear_the_screen&oldid=328572",
#   note = "[Online; accessed 5-October-2022]"
# }
clear_screen <- function() {
  if (interactive()) {
    cat("\014") #cat("\f")
  } else {
    cat("\33[2J")
  }
}

#
convert_to_invctr_position <- function(position) {
  invctr_position <- data.frame(nv=1, row=position$row, col=position$col, row.names = NULL)
}

debug_matrix <- function(matrix,name) {
  cat(paste0(name,"\n"))
  cat(paste(apply(matrix, 1, paste, collapse = ""),collapse = "\n"))
  cat("\n")
}

# Gets a random position in a corridor
# return position: list
# * row:int
# * col:int
get_random_position <- function (maze, occupied_positions = list()) {
  #debug_matrix(matrix=maze,name="maze")
  corridor_positions <- CORRIDOR %ai% maze # gets the indeces for all CORRIDOR places in the maze
  available_positions <- corridor_positions
   for(occupied in occupied_positions) {
     available_positions <- available_positions[!((occupied$row == available_positions$row) & (occupied$col == available_positions$col)),]
   }
  #debug_matrix(matrix =  available_positions, name="available_positions")
  new_position <- available_positions[sample(1:nrow(available_positions),1),]
  return(list("row" = new_position$row, "col" = new_position$col))
}


#
render_bye <- function() {
  cat("🎉 🎊 👏 👏 👏\n")
}

#
#https://manytools.org/hacker-tools/ascii-banner/
#Font:poison
title <- function() {
  title <-  "                                                                                                                     
@@@@@@@@@@    @@@@@@   @@@  @@@   @@@@@@  @@@@@@@  @@@@@@@@  @@@@@@@      @@@@@@@@@@    @@@@@@   @@@@@@@@  @@@@@@@@  
@@@@@@@@@@@  @@@@@@@@  @@@@ @@@  @@@@@@@  @@@@@@@  @@@@@@@@  @@@@@@@@     @@@@@@@@@@@  @@@@@@@@  @@@@@@@@  @@@@@@@@  
@@! @@! @@!  @@!  @@@  @@!@!@@@  !@@        @@!    @@!       @@!  @@@     @@! @@! @@!  @@!  @@@       @@!  @@!       
!@! !@! !@!  !@!  @!@  !@!!@!@!  !@!        !@!    !@!       !@!  @!@     !@! !@! !@!  !@!  @!@      !@!   !@!       
@!! !!@ @!@  @!@  !@!  @!@ !!@!  !!@@!!     @!!    @!!!:!    @!@!!@!      @!! !!@ @!@  @!@!@!@!     @!!    @!!!:!    
!@!   ! !@!  !@!  !!!  !@!  !!!   !!@!!!    !!!    !!!!!:    !!@!@!       !@!   ! !@!  !!!@!!!!    !!!     !!!!!:    
!!:     !!:  !!:  !!!  !!:  !!!       !:!   !!:    !!:       !!: :!!      !!:     !!:  !!:  !!!   !!:      !!:       
:!:     :!:  :!:  !:!  :!:  !:!      !:!    :!:    :!:       :!:  !:!     :!:     :!:  :!:  !:!  :!:       :!:       
:::     ::   ::::: ::   ::   ::  :::: ::     ::     :: ::::  ::   :::     :::     ::   ::   :::   :: ::::   :: ::::  
 :      :     : :  :   ::    :   :: : :      :     : :: ::    :   : :      :      :     :   : :  : :: : :  : :: ::   
                                                                                                                      "
}

game_over <- function() {
  msg <- "                                                                               
                                                                                       
 @@@@@@@@   @@@@@@   @@@@@@@@@@   @@@@@@@@      @@@@@@   @@@  @@@  @@@@@@@@  @@@@@@@   
@@@@@@@@@  @@@@@@@@  @@@@@@@@@@@  @@@@@@@@     @@@@@@@@  @@@  @@@  @@@@@@@@  @@@@@@@@  
!@@        @@!  @@@  @@! @@! @@!  @@!          @@!  @@@  @@!  @@@  @@!       @@!  @@@  
!@!        !@!  @!@  !@! !@! !@!  !@!          !@!  @!@  !@!  @!@  !@!       !@!  @!@  
!@! @!@!@  @!@!@!@!  @!! !!@ @!@  @!!!:!       @!@  !@!  @!@  !@!  @!!!:!    @!@!!@!   
!!! !!@!!  !!!@!!!!  !@!   ! !@!  !!!!!:       !@!  !!!  !@!  !!!  !!!!!:    !!@!@!    
:!!   !!:  !!:  !!!  !!:     !!:  !!:          !!:  !!!  :!:  !!:  !!:       !!: :!!   
:!:   !::  :!:  !:!  :!:     :!:  :!:          :!:  !:!   ::!!:!   :!:       :!:  !:!  
 ::: ::::  ::   :::  :::     ::    :: ::::     ::::: ::    ::::     :: ::::  ::   :::  
 :: :: :    :   : :   :      :    : :: ::       : :  :      :      : :: ::    :   : :  
                                                                                        "
}


#
#https://textart.sh/topic/ghost
ghost_intro <- function() {
ghost_intro <- "
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████        ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                    ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                        ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                            ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                            ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                                ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██        ████        ████        ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██        ██            ██        ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██        ████        ████        ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██    ░░░░                ░░░░    ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██            ████████            ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██    ██████            ██████    ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██          ██        ██          ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██          ██        ██          ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██    ██████            ██████      ██▓▓▓▓██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                                  ████  ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                                        ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                                    ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                                  ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██                            ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████                    ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████████████████████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
"
}

#
ghost_encounter <- function() {
  ghost_encounter <-"  ________________________________________________________________________________________
  _______________________ BOOOO BOOOOOOO!   SHUFFLE!   ___________________________________
  ________________________________________________________________________________________
  ░░______░░______░░____________░░__░░__░░__░░██______░░__░░░░______________░░______░░__░░
  ______________________░░__________________██████__________________░░____________________
  __________________________________________██████________________________________________
  ________________________________________██████████______________________________________
  ________________________________________██████████______________________________________
  ______________________________________██████████████____________________________________
  ______________________________________██████████████___________________________________
  __________________________________████__██__██__██__████________________________________
  ____________________________██████████████████████████████████__________________________
  ____________________________██████████████████████████████████__________________________
  ________________________________██████████████████████████______________________________
  ______________________________██__________________________██____________________________
  ______________________________██______██________██________██____________________________
  ______________________________██____████________████______██____________________________
  ______________________________██____████________████______██____________________________
  ________░░______░░____________██__________________________██______________░░______░░____
  ______________________░░____██____________████______________██____░░____________________
  ____________________________██______________________________██__________________________
  ______________________________████______________________████____________________________
  __________________________________██__________________██__░░____________________________
  __________________________________██__________________██________________________________
  ____________________________________████__________████__________________________________
  ________________________________________██████████______________________________________
  ________________________________________________________________________________________
  ________________________________________________________________________________________"
}

zombie_encounter <- function() {
  zombie_encounter <- "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░████░░░░░░░░░░░░░░████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░██░░░░░░░░░░██░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░██░░░░░░░░░░██░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░██░░░░░░░░░░░░░░░░░░░░░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░██░░░░░░░░░░██░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░██░░░░░░░░██░░██░░░░░░░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░▒▒▒▒░░░░░░▒▒▒▒░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░▒▒▒▒▒▒░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░████████░░▒▒░░████████░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░████████░░░░░░████████░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░████░░░░░░░░░░████░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░▒▒░░░░░░░░░░▒▒░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░▒▒░░░░██░░██░░░░▒▒░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██▒▒░░██░░░░░░░░░░██░░▒▒██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░██░░██░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░██░░██░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"

}

#
get_random_direction <- function() {
  lower_limit <- 1
  upper_limit <- length(DIRECTIONS) # length of a vector
  idx <- round(runif(1,lower_limit,upper_limit))
  DIRECTIONS[idx]
}

#
can_move_to <- function(maze, destination) {
  return (maze[destination$row,destination$col] == CORRIDOR ||
            maze[destination$row,destination$col] == EXIT)
}

#
is_exit <- function(maze,position) {
  return (maze[position$row,position$col] == EXIT)
}

#
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

calc_distance <- function(position_1, position_2) {
  row_distance <- abs(position_1$row - position_2$row)
  col_distance <- abs(position_1$col - position_2$col)
  distance <- row_distance + col_distance
}

#  FTF
#  TXT
#  FTF
is_next_to <- function(position_1, position_2, max_distance) {
  distance <- calc_distance(position_1 = position_1, position_2 = position_2)
  if(distance > max_distance) {
    return(FALSE)
  }
  TRUE
}

#
is_player_next_to_any_ghost <-  function(position_1, positions) {
  Reduce('|',lapply(positions,is_next_to,position_1 = position_1, max_distance = 1))
}

#
is_player_caught_by_any_zombie <- function(player_position, zombie_positions) {
  Reduce('|',lapply(zombie_positions,is_next_to,position_1 = player_position, max_distance = 0))
}

#
get_graphics <- function(maze_view,graph_map) {
  nrow <- nrow(maze_view)
  ncol <- ncol(maze_view)
  matrix(lapply(lapply(c(maze_view),graph_map$get),function(x) {return (x$block)}), nrow,ncol)
}

#
rotate_clockwise <- function(x) {t( apply(x, 2, rev))}

#
what_player_can_see <- function (maze, player_position, ghost_positions, direction, forward_vision = 4, rear_vision = 0) {
  
  lateral_vision <- floor((forward_vision - 1)/2)
  padding <- forward_vision
  number_rot <- 0
  meta_maze <- matrix(0,nrow(maze) + (2 * padding), ncol(maze) + (2 * padding))
  meta_maze[(1 + padding):(nrow(maze) + padding ), (1 + padding):(ncol(maze) + padding)] <- maze
  for (ghost_position in ghost_positions) {
    meta_maze[ghost_position$row + padding,ghost_position$col + padding] <- GHOST
  }
  for (zombie_position in zombie_positions) {
    meta_maze[zombie_position$row + padding,zombie_position$col + padding] <- ZOMBIE
  }
  if(direction == "N") {
    start_row <- player_position$row  - forward_vision
    end_row <- player_position$row + rear_vision
    start_col <- player_position$col - lateral_vision
    end_col <- player_position$col + lateral_vision 
    number_rot <- 0
  }
  else if (direction == "W") {
    start_row <- player_position$row - lateral_vision
    end_row <- player_position$row + lateral_vision 
    start_col <- player_position$col - forward_vision
    end_col <- player_position$col + rear_vision
    number_rot <- 1
  }
  else if (direction == "S") {
    start_row <- player_position$row - rear_vision
    end_row <- player_position$row + forward_vision
    start_col <- player_position$col - lateral_vision
    end_col <- player_position$col + lateral_vision 
    number_rot <- 2
  }
  else if (direction == "E") {
    start_row <- player_position$row - lateral_vision
    end_row <- player_position$row + lateral_vision
    start_col <- player_position$col - rear_vision
    end_col <- player_position$col + forward_vision
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

  maze_view[nrow(maze_view) - rear_vision,lateral_vision  + 1] <- PLAYER
  maze_view
}

# Created with
# https://manytools.org/hacker-tools/ascii-banner/
render_view <- function(maze, direction, action_map, graph_map) {
  
  echo(title(),clear=T)
  #layout: Actions | Map | Legend
  #cat(sprintf("Player direction: %s \n", player_direction))
  
  #print(kable(maze, "simple", align = "ccc"))
  action_height <- action_map$size()
  legend_height <- graph_map$size()
  map_height <- nrow(maze)
  
  pane1_height <- max(c(legend_height, action_height)) + 1
  pane1 <- matrix("", nrow = pane1_height, ncol = 2 )
  colnames(pane1) <- c("Legend","Actions")
  
  pane2_height <- map_height + 1
  pane2 <- matrix("", nrow = pane2_height, ncol = 1 )
  colnames(pane2) <- c("Map")
  
  pane2[1,"Map"] <- ".\t\t\tMap"
  
  map_idx <- 2
  for (line in apply(maze, 1, paste, collapse = "")) {
    pane2[map_idx,"Map"] <- paste0(".\t\t\t",line)
    map_idx <- map_idx + 1
  }
  

  pane1[1,"Legend"] <- "Legend"
  legend_idx <- 2
  for(stripe in graph_map$values()) {
    pane1[legend_idx,"Legend"] <- paste0("• ", stripe$block," ", stripe$desc)
    legend_idx <- legend_idx + 1
  }

  pane1[1,"Actions"] <- "Actions"
  action_idx <- 2
  for(action_value in action_map$values()) {
    pane1[action_idx,"Actions"] <- paste0("• ", action_value$desc," ", paste(action_value$keys,collapse=" or "))
    action_idx <- action_idx + 1
  }

  #print(kable(pane1, format="simple",align = "ll",col.names = NULL))
  #print(kable(pane2, format="simple",align = "l",col.names = NULL))
  
  cat("\n")
  cat(paste(pane1,collapse="\n"))
  cat(paste0("Lives: ", paste(rep("🧡",lives),sep="",collapse=" "),"\n"))
  cat(paste(pane2,collapse="\n"))
  cat("\n\n\n\n\n")
}

#
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

#
play <- function(sound_map, x) {
  sound <- sound_map$get(x)
  beep(sound$beep)
  #Sys.sleep(sound$duration)
}

#
echo <- function(msg, sound_map = NULL, sound_key=NULL, clear = FALSE, duration = 0){
  if (clear == T) {
    clear_screen()
  }
  if (!is.null(sound_key)) {
    sound <- sound_map$get(sound_key)
    beep(sound$beep)
    duration = sound$duration
  }
  cat(paste0(msg,"\n"))
  if(duration != 0 ) {
    Sys.sleep(duration)
  }
}

#
get_closer_to_player <- function(maze, position_1, position_2) {
   maze_layer <- matrix(0,nrow=nrow(maze),ncol=ncol(maze))
   maze_layer[(position_1$row-1):(position_1$row+1), (position_1$col-1):(position_1$col+1)] <- maze[(position_1$row-1):(position_1$row+1), (position_1$col-1):(position_1$col+1)]
   corridor <- CORRIDOR %ai% maze_layer # gets the indeces for all CORRIDOR places in the around
   curr_distance <- calc_distance(position_1 = position_1, position_2 = position_2)
   for (i in 1:nrow(corridor)) {
     new_position <- list("row"=corridor[i,]$row,"col"=corridor[i,]$col)
     new_distance <- calc_distance(new_position, position_2)
     if (new_distance < curr_distance) {
       return(new_position)
     }
   }
   position_1
}

#
move_zombies <- function(maze, zombie_positions, player_position) {
  
  new_zombie_positions <- list()
  for(zombie_position in zombie_positions) {
    new_zombie_positions <- append(new_zombie_positions,list(get_closer_to_player(maze = maze, position_1=zombie_position,position_2=player_position)))
  }
  return(new_zombie_positions)
}


#
get_random_positions <- function(maze, num, occupied_positions) {
  
  positions <- list()
  if(num > 0) {
    for (counter in 1:num) {
      positions <- append(positions, list(get_random_position(maze=maze,occupied_positions = positions)) )
    }
  }
  return (positions)
}

#
shuffle <- function(maze, num_ghosts = 1, num_zombies = 0, occupied_positions = list()) {
  player_position <- get_random_position(maze=maze)
  ghost_positions <- get_random_positions(maze=maze,num=num_ghosts,occupied_positions=c(player_position,occupied_positions))
  zombie_positions <- get_random_positions(maze=maze,num=num_zombies, occupied_positions=c(player_position,ghost_positions,occupied_positions))
  player_direction <- get_random_direction()
  
  cat(sprintf("shuffle: player position: %d,%d \n", player_position$row, player_position$col))
  cat("shuffle: ghost_positions")
  cat(paste(ghost_positions, sep="", collapse="\n"))
  cat("\n")
  cat("shuffle: zombies_positions")
  cat(paste(zombie_positions, sep="", collapse="\n"))
  cat("\n")
  
  return (list("player_position"=player_position, 
               "ghost_positions"=ghost_positions, 
               "player_direction"=player_direction, 
               "zombie_positions"=zombie_positions))
}

# Mazes
maze1_data <-            c(0,0,0,0,0,0,0,0,0,0)
maze1_data <- c(maze1_data,0,1,1,1,1,0,0,1,1,0)
maze1_data <- c(maze1_data,0,0,1,0,0,1,1,1,0,0)
maze1_data <- c(maze1_data,0,0,1,1,0,1,0,1,1,0)
maze1_data <- c(maze1_data,0,1,1,0,1,0,0,1,0,0)
maze1_data <- c(maze1_data,0,1,1,1,1,1,1,1,0,0)
maze1_data <- c(maze1_data,0,0,0,0,0,0,0,9,0,0)
maze1 = matrix(maze1_data,nrow=7,ncol=10,byrow=TRUE);

maze2_data <-            c(0,0,0,0,0,0,0,0,0,0,0,0,0)
maze2_data <- c(maze2_data,0,1,1,1,1,0,0,1,1,1,0,0,0)
maze2_data <- c(maze2_data,0,0,1,0,0,1,1,1,0,0,1,0,0)
maze2_data <- c(maze2_data,0,1,1,1,0,1,0,1,1,0,1,0,0)
maze2_data <- c(maze2_data,0,0,0,1,0,1,0,1,1,1,1,1,0)
maze2_data <- c(maze2_data,0,0,1,1,0,1,0,1,1,0,0,1,0)
maze2_data <- c(maze2_data,0,1,1,0,9,0,0,1,0,0,0,1,0)
maze2_data <- c(maze2_data,0,0,1,1,1,1,1,1,0,0,0,1,0)
maze2_data <- c(maze2_data,0,0,0,0,0,0,0,0,0,0,0,0,0)
maze2 = matrix(maze2_data,nrow=9,ncol=13,byrow=TRUE);


maze3_data <-            c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
maze3_data <- c(maze3_data,0,1,1,1,0,1,1,0,1,1,0,1,1,0,0)
maze3_data <- c(maze3_data,0,1,0,1,1,1,1,0,1,1,1,1,0,0,0)
maze3_data <- c(maze3_data,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0)
maze3_data <- c(maze3_data,0,0,1,0,0,1,1,1,1,0,0,1,1,0,0)
maze3_data <- c(maze3_data,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0)
maze3_data <- c(maze3_data,0,1,1,1,0,1,1,0,1,1,0,1,0,0,0)
maze3_data <- c(maze3_data,0,0,1,1,0,1,1,0,1,1,1,1,1,1,0)
maze3_data <- c(maze3_data,0,0,1,1,0,1,1,9,1,1,0,0,0,1,0)
maze3_data <- c(maze3_data,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0)
maze3_data <- c(maze3_data,0,1,1,1,1,1,1,0,1,1,0,1,0,0,0)
maze3_data <- c(maze3_data,0,1,1,0,1,0,1,1,1,1,1,1,1,1,0)
maze3_data <- c(maze3_data,0,0,1,1,1,1,1,0,1,0,0,0,1,0,0)
maze3_data <- c(maze3_data,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
maze3 = matrix(maze3_data,nrow=15,ncol=15,byrow=TRUE);


mazes <- list(
  #maze1#,
#  maze2,
  maze3
  )

forward_vision <- 8 
rear_vision <- 0
num_ghosts <- 2
num_zombies <- 3
ghost_speed <- 10
zombie_speed <- 3
lives <- 5
maze <- mazes[[sample(1:length(mazes),1)]]


# Game init
set.seed(NULL)  

after_shuffle <- shuffle(maze=maze, num_ghosts= num_ghosts, num_zombies = num_zombies)
player_position <- after_shuffle$player_position
player_direction <- after_shuffle$player_direction
ghost_positions <- after_shuffle$ghost_positions
zombie_positions <- after_shuffle$zombie_positions
ghost_moves <- 0
player_moves_since_last_ghost_move <- 0
zombie_moves <- 0
player_moves_since_last_zombie_move <- 0
player_moves <- 0
game <- TRUE

# Play Intro
echo(title(),sound_map,"move", clear=TRUE)
echo(ghost_intro(),sound_map,"intro")

# Game loop
while(game) {

  #monster (ghost,zombie) player collision detection
  repeat {
    if (is_player_next_to_any_ghost(player_position, ghost_positions)) {
      
      if(ghost_moves > 1 || player_moves_since_last_ghost_move >= 1) {
        echo(ghost_encounter(), sound_map,"ghost",clear = T)
      }
      #zombies are not shuffled, they keep their positions
      after_shuffle <- shuffle(maze=maze, num_ghosts= num_ghosts, num_zombies = 0, occupied_positions = zombie_positions)
      player_position <- after_shuffle$player_position
      player_direction <- after_shuffle$player_direction
      ghost_positions <- after_shuffle$ghost_positions
    }
    else if (is_player_caught_by_any_zombie(player_position, zombie_positions)) {
      if(zombie_moves > 1 || player_moves_since_last_zombie_move >= 1) {
        echo(zombie_encounter(), sound_map,"zombie", clear = T)
      }
      #zombies are not shuffled, they keep their positions
      after_shuffle <- shuffle(maze=maze, num_ghosts= 0, num_zombies = num_zombies, occupied_positions = ghost_positions)
      player_position <- after_shuffle$player_position
      player_direction <- after_shuffle$player_direction
      zombie_positions <- after_shuffle$zombie_positions
      lives <- lives - 1
    }
    else {
      break
    }
  }
  
  if(lives == 0) {
    game <- FALSE
    echo(msg=game_over(),clear= TRUE)
  }
  else {

    #what the player can see
    maze_view <- what_player_can_see(maze = maze,
                                     player_position = player_position, 
                                     ghost_positions = ghost_positions,
                                     direction = player_direction,
                                     forward_vision = forward_vision,
                                     rear_vision  = rear_vision)
    
    #render player view
    render_view(get_graphics(maze_view,graph_map),player_direction,action_map, graph_map)
    
    #player input 
    repeat {
      input <- user_input("Choose your next move and press enter: ")
      if (input %in% action_map$get("turnl")$keys) {
        action <- action_map$get("turnl")
        echo(sprintf(action$echo, input),sound_map,"move")
        player_direction <- turn(player_direction,"LEFT")
        player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
        player_moves_since_last_zombie_move <- player_moves_since_last_zombie_move + 1
        player_moves <- player_moves + 1
        break
      } 
      else if (input %in% action_map$get("turnr")$keys) {
        action <- action_map$get("turnr")
        echo(sprintf(action$echo, input),sound_map,"move")
        player_direction <- turn(player_direction,"RIGHT")
        player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
        player_moves_since_last_zombie_move <- player_moves_since_last_zombie_move + 1
        player_moves <- player_moves + 1
        break
      } 
      else if (input %in% action_map$get("walk")$keys) {
        action <- action_map$get("walk")
        echo(sprintf(action$echo, input),sound_map,"move")
        next_position <- get_position_forward(player_position, player_direction)
        # Wall player collision detection
        if(can_move_to(maze,next_position)) {
          player_position <- next_position
          player_moves_since_last_ghost_move <- player_moves_since_last_ghost_move + 1
          player_moves_since_last_zombie_move <- player_moves_since_last_zombie_move + 1
          player_moves <- player_moves + 1
          if(is_exit(maze,player_position)) {
            render_bye()
            echo(sprintf("You have escaped in %d moves\n", ghost_moves * ghost_speed + player_moves_since_last_ghost_move),sound_map,"finish")
            game <- FALSE
          }
          break
        }
        else {
          cat("You are a muggle, you cannot walk through the walls!!\n")
        }
      } 
      else if (input %in% action_map$get("quit")$keys) {
        action <- action_map$get("quit")
        echo(sprintf(action$echo, input),sound_map,"quit")
        echo("We miss you already\n")
        game <- FALSE
        break
      } 
      else if (input %in% c("x","X")) {
        game <- FALSE
        break
      }
    }
    
    #ghosts move according to ghost speed
    if (player_moves_since_last_ghost_move == ghost_speed) {
      ghost_positions <- get_random_positions(maze = maze, num = num_ghosts, occupied_positions = c(player_direction,zombie_positions))
      ghost_moves <- ghost_moves +  1
      player_moves_since_last_ghost_move <- 0
    }
    
    #zombies move according to zombie speed
    if (player_moves_since_last_zombie_move == zombie_speed) {
      zombie_positions <- move_zombies(maze=maze,zombie_positions = zombie_positions, player_position = player_position)
      zombie_moves <- zombie_moves +  1
      player_moves_since_last_zombie_move <- 0
    }
    
  }
}