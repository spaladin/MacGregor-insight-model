
require(data.table)
require(ggplot2)


# NEEDED FUNCTIONS
source('files/needed_funs.R', echo=F)

# saved variables
load('files/all_moves.RData')
load('files/all_moves2.RData')
load('files/all_moves_value.RData')
load('files/all_moves_value2.RData')

load('files/all_moves_11.RData')
load('files/all_moves_value_11.RData')
load('files/all_moves_12.RData')
load('files/all_moves_value_12.RData')

all_operators <- fread("files/all_operators.csv")
dot_address <- fread('files/dot_address.csv')




sim_macgregor <- function(dots_number = 9, attempts = 20, lookahead, k = 1){
  ### 'dots_number' argument defines the task ###
  ### do not forget to set 'lookahead' ###
  
  if (dots_number == 11){
    all_moves <- all_moves_11
    all_moves_value <- all_moves_value_11
    all_operators[, V1 := ifelse(start %in% c(10,11), start, V1)]
    all_operators[, V2 := ifelse(end %in% c(10,11), end, V2)]
  } else if (dots_number == 12) {
    all_moves <- all_moves_12
    all_moves_value <- all_moves_value_12
    all_operators[, V1 := ifelse(start %in% 10:12, start, V1)]
    all_operators[, V2 := ifelse(end %in% 10:12, end, V1)]
  }
  
  solutions <- list(trials = list(), success = c(), plots = c())
  
  for (i in 1:attempts){
    
    # lookahead = 3 - for lookahead 3 simulation
    lines_lost <- 4
    rem_dots <- 1:dots_number
    dots_lost <- dots_number
    trial <- list()
    
    while (lines_lost > 0 & dots_lost > 0){
      
      # generate all possible moves
      operators <- all_operators[start < (dots_number+1) & end < (dots_number+1)]
      
      if (lines_lost == 4){ # first move
        selected_moves <- all_moves[sapply(all_moves, length) == lookahead]
        selected_moves_value <- all_moves_value[sapply(all_moves, length) == lookahead]
      } else { # not the first move
        if(lookahead %in% 1:2){
          selected_moves <- moves_from_dot(all_moves[sapply(all_moves, length) == lookahead], dot = last_dot, operators)
          selected_moves_value <- sapply(selected_moves, get_value, rem_dots, operators)
        } else if(lookahead == 3){
          selected_moves <- moves_from_dot(all_moves[sapply(all_moves, length) == 1], dot = last_dot, operators)
          selected_moves_value <- sapply(selected_moves, get_value, rem_dots, operators)
        } 
      } 
      
      # choose the best dot move
      if(lines_lost < 4) {print(draw_trial(trial, task_dots = dots_number, operators))}
      best_move <- which(selected_moves_value == max(selected_moves_value))
      best_move <- ifelse(length(best_move) == 1, best_move, sample(best_move, 1)) # random choise of the best move

      # evaluate moves with metacognition
      criterion <- dots_lost / (lines_lost/lookahead)
      
      value_of_the_best_available_move <- selected_moves_value[best_move]
      m <- k*(criterion - value_of_the_best_available_move) / criterion # impulse (m) to seek new dots. k - parameter 
      if(m < 0){m <- 0}
      
      # unmarked dots adjacent to marked dots can be used only if it highers the value of the move.
      # look for unmarked dots with the probability m
      if(sample(c(1,0),1, prob = c(m,1-m))){
        
        # generate all possible moves
        operators <- all_operators
        
        if (lines_lost == 4){ # first move
          selected_moves2 <- all_moves2[sapply(all_moves2, length) == lookahead]
          selected_moves_value2 <- all_moves_value2[sapply(all_moves2, length) == lookahead]
        } else { # not the first move
          if(lookahead %in% 1:2){
            selected_moves2 <- moves_from_dot(all_moves2[sapply(all_moves2, length) == lookahead], dot = last_dot, operators)
            selected_moves_value2 <- sapply(selected_moves2, get_value, rem_dots, operators)
          } else if(lookahead == 3){
            selected_moves2 <- moves_from_dot(all_moves2[sapply(all_moves2, length) == 1], dot = last_dot, operators)
            selected_moves_value2 <- sapply(selected_moves2, get_value, rem_dots, operators)
          } 
        } 
        
        # choose the best dot move
        best_move2 <- which(selected_moves_value2 == max(selected_moves_value2))
        
        if (length(best_move2) > 1){
          shortest <- sapply(selected_moves2[best_move2], FUN = move_length, operators)
          best_move2 <- best_move2[shortest == min(shortest)]
        }     
        best_move2 <- ifelse(length(best_move2) == 1, best_move2, sample(best_move2, 1)) # random choise of the best move

        value_of_the_best_available_move2 <- selected_moves_value2[best_move2]
        
        if (value_of_the_best_available_move2 > value_of_the_best_available_move){
          move <- selected_moves2[[best_move2]] # choose move finally
        } else move <- selected_moves[[best_move]] # choose move finally
      } else move <- selected_moves[[best_move]] # choose move finally
      
      # compute all values nessesary for the next move
      lines_lost <- lines_lost - length(move)
      rem_dots <- rem_dots[!(rem_dots %in% crossed_dots(move, operators))]
      dots_lost <- length(rem_dots)
      last_dot <- operators[number == move[length(move)], end]
      trial[[length(trial)+1]] <- move
      # lookahead = 1 # for lookahead 3 simulation
    }
    
    solutions$trials[[length(solutions$trials)+1]] <- trial
    solutions$success <- append(solutions$success, ifelse(dots_lost == 0, 1, 0))
    print(draw_trial(trial, task_dots = dots_number, operators))
    ggsave(filename = as.character(i), plot = draw_trial(trial, task_dots = dots_number, ops = operators), path = "sim_plots", device = 'png')
  }
  return(solutions)
}








