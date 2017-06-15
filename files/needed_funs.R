
require(data.table)

# FUNCTION FOR GETTING CROSSED DOTS FROM THE MOVE
crossed_dots <- function(x, ops){
  # x - vector of operators #
  unique(as.vector(as.matrix(ops[number %in% x, 5:ncol(ops)])))
}

# FUNCTION FOR GETTING MOVE VALUE
get_value <- function(x, rem_dots, ops){
  # x - vector of operators; rem_dots - remaining uncrossed dots #
  sum(crossed_dots(x, ops) %in% rem_dots)
}

# FUNCTION TO FIND LENGTH OF THE LINES IN A MOVE
move_length <- function(move_number, ops){
  # one move
  sum(ops[number %in% move_number, dots]) - length(move_number) + 1
}

# FUNCTION FOR GETTING FIRST POINT OF THE MOVE
moves_from_dot <- function(moves_list, dot, ops){
  moves_list[sapply(moves_list, FUN = function(x) ops[number == x[1], start]) == dot]
}


# FUNCTIONS TO DRAW THE TRIAL
draw_trial <- function(trial, task_dots = 9, ops){
  # trial - list of trials
  
  dot_address[, nine := ifelse(dot_num > task_dots, 'white', 'black')]
  
  p1 <- sapply(unlist(trial), FUN = function(x) ops[number == x, c(start, end)][1])
  p2 <- sapply(unlist(trial), FUN = function(x) ops[number == x, c(start, end)][2])
  x1 <<- sapply(p1, FUN = function(x) dot_address[dot_num == x, x_lab])
  y1 <<- sapply(p1, FUN = function(x) dot_address[dot_num == x, y_lab])
  x2 <<- sapply(p2, FUN = function(x) dot_address[dot_num == x, x_lab])
  y2 <<- sapply(p2, FUN = function(x) dot_address[dot_num == x, y_lab])
  
  p <- ggplot(data = dot_address, aes(x_lab, y_lab)) + 
    geom_point(size = 7, col = dot_address$nine) + 
    theme_void() + 
    geom_segment(aes(x = x1[1], y = y1[1], xend = x2[1], yend = y2[1]), arrow = arrow(length = unit(0.5, "cm")), col = 'blue', size = 1) + 
    geom_segment(aes(x = x1[2], y = y1[2], xend = x2[2], yend = y2[2]), arrow = arrow(length = unit(0.5, "cm")), col = 'blue', size = 1) + 
    geom_segment(aes(x = x1[3], y = y1[3], xend = x2[3], yend = y2[3]), arrow = arrow(length = unit(0.5, "cm")), col = 'blue', size = 1) +
    geom_segment(aes(x = x1[4], y = y1[4], xend = x2[4], yend = y2[4]), arrow = arrow(length = unit(0.5, "cm")), col = 'blue', size = 1)
  
  return(p)
}


