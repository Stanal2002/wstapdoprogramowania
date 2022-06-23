sudoku <- matrix(data=c(
  7,0,0,0,2,0,4,8,0,
  2,0,6,0,0,8,0,0,5,
  5,0,0,9,0,0,0,0,0,
  0,0,0,1,5,0,0,0,0,
  0,2,0,0,0,0,0,6,0,
  0,0,0,0,6,7,0,0,0,
  0,0,0,0,0,6,0,0,3,
  6,0,0,5,0,0,1,0,4,
  0,9,3,0,4,0,0,0,7), nrow=9, ncol=9, byrow=FALSE
)

find_empty_cells <- function(board) (
  
  which(board == 0, arr.ind = TRUE)
  
)
is_valid <- function(board, num, row, col) {

  if(any(board[row, ] == num)) {
    
    return(FALSE)
    
  }
  
  
  if(any(board[, col] == num)) {
    
    return(FALSE)
    
  }
  
 
  box_x <- floor((row - 1) / 3) + 1
  box_y <- floor((col - 1) / 3) + 1
  
  
  box <- board[(3 * box_x - 2):(3 * box_x), (3 * box_y - 2):(3 * box_y)]
  
  
  if(any(box == num)) {
    
    return(FALSE)
    
  }
  
  return(TRUE)
  
}

result <- sudoku
solve_sudoku <- function(board, needed_cells = NULL, index = 1) {
  
  
  if(is.null(needed_cells)) 
    needed_cells <- find_empty(board)
  
  if(index > nrow(needed_cells)) {
    
    
    result <<- board
    return(TRUE)
    
  } else {
    
    row <- needed_cells[index, 1]
    col <- needed_cells[index, 2]
  }
  
  
  for(num in 1:9) {
    
    
    if(!is_valid(board, num, row, col)) {next} else{
      board2 = board
      board2[row, col] <- num
      
      
      if(solve_sudoku(board2, needed_cells, index + 1)) {
        return(TRUE)
        
      }
      
    }
    
  }
  
  
  return(FALSE)
  
}

solve_sudoku(board)
