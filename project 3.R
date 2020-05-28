
get_symbols <- function(){
  wheel <-c("DD", "7","BBB", "BB", "B","C","0")
  sample(wheel, size = 3, replace = TRUE, prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

symbTest <- c("DD","C","B")
sum(symbTest == "C")

score <- function(symbols){
  #if 3 of a kind
  if(symbols[1] == symbols[2] && symbols[1] == symbols[3]){
    #lookup table of payouts
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    #assign prize
    prize <- unname(payouts[symbols[1]])
  } #else if all symbols are bars
    else if(all(symbols %in% c("B","BB","BBB"))){
    prize <- 5
  } else {
    #else check for cherries
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries +1]
  }
  #diamonds
  diamonds <- sum(symbols == "DD")
  
  prize * (2 ^ diamonds)
  
}


play <-function(){
  # generate 3 random symbols
  roll <- get_symbols()
  #print symbols to console
  print(roll)
  #assign score value based on symbols
  score(roll)
}