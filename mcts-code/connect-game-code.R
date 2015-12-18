# Game_mode function
# Aquí se puede indicar el tipo de juego con el número de filas, columnas, condiciones para ganar
# si se jugara con la computadora o humano, etc...
game_mode <- function(nrow=4,ncol=4,winval=3,AI=FALSE,bias=1.2,Nsim=10,weight=TRUE){
  if (!AI){
		start.no.AI.game(nrow=nrow,ncol=ncol,winval=winval)
		}else{
		start.AI.game(nrow=nrow,ncol=ncol,winval=winval,AI.player=1,Nsim,bias=bias,weight=weight)
		}
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# Start.AI.game function
# inicia un juego humano VS computadora, termina cuando alguien gana
# o se terminan los espacios disponibles
start.AI.game <- function(nrow=4,ncol=4,winval=3,AI.player=1,Nsim,bias,weight){
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	human.player <- (AI.player+1)%%2
	player <- 0 
	move <- 1
	visited.states <- stateID(board)

	while(!game.over && move < nrow*ncol){
		if (player == human.player){
			board <- move(board, player)
			print.board(board)
			}
		else if (player == AI.player) {
			AI.state <- AI.move( board, player, winval, Nsim,
								bias=bias,
								weight=weight )
			board <- toBoard(state=AI.state,some.board=board)

			}
		move <- move + 1
		visited.states[move] <- stateID(board)
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}
		
	winner <- (player+1)%%2
	last.state <- stateID(board)
	
	game.summary <- list()
	game.summary[[1]] <- list(winning.player=winner,visited.states=visited.states,last.state=last.state)
	
	update.blr(game.summary)

	print("Final Board")
	print.board(board)
	print(paste("***** Game Over ****** Winner was Player",(player+1)%%2))
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# start.no.AI.game function
# inicia un juego humano VS humano, termina cuando alguien gana
# o se terminan los espacios disponibles
start.no.AI.game <- function(nrow=4,ncol=4,winval=3){
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	player <- 0
	move <- 1
	while(!game.over && move < nrow*ncol){
		board <- move(board, player)
		move <- move + 1
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}

	print("Final Board")
	print.board(board)
	print(paste("***** Game Over ****** Winner was Player",(player+1)%%2))
}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# move function
# Pide al jugador ingrese el número de columna donde se generará el movimiento, si no es valido
# entonces no se ejecuta la jugada y se vuelve a pedir la columna al jugador
move <- function(board,player){
	valid.input <- FALSE
	while(!valid.input){
		print.board(board);
		print(paste("Player ",player,", select a column: ",sep=""))
		column <- as.numeric(readLines(n=1))
		result <- addMove(board,column,player)
		board <- result$board
		valid.input <- result$valid.input
		}
	return(board);
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# addmove function
# intenta aplicar la jugada ssi es valida
addMove <- function(board,column,player){
    row <- which(is.na(board[,column]))[1]
		valid.input <- !is.na(row)
		if (valid.input)
			board[row,column] <- player
		return(list(board=board,valid.input=valid.input))
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# createBoard function
# crea el tablero con una matriz llena de NAs con las columnas y filas especificadas
createBoard <- function(rows, cols){
	return(matrix(data=NA,nrow=rows,ncol=cols))
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# checkWinner function
# checa si hay un ganador en las filas, columnas y diagonales, aplicando la función matcher
checkWinner <- function(board, winval, player){
	winner <- rep(player, winval); 
	w <- length(winner);
	win <- FALSE;
	
	rows <- nrow(board);
	for (i in 1:rows){
		if(matcher(winner, board[i,])){
			win <- TRUE;
			return(win);
			}
		}

	cols <- ncol(board);
	for (i in 1:cols){
		if(matcher(winner,board[,i])){
			win <- TRUE;
			return(win);
			}
		}
		
	win <- checkDiags(board, rows, cols, winner);
	if (win) return(win);
	win <- checkDiags(board[,cols:1], rows, cols, winner);
	if (win) return(win);

	return(win);
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# checkDiags function
# checa las diagonales del tablero con la función matcher
checkDiags <- function(board, r, c, winner){
	win <- FALSE;
	w <- length(winner)
	
	current.vec <- c()
	element <- 1
	for (i in nrow(board):1){
		cur.r <- i
		cur.c <- 1;
		while( (cur.r <= r && cur.c <= c) ){
			current.vec[element] <- board[cur.r,cur.c]
			element <- element + 1
			cur.r <- cur.r + 1
			cur.c <- cur.c + 1
			}
		if (length(current.vec) >= w){
			if (matcher(winner,current.vec)){
				win <- TRUE;
				return(win);
				}
			}
		current.vec <- c()
		element <- 1
		}
		
	current.vec <- c()
	element <- 1
	for (i in 1:ncol(board)){
		cur.c <- i
		cur.r <- 1;
		while( (cur.r <= r && cur.c <= c) ){
			current.vec[element] <- board[cur.r,cur.c]
			element <- element + 1
			cur.r <- cur.r + 1
			cur.c <- cur.c + 1
			}
		if (length(current.vec) >= w){
			if (matcher(winner,current.vec)){
				win <- TRUE;
				return(win);
				}
			}
		current.vec <- c()
		element <- 1
		}
		
	return(win);
	}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# matcher function
# checa si hay una combinación ganadora en vec (000) o (111) por ejemplo, también checa que
# no tenga NAs
matcher <- function(winner, vec){
	w <- length(winner); 
	n <- length(vec);
	win <- FALSE;
	for (ii in 1:(n-w+1)){
		if (length(na.omit(vec[ii:(ii+w-1)])) >= w) {
			if (sum(winner == na.omit(vec[ii:(ii+w-1)])) == w){
				win <- TRUE;
				return(win); 
				}
			}
		}
	return(win);
}
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
# print.board function
# Imprime correctamente el tablero (al revez) 
print.board <-function(board){
  board2 <- apply(board, 2, rev)
  #print(board2)
  return (board2)
}


