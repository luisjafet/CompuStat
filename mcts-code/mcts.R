branch.leaf.relationships <- list()

AI.move <- function(board, player, winval, Nsim=10,bias=1.2,weight=TRUE){
	ucb.bias <<- bias 
	current.state <- stateID(board);
	choices <- identifyNextStates(board=board,player=player)
	next.state <- evaluateNextStates(choices=choices,
										parent=current.state,
										board=board,
										player=player, 
										winval=winval,
										weight=weight)
	column <- stateToColumn(state=next.state,parent=current.state,board=board)
	return(next.state)
	}

stateToColumn <- function(state,parent,board){
	new.board <- toBoard(state=state,some.board=board)
	old.board <- toBoard(state=parent,some.board=board)
	new.board[which(is.na(new.board))] <- -66
	old.board[which(is.na(old.board))] <- -66
	for (i in 1:ncol(new.board)){
		col.equal <- sum(!(new.board[,i]==old.board[,i])) 
		if (col.equal != 0)
			return(i)
		}
	}
	
stateID <- function(board){
	temp.state <- as.vector(board)
	temp.state[which(is.na(temp.state))] <- 2
	state <- convertToDecimal(temp.state)
	return(state)
	}
	
convertToDecimal <- function(base3state){
	base3state <- base3state[length(base3state):1]
	temp <- 0
	for (i in 1:length(base3state)){
		temp <- temp + base3state[i]*(3^(i-1))
		}
	return(temp)
	}
	
findState <- function(state){
	return(!is.null(branch.leaf.relationships[[as.character(state)]]))
	}
	
identifyNextStates <- function(board,player){
	r <- ncol(board)
	valid.next.board <- list()
	for (i in 1:r){
		temp <- addMove(board,column=i,player=player)
		if (temp$valid.input){
			valid.next.board[[i]] <- temp$board
			}
		}
	valid.next.nodes <- c()
	if (length(valid.next.board) > 0){
		for (i in 1:length(valid.next.board)){
			if (!is.null(valid.next.board[[i]])){
				valid.next.nodes[i] <- stateID(valid.next.board[[i]]);
				}
			}
		}
	return(na.omit(valid.next.nodes))
	}


evaluateNextStates <- function( choices, parent, board,
								player,
								winval,
								Nsims=10,
								weight){
	winning.board <- c()
	for (i in 1:length(choices)){
		win <- checkWinner(toBoard(choices[i],board),winval=winval,player=player)
		if (win) winning.board <- choices[i]
		}
	if (!is.null(winning.board)) {
		return(winning.board)
		}
	
	

	block.board <- c()
	opponent <- (player+1)%%2
	opponent.moves <- identifyNextStates(board=board,player=opponent)
	for (i in 1:length(opponent.moves)){
		block <- checkWinner(toBoard(opponent.moves[i],board),winval=winval,player=opponent)
		if (block) {
			block.board <- choices[i] 
			}

		} 
	if (!is.null(block.board)){
		column <- stateToColumn(block.board,parent,board)
		true.block.board <- addMove(board=toBoard(parent,board),column=column,player=player)
		block.state <- stateID(true.block.board$board)
		return(block.state) 
		}
	
	toExplore <- c()
	for (i in 1:length(choices)){
		if (!findState(choices[i])){ 
			toExplore[i] <- choices[i]
			}
		}
	
	if (!is.null(toExplore)){
		toExplore <- na.omit(toExplore)
		for(i in 1:length(toExplore)){
			simulated.game <- list()
			for (jj in 1:Nsims){ 
				simulated.game[[jj+(Nsims*(i-1))]] <- explore.tree(start.state=toExplore[i],player=player,board=board,winval=winval)
				}
			update.blr(simulated.game);
			}
		}
	
	moveProps <- identifyMoveProps(player=player,choices=choices)
	props <- moveProps$props
	ni <- moveProps$visited
	current.state.visits <- branch.leaf.relationships[[as.character(parent)]][["visited"]][1]
	if (is.null(current.state.visits)) current.state.visits <- 1
	ME <- ucb.bias*sqrt(max(log(current.state.visits),1)/ni)
	UCB <- props + ME
	choice.index.chosen <- UCT(props,UCB,weight=weight)
	state.chosen <- choices[choice.index.chosen]
	return(state.chosen)
	}
	
UCT <- function(props,UCB,weight=TRUE){
	n <- length(props)
	indices <- 1:n
	keep.for.sure <- drop.for.sure <- c()
	for (i in 1:n){
		if (sum(!(props[i] > UCB[-i]))==0) 
			keep.for.sure <- i
		if (sum(!(UCB[i] < props[i]))==0)  
			drop.for.sure <- i
		}
	if (!is.null(keep.for.sure)) {
		choice <- keep.for.sure
		}
	else{
		if(!is.null(drop.for.sure)){
			indices <- indices[-drop.for.sure]
			}
		if (length(indices) > 1){
			if (weight) {
				if (sum(props[indices]==0)==length(props[indices])){
					choice <- sample(indices,1)
				}
				else {
					choice <- sample(indices,1,prob=props[indices])
					}
				}
			else {
				choice <- sample(indices,1)
				}
			}
		else {
			choice <- indices
			}
		}
	return(choice)
	}
	
update.blr <- function(sim.big){
	for (jj in 1:length(sim.big)){
		sim <- sim.big[[jj]]
		leaf <- sim$last.state
		vs <- sim$visited.states 
		for (i in vs){
			cur.str <- as.character(i)
			if (is.null(branch.leaf.relationships[[cur.str]])){
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- rep(leaf,2)
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- leaf
		
				}
			else{
				all.leaves <- branch.leaf.relationships[[cur.str]][["leaves"]]
				all.leaves <- unique(c(all.leaves,leaf))
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- all.leaves
				}
			
			if (i == leaf){
				branch.leaf.relationships[[cur.str]][["winner"]] <<- sim$winning.player
				}
			if (i != leaf) {
				if (is.null(branch.leaf.relationships[[cur.str]][["visited"]])){
					branch.leaf.relationships[[cur.str]][["visited"]] <<- 1
					}
				else {
					k <- branch.leaf.relationships[[cur.str]][["visited"]]
					branch.leaf.relationships[[cur.str]][["visited"]] <<- k+1
					}
				r <- branch.leaf.relationships[[cur.str]][["win1"]]
				if (is.null(r)) r <- 0
				if (sim$winning.player==1){
					branch.leaf.relationships[[cur.str]][["win1"]] <<- r + 1
					}
				else {
					branch.leaf.relationships[[cur.str]][["win1"]] <<- r + 0 
					}
				}
			}
		}
	}
	
identifyMoveProps <- function(player,choices){
	player1 <- (player==1)
	props <- visited <- c()
	for (i in 1:length(choices)){
		cur.str <- as.character(choices[i])
		w <- branch.leaf.relationships[[cur.str]][["win1"]]
		v <- branch.leaf.relationships[[cur.str]][["visited"]]
		visited[i] <- v
		if ( player1) { props[i] <- w/v }
		if (!player1) { props[i] <- 1-(w/v) }
		}

	return(list(props=props,visited=visited))
	}


explore.tree <- function(start.state,player,board,winval){
	visited.states <- start.state
	game.over <- FALSE
	full.board <- FALSE
	while(!game.over){
		current.board <- toBoard(visited.states[length(visited.states)],some.board=board)
		game.over.a <- checkWinner(current.board,winval=winval,player=player)
		game.over.b <- checkWinner(current.board,winval=winval,player=(player+1)%%2)
		full.board <- game.over.c <- (sum(is.na(current.board))==0)
		game.over <- (game.over.a || game.over.b || game.over.c)
		valid.next.nodes <- identifyNextStates(board=current.board,player=player)
		if (full.board || game.over || length(valid.next.nodes)==0 || visited.states[length(visited.states)] == 0) {
			game.over <- TRUE;
			winning.state <- visited.states[length(visited.states)];
			if (full.board){
				winning.player <- -99;
				}
			else {
				winning.player <- (player + 1) %% 2
				}
			final.results <- list(winning.player=winning.player,visited.states=visited.states,last.state=winning.state)
			return(final.results)
			}
		else {
			if (length(valid.next.nodes) > 1){
				rand.state <- sample(valid.next.nodes,1)
				}
			else {
				rand.state <- valid.next.nodes
				}
			visited.states[length(visited.states)+1] <- rand.state
			}
		player <- (player + 1)%%2
		}
	}
	





toBoard <- function(state,some.board){
	id <- convertToTrinary(state,some.board)
	id[which(id==2)] <- NA
	board <- matrix(data=id,nrow=nrow(some.board),ncol=ncol(some.board))
	return(board)
	}
	
convertToTrinary <- function(state,board){
	n <- nrow(board)*ncol(board)
	num <- c()
	temp <- state
	for (i in 1:n){
		num[i] <- floor(temp / 3^(n-i))
		temp <- temp %% 3^(n-i)
		}
	return(num)
	}
