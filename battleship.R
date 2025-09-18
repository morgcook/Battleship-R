n = 10

battleship = function() {
  # get player and enemy ships
  user_ships = get_ships(n)
  enem_ships = get_ships(n)
  
  user.mat = get_board_matrix(n, user_ships)
  enem.mat = get_board_matrix(n, enem_ships)
  
  u.ships = user_ships
  e.ships = enem_ships
  
  total.ships = sum(sapply(user_ships, ncol))
  enem.targ = rep(0, 20)
  enem.tlen = 0
  user.targ = rep(0, 20)
  user.tlen = 0
  
  enem.lens = c(5, 4, 3, 3, 2)
  user.lens = c(5, 4, 3, 3, 2)
  
  sunken.tiles = matrix(0, 2, total.ships)
  esunken.tiles = matrix(0, 2, total.ships)
  
  i = 0
  v = 1:5
  ev = 1:5
  
  # draw board
  draw(n, user_ships)
  
  # game loop
  repeat {
    i = i + 1
    user.move = get_move(enem.mat)
    
    enem.mat = make_move(user.move, enem.mat, TRUE)
    
    if (total.ships - sum(enem.mat == 3) == 0) break
    
    # hunt
    enem.targ = get.targets(user.mat, enem.targ, enem.tlen,
                            user.lens, sunken.tiles)
    enem.tlen = sum(enem.targ != 0)
    
    # target phase
    enem.move = enem.targ[1]
    enem.targ = adjust(enem.targ, (enem.tlen = enem.tlen - 1))
    user_ships = update.ships(enem.move, user_ships)
    user.lens = update.lengths(user_ships)
    x = update.sunken(sunken.tiles, user_ships, u.ships, v)
    sunken.tiles = x[[1]]
    v = x[[2]]
    
    Sys.sleep(2)
    user.mat = make_move(enem.move, user.mat)
    
    if (total.ships - sum(user.mat == 3) == 0) break
  }
  print("Game over!")
  return(user.mat)
}

get.targets <- function(m, targets, len, lens, sunken) {
  if (len == 0) {
    pmat = gen.prob.mat(m, lens, sunken)
    move = sample(rep(which(pmat == max(pmat)), 2), 1)
    targets[(len=len+1)] = move
  } else
    move = targets[1]
  
  if (m[move] == 1) {
    potential_targets = c(move+1, move-1, move+n, move-n)
    for (targ in potential_targets)
      if (targ <= n*n && targ >= 1)
        if (abs(move - targ) != 1 || (targ - 1) %/% n + 1 == (move - 1) %/% n + 1)
          if (m[targ] < 2 && !contains(targets, targ))
            targets[(len=len+1)] = targ
  }
  
  return(targets)
}

# determines whether x is in v
contains <- function(v, x) {
  for (i in v) 
    if (i == x)
      return(TRUE)
  return(FALSE)
}

# generates probability matrix for where a ship is likely to be
gen.prob.mat <- function(mat, lens, sunken) {
  prob_mat = matrix(0, n, n)
  for (ship_size in lens) {
    use_size = ship_size - 1
    for (row in 1:n)
      for (col in 1:n) {
        if (mat[row, col] < 2) {
          endpoints = matrix(0, 4, 4)
          len = 0
          if (row - use_size >= 1)
            endpoints[,(len = len + 1)] = c(row - use_size, col, row, col)
          if (row + use_size <= n)
            endpoints[,(len = len + 1)] = c(row, col, row + use_size, col)
          if (col - use_size >= 1)
            endpoints[,(len = len + 1)] = c(row, col - use_size, row, col)
          if (col + use_size <= n)
            endpoints[,(len = len + 1)] = c(row, col, row, col + use_size)
          
          if (len > 0)
            for (i in 1:len) {
              rows = endpoints[c(1,3),i]
              cols = endpoints[c(2,4),i]
              if (all(mat[rows[1]:rows[2], cols[1]:cols[2]] < 2))
                prob_mat[rows[1]:rows[2], cols[1]:cols[2]] = 
                  prob_mat[rows[1]:rows[2], cols[1]:cols[2]] + 1
            }
        }
        
        if (mat[row, col] == 3 && !mcontains(sunken, c(row, col))){
          if (row + 1 <= n && mat[row+1, col] < 2) {
            if (row - 1 >= 1 && !mcontains(sunken, c(row-1, col)) &&
                mat[row-1, col] == 3) {
              prob_mat[row+1, col] = prob_mat[row+1, col] + 15
            } else {
              prob_mat[row+1, col] = prob_mat[row+1, col] + 10
            }
          }
          if (row - 1 >= 1 && mat[row-1, col] < 2) {
            if (row + 1 <= n && !mcontains(sunken, c(row+1, col)) &&
                mat[row+1, col] == 3) {
              prob_mat[row-1, col] = prob_mat[row-1, col] + 15
            } else {
              prob_mat[row-1, col] = prob_mat[row-1, col] + 10
            }
          }
          if (col + 1 <= n && mat[row, col + 1] < 2) {
            if (col - 1 >= 1 && !mcontains(sunken, c(row, col-1)) &&
                mat[row, col-1] == 3) {
              prob_mat[row, col+1] = prob_mat[row, col+1] + 15
            } else {
              prob_mat[row, col+1] = prob_mat[row, col+1] + 10
            }
          }
          if (col - 1 >= 1 && mat[row, col-1] < 2) {
            if (col + 1 <= n && !mcontains(sunken, c(row, col+1)) &&
                mat[row, col+1] == 3) {
              prob_mat[row, col-1] = prob_mat[row, col-1] + 15
            } else {
              prob_mat[row, col-1] = prob_mat[row, col-1] + 10
            }
          }
        } else if (mat[row,col] == 2)
          prob_mat[row,col] = 0
      }
    }
  return(prob_mat)
}

# adjust targets to be length tlen
adjust <- function(targets, tlen) {
  for (i in 1:length(targets)){
    if (i <= tlen)
      targets[i] = targets[i+1]
    else
      targets[i] = 0
  }
  return(targets)
}

mcontains <- function(m, v) {
  for (col in ncol(m)) 
    if (all(m[,col] == v))
      return(TRUE)
  return(FALSE)
}

# update lengths of each ship
update.lengths <- function(ships) {
  lens = c(5, 4, 3, 3, 2)
  for (i in 1:length(ships))
    if (ncol(ships[[i]]) == 0)
      lens[i] = -1
  return(lens[lens != -1])
}

# update list of sunken ships
update.sunken <- function(sunken, cur_ships, ships, v) {
  slen = sum(apply(sunken, 2, sum) != 0)
  for (i in v) {
    if (ncol(cur_ships[[i]]) == 0) {
      v = v[v != i]
      for (j in 1:ncol(ships[[i]]))
        sunken[,(slen = slen + 1)] = ships[[i]][,j]+1
    }
  }
  return(list(sunken, v))
}

# update ships array
update.ships <- function(move, ships) {
  x = (move - 1) %% n
  y = (move - 1) %/% n
  for (i in 1:length(ships))
    if (ncol(ships[[i]]) > 0)
      for (j in 1:ncol(ships[[i]]))
        if (all(ships[[i]][,j] == c(x, y))) {
          ships[[i]] = ships[[i]][,-j, drop = FALSE]
          return(ships)
        }
  return(ships)
}

# visualizes the result of the previous move and updates matrix
make_move <- function(move, m, is.user.turn = FALSE) {
  x = (move - 1) %% n + 1
  y = (move - 1) %/% n + 1
  if (m[move] == 0) {
    # miss
    points(x-1+0.5, y-1+0.5+(n*is.user.turn), pch=4, lwd = 4)
    m[move] = 2
  } else {
    # hit
    points(x-1+0.5, y-1+0.5+(n*is.user.turn), pch = 19, col="red", lwd = 4)
    m[move] = 3
  }
  return(m)
}

# collects user input
get_move <- function(mat) {
  repeat {
    move = readline("Enter a valid move (e.g. A1): ")
    if (is_valid(move_to_vector(toupper(move)), mat) == TRUE)
      break
  }
  return(move_to_vector(toupper(move)))
}

# converts user input into vector
move_to_vector <- function(move) {
  # if move is not valid input
  if (nchar(move) != 2 && nchar(move) != 3 
      || is.na(as.numeric(substring(move, 2)))
      || as.numeric(substring(move, 2)) < 1
      || as.numeric(substring(move, 2)) > n
      || substring(move, 1, 1) < "A" 
      || substring(move, 1, 1) > "J") 
    return(-1)
  x = as.numeric(substring(move, 2))
  y = n+1 - which(LETTERS[1:n] == substring(move, 1, 1))
  return((y-1) * n + (x-1) %% n + 1)
}

# determines whether user input is valid
is_valid <- function(move, mat) {
  # check if input is valid
  if (move == -1) return(FALSE)
  # check if move has already been done
  if (mat[move] >= 2) return(FALSE)
  return(TRUE)
}

# draws the initial board
draw = function(n, ship.list) {
  par(mar = rep(0, 4), xaxs="i", yaxs="i")
  plot(0, 0, xlim = c(-1, n+1), 
       ylim = c(-1, n*2+1), axes = FALSE)
  rect(0, 0.05, n-0.01, n*2, col = "lightgray", lw=3)
  lines(c(-1, n+1), c(n, n), lw = 4)
  
  for (y in 1:n) {
    lines(c(y, y), c(0, n*2))
    text(y-0.5, -0.5, y, font=2)
    text(y-0.5, 2*n+0.5, y, font=2)
    text(-0.5, 2*n+0.55-y, LETTERS[y], font=2)
    text(n+0.5, n+0.5-y, LETTERS[y], font=2)
  }
  for (x in 1:(n*2)) lines(c(0, n), c(x, x))
  lrmar = 0.2
  tbmar = 0.2
  #print(ship.list)
  for (ship in ship.list) {
    coord1 = ship[,1]
    coord2 = ship[,ncol(ship)]
    rect(coord1[1]+lrmar, coord2[2]+1-tbmar, 
         coord2[1]+1-lrmar, coord1[2]+tbmar, col = "darkgray")

}

# creates a list of ships
get_ships <- function(n) {
  ship.list = vector(mode = "list", 5)
  lens = c(5, 4, 3, 3, 2)
  ship.list[[1]] = get_coords(n, lens[1])
  for (i in 2:5) {
    repeat {
      coords = get_coords(n, lens[i])
      good = TRUE
      for (ship in 1:(i-1)) {
        prev = ship.list[[ship]]
        for (j in 1:ncol(prev)) {
          for (k in 1:ncol(coords)) {
            if (all(prev[,j] == coords[,k])) {
              good = FALSE
              break
            }
          }
        }
      }
      if (good == TRUE) break
    }
    ship.list[[i]] = coords
  }
  return(ship.list)
}

# returns board as a matrix object
get_board_matrix <- function(n, ship.list) {
  # 0 for empty, 1 for ship, 2 for miss/hit
  m = matrix(0, n, n)
  for (ship in ship.list)
    for (i in 1:ncol(ship)) {
      m[ship[,i][1]+1, ship[,i][2]+1] = 1
    }
  return(m)
}

# returns the coordinates that the randomly placed ship inhabits
get_coords <- function(n, len) {
  dir = sample(0:1, 1)
  x = sample(0:(n - 1 - dir * len), 1)
  y = sample(0:((n - 1 - len) + dir * len), 1)
  if (dir == 1)
    coords = matrix(c(x:(x+len-1), rep(y, len)), 2, len, byrow=TRUE)
  else
    coords = matrix(c(rep(x, len), y:(y+len-1)), 2, len, byrow=TRUE)
  return(coords)
}

m = battleship()