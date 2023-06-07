#each player would have a vector of relevant stats.
#these are all doubles, representing percentages of their probability to do x

#------------------PLAYER VECTOR VALUES---------------------

# FG%, FG3%, PPG, APG, RPG , STL, BLK, TOV, 2PA, 3PA
kd <- c(.499, .385, 27.3, 4.3, 7.1, 1.1, 1.1, 3.2, 13.8, 4.9)
steph <- c(.475, .428, 24.6, 6.5, 4.7, 1.6, 0.2, 3.1, 8.8, 9)
mike <- c(.497, .327, 30.1, 5.3, 6.2, 2.3, 0.8, 2.7, 21.2, 1.7)
scottie <- c(.473, .326, 16.1, 5.2, 6.4, 2, 0.8, 2.8, 10.8, 2.5)
hakeem <- c(.512, .202, 21.8, 2.5, 11.1, 1.7, 3.1, 3.0, 16.9, 0.1)

jaylen23 <- c(.491, .335, 26.6, 3.5, 6.9, 1.1, 0.4, 2.9, 13.4, 7.3)
tatum23 <- c(.466, .350, 30.1, 4.6, 8.8, 1.1, 0.7, 2.9, 11.8, 9.3)
bird <- c(.496, .376, 24.3, 6.3, 10, 1.7, 0.8, 3.1, 17.4, 1.9)
mchale <- c(.554, .261, 17.9, 1.7, 7.3, 0.4, 1.7, 1.9, 12.5, 0.2)
pierce08 <- c(.464, .392, 19.6, 4.5, 5.1, 1.3, 0.5, 2.8, 9.2, 4.6)

#now homogenizing the names
p1 <- kd
p2 <- steph
p3 <- mike
p4 <- scottie
p5 <- hakeem
p6 <- jaylen23
p7 <- tatum23
p8 <- bird
p9 <- mchale
p10 <- pierce08

#to hold total game stats:
# PTS, AST, REB , STL, BLK, TOV, 2PM, 2PA, 3PM, 3PA
totStatsV <- rep(0, 100)


#-------------------PUT ALL STATS IN GROUP VECTORS--------------------

# put all stats in a vector
otherV <- c(p1, p2, p3, p4, p5)
celticV <- c(p6, p7, p8, p9, p10)
allStatsV <- c(otherV, celticV)

otherShots <- otherV[c(3, 4, 13, 14, 23, 24, 33, 34, 43, 44)]
otherShotsTot <- sum(otherShots[1:10])

celticShots <- celticV[c(3, 4, 13, 14, 23, 24, 33, 34, 43, 44)]
celticShotsTot <- sum(celticShots[1:10])

p1Pct <- (p1[3] + p1[4]) / otherShotsTot
p2Pct <- (p2[3] + p2[4]) / otherShotsTot
p3Pct <- (p3[3] + p3[4]) / otherShotsTot
p4Pct <- (p4[3] + p4[4]) / otherShotsTot
p5Pct <- (p5[3] + p5[4]) / otherShotsTot

p6Pct <- (p6[3] + p6[4]) / celticShotsTot
p7Pct <- (p7[3] + p7[4]) / celticShotsTot
p8Pct <- (p8[3] + p8[4]) / celticShotsTot
p9Pct <- (p9[3] + p9[4]) / celticShotsTot
p10Pct <- (p10[3] + p10[4]) / celticShotsTot

allPlayerPct <-
  c(p1Pct,
    p2Pct,
    p3Pct,
    p4Pct,
    p5Pct,
    p6Pct,
    p7Pct,
    p8Pct,
    p9Pct,
    p10Pct)

#STL vector add
otherSTL <-
  otherV[6] + otherV[16] + otherV[26] + otherV[36] + otherV[46]
celticSTL <-
  celticV[6] + celticV[16] + celticV[26] + celticV[36] + celticV[46]
teamStlV <- c(otherSTL, celticSTL)

#RPG vector add
otherRPG <-
  otherV[5] + otherV[15] + otherV[25] + otherV[35] + otherV[45]
celticRPG <-
  otherV[5] + otherV[15] + otherV[25] + otherV[35] + otherV[45]
otherDefRbdPct <- 73 + otherRPG - celticRPG
otherOffRbdPct <- 27 + otherRPG - celticRPG
celticDefRbdPct <- 73 + celticRPG - otherRPG
celticOffRbdPct <- 27 + celticRPG - otherRPG
rbdPctV <-
  c(otherDefRbdPct,
    otherOffRbdPct,
    celticDefRbdPct,
    celticOffRbdPct)

#individual player RPG PCT
celtic1 <- celticV[5] / celticRPG
celtic2 <- celticV[15] / celticRPG
celtic3 <- celticV[25] / celticRPG
celtic4 <- celticV[35] / celticRPG
celtic5 <- celticV[45] / celticRPG
celticRbdV <- c(celtic1, celtic2, celtic3, celtic4, celtic5)
other1 <- otherV[5] / otherRPG
other2 <- otherV[15] / otherRPG
other3 <- otherV[25] / otherRPG
other4 <- otherV[35] / otherRPG
other5 <- otherV[45] / otherRPG
otherRbdV <- c(other1, other2, other3, other4, other5)
allRbdV <- c(otherRbdV, celticRbdV)

#individual player STL PCT
celtic1 <- celticV[6] / celticSTL
celtic2 <- celticV[16] / celticSTL
celtic3 <- celticV[26] / celticSTL
celtic4 <- celticV[36] / celticSTL
celtic5 <- celticV[46] / celticSTL
celticStlV <- c(celtic1, celtic2, celtic3, celtic4, celtic5)
other1 <- otherV[6] / otherSTL
other2 <- otherV[16] / otherSTL
other3 <- otherV[26] / otherSTL
other4 <- otherV[36] / otherSTL
other5 <- otherV[46] / otherSTL
otherStlV <- c(other1, other2, other3, other4, other5)
plyrStlV <- c(otherStlV, celticStlV)

#Blk vector add
otherBlk <-
  otherV[7] + otherV[17] + otherV[27] + otherV[37] + otherV[47]
celticBlk <-
  celticV[7] + celticV[17] + celticV[27] + celticV[37] + celticV[47]
teamBlkV <- c(otherBlk, celticBlk)

#individual player Blk PCT
celtic1 <- celticV[7] / celticBlk
celtic2 <- celticV[17] / celticBlk
celtic3 <- celticV[27] / celticBlk
celtic4 <- celticV[37] / celticBlk
celtic5 <- celticV[47] / celticBlk
celticBlkV <- c(celtic1, celtic2, celtic3, celtic4, celtic5)
other1 <- otherV[7] / otherBlk
other2 <- otherV[17] / otherBlk
other3 <- otherV[27] / otherBlk
other4 <- otherV[37] / otherBlk
other5 <- otherV[47] / otherBlk
otherBlkV <- c(other1, other2, other3, other4, other5)
plyrBlkV <- c(otherBlkV, celticBlkV)

#set player names in vector
otherNames <- c("KD", "Steph", "Mike", "Scottie", "Hakeem")
celticNames <- c("Jaylen", "Tatum", "Bird", "McHale", "Pierce")
allNames <- c(otherNames, celticNames)

#----------------------------------FUNCTIONS----------------------

#rebound function
rebFunction <- function() {
  #set vector positions based on who shot
  if (teamOneBall) {
    rPct <- 2
    n <- 0
  } else{
    rPct <- 4
    n <- 5
  }
  ranChk <- sample(1:100, 1, replace = TRUE)
  if (ranChk > rbdPctV[rPct]) {
    if (rPct == 4) {
      n <- 0
    } else{
      n <- 5
    }
  }
  #determine who
  ranChk <- sample(1:100, 1, replace = TRUE) / 100
  if (ranChk < allRbdV[1 + n]) {
    holdBall <- 1 + n
  } else if (ranChk < allRbdV[1 + n] + allRbdV[2 + n]) {
    holdBall <- 2 + n
  } else if (ranChk < allRbdV[1 + n] + allRbdV[2 + n] + allRbdV[3 + n]) {
    holdBall <- 3 + n
  } else if (ranChk < (1 - allRbdV[5 + n])) {
    holdBall <- 4 + n
  } else{
    holdBall <- 5 + n
  }
  
  return(holdBall)
}

blkFunction <- function() {
  #set vector positions based on who shot
  if (teamOneBall) {
    blkChk <- 2
    n <- 5
  } else{
    blkChk <- 1
    n <- 0
  }
  blkBall <- 0
  ranChk <- sample(1:100, 1, replace = TRUE)
  if (ranChk < teamBlkV[blkChk]) {
    #determine who
    ranChk <- sample(1:100, 1, replace = TRUE) / 100
    if (ranChk < plyrBlkV[1 + n]) {
      blkBall <- 1 + n
    } else if (ranChk < plyrBlkV[1 + n] + plyrBlkV[2 + n]) {
      blkBall <- 2 + n
    } else if (ranChk < plyrBlkV[1 + n] + plyrBlkV[2 + n] + plyrBlkV[3 + n]) {
      blkBall <- 3 + n
    } else if (ranChk < (1 - plyrBlkV[5 + n])) {
      blkBall <- 4 + n
    } else{
      blkBall <- 5 + n
    }
  }
  
  return(blkBall)
}

getVecPos <- function(holdBall, statNum) {
  vPos <- 10 * (holdBall) - 10 + statNum
  return(vPos)
}


#-------------------SET INITAL CONDITIONS FOR GAME START-----------------------

playCount <- 0
otherPointTotal <- 0
celticPointTotal <- 0
teamOneBall <- TRUE
noAssist <- TRUE
pendAssist <- ""
holdBall <- 0


#------------------------------GAME LOOP---------------------------------------

while (playCount < 225) {
  #SET team variables
  if (teamOneBall) {
    n <- 0
    d <- 5
    dChk <- 2
  } else{
    n <- 5
    d <- 0
    dChk <- 1
  }
  
  playDone <- FALSE
  
  if (holdBall == 0) {
    ranChk <- sample(1:100, 1, replace = TRUE)
    ballGoes <- ranChk / 100
    #who has the ball?
    # addForPlayer creates a base to find vector values from
    if ((ballGoes < allPlayerPct[1 + n]) & pendAssist != (1 + n)) {
      addForPlayer <- 0
      print(allNames[1 + n])
      holdBall <- 1 + n
    } else if ((ballGoes < (allPlayerPct[1 + n] + allPlayerPct[2 + n])) &
               pendAssist != (2 + n)) {
      addForPlayer <- 10
      holdBall <- 2 + n
      print(paste(allNames[holdBall], "has the ball"))
    } else if ((ballGoes < (allPlayerPct[1 + n] + allPlayerPct[2 + n] + allPlayerPct[3 + n])) &
               pendAssist != (3 + n)) {
      addForPlayer <- 20
      holdBall <- 3 + n
      print(paste(allNames[holdBall], "has the ball"))
    } else if ((ballGoes < (
      allPlayerPct[1 + n] + allPlayerPct[2 + n] + allPlayerPct[3 + n] + allPlayerPct[4 + n]
    )) & pendAssist != (4 + n)) {
      addForPlayer <- 30
      holdBall <- 4 + n
      print(paste(allNames[holdBall], "has the ball"))
    } else {
      if (pendAssist != 5 + n) {
        addForPlayer <- 40
        holdBall <- 5 + n
        print(paste(allNames[holdBall], "has the ball"))
      } else{
        playDone <- TRUE
      }
    }
    if (n == 5 & !playDone) {
      addForPlayer <- addForPlayer + 50
    }
  } else{
    addForPlayer <- (holdBall) * 10 - 10
    print(paste(allNames[holdBall], "has the ball"))
  }
  
  #first, test steal
  ranChk <- sample(1:100, 1, replace = TRUE)
  if (ranChk < teamStlV[dChk]) {
    #so, who steals?
    ranChk <- sample(1:100, 1, replace = TRUE) / 100
    if (ranChk < plyrStlV[1 + d]) {
      print(paste(allNames[1 + d], "steals the ball!"))
      holdBall <- d + 1
    } else if (ranChk < plyrStlV[1 + d] + plyrStlV[2 + d]) {
      print(paste(allNames[2 + d], "steals the ball!"))
      holdBall <- d + 2
    } else if (ranChk < plyrStlV[1 + d] + plyrStlV[2 + d] + plyrStlV[3 + d]) {
      print(paste(allNames[3 + d], "steals the ball!"))
      holdBall <- d + 3
    } else if (ranChk < (1 - plyrStlV[5 + d])) {
      print(paste(allNames[4 + d], "steals the ball!"))
      holdBall <- d + 4
    } else{
      print(paste(allNames[5 + d], "steals the ball!"))
      holdBall <- d + 5
    }
    vecPos <- getVecPos(holdBall, 4)
    totStatsV[vecPos] <- totStatsV[vecPos] + 1
    print(paste(allNames[holdBall], "steals the ball!", totStatsV[vecPos], "total"))
    playDone <- TRUE
    if (teamOneBall) {
      teamOneBall <- FALSE
    } else{
      teamOneBall <- TRUE
    }
    playCount <- playCount + 1
  }
  #then test turnover
  ranChk <- sample(1:100, 1, replace = TRUE)
  i <- 8 + addForPlayer
  if ((ranChk < allStatsV[i]) & !playDone) {
    vecPos <- getVecPos(holdBall, 6)
    totStatsV[vecPos] <- totStatsV[vecPos] + 1
    print(paste(allNames[holdBall], "turns it over!!", totStatsV[vecPos], "total"))
    playDone <- TRUE
    if (teamOneBall) {
      teamOneBall <- FALSE
    } else{
      teamOneBall <- TRUE
    }
    playCount <- playCount + 1
  }
  
  #first set some variables
  #APG of player:
  i <- 4 + addForPlayer
  #2PA of player
  j <- 9 + addForPlayer
  #3PA of player
  k <- 10 + addForPlayer
  #FG% of player
  l <- 1 + addForPlayer
  #FG3%
  m <- 2 + addForPlayer
  #shotType
  player2PA <- allStatsV[j] / (allStatsV[j] + allStatsV[k])
  ranChk <- sample(1:100, 1, replace = TRUE) / 100
  if (ranChk > player2PA) {
    shootTwo <- FALSE
  } else{
    shootTwo <- TRUE
  }
  
  #test pass or shoot if noAssist = true
  if (noAssist & !playDone) {
    #double the likelihood of pass
    astChance <- allStatsV[i] * 5
    ranChk <- sample(1:100, 1, replace = TRUE)
    if (ranChk < astChance) {
      print(paste(allNames[holdBall], "passes the ball"))
      noAssist <- FALSE
      pendAssist <- holdBall
      holdBall <- 0
      playDone <- TRUE
    }
    #this will be false if he fell in the assist bucket on this move
    if (noAssist) {
      #first check block
      blkBall <- blkFunction()
      if (blkBall != 0) {
        vecPos <- getVecPos(holdBall, 5)
        totStatsV[vecPos] <- totStatsV[vecPos] + 1
        playDone <- TRUE
        if (teamOneBall) {
          teamOneBall <- FALSE
        } else{
          teamOneBall <- TRUE
        }
        playCount <- playCount + 1
        noAssist <- TRUE
      }
      if (!playDone) {
        #check SHOT
        if (shootTwo) {
          print(paste(allNames[holdBall], "shoots a 2..."))
          noAssist <- TRUE
          vecPos <- getVecPos(holdBall, 8)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          if ((ranChk / 100) < allStatsV[l]) {
            print(paste(allNames[holdBall], "Scored 2!"))
            vecPos <- getVecPos(holdBall, 7)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            vecPos <- getVecPos(holdBall, 1)
            totStatsV[vecPos] <- totStatsV[vecPos] + 2
            holdBall <- 0
            playDone <- TRUE
            if (teamOneBall) {
              otherPointTotal <- otherPointTotal + 2
              teamOneBall <- FALSE
            } else{
              celticPointTotal <- celticPointTotal + 2
              teamOneBall <- TRUE
            }
            playCount <- playCount + 1
            if (pendAssist > 0) {
              print(paste(allNames[pendAssist], "gets the assist."))
              vecPos <- getVecPos(pendAssist, 2)
              totStatsV[vecPos] <- totStatsV[vecPos] + 1
              pendAssist <- 0
            }
          } else{
            print(paste(allNames[holdBall], "missed the 2!"))
            #check who gets rebound
            holdBall <- rebFunction()
            vecPos <- getVecPos(holdBall, 3)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            print(paste(allNames[holdBall], "gets the rebound!", totStatsV[vecPos], "total"))
            playCount <- playCount + 1
            if (holdBall > 5) {
              teamOneBall <- FALSE
            } else{
              teamOneBall <- TRUE
            }
            playDone <- TRUE
            pendAssist <- 0
          }
        } else{
          i <- 2 + addForPlayer
          print(paste(allNames[holdBall], "shoots a 3..."))
          noAssist <- TRUE
          vecPos <- getVecPos(holdBall, 10)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          if ((ranChk / 100) < allStatsV[m]) {
            print(paste(allNames[holdBall], "Scored 3!"))
            vecPos <- getVecPos(holdBall, 9)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            vecPos <- getVecPos(holdBall, 1)
            totStatsV[vecPos] <- totStatsV[vecPos] + 3
            playDone <- TRUE
            if (teamOneBall) {
              otherPointTotal <- otherPointTotal + 3
              teamOneBall <- FALSE
            } else{
              celticPointTotal <- celticPointTotal + 3
              teamOneBall <- TRUE
            }
            playCount <- playCount + 1
            holdBall <- 0
            if (pendAssist > 0) {
              print(paste(allNames[pendAssist], "gets the assist."))
              vecPos <- getVecPos(pendAssist, 2)
              totStatsV[vecPos] <- totStatsV[vecPos] + 1
              pendAssist <- 0
            }
          } else{
            print(paste(allNames[holdBall], "missed the 3!"))
            #check who gets rebound
            holdBall <- rebFunction()
            vecPos <- getVecPos(holdBall, 3)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            print(paste(allNames[holdBall], "gets the rebound!", totStatsV[vecPos], "total"))
            playCount <- playCount + 1
            if (holdBall > 5) {
              teamOneBall <- FALSE
            } else{
              teamOneBall <- TRUE
            }
            playDone <- TRUE
            pendAssist <- 0
          }
        }
      }
    }
  }
  if (!playDone & !noAssist) {
    #first check block
    blkBall <- blkFunction()
    if (blkBall != 0) {
      vecPos <- getVecPos(holdBall, 5)
      totStatsV[vecPos] <- totStatsV[vecPos] + 1
      playDone <- TRUE
      if (teamOneBall) {
        teamOneBall <- FALSE
      } else{
        teamOneBall <- TRUE
      }
      playCount <- playCount + 1
      noAssist <- TRUE
    }
    if (!playDone) {
      #then check SHOT
      if (shootTwo) {
        print(paste(allNames[holdBall], "shoots a 2..."))
        noAssist <- TRUE
        i <- 1 + addForPlayer
        vecPos <- getVecPos(holdBall, 8)
        totStatsV[vecPos] <- totStatsV[vecPos] + 1
        if ((ranChk / 100) < allStatsV[l]) {
          print(paste(allNames[holdBall], "Scored 2!"))
          vecPos <- getVecPos(holdBall, 7)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          vecPos <- getVecPos(holdBall, 1)
          totStatsV[vecPos] <- totStatsV[vecPos] + 2
          playDone <- TRUE
          if (teamOneBall) {
            otherPointTotal <- otherPointTotal + 2
            teamOneBall <- FALSE
          } else{
            celticPointTotal <- celticPointTotal + 2
            teamOneBall <- TRUE
          }
          if (pendAssist > 0) {
            print(paste(allNames[pendAssist], "gets the assist."))
            vecPos <- getVecPos(pendAssist, 2)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            pendAssist <- 0
          }
          playCount <- playCount + 1
          holdBall <- 0
        } else{
          print(paste(allNames[holdBall], "missed the 2!"))
          #check who gets rebound
          holdBall <- rebFunction()
          vecPos <- getVecPos(holdBall, 3)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          print(paste(allNames[holdBall], "gets the rebound!", totStatsV[vecPos], "total"))
          playCount <- playCount + 1
          if (holdBall > 5) {
            teamOneBall <- FALSE
          } else{
            teamOneBall <- TRUE
          }
          playDone <- TRUE
          pendAssist <- 0
        }
      } else{
        print(paste(allNames[holdBall], "Shoots a 3..."))
        noAssist <- TRUE
        vecPos <- getVecPos(holdBall, 10)
        totStatsV[vecPos] <- totStatsV[vecPos] + 1
        i <- 2 + addForPlayer
        if ((ranChk / 100) < allStatsV[m]) {
          print(paste(allNames[holdBall], "Scored 3!"))
          vecPos <- getVecPos(holdBall, 9)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          vecPos <- getVecPos(holdBall, 1)
          totStatsV[vecPos] <- totStatsV[vecPos] + 3
          playDone <- TRUE
          if (teamOneBall) {
            otherPointTotal <- otherPointTotal + 3
            teamOneBall <- FALSE
          } else{
            celticPointTotal <- celticPointTotal + 3
            teamOneBall <- TRUE
          }
          playCount <- playCount + 1
          holdBall <- 0
          if (pendAssist > 0) {
            print(paste(allNames[pendAssist], "gets the assist."))
            vecPos <- getVecPos(pendAssist, 2)
            totStatsV[vecPos] <- totStatsV[vecPos] + 1
            pendAssist <- 0
          }
        } else{
          #rebound
          print(paste(allNames[holdBall], "missed the 3!"))
          #check who gets rebound
          holdBall <- rebFunction()
          vecPos <- getVecPos(holdBall, 3)
          totStatsV[vecPos] <- totStatsV[vecPos] + 1
          print(paste(allNames[holdBall], "gets the rebound!", totStatsV[vecPos], "total"))
          playCount <- playCount + 1
          if (holdBall > 5) {
            teamOneBall <- FALSE
          } else{
            teamOneBall <- TRUE
          }
          playDone <- TRUE
          pendAssist <- 0
        }
      }
    }
  }
  
  p1stats <- totStatsV[1:10]
  p2stats <- totStatsV[11:20]
  p3stats <- totStatsV[21:30]
  p4stats <- totStatsV[31:40]
  p5stats <- totStatsV[41:50]
  p6stats <- totStatsV[51:60]
  p7stats <- totStatsV[61:70]
  p8stats <- totStatsV[71:80]
  p9stats <- totStatsV[81:90]
  p10stats <- totStatsV[91:100]
}


if (playCount == 225) {
  print("Game Over :)")
}




# the avg Def/Off Rebound Split: 73/27 +<- the differential of teams total RPG
# APG + PPG / Teams Total of APG+PPG <- % OF chance player gets the ball

# FG% v FG3 % possible determination of shot taken
# Assists vs. shots may be ratio of which the person would try
# perhaps that's Assists vs. FG + FG3

# total BPG or SPG of team determines likelihood of either
# defensive action occurring - based on however many "moves" I set a game to be
#so total BLK of team / total MOVES

#enough shots to assume you'd score points every 2.5 turns
#and to assume 55% of your points come from 2s?
#so 28 turns of 2s, 15 turns of 3s <- 43
# 43 x 2.5 <- 108 drives each
# googled and found the average to be around 100.
# everything would be much simpler if divided by 100
# this shuold potentially be 133 to adjust for bench players being absent -
# but all the stats still calculated off of 100
# stats for all players kept in vector - (Pts,Ast,Rbd,Stl,Blk,Tov)

p1stats <- totStatsV[1:10]
p2stats <- totStatsV[11:20]
p3stats <- totStatsV[21:30]
p4stats <- totStatsV[31:40]
p5stats <- totStatsV[41:50]
p6stats <- totStatsV[51:60]
p7stats <- totStatsV[61:70]
p8stats <- totStatsV[71:80]
p9stats <- totStatsV[81:90]
p10stats <- totStatsV[91:100]
