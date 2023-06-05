#each player would have a vector of relvant stats. 
#these are all doubles, representing percentages of their probability to do x

# VECTOR VALUES:

# FG%, FG3%, PPG, APG, RPG , STL, BLK, TOV, 2PA, 3PA
kd <- c(.499,.385,27.3,4.3,7.1,1.1,1.1,3.2,13.8,4.9)
steph <- c(.475,.428,24.6,6.5,4.7,1.6,0.2,3.1,8.8,9)
mike <- c(.497,.327,30.1,5.3,6.2,2.3,0.8,2.7,21.2,1.7)
scottie <- c(.473,.326,16.1,5.2,6.4,2,0.8,2.8,10.8,2.5)
hakeem <- c(.512,.202,21.8,2.5,11.1,1.7,3.1,3.0,16.9,0.1)

jaylen23 <- c(.491,.335,26.6,3.5,6.9,1.1,0.4,2.9,13.4,7.3)
tatum23 <- c(.466,.350,30.1,4.6,8.8,1.1,0.7,2.9,11.8,9.3)
bird <- c(.496,.376,24.3,6.3,10,1.7,0.8,3.1,17.4,1.9)
mchale <- c(.554,.261,17.9,1.7,7.3,0.4,1.7,1.9,12.5,0.2)
pierce08 <- c(.464,.392,19.6,4.5,5.1,1.3,0.5,2.8,9.2,4.6)

# put all stats in a vector
otherV <- c(kd,steph,mike,scottie,hakeem)
celticV <- c(jaylen23,tatum23,bird,mchale,pierce08)

otherShots <- c(otherV[3:4],otherV[13:14],otherV[23:24],otherV[33:34],otherV[43:44])
otherShotsTot <- sum(otherShots[1:10])

celticShots <- c(celticV[3:4],celticV[13:14],celticV[23:24],celticV[33:34],celticV[43:44])
celticShotsTot <- sum(celticShots[1:10])

kdPct <- (kd[3] + kd[4]) / otherShotsTot
stephPct <- (steph[3] + steph[4]) / otherShotsTot
mikePct <- (mike[3] + mike[4]) / otherShotsTot
scottiePct <- (scottie[3] + scottie[4]) / otherShotsTot
hakeemPct <- (hakeem[3] + hakeem[4]) / otherShotsTot

jaylen23Pct <- (jaylen23[3] + jaylen23[4]) / otherShotsTot
tatum23Pct <- (tatum23[3] + tatum23[4]) / otherShotsTot
birdPct <- (bird[3] + bird[4]) / otherShotsTot
mchalePct <- (mchale[3] + mchale[4]) / otherShotsTot
pierce08Pct <- (pierce08[3] + pierce08[4]) / otherShotsTot

#STL vector add
otherSTL <- otherV[6] + otherV[16] + otherV[26] + otherV[36] + otherV[46]
celticSTL <- celticV[6] + celticV[16] + celticV[26] + celticV[36] + celticV[46]

#BLK vector add
otherBLK <- otherV[7] + otherV[17] + otherV[27] + otherV[37] + otherV[47]
celticBLK <- celticV[7] + celticV[17] + celticV[27] + celticV[37] + celticV[47]
  
#RPG vector add
otherRPG <- otherV[5] + otherV[15] + otherV[25] + otherV[35] + otherV[45]
celticRPG <- celticV[5] + celticV[15] + celticV[25] + celticV[35] + celticV[45]
otherDefRbdPct <- 73 + otherRPG - celticRPG
otherOffRbdPct <- 27 + otherRPG - celticRPG
celticDefRbdPct <- 73 + celticRPG - otherRPG
celticOffRbdPct <- 27 + celticRPG - otherRPG

#individual player RPG PCT
celtic1 = celticV[5] / celticRPG
celtic2 = celticV[15] / celticRPG
celtic3 = celticV[25] / celticRPG
celtic4 = celticV[35] / celticRPG
celtic5 = celticV[45] / celticRPG
celticRbdV = c(celtic1,celtic2,celtic3,celtic4,celtic5)
other1 = otherV[5] / otherRPG
other2 = otherV[15] / otherRPG
other3 = otherV[25] / otherRPG
other4 = otherV[35] / otherRPG
other5 = otherV[45] / otherRPG
otherRbdV = c(other1,other2,other3,other4,other5)

#STL vector add
otherSTL <- otherV[6] + otherV[16] + otherV[26] + otherV[36] + otherV[46]
celticSTL <- celticV[6] + celticV[16] + celticV[26] + celticV[36] + celticV[46]

#individual player STL PCT
celtic1 = celticV[6] / celticSTL
celtic2 = celticV[16] / celticSTL
celtic3 = celticV[26] / celticSTL
celtic4 = celticV[36] / celticSTL
celtic5 = celticV[46] / celticSTL
celticStlV = c(celtic1,celtic2,celtic3,celtic4,celtic5)
other1 = otherV[6] / otherSTL
other2 = otherV[16] / otherSTL
other3 = otherV[26] / otherSTL
other4 = otherV[36] / otherSTL
other5 = otherV[46] / otherSTL
otherStlV = c(other1,other2,other3,other4,other5)


#Blk vector add
otherBlk <- otherV[7] + otherV[17] + otherV[27] + otherV[37] + otherV[47]
celticBlk <- celticV[7] + celticV[17] + celticV[27] + celticV[37] + celticV[47]

#individual player Blk PCT
celtic1 = celticV[7] / celticBlk
celtic2 = celticV[17] / celticBlk
celtic3 = celticV[27] / celticBlk
celtic4 = celticV[37] / celticBlk
celtic5 = celticV[47] / celticBlk
celticBlkV = c(celtic1,celtic2,celtic3,celtic4,celtic5)
other1 = otherV[7] / otherBlk
other2 = otherV[17] / otherBlk
other3 = otherV[27] / otherBlk
other4 = otherV[37] / otherBlk
other5 = otherV[47] / otherBlk
otherBlkV = c(other1,other2,other3,other4,other5)

#set player names in vector
otherNames <- c("KD","Steph","Mike","Scottie","Hakeem")
celticNames <- c("Jaylen","Tatum","Bird","McHale","Pierce")
allNames <- c(otherNames,celticNames)

playCount <- 0
otherPointTotal <- 0
celticPointTotal <- 0

#this begins the move by move block
otherBall<- TRUE
noAssist <- TRUE
rbdNum <- 0

if(otherBall){
  #other goes
  if(rbdNum == 0){
    ranChk <- sample(1:100,1,replace=TRUE)
    ballGoes <- ranChk / 100
    if(ballGoes < kdPct){
      addForPlayer <- 0
      print("KD")
    }else if(ballGoes < (kdPct + stephPct)) {
      addForPlayer <- 10
      print("Steph")
    }else if(ballGoes < (kdPct + stephPct + mikePct)) {
      addForPlayer <- 20
      print("Mike")
    }else if(ballGoes < (kdPct + stephPct + mikePct + scottiePct)) {
      addForPlayer <- 30
      print("Scottie")
    }else {
      addForPlayer <- 40
      print("Hakeem")
    }
  }else{
    print(paste(otherNames[rbdNum],"has the ball"))
  }  
  #identify who has ball
  playerNum <- as.integer((addForPlayer + 10) / 10)
  
  playDone <- FALSE
  
  #first test steal
  ranChk <- sample(1:100,1,replace=TRUE)
  if(ranChk < celticSTL) {
    #who steals?
    ranChk <- sample(1:100,1,replace=TRUE) / 100
    if(ranChk < celticStlV[1]){
      print(paste(allNames[6],"steals the ball!"))
      stlNum = 1
    }else if(ranChk < celticStlV[1] + celticStlV[2]){
      print(paste(allNames[7],"steals the ball!"))
      stlNum = 2
    }else if(ranChk < celticStlV[1] + celticStlV[2] + celticStlV[3]){
      print(paste(allNames[8],"steals the ball!"))
      stlNum = 3
    }else if(ranChk < (1 - celticStlV[5])){
      print(paste(allNames[9],"steals the ball!"))
      stlNum = 4
    }else{
      print(paste(allNames[10],"steals the ball!"))
      stlNum = 5
    }
    playDone <- TRUE
    otherBall<- FALSE
  }
  #then test turnover
  ranChk <- sample(1:100,1,replace=TRUE)
  i <- 8 + addForPlayer
  if((ranChk < celticV[i]) & !playDone) {
    print(paste(otherNames[playerNum],"turned it over!"))
    playDone <- TRUE
    otherBall<- FALSE
  }
  
  #first set some variables
  ranChk <- sample(1:100,1,replace=TRUE)
  #APG of player:
  i <- 4 + addForPlayer
  #2PA of player
  j <- 9 + addForPlayer
  #3PA of player
  k <- 10 + addForPlayer
  #shotType
  player2PA <- otherV[j] / (otherV[j] + otherV[k])
  #assume2
  shootTwo <- TRUE
  
    #test pass or shoot if noAssist = true
  if(noAssist & !playDone){
    if(ranChk < otherV[i]){
      print(paste(otherNames[playerNum],"passes the ball"))
      noAssist <- FALSE
    }else{
      ranChk <- sample(1:100,1,replace=TRUE)
      if(ranChk > player2PA){
        shootTwo <- FALSE
        otherBall<- FALSE
      }
    }
    #this will be false if he fell in the assist bucket on this move
    if(noAssist){
      #first check block
      ranChk <- sample(1:100,1,replace=TRUE)
      if(ranChk < celticBlk) {
        #who blocks?
        ranChk <- sample(1:100,1,replace=TRUE) / 100
        if(ranChk < celticBlkV[1]){
          print(paste(allNames[6],"blocks the shot!"))
          BlkNum = 1
        }else if(ranChk < celticBlkV[1] + celticBlkV[2]){
          print(paste(allNames[7],"blocks the shot!"))
          BlkNum = 2
        }else if(ranChk < celticBlkV[1] + celticBlkV[2] + celticBlkV[3]){
          print(paste(allNames[8],"blocks the shot!"))
          BlkNum = 3
        }else if(ranChk < (1 - celticBlkV[5])){
          print(paste(allNames[9],"blocks the shot!"))
          BlkNum = 4
        }else{
          print(paste(allNames[10],"blocks the shot!"))
          BlkNum = 5
        }
        playDone <- TRUE
        otherBall<- FALSE
      }
      #then check SHOT
      if(shootTwo){
        print(paste(otherNames[playerNum],"shoots a 2..."))
        i <- 1 + addForPlayer
        if((ranChk / 100) < otherV[i]){
          print(paste(otherNames[playerNum],"Scored 2!"))
          otherPointTotal <- otherPointTotal + 2
          playDone <- TRUE
        }else{
          print(paste(otherNames[playerNum],"missed the 2!"))
          #check who gets rebound
          ranChk <- sample(1:100,1,replace=TRUE)
          if(ranChk < otherOffRbdPct){
            #offense gets rebound
            #determine who
            ranChk <- sample(1:100,1,replace=TRUE) / 100
            if(ranChk < otherRbdV[1]){
              print(paste(otherNames[1],"gets the rebound!"))
              rbdNum = 1
            }else if(ranChk < otherRbdV[1] + otherRbdV[2]){
              print(paste(otherNames[2],"gets the rebound!"))
              rbdNum = 2
            }else if(ranChk < otherRbdV[1] + otherRbdV[2] + otherRbdV[3]){
              print(paste(otherNames[3],"gets the rebound!"))
              rbdNum = 3
            }else if(ranChk < (1 - otherRbdV[5])){
              print(paste(otherNames[4],"gets the rebound!"))
              rbdNum = 4
            }else{
              print(paste(otherNames[5],"gets the rebound!"))
              rbdNum = 5
            }
            #reset play
            noAssist = TRUE
          }else{
            #defense gets rebound
            #determine who
            ranChk <- sample(1:100,1,replace=TRUE) / 100
            if(ranChk < celticRbdV[1]){
              print(paste(celticNames[1],"gets the rebound!"))
              rbdNum = 1
            }else if(ranChk < celticRbdV[1] + celticRbdV[2]){
              print(paste(celticNames[2],"gets the rebound!"))
              rbdNum = 2
            }else if(ranChk < celticRbdV[1] + celticRbdV[2] + celticRbdV[3]){
              print(paste(celticNames[3],"gets the rebound!"))
              rbdNum = 3
            }else if(ranChk < (1 - celticRbdV[5])){
              print(paste(celticNames[4],"gets the rebound!"))
              rbdNum = 4
            }else{
              print(paste(celticNames[5],"gets the rebound!"))
              rbdNum = 5
            }
            playCount = playCount + 1
            otherBall = FALSE
          }
        }
      }else{
        i <- 2 + addForPlayer
        print(paste(otherNames[playerNum],"shoots a 3..."))
        #first check block
        ranChk <- sample(1:100,1,replace=TRUE)
        if(ranChk < celticBlk) {
          #who blocks?
          ranChk <- sample(1:100,1,replace=TRUE) / 100
          if(ranChk < celticBlkV[1]){
            print(paste(allNames[6],"blocks the shot!"))
            BlkNum = 1
          }else if(ranChk < celticBlkV[1] + celticBlkV[2]){
            print(paste(allNames[7],"blocks the shot!"))
            BlkNum = 2
          }else if(ranChk < celticBlkV[1] + celticBlkV[2] + celticBlkV[3]){
            print(paste(allNames[8],"blocks the shot!"))
            BlkNum = 3
          }else if(ranChk < (1 - celticBlkV[5])){
            print(paste(allNames[9],"blocks the shot!"))
            BlkNum = 4
          }else{
            print(paste(allNames[10],"blocks the shot!"))
            BlkNum = 5
          }
          playDone <- TRUE
          otherBall<- FALSE
        }
        if((ranChk / 100) < otherV[i]){
          print(paste(otherNames[playerNum],"Scored 3!"))
          playDone <- TRUE
        }else{
          print(paste(otherNames[playerNum],"missed the 3!"))
          #check who gets rebound
          ranChk <- sample(1:100,1,replace=TRUE)
          if(ranChk < otherOffRbdPct){
            #offense gets rebound
            #determine who
            ranChk <- sample(1:100,1,replace=TRUE) / 100
            if(ranChk < otherRbdV[1]){
              print(paste(otherNames[1],"gets the rebound!"))
              rbdNum = 1
            }else if(ranChk < otherRbdV[1] + otherRbdV[2]){
              print(paste(otherNames[2],"gets the rebound!"))
              rbdNum = 2
            }else if(ranChk < otherRbdV[1] + otherRbdV[2] + otherRbdV[3]){
              print(paste(otherNames[3],"gets the rebound!"))
              rbdNum = 3
            }else if(ranChk < (1 - otherRbdV[5])){
              print(paste(otherNames[4],"gets the rebound!"))
              rbdNum = 4
            }else{
              print(paste(otherNames[5],"gets the rebound!"))
              rbdNum = 5
            }
            #reset play
            noAssist = TRUE
          }else{
            #defense gets rebound
            #determine who
            ranChk <- sample(1:100,1,replace=TRUE) / 100
            if(ranChk < celticRbdV[1]){
              print(paste(celticNames[1],"gets the rebound!"))
              rbdNum = 1
            }else if(ranChk < celticRbdV[1] + celticRbdV[2]){
              print(paste(celticNames[2],"gets the rebound!"))
              rbdNum = 2
            }else if(ranChk < celticRbdV[1] + celticRbdV[2] + celticRbdV[3]){
              print(paste(celticNames[3],"gets the rebound!"))
              rbdNum = 3
            }else if(ranChk < (1 - celticRbdV[5])){
              print(paste(celticNames[4],"gets the rebound!"))
              rbdNum = 4
            }else{
              print(paste(celticNames[5],"gets the rebound!"))
              rbdNum = 5
            }
            playCount = playCount + 1
            otherBall = FALSE
          }
        }
      }
    }
  }
  if(!playDone & !noAssist){
    ranChk <- sample(1:100,1,replace=TRUE)
    #first check BLOCK
    ranChk <- sample(1:100,1,replace=TRUE)
    #then check SHOT
    if(shootTwo){
      i <- 1 + addForPlayer
      if((ranChk / 100) < otherV[i]){
        print(paste(otherNames[playerNum],"Scored 2!"))
        otherPointTotal <- otherPointTotal + 2
        playDone <- TRUE
      }else{
        print(paste(otherNames[playerNum],"missed the 2!"))
        #check who gets rebound
        ranChk <- sample(1:100,1,replace=TRUE)
        if(ranChk < otherOffRbdPct){
          #offense gets rebound
          #determine who
          ranChk <- sample(1:100,1,replace=TRUE) / 100
          if(ranChk < otherRbdV[1]){
            print(paste(otherNames[1],"gets the rebound!"))
            rbdNum = 1
          }else if(ranChk < otherRbdV[1] + otherRbdV[2]){
            print(paste(otherNames[2],"gets the rebound!"))
            rbdNum = 2
          }else if(ranChk < otherRbdV[1] + otherRbdV[2] + otherRbdV[3]){
            print(paste(otherNames[3],"gets the rebound!"))
            rbdNum = 3
          }else if(ranChk < (1 - otherRbdV[5])){
            print(paste(otherNames[4],"gets the rebound!"))
            rbdNum = 4
          }else{
            print(paste(otherNames[5],"gets the rebound!"))
            rbdNum = 5
          }
          #reset play
          noAssist = TRUE
        }else{
          #defense gets rebound
          #determine who
          ranChk <- sample(1:100,1,replace=TRUE) / 100
          if(ranChk < celticRbdV[1]){
            print(paste(celticNames[1],"gets the rebound!"))
            rbdNum = 1
          }else if(ranChk < celticRbdV[1] + celticRbdV[2]){
            print(paste(celticNames[2],"gets the rebound!"))
            rbdNum = 2
          }else if(ranChk < celticRbdV[1] + celticRbdV[2] + celticRbdV[3]){
            print(paste(celticNames[3],"gets the rebound!"))
            rbdNum = 3
          }else if(ranChk < (1 - celticRbdV[5])){
            print(paste(celticNames[4],"gets the rebound!"))
            rbdNum = 4
          }else{
            print(paste(celticNames[5],"gets the rebound!"))
            rbdNum = 5
          }
          playCount = playCount + 1
          otherBall = FALSE
        }
      }
    }else{
      i <- 2 + addForPlayer
      if((ranChk / 100) < otherV[i]){
        print(paste(otherNames[playerNum],"Scored 3!"))
        playDone <- TRUE
      }else{
        #rebound
        print(paste(otherNames[playerNum],"missed the 2!"))
        #check who gets rebound
        ranChk <- sample(1:100,1,replace=TRUE)
        if(ranChk < otherOffRbdPct){
          #offense gets rebound
          #determine who
          ranChk <- sample(1:100,1,replace=TRUE) / 100
          if(ranChk < otherRbdV[1]){
            print(paste(otherNames[1],"gets the rebound!"))
            rbdNum = 1
          }else if(ranChk < otherRbdV[1] + otherRbdV[2]){
            print(paste(otherNames[2],"gets the rebound!"))
            rbdNum = 2
          }else if(ranChk < otherRbdV[1] + otherRbdV[2] + otherRbdV[3]){
            print(paste(otherNames[3],"gets the rebound!"))
            rbdNum = 3
          }else if(ranChk < (1 - otherRbdV[5])){
            print(paste(otherNames[4],"gets the rebound!"))
            rbdNum = 4
          }else{
            print(paste(otherNames[5],"gets the rebound!"))
            rbdNum = 5
          }
          #reset play
          noAssist = TRUE
        }else{
          #defense gets rebound
          #determine who
          ranChk <- sample(1:100,1,replace=TRUE) / 100
          if(ranChk < celticRbdV[1]){
            print(paste(celticNames[1],"gets the rebound!"))
            rbdNum = 1
          }else if(ranChk < celticRbdV[1] + celticRbdV[2]){
            print(paste(celticNames[2],"gets the rebound!"))
            rbdNum = 2
          }else if(ranChk < celticRbdV[1] + celticRbdV[2] + celticRbdV[3]){
            print(paste(celticNames[3],"gets the rebound!"))
            rbdNum = 3
          }else if(ranChk < (1 - celticRbdV[5])){
            print(paste(celticNames[4],"gets the rebound!"))
            rbdNum = 4
          }else{
            print(paste(celticNames[5],"gets the rebound!"))
            rbdNum = 5
          }
          playCount = playCount + 1
          otherBall = FALSE
        }
      }
    }
    otherBall<- FALSE
    playDone <- TRUE
  }
  #if assist , loop repeats
  
  if(noAssist){
  playCount <- playCount + 1
  }
}else{
  #celtics goes
  #switch sides
  playCount <- playCount + 1
  otherBall<- TRUE
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
