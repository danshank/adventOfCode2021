@main def solveAdventDay(day: Int, part: Int): Unit = 
  if day == 1 && part == 1 then println(solveDayOnePartOne())
  else if day == 1 && part == 2 then println(solveDayOnePartTwo())
  else if day == 2 && part == 1 then println(solveDayTwoPartOne())
  else if day == 2 && part == 2 then println(solveDayTwoPartTwo())
  else if day == 3 && part == 1 then println(solveDayThreePartOne())
  else if day == 3 && part == 2 then println(solveDayThreePartTwo())