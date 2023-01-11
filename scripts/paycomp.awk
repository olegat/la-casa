BEGIN {
  FS = ","
  n = 0
  LocationCOL   =  3; Location   = "UK-LON"
  LevelCOL      =  6; Level      = "4"
  JobFamilyCOL  =  7; JobFamily  = "Software Engineer"
  BaseSalaryCOL = 11; BaseSalaryTotal = 0
}

{
  if ($LocationCOL  == Location  &&
      $LevelCOL     == Level     &&
      $JobFamilyCOL == JobFamily )
  {
    BaseSalaries[n++]  = $BaseSalaryCOL
    BaseSalaryTotal   += $BaseSalaryCOL
  }
}

END {
  if (n == 0) {
    print "No matching paycomps found."
  }
  else {
    print "Base salaries for L"Level" "JobFamily" in "Location":"
    print "  n      = "n
    print "  Mean   = "BaseSalaryTotal / n
  }
}
