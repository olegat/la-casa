function perftoint(perfrating) {
  if (perfrating == "Needs Improvement") {
    return 1
  }
  if (perfrating == "Consistently Meets Expectations") {
    return 2
  }
  if (perfrating == "Exceeds Expectations") {
    return 3
  }
  if (perfrating == "Strongly Exceeds Expectations") {
    return 4
  }
  if (perfrating == "Superb") {
    return 5
  }
  return 0
}
BEGIN {
  FS = ","
  LocationCOL   =  3; Location   = "UK-LON"
  LevelCOL      =  6; Level      = "4"
  JobFamilyCOL  =  7; JobFamily  = "Software Engineer"
  BaseSalaryCOL = 11; BaseSalaryTotal = 0; nBaseSalary = 0
  BonusCOL      = 18; BonusTotal      = 0; nBonus      = 0
  EquityCOL     = 20; EquityTotal     = 0; nEquity     = 0
  PerfQ1COL     = 21; PerfQ1Total     = 0; nPerfQ1     = 0
  PerfQ3COL     = 22; PerfQ3Total     = 0; nPerfQ3     = 0
}

{
  if ($LocationCOL  == Location  &&
      $LevelCOL     == Level     &&
      $JobFamilyCOL == JobFamily )
  {
    BaseSalaryTotal += $BaseSalaryCOL
    nBaseSalary++

    BonusTotal += $BonusCOL
    nBonus++

    if ( $EquityCOL > 0 ) {
      EquityTotal += $EquityCOL
      nEquity++
    }

    if ( perftoint($PerfQ1COL) > 0 ) {
      PerfQ1Total += perftoint( $PerfQ1COL )
      nPerfQ1++
    }
    if ( perftoint($PerfQ3COL) > 0 ) {
      PerfQ3Total += perftoint( $PerfQ3COL )
      nPerfQ3++
    }
  }
}

END {
  if (nBaseSalary == 0) {
    print "No matching paycomps found."
  }
  else {
    print "Compensation summary for L"Level" "JobFamily" in "Location""
    print ""
    print "  Base salaries"
    print "    Mean (n="nBaseSalary"): £ "BaseSalaryTotal / nBaseSalary
    print ""
    print "  Bonus (Jan 2022)"
    print "    Mean (n="nBonus"): £ "BonusTotal / nBonus
    print "    ("BonusTotal / BaseSalaryTotal"%)"
    print ""
    print "  Equity Refresh (2022)"
    print "    Mean (n="nEquity"): $ "EquityTotal / nEquity
    print ""
    print "  Perf ratings"
    print "    Q1 Mean (n="nPerfQ1"): "PerfQ1Total / nPerfQ1
    print "    Q3 Mean (n="nPerfQ3"): "PerfQ3Total / nPerfQ3
  }
}
