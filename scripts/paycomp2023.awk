# Usage:
#   awk -f paycomp2023.awk CSV_FILE
#
# CSV_FILE must be a comma-separated file of the employee disclosed salaries for
# the year 2023.
#
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

function gradtoint(gradrating) {
  if (gradrating == "Not Enough Impact (NE)") {
    return 1
  }
  if (gradrating == "Moderate Impact (M)") {
    return 2
  }
  if (gradrating == "Significant Impact (S)") {
    return 3
  }
  if (gradrating == "Outstanding Impact (O)") {
    return 4
  }
  if (gradrating == "Transformative Impact (T)") {
    return 5
  }
  return 0
}

# Division, but return 0 when dividing by 0
function div0(n, d) {
  if (d == 0) {
    return 0
  } else {
    return n/d
  }
}

BEGIN {
  FS = ","
  LocationCOL   =  5; Location   = "UK-LON"
  LevelCOL      = 13; Level      = "4"
  JobFamilyCOL  = 10; JobFamily  = "Software Engineering"
  BaseSalaryCOL = 16; BaseSalaryTotal = 0; nBaseSalary = 0
  BonusCOL      = 26; BonusTotal      = 0; nBonus      = 0
  EquityCOL     = 27; EquityTotal     = 0; nEquity     = 0
  Perf2022COL   = 30; Perf2022Total   = 0; nPerf2022   = 0
  Grad2023COL   = 31; Grad2023Total   = 0; nGrad2023   = 0
}

{
  if ($LocationCOL  == Location  &&
      $LevelCOL     == Level     &&
      $JobFamilyCOL == JobFamily )
  {
    if ( $BaseSalaryCOL > 0 ) {
      BaseSalaryTotal += $BaseSalaryCOL
      nBaseSalary++
    }

    if ( $BonusCOL > 0 ) {
      BonusTotal += $BonusCOL
      nBonus++
    }

    if ( $EquityCOL > 0 ) {
      EquityTotal += $EquityCOL
      nEquity++
    }

    if ( perftoint($Perf2022COL) > 0 ) {
      Perf2022Total += perftoint( $Perf2022COL )
      nPerf2022++
    }
    if ( gradtoint($Grad2023COL) > 0 ) {
      Grad2023Total += gradtoint( $Grad2023COL )
      nGrad2023++
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
    print "    Mean (n="nBaseSalary"): £ "div0(BaseSalaryTotal, nBaseSalary)
    print ""
    print "  Bonus (Jan 2023)"
    print "    Mean (n="nBonus"): £ "div0(BonusTotal, nBonus)
    print "    ("div0(BonusTotal, BaseSalaryTotal)"%)"
    print ""
    print "  Equity Refresh (2023)"
    print "    Mean (n="nEquity"): $ "div0(EquityTotal, nEquity)
    print ""
    print "  Perf/GRAD ratings"
    print "    Perf Q1 2022 Mean (n="nPerf2022"): "div0(Perf2022Total, nPerf2022)
    print "    GRAD Q1 2023 Mean (n="nGrad2023"): "div0(Grad2023Total, nGrad2023)
  }
}
