  SORT FIELDS=(4,5,CH,A)
  OUTREC IFTHEN=(WHEN(20,8,CH,EQ,C'LIFE    '),
                 OVERLAY=(4:1,35,1:C'LP-')),
         IFTHEN=(WHEN(20,8,CH,EQ,C'MEDICAL '),
                 OVERLAY=(4:1,35,1:C'MP-')),
         IFTHEN=(WHEN(20,8,CH,EQ,C'TERM    '),
                 OVERLAY=(4:1,35,1:C'TP-')),
         IFTHEN=(WHEN=NONE,OVERLAY=(4:1,35,1:C'XX-'))