             START 0 
 HELLO     CSECT 
           BALR 11,0 
           USING *,11 
 * PRINT ARRAY TO DISPLAY 
           WTO 'INPUT ARRAY' 
 PRTODIS   XR  8,8 R8 - CMECHENIE 
           LA  9,INARR R9 - BASE, NACHALO MASSIVA 
           MP  I,ZERO CLEANING I 
 OUTMASS   CP  I,LEN CRAVNIVAEM I AND DLINY MASSIVA 
           BH  ENDOUT IF I>LEN GOTO ENDOUT 
           LA  10,0(8,9) V R10 - ADRESS TEKYCHEGO ELEMENTA 
           MVC OUTNUM,0(10) ZNACHENIE, CHRON PO ADRESY V R10 - V OUTNUM 
           UNPK OUTNUM,OUTNUM 
           OC  OUTNUM,MASK OUTNUM || MASK, DLY PRIVODA Z K EBCDIC 
           WTO TEXT=WTOT 
           A   8,NUMLEN YVELICHIVAEM SMECHENIE DLY SLED. ELEMENTA 
           BO  OVERFLOW 
           AP  I,ONE I++ 
           BO  OVERFLOW 
           B   OUTMASS GOTO OUTMASS 
 ENDOUT    WTO '*********' 
           AP  STEPNOW,ONE ANTI INFINITE LOOP 
           BO  OVERFLOW 
           CP  STEPNOW,STEPS 
           BH FEXCL 
 ****************** 
 ****************** 
 * SORT ARRAY 
 SORTARR  XR  8,8 R9 - SMECHENIE 
          LA  9,INARR R9 - BASE, NACHALO MASSIVA 
          MP  I,ZERO CLEANING I 
          WTO 'SORTED ARRAY' 
 SORTST1  MP  J,ZERO CLEANING J 
 * FOR (I=0; I<LEN; I++) 
          XR  8,8 
          LA  10,0(8,9) 
          CP  I,LEN I <=LEN ? 
          BE  ENDSORT  IF I>=LEN GOTO ENDSORT 
          AP  I,ONE I++ 
          BO  OVERFLOW 
          B   SORTST2 
 * FOR (J=I+1; J<LEN; J++) 
 SORTST2  CP  J,LEN J <=LEN ? 
          BE  SORTST1 IF J>=LEN GOTO SORTST1 
          AP  J,ONE J++ 
          BO  OVERFLOW 
 SORTST3  MVC AI,0(10) AJ - FIRS ELEMENT 
          MVC AIP1,4(10) AJP1=AJ+1 
          A   8,NUMLEN 
          BO  OVERFLOW 
          LA  10,0(8,9) 
 * IF A[J]>A[J+1] 
          CP  AI,AIP1 IF A[J]<A[J+1] ? 
          BL  SORTST2 GOTO SORTST2 
          S   8,NUMLEN 
          BO  OVERFLOW 
          LA  10,0(8,9) 
          MVC SWAP,AI SWAP=A[J] 
          MVC AI,AIP1 A[J+1]=AJ 
          MVC AIP1,SWAP A[J+1]=SWAP 
          MVC 0(4,10),AI 
          A   8,NUMLEN 
          BO  OVERFLOW 
          LA  10,0(8,9) 
          MVC 0(4,10),AIP1 
          B   SORTST2 
 ENDSORT   B PRTODIS  PRINT SORTED ARRAY 
 ********************* 
 *ISKLYCHAEM POVTORENIYA 
 FEXCL    XR 8,8 
          XR 7,7 
          MVC NEWLEN,LEN 
          LA 6,EXCLARR 
          LA 9,INARR 
          MP I,ZERO 
 FEXCLST1 LA 10,0(8,9) 
          CP I,LEN 
          BE FEND 
          AP I,ONE 
          BO  OVERFLOW 
          MVC AI,0(10) 
          MVC AIP1,4(10) 
          A 8,NUMLEN 
          BO  OVERFLOW 
          CP AI,AIP1 
          BNE FPEND 
          SP  NEWLEN,ONE 
          BO  OVERFLOW 
          B FEXCLST1 
 FPEND    LA 5,0(7,6) 
          A  7,NUMLEN 
          BO  OVERFLOW 
          MVC 0(4,5),AI 
          B FEXCLST1 
 FEND     LA  5,0(7,6) 
          MVC 0(4,5),AIP1 
          B   PRTEXCL 
 ********************** 
 * PRINT EXCL ARRAY TO DISPLAY 
 PRTEXCL  XR  8,8 
          LA  9,EXCLARR 
          MP  I,ZERO 
          WTO 'EXCL ARRAY' 
 OUTEXCLM CP  I,NEWLEN 
          BH  ENDOUTEX 
          LA  10,0(8,9) 
          MVC OUTNUM,0(10) 
          UNPK OUTNUM,OUTNUM 
          OC  OUTNUM,MASK 
          WTO TEXT=WTOT 
          A   8,NUMLEN 
          BO  OVERFLOW 
          AP  I,ONE 
          BO  OVERFLOW 
          B   OUTEXCLM 
 ENDOUTEX WTO '**************' 
          B MSEQ 
 ********************** 
 *FORMIRUEM VSPOMOGATELNY MASSIV\MASKY. ZERO - POSSLEDOVATELNOST 
 MSEQ     XR  8,8 
          XR  7,7 
          LA  9,EXCLARR 
          LA  6,SEQARR 
          MP I,ZERO 
 MSEQST1  CP  I,NEWLEN 
          BE  ENDMSEQ 
          AP  I,ONE 
          LA  10,0(8,9) 
          LA  5,0(7,6) 
          MVC AI,0(10) 
          MVC AIP1,4(10) 
          MVC SWAP,0(10) 
          A   8,NUMLEN 
          BO  OVERFLOW 
          A   7,NUMLEN 
          BO  OVERFLOW 
          AP  SWAP,ONE 
          BO  OVERFLOW 
          CP  SWAP,AIP1 
          BNE MSEQST1 
          MVC 0(4,5),ZERO2 A[I]:=0 
          MVC 4(4,5),ZERO2 A[I+1]:=0 
          B MSEQST1 
 ENDMSEQ  B PRTSEQ 
 ******************* 
 * PRINT SEQUENCE 
 PSEQ     WTO '-------' 
          XR 8,8 
          XR 7,7 
          LA 6,EXCLARR 
          LA 9,SEQARR 
          MP I,ZERO 
          MP J,ZERO 
 PSEQST1  CP I,NEWLEN 
          BH ENDPSEQ 
          LA 10,0(8,9) 
          LA 5,0(7,6) 
          MVC AI,0(10) 
          MVC AIP1,4(10) 
          A  8,NUMLEN 
          BO  OVERFLOW 
          A  7,NUMLEN 
          BO  OVERFLOW 
          AP I,ONE 
          BO  OVERFLOW 
          CP AI,ZERO 
          BNE  PSEQST1 
          MVC OUTNUM,0(5) 
          UNPK OUTNUM,OUTNUM 
          OC  OUTNUM,MASK 
          WTO TEXT=WTOT 
          CP AIP1,ONE 
          BNE PSEQST1 RASGRANICHENIE SEQUENCE 
          WTO '-------' 
          B PSEQST1 
 ENDPSEQ  B END 
 ****************** 
 * PRINT ARRAY\MASK 
 PRTSEQ   XR  8,8 
          LA  9,SEQARR 
          MP  I,ZERO 
          WTO 'SEQARR' 
 OUTSEQ   CP  I,NEWLEN 
          BH  ENDOUSEQ 
          LA  10,0(8,9) 
          MVC OUTNUM,0(10) 
          UNPK OUTNUM,OUTNUM 
          OC  OUTNUM,MASK 
          WTO TEXT=WTOT 
          A   8,NUMLEN 
          BO  OVERFLOW 
          AP  I,ONE 
          BO  OVERFLOW 
          B   OUTSEQ 
 ENDOUSEQ WTO '**************' 
          B   PSEQ 
 ********************** 
 OVERFLOW WTO 'OVERFLOW' 
          B END 
 ********************** 
 *INPUT ARRAY 
 WTOT     DC H'4' 
 OUTNUM   DS ZL4 
 INARR    DC PL4'1' 
           DC PL4'8' 
           DC PL4'4' 
           DC PL4'3' 
           DC PL4'44' 
           DC PL4'91' 
           DC PL4'45' 
           DC PL4'83' 
           DC PL4'98' 
           DC PL4'8' 
           DC PL4'5' 
           DC PL4'44' 
 ********************** 
 EXCLARR   DS 12PL4 
 ********************************* 
 SEQARR    DC 12PL4'1' 
 ********************* 
 MASK     DC BL4'11110000' MASKA ZONE/EBCDIC 
 ONE      DC PL4'1' 
 NUMLEN   DC XL4'4' DLINA SLOVA V BAYTAX 
 ZERO     DC PL2'0' 
 ZERO2    DC PL4'0' 
 LEN      DC PL4'11'  LEN ARRAYA-1 
 OUTNUM   DS ZL4                        
 INARR    DC PL4'1' 
           DC PL4'8' 
           DC PL4'4' 
           DC PL4'3' 
           DC PL4'44' 
           DC PL4'91' 
           DC PL4'45' 
           DC PL4'83' 
           DC PL4'98' 
           DC PL4'8' 
           DC PL4'5' 
           DC PL4'44' 
 ********************** 
 EXCLARR   DS 12PL4 
 ********************************* 
 SEQARR    DC 12PL4'1' 
 ********************* 
 MASK     DC BL4'11110000' MASKA ZONE/EBCDIC 
 ONE      DC PL4'1' 
 NUMLEN   DC XL4'4' DLINA SLOVA V BAYTAX 
 ZERO     DC PL2'0' 
 ZERO2    DC PL4'0' 
 LEN      DC PL4'11'  LEN ARRAYA-1 
 NEWLEN   DS PL4 
 I        DC PL4'0' 
 J        DC PL4'0' 
 STEPS    DC PL4'1' 
 STEPNOW  DC PL4'0' 
 AI       DS PL4 
 AIP1     DS PL4 
 SWAP     DS PL4 
 ********************** 
 END      BR 14 
          END 
