' FileName: DVWin.Bas
' Date: 1/1/2017
' Author: Ben Ritchey
' Description: DASD Vision File Manager (WinXP/7/10)

#COMPILER PBCC 5
#COMPILE EXE
#DIM ALL

DECLARE FUNCTION TextInput(TCol as Long, CurFile AS STRING, Buffer AS STRING, _
                        Row AS LONG, Col AS LONG, History() AS STRING) AS LONG

FUNCTION PBMAIN () AS LONG  ' (c) 2017 Ben Ritchey
Local ZZ, A, C, F, CC, CName, Drv, DrvList, DVol, Saveit, Z as String
Local AA, B, D, SizeS, FstData, CX, CY, H, HH, I, II, K, KK, L, LL as Long
Local AD, AC as Double
Local TFlag, RLeg, TDDT, FAttr, LLen, M, T, TCol, TRow as Long
Local DirDataTab() as String
LOCAL MONTHTAB() AS STRING
Local EFlag, SFlag, VFlag, PName, PgmVer, SubDir, TBytes, BFree as string
Local TStore, ZDesc, CFSuf, DFSuf, PLeft, PRite, TDir as string
Local RC as integer

  LOCAL CPUs, CurFile, buffer AS STRING
  LOCAL CurPos AS LONG
  LOCAL BitShift, retVal AS LONG
  LOCAL ti      AS LONG

  REDIM history1(0) AS STRING  'add a history array for each text field if needed.

  PgmVer=" DVWin v2.2.Q "
  CName=Environ$("COMPUTERNAME")
  CPUs=Environ$("NUMBER_OF_PROCESSORS")
  TDir=Environ$("TEMP")
  RC=0  ' Exit Errorlevel (Return code)
  SFLag="N"  ' N ame, -D ate new, -S ize big
  EFlag="DVWEdit.Bat "
  VFlag="DVWView.Bat "
  TFlag=0  ' Type when ON=1
  TRow=60  ' Screen Rows
  TCol=132  ' Screen Columns
  TDDT=4096  ' Directory Table size
  DIM DirDataTab(TDDT)
  Dim MonthTab(12)

  A=ucase$(command$, ANSI)
  Drv="C"  ' default Work drive
  B=instr(A,":")
  If B>1 then Drv=mid$(A,B-1,1)  ' override drive if d:

  ZDesc="Google      "
  B=instr(A,"Z=")
  If B>0 then ZDesc=extract$(mid$(command$,B+2), ANY " "+chr$(34)+chr$(13))  ' override desc if Z=x-x
  If len(ZDesc)<13 then ZDesc=ZDesc+space$(13-len(ZDesc))
  If len(ZDesc)>13 then ZDesc=mid$(ZDesc,1,13)

  B=instr(A,"R=")
  If B>0 then TRow=val(mid$(A,B+2))  ' override Rows if R=nn
  If TRow<60 then TRow=60
  If TRow>100 then TRow=60
  B=instr(A,"C=")
  If B>0 then TCol=val(mid$(A,B+2))  ' override Cols if C=nnn
  If TCol<132 then TCol=132
  If TCol>240 then TCol=132

  B=instr(A,"/TOP")
  If B>0 then shell "cmd /c cmdow.exe "+chr$(34)+"DASD Vision for Windows"+chr$(34)+" /TOP"

  RLeg=TRow-4  ' Legend Row
  PLeft=chr$(176)+chr$(176)+chr$(177)+chr$(177)+chr$(178)+chr$(178)
  PRite=chr$(178)+chr$(178)+chr$(177)+chr$(177)+chr$(176)+chr$(176)
  If not TCol<140 then
    PLeft=string$(4,chr$(176))+string$(3,chr$(177))+string$(2,chr$(178))
    PRite=string$(2,chr$(178))+string$(3,chr$(177))+string$(4,chr$(176))
  End if
  PName=" "+PLeft+"  DASD Vision File Manager for MS-Windows  "+PRite+" "

  If isfolder(Drv+":\")=0 then Drv="C": beep  ' Work drive validation
  If isfolder(TDir)=0 then TDir=""
  If TDir="" then TDir=Drv+":\$DVWin$" else TDir=TDir+"\$DVWin$"

  DrvList="C": On Error Resume Next
  for L=68 to 90
    ChDrive Chr$(L)
    If Err=0 then DrvList=DrvList+" "+Chr$(L): else Errclear
   Next L 
  On Error Goto 0
  ChDrive "C"

  FOR L=1 TO 12
    MONTHTAB(L)=READ$(L)
  NEXT L

Startup:

  Shell "cmd /c mode con cols="+Trim$(Str$(TCol))+" lines="+trim$(str$(TRow))
  Console set screen TRow, TCol

reDIM DirDataTab(TDDT)

Shell "Cmd /c Dir /A:-S /O:G"+SFlag+" /X >"+TDir

Open TDir for Input as #1

B=1
If K<1 then K=1
If K>=T then K=1
LLen=0: DVol=""
While not Eof(1)
  Line Input #1,A
  If Instr(A,"<DIR>")>0 and Instr(A," .")>0 then goto LoadNext
  If A<>"" and mid$(A,1,1)<>" " then
    If Mid$(A,3,1)="/" then Mid$(A,3,1)="-"
    If Mid$(A,6,1)="/" then Mid$(A,6,1)="-"
    If Mid$(A,1,1)="0" then Mid$(A,1,1)=" "
    If Mid$(A,13,1)="0" and Mid$(A,14,1)<>"0" then Mid$(A,13,1)=" "
    If Mid$(A,20,1)="M" then Mid$(A,20,1)="m"
    If instr(A,"$DVWin$")=0 then DirDataTab(B)=A: B=B+1  ' Add file to table
    If LLen<len(A) then LLen=len(A)+1
  End if

  l=instr(a,"Directory of")
  if l>0 then SubDir=Trim$(mid$(a,l+13))

  l=instr(a," File(s) ")
  if l>0 then TBytes=mid$(a,l+8)

  l=instr(a," bytes free")
  if l>0 then
    m=instr(a,"Dir(s)")
    BFree=mid$(a,m+6): BFree=RTrim$(BFree," bytes free")
  End if

  l=instr(a," Volume in drive ")
  if l>0 then DVol=Trim$(mid$(a,23))

LoadNext:
WEnd

Close 1
T=B ' Last Record index (::EOF)
DirDataTab(T)="::EOF"

If DVol="" then DVol="{none}"

B=1: I=K: SizeS=TRow-20: Z=""

NextScr:
K=I

GoSub NewScr

  Locate RLeg,16
  Color 14,0: Print mid$(Subdir,1,2);
  Color 6,1: Print " Bytes free= ";

  AD=diskfree(mid$(Subdir,1,1)): DFSuf="Bytes"
  If AD>1000 then AD=AD/1000: DFSuf="KB"
  If AD>1000 then AD=AD/1000: DFSuf="MB"
  If AD>1000 then AD=AD/1000: DFSuf="GB"
  If AD>1000 then AD=AD/1000: DFSuf="TB"
  If AD>1000 then AD=AD/1000: DFSuf="PB"
  If AD>1000 then AD=AD/1000: DFSuf="EB"

  AC=disksize(mid$(Subdir,1,1)): CFSuf="Bytes"
  If AC>1000 then AC=AC/1000: CFSuf="KiloBytes"
  If AC>1000 then AC=AC/1000: CFSuf="MegaBytes"
  If AC>1000 then AC=AC/1000: CFSuf="GigaBytes"
  If AC>1000 then AC=AC/1000: CFSuf="TeraBytes"
  If AC>1000 then AC=AC/1000: CFSuf="PetaBytes"
  If AC>1000 then AC=AC/1000: CFSuf="ExaBytes"
  Color 14,0: Print using$("###.##",AD);
  Color 6,1: Print " ";DFSuf;"  (";
  Color 14,0: Print trim$(str$(int(((AD/AC)*100)+.5)));
  Color 6,1: Print "% of ";
  Color 14,0: Print using$("###.##",AC);
  Color 6,1: Print " ";CFSuf;")";

  Locate 4,1
  Color 4,0: Print "  ";: Color 4,0: Print Using$("####",T-1);
  Color 7,0: Print "   Directory of ";
  Color 11,0: Print Space$(TCol-22);
  Locate 5,1: Print space$(TCol);
  Locate 4,23: Print " ";SubDir;" ";
  Color 7,1

  If T=1 or instr(SFlag,"/A:")>0 then
   If T=1 then color 4,0: print " is empty! ";
   color 15,1
   If instr(sflag,":R") then Color 4: Print "   Read Only files ... HOME to reset ";
   If instr(sflag,":H") then Color 4: Print "   HIDden files ... HOME to reset ";
   If instr(sflag,":S") then Color 4: Print "   SYStem files ... HOME to reset ";
  End if

  Locate RLeg,TCol-10: Color 6,1
  Print "  Work=";: Color 14,0: Print Drv;":"

  Locate RLeg,TCol-58-Len(DrvList)
  Color 6,1: Print "Type: ";
  Color 14,0: If TFlag=0 then print "OFF"; else print "ON ";
  Color 6,1: Print "  Window: ";
  Color 14,0: Print trim$(str$(TCol));"x";trim$(str$(TRow));
  Color 6,1: Print "    Available drives: ";
  Color 14,0: Print DrvList

  Locate 2,8
  Color 6,7
  Print " Volume ID: ";
  Color 9,7
  Print " ";DVol;" "
  Print
  If T=1 then goto getfunc

  Locate 6,1
  Color 1,7
  Print space$(TCol);
  Locate 6,1: Color 6,7
  Print "     #     Date       Time          Bytes       Short FN     Long File Name ";
  If LLen-60>=44 then Print space$(41); else Print space$(LLen-69);
  Print "   ARHS     #"
  Color 6,1
  Print string$(TCol-1,chr$(196));: Locate ,1
  Print chr$(214);
  Locate ,7: Print " "; ' chr$(213);string$(2,chr$(196));chr$(184);
  Print " ";chr$(218);string$(8,chr$(196));chr$(191);
  Print "  ";chr$(218);string$(6,chr$(196));chr$(191);
  Print "  ";chr$(218);string$(14,chr$(196));chr$(191);
  Print " ";chr$(218);string$(10,chr$(196));chr$(191);
  Print " ";chr$(218);string$(14,chr$(196));chr$(16);" ";
  Locate ,TCol: print chr$(183);
  Color 7,1

  FstData=CursorY-1  ' Start of Files section ... (see HiLite routines)

While DirDataTab(I)<>"::EOF" and B<=SizeS
  A=DirDataTab(I): B=B+1: I=I+1
  Color 6
  Print chr$(186);
  If instr(a,"<DIR>")>0 then Color 6 else Color 7
  Print " ";Using$("####",I-1);
  If instr(a,"<DIR>")>0 then Color 11 else Color 15
  Print "  ";

  If len(A)>=TCol-20 then Print mid$(A,1,TCol-22);: else Print A;
  Color 6,1
  If len(A)<TCol-20 and LLen<TCol-20 then 
    Print string$(LLen-len(A),250);
   Else
    Print string$(TCol-22-len(A),250);
  End if

    GoSub DoAttr  ' Display ARHS attributes
    If instr(a,"<DIR>")>0 then Color 6 else Color 7
    Print "  ";Using$("####",I-1);" ";
    Color 6: locate ,TCol: print chr$(186);
  Color 7

BadData:
Wend  

KK=I-K-1
CY=CursorY
If I>=T then
  Color 6
  Print String$(TCol,chr$(205));
  Locate CY,9: Print chr$(181);
  Locate CY,54: Print chr$(198);
  Locate ,TCol: Print chr$(188);
End if

Color 6,1
Locate CY,1
Print chr$(200);
Color 7,1
If I>=T then
  Locate CY,10
  Color 6: Print " Total sub-dir alloc= ";
  Color 14,0: Print TBytes;: Color 6,1: Print " ";
  Locate CY,47: Color 6: Print " bytes ";
 Else
  Color 6: Locate CY,52
  Print "  . . .  ";chr$(25);"  c o n t i n u e d  ";chr$(25);
  Locate ,TCol: print chr$(188);
End if

Color 7,1

If H<1 or H>=T or H>SizeS OR H>T-K then H=1 ' Reset current line?

Gosub HiLite

GetFunc:

Locate RLeg+1,1
Color 0,3
Print space$((TCol*4)-0);

locate RLeg+1,2
L=0: If TCol>132 then L=(TCol-132)/2
Print space$(L);" Scroll=PageUp/Down/Home/End/";chr$(24);chr$(25);
Print "  INS/Chars=CMD  F2=Exec  SPace=Mark  F8=Do Marks ";
Print " F9=Drive  Home=Reset All  F3=Exit  Alt+V=VTerm"

Print space$(L);"   Attr:F5=R/O, F6=HIDden, F7=SYStem  Sort:F10=Name, ";
Print "F11=Date, F12=Size  Enter/";chr$(16);"=EDit or CD <DIR> ";
Print " Esc/";chr$(17);"=CD ..  F4=View  Tab=+10";

GoSub DoHelp

DoKeyNext:                              ' Idle loop ********

Locate SizeS+9,1
Cursor OFF
C=""
While C=""
 Gosub DOTOD
 If Instat=0 then Sleep 45 else C=Inkey$: BitShift=INSHIFT
WEnd
Cursor ON, 100

Stackit:
If len(C)=2 then
  if mid$(C,2,1)=chr$(72) then   ' Up arw
    If Bit(BitShift,1) or Bit(BitShift,4) then ' Left-Alt or Shift?
      H=1: goto Startup
    end if
    H=H-1
    If H=0 then
      If K>1 then H=SizeS else H=1
      goto DoPrev
    End if
    Gosub HiLite: goto GetFunc
  end if
  if mid$(C,2,1)=chr$(80) then   ' Down arw
    If Bit(BitShift,1) or Bit(BitShift,4) then ' Left-Alt or Shift?
      H=SizeS: If H>KK then H=KK+1
      gosub HiLite: goto GetFunc
    end if
    H=H+1: If H>SizeS or H+K>T then H=1: goto DoNext
    Gosub HiLite: goto GetFunc
  end if
  if mid$(C,2,1)=chr$(75) then ChDir "..": H=1: K=1: goto Startup  ' Left arw = ..
  if mid$(C,2,1)=chr$(77) then goto Parse13  ' Right arw = Enter simulation (EDit)
  if mid$(C,2,1)=chr$(62) then goto doglogg  ' F4 = View
  if mid$(C,2,1)=chr$(81) then goto DoNext  ' PageDown
  if mid$(C,2,1)=chr$(73) then goto DoPrev  ' PageUp
  if mid$(C,2,1)=chr$(71) then SFLag="N": H=1: K=1: goto Startup  ' Home/Reset
  if mid$(C,2,1)=chr$(82) then A$="": goto DoCmd  ' Insert
  if mid$(C,2,1)=chr$(60) then C="== ": goto Execit  ' F2=Exec
  if mid$(C,2,1)=chr$(38) then C="cmd /c LTFViewR5U.Exe ==": goto Execit  ' Alt+L=Large 
  if mid$(C,2,1)=chr$(25) then C="cmd /c EditPadLite7.Exe ==": goto Execit  ' Alt+P=PadEdit
  if mid$(C,2,1)=chr$(30) then C="cmd /c dvwview \dos\dv3\dvnotea": goto Execit  ' Alt+A=ASCII
  if mid$(C,2,1)=chr$(18) then C="cmd /c dvwview \dos\dv3\dvnotee": goto Execit  ' Alt+E=Electronics
  if mid$(C,2,1)=chr$(37) then C="cmd /c dvwview \dos\dv3\dvnotek": goto Execit  ' Alt+K=Scans
  if mid$(C,2,1)=chr$(19) then C="cmd /c dvwview \dos\dv3\dvnoter": goto Execit  ' Alt+R=Resistors
  if mid$(C,2,1)=chr$(17) then C="cmd /c dvwview \dos\dv3\dvnotew": goto Execit  ' Alt+W=Wire
  if mid$(C,2,1)=chr$(35) then shell "cmd /c dvwhelp": goto Startup  ' Alt+H=DVWHelp.Bat
  if mid$(C,2,1)=chr$(47) then shell "Cmd /T:1F /K Cls & Set|more & Ver": goto Startup  ' Alt+V=VTerm
  if mid$(C,2,1)=chr$(44) then shell "cmd /c dvwuserz": goto Startup  ' Alt+Z=DVWUserZ.Bat
  if mid$(C,2,1)=chr$(34) then gosub DVWGen: goto Startup ' Alt+G=Generate DVWin
  if mid$(C,2,1)=chr$(46) then gosub colortab: goto startup ' Alt+C = Colors
  if mid$(C,2,1)=chr$(31) then gosub KeyScan: goto startup ' Alt+S = ScanKeys
  If mid$(C,2,1)=chr$(20) then
    if TFlag=0 then TFlag=1 else TFlag=0 ' Alt+T = Toggle Typing
    goto Startup
  End if
  If mid$(C,2,1)=chr$(23) then 
    If instr(DirDataTab(K+H-1),"<DIR>")>0 then beep: goto getfunc
    C="cmd /c DVWUserI.Bat =="
    goto Execit ' Alt+I = Inspect
  End if
  if mid$(C,2,1)=chr$(83) then goto DoKill  ' Delete
  if mid$(C,2,1)=chr$(61) then goto Enditall  ' F3 = Exit
  if mid$(C,2,1)=chr$(45) then goto Enditall  ' Alt+X = Exit
  if mid$(C,2,1)=chr$(79) then K=T-SizeS: goto Startup  ' End
  if mid$(C,2,1)=chr$(66) then goto DoMarked  ' F8=Do marks
  if mid$(C,2,1)=chr$(68) then SFlag="N": goto Startup  ' F10=Name sort
  if mid$(C,2,1)=chr$(87) then SFlag="-D": goto Startup  ' F11=-Date sort
  if mid$(C,2,1)=chr$(88) then SFlag="-S": goto Startup  ' F12=-Size sort
  if mid$(C,2,1)=chr$(63) then SFlag="N /A:R": goto Startup  ' F5=R/O files
  if mid$(C,2,1)=chr$(64) then SFlag="N /A:H": goto Startup  ' F6=Hidden files
  if mid$(C,2,1)=chr$(65) then SFlag="N /A:S": goto Startup  ' F7=System files
end if

If C="+" then shell "cmd /c DVWPlus.Bat": goto Startup ' +=GoldEd+

If C=Chr$(9) then H=H+(SizeS/4): Gosub HiLite: goto GetFunc ' Tab

If len(C)=2 and mid$(C,2,1)=chr$(67) then  ' F9=Drive
 Print "Select DRIVE {";DrvList;", or Enter to Abort} ";

Getdrv:
 Locate ,36+Len(DrvList)
 A=""
 While A=""
  Gosub DOTOD
  If Instat=0 then Sleep 45 else A=Inkey$
 WEnd

 If A=Chr$(13) then goto Startup
 If len(A)<>1 then Beep: goto Getdrv
 If Asc(A)>=97 and Asc(A)<=122 then A=Chr$(Asc(A)-32)  ' force Uppercase
 If Asc(A)<65 or Asc(A)>90 then Beep: goto Getdrv

 On Error Resume Next
 ChDrive A
 If Err=76 then Errclear: Beep: Print " *** Error: Drive ";A;": does NOT exist! ";: goto Getdrv

 On Error goto 0
 Goto Startup
End if

If C=" " then  ' Space bar = mark
  If Mid$(DirDataTab(K+H-1),52,1)=" " then
    Mid$(DirDataTab(K+H-1),52,1)=chr$(251)
   else
    Mid$(DirDataTab(K+H-1),52,1)=" "
  End if
  If H<SizeS and H+K<T then H=H+1
  Gosub HiLite
 goto GetFunc
End if

if C=chr$(13) then  ' ENTER key = ChDir if <DIR>, else Edit

Parse13:
 A=Trim$(Mid$(DirDataTab(K+H-1),53))
 Z=Trim$(Mid$(DirDataTab(K+H-1),40,12))

 If Instr(DirDataTab(K+H-1),"<DIR>")>0 then
   ChDir A: H=1: K=1
   Goto Startup
  Else
   If Z<>"" then A=Z  ' prefer SFN
   Shell "Cmd /c "+EFlag+A  'Edit file
  goto Startup
 End if  
End if

if C=chr$(10) then  ' Ctrl+ENTER key = Win Start or ChDir

Parse10:
 A=Trim$(Mid$(DirDataTab(K+H-1),53))
 Z=Trim$(Mid$(DirDataTab(K+H-1),40,12))

   If Z<>"" then
     A=chr$(34)+A+chr$(34)+" "+Z  ' prefer SFN with LFN Title
   end if
   Shell "cmd /c start "+A  ' Win Start file
   goto Startup
End if

If C=Chr$(27) then
 If SubDir="C:\" then goto ENDITALL
 ChDir "..": H=1: K=1: goto Startup
End if

If len(C)=2 then beep:beep:beep: goto GetFunc

A$=C$: goto docmd ' start typing, first char re-keyed (A$)

Goto ENDITALL ' trap, never gets here

'* * * * * * * * * * * * * * * * Sub-routines * * * * * * * * * * * * * * * *

DoAttr:
    Saveit=A
    A=Trim$(Mid$(Saveit,53))
    Z=Trim$(Mid$(Saveit,40,12))
    If Z<>"" then A=Z

    FAttr=GetAttr(A): A=Saveit
    CX=CURSORX
    If CX<78 then Color 6: Print string$(78-CX,250);
    Print "  ";

    AC=CursorX
    If (FAttr and 32)=32 then Color 7: Print "A"; else Color 6: Print chr$(250);
    If (FAttr and 1)=1 then Color 14: Print "R"; else Color 6: Print chr$(250);
    If (FAttr and 2)=2 then Color 4: Print "H"; else Color 6: Print chr$(250);
    If (FAttr and 4)=4 then Color 4: Print "S"; else Color 6: Print chr$(250);

    If (FAttr and 2)=0 and (FAttr and 4)=0 and (FAttr and 1)=1 then
      Locate ,AC+1: Color 14: Print "R/O";
    End if
    If (FAttr and 1)=0 and (FAttr and 4)=0 and (FAttr and 2)=2 then
      Locate ,AC+1: Color 4: Print "Hid";
    End if

    If instr(A,"<DIR>")>0 then Color 6 else Color 7
  Return

DVWGen:
  If not isfile("dvwgen.bat") then beep: return
  Shell "cmd /c start dvwgen.bat"
  Goto Enditall
Return

DoNext:
  B=1
  I=K+SizeS
  If I>=T Then I=1
  Goto NextScr

DoPrev:
  B=1
  If K>SizeS then I=K-SizeS else I=1
  Goto NextScr

PeekBas:

 A=ucase$(DirDataTab(K+H-1))
 If Instr(A,"<DIR>")>0 then return
 If Instr(A,".LOG")>0 then goto PeekOk
 If Instr(A,".TXT")>0 then goto PeekOk
 If Instr(A,".BAS")>0 then goto PeekOk
 If Instr(A,".ASM")>0 then goto PeekOk
 If Instr(A,".PAS")>0 then goto PeekOk
 If Instr(A,".PL")>0 then goto PeekOk
 If Instr(A,".MTX")>0 then goto PeekOk
 If Instr(A,".HTM")>0 then goto PeekOk
 If Instr(A,".BBS")>0 then goto PeekOk
 If Instr(A,".RPT")>0 then goto PeekOk
 If Instr(A,".CSV")>0 then goto PeekOk
 If Instr(A,".INI")>0 then goto PeekOk
 If Instr(A,".BAT")>0 then goto PeekOk
 If Instr(A,".CFG")>0 then goto PeekOk
 If Instr(A,".LST")>0 then goto PeekOk
 If Instr(A,".ASC")>0 then goto PeekOk
 If Instr(A,".PKT")>0 then goto PeekOk
 If Instr(A,".MSG")>0 then goto PeekOk
 If Instr(A,".XML")>0 then goto PeekOk
 If Instr(A,".ION")>0 then goto PeekOk
 If Instr(A,".FAQ")>0 then goto PeekOk
 If Instr(A,".MHT")>0 then goto PeekOk
 If Instr(A,".REG")>0 then goto PeekOk
 If Instr(A,".XED")>0 then goto PeekOk
 If Instr(A,".WCC")>0 then goto PeekOk
 If Instr(A,".DIZ")>0 then goto PeekOk
 If Instr(A,".REF")>0 then goto PeekOk
 If Instr(A,".FAR")>0 then goto PeekOk
 If Instr(A,".INC")>0 then goto PeekOk
 Print string$(TCol*7," ");
 Locate SizeS+9,1
 Return

PeekOk:
 A=Trim$(Mid$(DirDataTab(K+H-1),53)): ZZ=A  ' LFN
 Z=Trim$(Mid$(DirDataTab(K+H-1),40,12))
 If Z<>"" then A=Z  ' Prefer SFN
 Open A for input as #1

 Print string$(TCol*7," ");
 Locate SizeS+9,1
 Color 10,0
 Print "  File: ";mid$(ZZ,1,TCol-9);" "

 For L=1 to 6
Peekskip:
   Line input#1,A
   If not eof(1) and (A="" or A=" ") then goto Peekskip
   If A<>"" then
     Locate SizeS+9+L,1
     Print "    ";mid$(A,1,TCol-5);" ";
   End if
   If eof(1) and L<6 then
     locate CursorY+1,1: print " -=:{ EoF }:=- ";: L=6
   End if
Peekend:
 Next L

 Close 1
 Color 7,1
Return

DoGlogg:  ' F4 = View file
 A=Trim$(Mid$(DirDataTab(K+H-1),53))
 Z=Trim$(Mid$(DirDataTab(K+H-1),40,12))

 If Instr(DirDataTab(K+H-1),"<DIR>")>0 then
   Beep
   Goto Startup
  Else
   If Z<>"" then A=Z
   Shell "Cmd /c "+VFlag+A
  goto Startup
 End if  
Return

DoMarked:

Locate CursorY,1: CY=CursorY
Print space$(TCol*5);
Locate CY+1,1
Print "Notes: == will INSERT each Marked directory/file NAME(s) in the current line (i.e. COPY == ==2 or DEL ==) "
Print "DOS command? {or ENTER to Abort} "
Print
Print " > ";

CX=CURSORX: CY=CURSORY
If (TCol*2)-3 > 255 then buffer=space$(255) else buffer=space$((TCol*2)-3)

retVal = TextInput(TCol, CurFile, buffer, CY, CX, history1())

C=Trim$(buffer)
If C="" goto Startup

CC=C
II=1
Cls

NxtMark:
While II<T and mid$(DirDataTab(II),52,1)=" ": II=II+1: WEnd
If II=T goto EndMark

C=CC
While instr(C,"==")>0
  L=instr(C,"==")
  B=II  ' Current line
  If B<=0 or B>=T then goto Startup  ' Abort, bad Rec #

  A=DirDataTab(B)
  If len(A)>39 and mid$(A,40,1)<>" " then A=trim$(mid$(A,40,12)) _ ' Prefer SFN
   else If len(A)>52 and mid$(A,53,1)<>" " then A=mid$(A,53) else A=""  ' Extract LFN
  If A="" then II=II+1: goto NxtMark

  Z=mid$(C,L+2)
  C=mid$(C,1,L-1)+A+Z
wend  

Color 7: Print "Executing [";
Color 14: Print C;
Color 7: Print "] ..."
Shell "Cmd /c "+C
II=II+1
Goto NxtMark

EndMark:
Print
Color 6: Print String$(TCol,"_")
Color 15: Beep: Print "Press ENTER Key to continue ...";: Input A
Color 7: A=""

Goto Startup

DoHelp:
  Locate RLeg+3,3
  Print space$(L);" Alt+A=ASCII +C=Colors +E=Electronics +H=DVWHelp";
  Print " +K=Kybd-Codes +R=Resistors +S=Scan-Codes +W=Wire +Z=DVWUserZ ";
  Print "+";chr$(24);"=Top +";chr$(25);"=Bottom ";

  Locate RLeg+4,3
  Print space$(L);"   Alt+P=editPadLite7 +L=LTFViewR5U +G=dvwGen +I";
  Print "=Inspect +T=Type On/Off Ctrl+Enter=Start or CD  ";
  Print "   ";chr$(192);chr$(16);" ";ZDesc;" ";
  Print " +X=eXit    "; 

 Return

DoKill:
 L=instr(DirDataTab(K+H-1),"<DIR>")
 If L>0 then Beep: goto Getfunc

 A=Trim$(Mid$(DirDataTab(K+H-1),53))  ' Parse LFN
 C=A
 Z=Trim$(Mid$(DirDataTab(K+H-1),40,12))  ' Parse SFN
 If Z<>"" then A=Z  ' Prefer SFN

 Color 4,0: Print "Delete";
 color 7: Print " file ";
 Color 11: Print C;
 Color 7: Print " {";
 Color 11: Print Z;
 Color 7: Print "} ";
 Color 11: Print "???";
 Color 7: Print " [Y=Yes] ";
 C=Waitkey$
 If C="y" or C="Y" then Kill A else Beep
goto Startup

HiLite:

 If H<1 then H=1
 If H>SizeS then H=SizeS
 If DirDataTab(K+H-1)="::EOF" or K+H-1>=T then K=(int(T/SizeS)*SizeS)+1: goto Startup

 If HH>=1 and HH<=SizeS and K+HH-1<T then  ' Undo HiLite
  Locate FstData+HH,1
  Color 7,1
  Print Space$(TCol);
  Locate FstData+HH,1
  F=DirDataTab(K+HH-1)
  Color 6
  Print chr$(186);
  If instr(F,"<DIR>")>0 then Color 6 else Color 7
  Print " ";Using$("####",K+HH-1);
  If instr(F,"<DIR>")>0 then Color 11 else Color 15
  Print "  ";

  If len(F)>=TCol-20 then Print mid$(F,1,TCol-22);: else Print F;
  Color 6
  If len(F)<TCol-20 and LLen<TCol-20 then 
    Print string$(LLen-len(F),250);
   Else
    Print string$(TCol-22-len(F),250);
  End if

    A=F
    GoSub DoAttr  ' Display ARHS attributes
    If instr(F,"<DIR>")>0 then Color 6 else Color 7
    Print "  ";Using$("####",K+HH-1);
    Color 6: Locate ,TCol: print chr$(186);
  Color 7
 End if

Redoit:  ' HILite current line
 Locate FstData+H,1
 Color 14,3
 Print Space$(TCol);
 Locate FstData+H,1
 Print chr$(186);" ";Using$("####",K+H-1);
 F=DirDataTab(K+H-1)
 Print "  ";
 CurFile=F

  If len(F)>=TCol-20 then Print mid$(F,1,TCol-22);: else Print F;
  If len(F)<TCol-20 and LLen<TCol-20 then 
    Print string$(LLen-len(F),250);
   Else
    Print string$(TCol-22-len(F),250);
  End if

    A=F
    GoSub DoAttr
    Print "  ";Using$("####",K+H-1);
    Color 6: locate ,TCol: print chr$(186);

 CY=CursorY
 Locate SizeS+9,1
 Color 7,1
 If TFlag then
   gosub PeekBas ' Type file ON
'  else
'   Print string$(TCol*7," ");  ' Type OFF
'   Locate RLeg-2,3
'   Color 15,0
'   Print Trim$(Mid$(DirDataTab(K+H-1),53));
 End if

 Color 7,1
 Locate CY,1

 HH=H  ' Set prev Hilite Row

Return

DoCmd:

Locate CursorY,1: CY=CursorY
Print space$(TCol*5);
Locate CY+1,1
Print "Notes: == Inserts directory/file NAME.EXT in current line,  =. to Insert file NAME.,  .= to Insert file .EXT "
Print "  DOS command? {or ENTER to abort}                          Alt+L Copies LONG File Name to entry area at cursor "
Print
Print " > ";

CX=CURSORX: CY=CURSORY
If (TCol*2)-3 > 255 then buffer=space$(255) else buffer=space$((TCol*2)-3)
If A<>"" then buffer=A+Mid$(buffer,2) ' pass 1st key ...

retVal = TextInput(TCol, CurFile, buffer, CY, CX, history1())
C=Trim$(buffer)

Execit:
If C="" goto Startup
CC=C

If UCase$(mid$(C,1,3))="CD " then Chdir mid$(C,4): Goto Startup
If UCase$(mid$(C,1,6))="CHDIR " then Chdir mid$(C,7): Goto Startup

While instr(C,"==")>0
  L=instr(C,"==")
  B=K+H-1  ' Current line
  If B<=0 then goto Startup  ' Abort, bad Rec #

  If B<T then A=DirDataTab(B) else A=""
  If len(A)>39 and mid$(A,40,1)<>" " then A=trim$(mid$(A,40,12)) _ ' Prefer SFN
   else If len(a)>52 and mid$(A,53,1)<>" " then A=mid$(A,53) else A=""  ' Extract LFN

  Z=mid$(C,l+2): M=instr(Z," ")
  If M>0 then Z=mid$(Z,M) else Z=""
  C=mid$(C,1,L-1)+A+Z
wend  

While instr(C,"=.")>0
  L=instr(C,"=.")
  B=K+H-1  ' Current line
  If B<=0 then goto Startup  ' Abort, bad Rec #

  If B<T then A=DirDataTab(B) else A=""
  If len(A)>39 and mid$(A,40,1)<>" " then A=trim$(mid$(A,40,12)) _ ' Prefer SFN
   else If len(a)>52 and mid$(A,53,1)<>" " then A=mid$(A,53) else A=""  ' Extract LFN
  If A<>"" then
    for ll=len(a) to 1 step -1
      if mid$(a,ll,1)="." then A=mid$(A,1,ll-1): ll=1
    next ll
  End if
  Z=mid$(C,l+2)
  If Z="" then C=mid$(C,1,L-1)+A else C=mid$(C,1,L-1)+A+"."+Z
wend

While instr(C,".=")>0
  L=instr(C,".=")
  B=K+H-1  ' Current line
  If B<=0 then goto Startup  ' Abort, bad Rec #

  If B<T then A=DirDataTab(B) else A=""
  If len(A)>39 and mid$(A,40,1)<>" " then A=trim$(mid$(A,40,12)) _ ' Prefer SFN
   else If len(a)>52 and mid$(A,53,1)<>" " then A=mid$(A,53) else A=""  ' Extract LFN
  If A<>"" then
    for ll=len(a) to 1 step -1
      if mid$(a,ll,1)="." then A=mid$(A,ll+1): ll=1
    next ll
  End if
  Z=mid$(C,l+2)
  C=mid$(C,1,L-1)+"."+A+Z
wend

While instr(C,"::")>0
  L=instr(C,"::")
  B=val(mid$(c,l+2))
  If B=0 then goto Startup  ' Abort, bad Rec #

  If B<T then A=DirDataTab(B) else A=""
  If len(A)>39 and mid$(A,40,1)<>" " then A=trim$(mid$(A,40,12)) _ ' Prefer SFN
   else If len(a)>52 and mid$(A,53,1)<>" " then A=mid$(A,53) else A=""  ' Extract LFN

  Z=mid$(C,l+2): M=instr(Z," ")
  If M>0 then Z=mid$(Z,M) else Z=""
  C=mid$(C,1,L-1)+A+Z
wend  

Cls
Color 7: Print " ";TIME$;" Executing  -=:{ ";
Color 14: Print C;
Color 7: Print " }:=- "
TStore=TIME$
Shell "Cmd /c "+C
If mid$(C,1,3)="cd " or mid$(C,1,3)="CD " then K=1
If mid$(C,1,6)="chdir " or mid$(C,1,6)="CHDIR " then K=1

Print
Color 6: Print chr$(212);string$(TCol-2,chr$(205));chr$(190);
If instr(command$,"/B") then beep: sleep 175: beep: sleep 175: rem beep
Color 15: Print " ";TStore;" to ";TIME$;
Print " Press {ENTER} Key to continue ...";: Input A
Color 7: A=""

Goto Startup

Goto ENDITALL

NewScr:
  Color 7,1
  Cls
  Color 1,7: Print Space$(TCol*2);
200
  Locate 1,2
  Print PgmVer;
  Color 9,7: Locate ,Int((TCol - LEN(PNAME$)) / 2): Print PNAME$;

  Locate 2,TCol-Len(CName)-17
  Color 6,7: Print "Machine: ";
  Color 9,7: Print CName; 

  Locate 1,1+len(PgmVer)+8
  Color 9,7: Print CPUs; 
  Color 6,7: Print " CPUs ";

  Locate 2,(TCol-20)/2
  Color 5: Print " {c} ";mid$(date$,7,4);" Ben Ritchey ";
  Color 1

  Z=Z+MONTHTAB(VAL(MID$(DATE$,1,2)))+STR$(VAL(MID$(DATE$,4,2)))
  A="th": AA=VAL(MID$(DATE$,4,2))
  IF AA=1 OR AA=21 OR AA=31 THEN A="st"
  IF AA = 2 OR AA = 22 THEN A = "nd"
  IF AA = 3 OR AA = 23 THEN A = "rd"
  Z = Z + A + ", " + MID$(DATE$, 7, 4): AA=127-LEN(Z)
  Locate 1,(TCol-34): Print Z;"  ";: Z=""

  GOSUB DOTOD
  Color 7,1
 Return

DOTOD:
  CX=CURSORX: CY=CURSORY
  Z=TIME$: AA=VAL(MID$(TIME$, 1, 2))
  IF AA>11 THEN Z=Z+" Pm" ELSE Z=Z+" Am"
  IF AA>12 THEN
    A=MID$(STR$(AA-12),2)
    IF LEN(A)=1 THEN A=" "+A
    MID$(Z,1)=A: A=""
  END IF
   Locate 1,(TCol-13): Color 0,7: PRINT " ";Z;"  ";: Z=""
  LOCATE CY,CX
  Color 7,1
 RETURN

KeyScan:
  Cls
  Print
       PRINT "Press any key combination ... [Code 13=ENTER] to exit"
ScanNxt:
  A="": Print
       While A="": If InStat then A$=INKEY$ else sleep 45: gosub dotod: WEnd
       IF A$=CHR$(13) THEN return
       IF LEN(A$)=1 THEN 
         Print "  1 byte Scan code: ";
         PRINT "[";ASC(A$);"Dec, ";
         PRINT Hex$(ASC(A$));" Hex, ";chr$(34);A;chr$(34);" ASCII ]"
       End if
       IF LEN(A$)=2 THEN 
         Print "  2 byte Control codes: ";
         PRINT "[";ASC(LEFT$(A$,1));" + ";ASC(RIGHT$(A$,1));"Dec, ";
         PRINT Hex$(ASC(LEFT$(A$,1)));" + ";Hex$(ASC(RIGHT$(A$,1)));" Hex ]"
       End if
       IF LEN(A$)>=3 THEN 
         Print "n/a"
         Print CHR$(7);"* Error: Length = ";Len(A);" bytes!"
       End if
  GOTO ScanNxt
 Return

ColorTab:
cls
Print "Enter Background color (0-15): ";: input AA

Color l,aa
Print space$(74)

for l=0 to 15
  Color l,aa
  Print " ";string$(72,chr$(219));" "
  Print " ";string$(72,chr$(219));" ";
  Color 15,1: Print l
  Color l,aa
  Print space$(74)
 next l

Color 7,1
Print
Print "Press any key ...";
A=""
while A$=""
  Gosub dotod
  If INSTAT then A$=INKEY$ else sleep 45
WEND

return

ENDITALL:
RC=ERR
End RC

REM Month Table Data
DATA January, February, March, April, May, June
DATA July, August, September, October, November, December

END FUNCTION

' TextInput function (c) 2005 Geo Massar with mods by Ben Ritchey

FUNCTION TextInput(TCol as Long, CurFile AS STRING, Buffer AS STRING, _
              Row AS LONG, Col AS LONG, History() AS STRING) AS LONG
  STATIC InsertOff    AS LONG
  LOCAL  BufferLen    AS LONG
  LOCAL  MyKey        AS STRING
  LOCAL  KeyVal       AS LONG
  LOCAL  ColorDefault AS BYTE
  LOCAL  CurPos       AS LONG
  LOCAL  HistoryIdx   AS LONG
  Local A, Z as String
  Local AA, CX, CY as Long

  BufferLen = LEN(Buffer)
  HistoryIdx = UBOUND(History)                 'index to the empty slot
  ColorDefault = SCREENATTR(CURSORY,CURSORX)
  LOCATE Row, Col : COLOR 15, 1, BufferLen     'bright white against dark blue
  If mid$(Buffer,1,1)<>" " then
    PRINT String$(BufferLen,chr$(249));
    LOCATE Row, Col: print mid$(buffer,1,1);
    CurPos=2: LOCATE Row, Col+1
   else
    PRINT String$(BufferLen,chr$(249));
    CurPos=1: LOCATE Row, Col
  End if

  DO
    MyKey=""
    While MyKey=""
     If Instat=0 then Sleep 45 else MyKey=Inkey$
    WEnd

    SELECT CASE LEN(MyKey)
      CASE 1    : KeyVal = ASC(MyKey)
      CASE 2    : KeyVal = - ASC(MyKey, 2)
      CASE ELSE : ITERATE
    END SELECT

    SELECT CASE KeyVal
      CASE 13, 9  'Enter, Tab
        COLOR ColorDefault MOD 16, ColorDefault \ 16
        FUNCTION = KeyVal
        IF TRIM$(Buffer) = "" THEN EXIT FUNCTION
        HistoryIdx = UBOUND(History)
        IF HistoryIdx > 0 THEN IF Buffer = History(HistoryIdx-1) THEN EXIT FUNCTION
        History(HistoryIdx) = Buffer
        INCR HistoryIdx
        REDIM PRESERVE History(HistoryIdx)
        History(HistoryIdx) = SPACE$(BufferLen)  'new slot for the next entry
        EXIT FUNCTION

      CASE 27  ' ESCape
        If CurPos=1 then 
          If Buffer="" then Exit Function
          LOCATE Row, Col
          PRINT Space$(Len(Buffer));
          Buffer=""
         else
          CurPos=1
        End if

      CASE -72  'Up
        IF HistoryIdx > 0 THEN  DECR HistoryIdx
        Buffer = History(HistoryIdx)
        CurPos = LEN(RTRIM$(Buffer)) + 1

      CASE -80  'Down
        IF HistoryIdx < UBOUND(History) THEN INCR HistoryIdx
        Buffer = History(HistoryIdx)
        CurPos = LEN(RTRIM$(Buffer)) + 1

      CASE -82 'Insert
       IF InsertOff THEN
         InsertOff = 0
         CURSOR ON, 1
       ELSE
         InsertOff = 1
         CURSOR ON, 100
       END IF

      CASE 8 'BackSpace
       IF CurPos > 1 THEN
         DECR CurPos
         Buffer = LEFT$(Buffer, CurPos - 1) & MID$(Buffer, CurPos + 1) & " "
       END IF

      CASE -75 'Left
       IF CurPos > 1 THEN DECR CurPos

      CASE -77 'Right
       IF CurPos < BufferLen THEN INCR CurPos

      CASE -71 'Home
       CurPos = 1

      CASE -79 'End
       CurPos = MIN(LEN(RTRIM$(Buffer)) + 1, BufferLen)

      CASE -83 'Del
       Buffer = LEFT$(Buffer, CurPos - 1) & MID$(Buffer, CurPos + 1) & " "

      CASE -38 'Alt+L = LFN Insert
       IF len(CurFile) > 52 and mid$(CurFile, 53, 1) <> " " then  ' Extract LFN
         MyKey = mid$(CurFile, 53)
        ELSE
         MyKey=""
       END IF
       Buffer = LEFT$(Buffer, CurPos - 1) & chr$(34) & MyKey & chr$(34) _
                  & " " & MID$(Buffer, CurPos, BufferLen - CurPos)
       CurPos = CurPos + len(MyKey) + 3

      CASE 32 TO 255 'Valid charaters
       IF InsertOff THEN
         MID$(Buffer, CurPos, 1) = MyKey
       ELSE
         Buffer = LEFT$(Buffer, CurPos - 1) & MyKey & MID$(Buffer, CurPos, BufferLen - CurPos)
       END IF
       IF CurPos < BufferLen THEN INCR CurPos

      CASE ELSE
       ITERATE
    END SELECT

    LOCATE Row, Col
    PRINT Buffer;
    LOCATE Row, Col + CurPos - 1
  LOOP

DOTOD:
  CX=CURSORX: CY=CURSORY
  Z=TIME$: AA=VAL(MID$(TIME$, 1, 2))
  IF AA>11 THEN Z=Z+" Pm" ELSE Z=Z+" Am"
  IF AA>12 THEN
    A=MID$(STR$(AA-12),2)
    IF LEN(A)=1 THEN A=" "+A
    MID$(Z,1)=A: A=""
  END IF
  Cursor OFF
  Locate 1,(TCol-13): Color 0,7: PRINT " ";Z;"  ";: Z=""
  LOCATE CY,CX
  Cursor ON, 100
  Color 7,1
 RETURN

END FUNCTION

' End of DVWin.Bas
