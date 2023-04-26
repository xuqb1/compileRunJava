'==============================================================================
'
' 程序说明
'
'   所属项目：
'   文件名：
'   功能说明：
'   逻辑描述：
'   文件版本：
'   创建时间：
'   修改时间：
'
'==============================================================================
#COMPILER PBWIN 9
#COMPILE EXE
#DIM ALL

'------------------------------------------------------------------------------
' 初始声明 - 移除COMMCTRL.INC中不需要的函数
'------------------------------------------------------------------------------
%NOANIMATE        = 1  ' Animate control
%NOBUTTON         = 1  ' Button
%NOCOMBO          = 1  ' Combo box
%NOCOMBOEX        = 1  ' ComboBoxEx
%NODATETIMEPICK   = 1  ' Date/time picker
%NODRAGLIST       = 1  ' Drag list control
%NOEDIT           = 1  ' Edit control
%NOFLATSBAPIS     = 1  ' Flat scroll bar
%NOHEADER         = 1  ' Header control
%NOHOTKEY         = 1  ' HotKey control
%NOIMAGELIST      = 1  ' Image APIs
%NOIPADDRESS      = 1  ' IP Address edit control
%NOLIST           = 1  ' List box control
%NOLISTVIEW       = 1  ' ListView control
%NOMENUHELP       = 1  ' Menu help
%NOMONTHCAL       = 1  ' MonthCal
%NOMUI            = 1  ' MUI
%NONATIVEFONTCTL  = 1  ' Native Font control
%NOPAGESCROLLER   = 1  ' Pager
%NOPROGRESS       = 1  ' Progress control
%NOREBAR          = 1  ' Rebar control
%NOSTATUSBAR      = 1  ' Status bar
%NOTABCONTROL     = 1  ' Tab control
%NOTOOLBAR        = 1  ' Tool bar
%NOTOOLTIPS       = 1  ' Tool tips
%NOTRACKBAR       = 1  ' Track bar
%NOTRACKMOUSEEVENT = 1 ' Track Mouse Event
%NOTREEVIEW       = 1  ' TreeView
%NOUPDOWN         = 1  ' Up Down arrow control


'------------------------------------------------------------------------------
' 头文件
'------------------------------------------------------------------------------
%USEMACROS = 1

#RESOURCE "CompileRunJava.pbr"
#INCLUDE "Win32API.inc"
#INCLUDE "CommCtrl.inc"
#INCLUDE "InitCtrl.inc"


'------------------------------------------------------------------------------
' ID变量声明
'------------------------------------------------------------------------------
%IDC_PRJNAMELB      = 101
%IDC_PRJNAMETB      = 102
%IDC_BASEDIRLB      = 103
%IDC_BASEDIRTB      = 104
%IDC_BASEDIRSELBTN  = 105
%IDC_SRCLB          = 106
%IDC_SRCTB          = 107
%IDC_SRCSELBTN      = 108
%IDC_LIBLB          = 109
%IDC_LIBTB          = 110
%IDC_LIBSELBTN      = 111
%IDC_ANTLB          = 112
%IDC_ANTTB          = 113
%IDC_ANTSELBTN      = 114
%IDC_JDKLB          = 115
%IDC_JDKTB          = 116
%IDC_JDKSELBTN      = 117

%IDC_MAINLB         = 118
%IDC_MAINTB         = 119
%IDC_MAKEBUILDBTN   = 120 '创建build.xml文件按钮
%IDC_COMPILEBTN     = 121 '编译按钮
%IDC_RUNBTN         = 122 '运行按钮

%IDC_SAVESETTINGBTN = 123 '保存配置
%IDC_ENCODELB       = 124 '编译编码
%IDC_ENCODECB       = 125 '编译编码

%IDC_ARGLB          = 126
%IDC_ARGTB          = 127
%IDC_ARGVALLB       = 128
%IDC_ARGVALTB       = 129
'------------------------------------------------------------------------------
GLOBAL iniFileName  AS STRING

'------------------------------------------------------------------------------
' 应用程序主入口点
'------------------------------------------------------------------------------
FUNCTION PBMAIN () AS LONG

  LOCAL hDlg      AS DWORD
  LOCAL tmpArr()  AS STRING
  LOCAL tmpStr    AS STRING

  tmpStr="gbk,utf-8"
  REDIM tmpArr(1)
  PARSE tmpStr,tmpArr(),","
  'InitComCtl32(%ICC_ANIMATE_CLASS)

  DIALOG NEW 0, "Java编译运行",,, 245, 180, %WS_CAPTION OR %WS_SYSMENU OR %WS_THICKFRAME, _
    0 TO hDlg
  CONTROL ADD LABEL,   hDlg, %IDC_PRJNAMELB,      "项目名称:",    5, 6, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_PRJNAMETB,      "",             45, 5, 158, 12

  CONTROL ADD LABEL,   hDlg, %IDC_BASEDIRLB,      "项目目录:",    5, 20, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_BASEDIRTB,      "",             45, 19, 158, 12
  CONTROL ADD BUTTON,  hDlg, %IDC_BASEDIRSELBTN,  "浏览",         205, 19, 30, 12

  CONTROL ADD LABEL,   hDlg, %IDC_SRCLB,          "src目录:",     5, 34, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_SRCTB,          "",             45, 33, 158, 12
  CONTROL ADD BUTTON,  hDlg, %IDC_SRCSELBTN,      "浏览",         205, 33, 30, 12

  CONTROL ADD LABEL,   hDlg, %IDC_LIBLB,          "lib目录:",     5, 48, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_LIBTB,          "",             45, 47, 158, 12
  CONTROL ADD BUTTON,  hDlg, %IDC_LIBSELBTN,      "浏览",         205, 47, 30, 12

  CONTROL ADD LABEL,   hDlg, %IDC_MAINLB,         "主程序:",      5, 62, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_MAINTB,         "",             45, 61, 158, 12

  CONTROL ADD LABEL,   hDlg, %IDC_ARGLB,          "参数名称:",    5, 76, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_ARGTB,          "",             45, 75, 158, 12

  CONTROL ADD LABEL,   hDlg, %IDC_ARGVALLB,       "参数值:",      5, 90, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_ARGVALTB,       "",             45,89, 158,12

  CONTROL ADD LABEL,   hDlg, %IDC_ANTLB,          "ANT目录:",     5, 104, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_ANTTB,          "",             45, 103, 158, 12
  CONTROL ADD BUTTON,  hDlg, %IDC_ANTSELBTN,      "浏览",         205, 103, 30, 12

  CONTROL ADD LABEL,   hDlg, %IDC_JDKLB,          "java目录:",    5, 118, 40, 12, %SS_RIGHT
  CONTROL ADD TEXTBOX, hDlg, %IDC_JDKTB,          "",             45, 117, 158, 12
  CONTROL ADD BUTTON,  hDlg, %IDC_JDKSELBTN,      "浏览",         205, 117, 30, 12

  CONTROL ADD LABEL,   hDlg, %IDC_ENCODELB,       "编译编码:",    5, 132, 40, 12, %SS_RIGHT
  CONTROL ADD COMBOBOX,hDlg, %IDC_ENCODECB,       tmpArr(),       45, 131, 50, 40
  CONTROL SET TEXT    hDlg, %IDC_ENCODECB, tmpArr(0)
  CONTROL ADD BUTTON,  hDlg, %IDC_SAVESETTINGBTN,"保存配置",      190,131,45,14

  CONTROL ADD BUTTON,  hDlg, %IDC_MAKEBUILDBTN,   "生成build.xml",20, 137,50,14
  CONTROL ADD BUTTON,  hDlg, %IDC_COMPILEBTN,     "编译成jar",    70, 137,45,14
  CONTROL ADD BUTTON,  hDlg, %IDC_RUNBTN,         "运行程序",     120,137,45,14
  CONTROL ADD BUTTON, hDlg, %IDCANCEL,            "关闭",         170,137,45,14

  DIALOG SHOW MODAL hDlg CALL DlgProc

END FUNCTION
'------------------------------------------------------------------------------
' 主回调函数
'------------------------------------------------------------------------------
CALLBACK FUNCTION DlgProc () AS LONG
  LOCAL xx,yy     AS LONG
  LOCAL tmpStr    AS STRING
'  LOCAL prjName   AS STRING
'  LOCAL baseDir   AS STRING
'  LOCAL srcDir    AS STRING
'  LOCAL libDir    AS STRING
'  LOCAL antDir    AS STRING
'  LOCAL jdkDir    AS STRING
'  LOCAL mainPath  AS STRING
'  LOCAL encode    AS STRING
'  LOCAL argStr    AS STRING

  SELECT CASE CB.MSG
    CASE %WM_INITDIALOG
      ReadConfig CB.HNDL
    CASE %WM_NCACTIVATE
      STATIC hWndSaveFocus AS DWORD
      IF ISFALSE CBWPARAM THEN
        ' Save control focus
        hWndSaveFocus = GetFocus()
      ELSEIF hWndSaveFocus THEN
        ' Restore control focus
        SetFocus(hWndSaveFocus)
        hWndSaveFocus = 0
      END IF
    CASE %WM_SIZE
      IF CBWPARAM = %SIZE_MINIMIZED THEN EXIT FUNCTION
      DIALOG GET CLIENT CB.HNDL TO xx,yy
      IF xx<245 THEN
        xx=245
        DIALOG SET CLIENT CB.HNDL,xx,yy
      END IF
      IF yy<180 THEN
        yy=180
        DIALOG SET CLIENT CB.HNDL,xx,yy
      END IF
      CONTROL SET SIZE CB.HNDL, %IDC_PRJNAMETB,       xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_BASEDIRTB,       xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_SRCTB,           xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_LIBTB,           xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_ANTTB,           xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_JDKTB,           xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_MAINTB,          xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_ARGTB,           xx-87, 12
      CONTROL SET SIZE CB.HNDL, %IDC_ARGVALTB,        xx-87, 12
      CONTROL SET LOC  CB.HNDL, %IDC_BASEDIRSELBTN,   xx-40, 19
      CONTROL SET LOC  CB.HNDL, %IDC_SRCSELBTN,       xx-40, 33
      CONTROL SET LOC  CB.HNDL, %IDC_LIBSELBTN,       xx-40, 47
      CONTROL SET LOC  CB.HNDL, %IDC_ANTSELBTN,       xx-40, 103
      CONTROL SET LOC  CB.HNDL, %IDC_JDKSELBTN,       xx-40, 117
      CONTROL SET LOC  CB.HNDL, %IDC_SAVESETTINGBTN,  xx-55, 131

      CONTROL SET LOC  CB.HNDL, %IDC_MAKEBUILDBTN,    xx-210, yy-25
      CONTROL SET LOC  CB.HNDL, %IDC_COMPILEBTN,      xx-155, yy-25
      CONTROL SET LOC  CB.HNDL, %IDC_RUNBTN,          xx-105, yy-25
      CONTROL SET LOC  CB.HNDL, %IDCANCEL,            xx-55, yy-25

      DIALOG REDRAW CB.HNDL

    CASE %WM_COMMAND
      ' Trap only click events
      IF CB.CTLMSG <> %BN_CLICKED THEN EXIT SELECT

      SELECT CASE CB.CTL
        CASE %IDC_BASEDIRSELBTN '根目录选择按钮
          DISPLAY BROWSE CB.HNDL,,,"选择项目根目录","",%BIF_NEWDIALOGSTYLE OR %BIF_DONTGOBELOWDOMAIN _
              OR %BIF_NONEWFOLDERBUTTON OR %BIF_VALIDATE TO tmpStr
          tmpStr=TRIM$(tmpStr)
          IF tmpStr<>"" THEN
            CONTROL SET TEXT CB.HNDL,%IDC_BASEDIRTB,tmpStr
            CONTROL SET TEXT CB.HNDL,%IDC_SRCTB,tmpStr & "\src"
            CONTROL SET TEXT CB.HNDL,%IDC_LIBTB,tmpStr & "\lib"
          END IF
        CASE %IDC_SRCSELBTN     'src目录选择按钮
          DISPLAY BROWSE CB.HNDL,,,"选择源文件目录","",%BIF_NEWDIALOGSTYLE OR %BIF_DONTGOBELOWDOMAIN _
              OR %BIF_NONEWFOLDERBUTTON OR %BIF_VALIDATE TO tmpStr
          tmpStr=TRIM$(tmpStr)
          IF tmpStr<>"" THEN
            CONTROL SET TEXT CB.HNDL,%IDC_SRCTB,tmpStr
          END IF
        CASE %IDC_LIBSELBTN     '依赖库jar文件目录选择按钮
          DISPLAY BROWSE CB.HNDL,,,"选择依赖库jar文件目录","",%BIF_NEWDIALOGSTYLE OR %BIF_DONTGOBELOWDOMAIN _
              OR %BIF_NONEWFOLDERBUTTON OR %BIF_VALIDATE TO tmpStr
          tmpStr=TRIM$(tmpStr)
          IF tmpStr<>"" THEN
            CONTROL SET TEXT CB.HNDL,%IDC_LIBTB,tmpStr
          END IF
        CASE %IDC_ANTSELBTN     'ANT目录选择按钮
          DISPLAY BROWSE CB.HNDL,,,"选择ANT根目录","",%BIF_NEWDIALOGSTYLE OR %BIF_DONTGOBELOWDOMAIN _
              OR %BIF_NONEWFOLDERBUTTON OR %BIF_VALIDATE TO tmpStr
          tmpStr=TRIM$(tmpStr)
          IF tmpStr<>"" THEN
            CONTROL SET TEXT CB.HNDL,%IDC_ANTTB,tmpStr
          END IF
        CASE %IDC_JDKSELBTN     'JDK目录选择按钮
          DISPLAY BROWSE CB.HNDL,,,"选择JAVA根目录","",%BIF_NEWDIALOGSTYLE OR %BIF_DONTGOBELOWDOMAIN _
              OR %BIF_NONEWFOLDERBUTTON OR %BIF_VALIDATE TO tmpStr
          tmpStr=TRIM$(tmpStr)
          IF tmpStr<>"" THEN
            CONTROL SET TEXT CB.HNDL,%IDC_JDKTB,tmpStr
          END IF
        CASE %IDC_MAKEBUILDBTN  '生成build.xml按钮
          IF CheckValid(CB.HNDL)<=0 THEN
            EXIT FUNCTION
          END IF
          'mainPath=tmpStr

          GenerateBuild CB.HNDL
        CASE %IDC_COMPILEBTN    '编译并生成jar 文件按钮
          IF CheckValid(CB.HNDL)<=0 THEN
            EXIT FUNCTION
          END IF
          MakeJar CB.HNDL
        CASE %IDC_RUNBTN        '运行按钮
          IF CheckValid(CB.HNDL)<=0 THEN
            EXIT FUNCTION
          END IF
          RunJar CB.HNDL
        CASE %IDC_SAVESETTINGBTN'保存配置按钮
          SaveConfig CB.HNDL
        CASE %IDCANCEL
          DIALOG END CB.HNDL, 0
      END SELECT
    CASE %WM_SYSCOLORCHANGE

  END SELECT
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION ReadConfig(BYVAL hWnd AS DWORD)AS LONG
  LOCAL tmpStr AS STRING

  IF iniFilename="" THEN
    iniFilename=EXE.PATH$ & "config.cfg"
  END IF
  tmpStr=IniRead(iniFilename,"general","antdir","")
  CONTROL SET TEXT hWnd,%IDC_ANTTB,tmpStr
  tmpStr=IniRead(iniFilename,"general","jdkdir","")
  CONTROL SET TEXT hWnd,%IDC_JDKTB,tmpStr
  tmpStr=IniRead(iniFilename,"general","encode","utf-8")
  CONTROL SET TEXT hWnd,%IDC_ENCODECB,tmpStr
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION SaveConfig(BYVAL hWnd AS DWORD)AS LONG
  LOCAL tmpStr AS STRING
  IF iniFilename="" THEN
    iniFilename=EXE.PATH$ & "config.cfg"
  END IF
  CONTROL GET TEXT hWnd,%IDC_ANTTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IniWrite(iniFilename,"general","antdir",tmpStr)
  CONTROL GET TEXT hWnd,%IDC_JDKTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IniWrite(iniFilename,"general","jdkdir",tmpStr)
  CONTROL GET TEXT hWnd,%IDC_ENCODECB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IniWrite(iniFilename,"general","encode",tmpStr)
  ? "保存成功",%MB_ICONINFORMATION,"提示"
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION CheckValid(BYVAL hWnd AS DWORD)AS LONG
  LOCAL tmpStr AS STRING

  CONTROL GET TEXT hWnd,%IDC_PRJNAMETB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请输入项目名称",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'prjName=tmpStr

  CONTROL GET TEXT hWnd,%IDC_BASEDIRTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入项目目录",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'baseDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_SRCTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入项目源文件目录",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'srcDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_LIBTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入依赖库jar文件目录",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'libDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_ANTTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入ANT目录",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'antDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_JDKTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入JAVA目录",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  'jdkDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_MAINTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    ? "请选择或输入主程序",%MB_ICONINFORMATION OR %MB_OK,"提示"
    EXIT FUNCTION
  END IF
  FUNCTION = 1
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION GenerateBuild(BYVAL hWnd AS DWORD)AS LONG
  LOCAL hFile     AS LONG
  LOCAL tmpStr    AS STRING
  LOCAL allStr    AS STRING
  LOCAL baseDir   AS STRING
  LOCAL jarlist() AS STRING
  LOCAL x,i       AS LONG
  LOCAL jarStr    AS STRING
  LOCAL libDir    AS STRING
  LOCAL jarsStr   AS STRING
  LOCAL argStr    AS STRING

  hFile = FREEFILE
  OPEN EXE.PATH$ & "build_template.xml" FOR BINARY AS hFile
  IF ERR THEN
    ? "错误 #" & FORMAT$(ERR) & ":" & ERROR$(ERR) & $CRLF & "打开文件时出错:build_template.xml", _
        %MB_ICONEXCLAMATION,"提示"
    FUNCTION = 0
    EXIT FUNCTION
  END IF
  tmpStr=SPACE$(LOF(hFile))
  GET #hFile,1,tmpStr
  CLOSE #hFile
  allStr=tmpStr

  CONTROL GET TEXT hWnd,%IDC_PRJNAMETB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  REPLACE "[projectname]" WITH tmpStr IN allStr

  CONTROL GET TEXT hWnd,%IDC_BASEDIRTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  baseDir=tmpStr
  'REPLACE "[basedir]" WITH tmpStr IN allStr
  REPLACE "[basedir]" WITH "." IN allStr

  CONTROL GET TEXT hWnd,%IDC_SRCTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr=baseDir THEN
    tmpStr="."
  END IF
  IF LEFT$(tmpStr,LEN(baseDir))=baseDir THEN
    tmpStr=MID$(tmpStr,LEN(baseDir)+1)
    IF LEFT$(tmpStr,1)="\" THEN
      tmpStr=MID$(tmpStr,2)
    END IF
  END IF
  REPLACE "[src]" WITH tmpStr IN allStr

  CONTROL GET TEXT hWnd,%IDC_LIBTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  libDir=tmpStr
  IF tmpStr=baseDir THEN
    tmpStr="."
  END IF
  IF LEFT$(tmpStr,LEN(baseDir))=baseDir THEN
    tmpStr=MID$(tmpStr,LEN(baseDir)+1)
    IF LEFT$(tmpStr,1)="\" THEN
      tmpStr=MID$(tmpStr,2)
    END IF
  END IF
  REPLACE "[lib]" WITH tmpStr IN allStr

  REDIM jarlist(100)
  x=0
  tmpStr=DIR$(libDir & "\*.jar") ', to jarlist(x))
  IF LEN(tmpStr)>0 THEN
    REPLACE libDir WITH "" IN tmpStr
    IF LEFT$(tmpStr,1)="\" THEN
      tmpStr=MID$(tmpStr,2)
    END IF
    jarsStr="        <pathelement location=""${lib}/" & tmpStr & """/>" & $CRLF
  END IF
  WHILE LEN(tmpStr) AND x<100
    INCR x
    REPLACE libDir WITH "" IN tmpStr
    IF LEFT$(tmpStr,1)="\" THEN
      tmpStr=MID$(tmpStr,2)
    END IF
    jarsStr=jarsStr & "        <pathelement location=""${lib}/" & tmpStr & """/>" & $CRLF
    tmpStr=DIR$(NEXT) ',to jarlist(x))
  WEND
  REPLACE "[jars]" WITH jarsStr IN allStr

  CONTROL GET TEXT hWnd,%IDC_ENCODECB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr="" THEN
    tmpStr="GBK"
  END IF
  REPLACE "[encode]" WITH tmpStr IN allStr

  CONTROL GET TEXT hWnd,%IDC_MAINTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  REPLACE "[mainpath]" WITH tmpStr IN allStr

  argStr=""
  CONTROL GET TEXT hWnd,%IDC_ARGTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF tmpStr<>"" THEN
    FOR i=1 TO PARSECOUNT(tmpStr,ANY " ,")
      argStr = argStr & "      <arg value=""" & PARSE$(tmpStr,ANY " ,",i) & """/>" & $CRLF
    NEXT i
  END IF
  REPLACE "[args]" WITH argStr IN allStr

  hFile=FREEFILE
  OPEN baseDir & "\build.xml" FOR OUTPUT AS #hFile
  PRINT #hFile,allStr
  CLOSE #hFile
  FUNCTION = 1
  ? "已成功创建 build.xml 文件",%MB_ICONINFORMATION,"提示"
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION MakeJar(BYVAL hWnd AS DWORD)AS LONG
  LOCAL tmpStr    AS STRING
  LOCAL hFile     AS LONG
  LOCAL mainPath  AS STRING
  LOCAL antDir    AS STRING
  LOCAL cmdStr    AS STRING
  LOCAL exePath   AS STRING

  CONTROL GET TEXT hWnd,%IDC_MAINTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  mainPath=tmpStr

  CONTROL GET TEXT hWnd,%IDC_BASEDIRTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF DIR$(tmpStr & "\build.xml")="" THEN
    ? "请先生成 build.xml",%MB_ICONINFORMATION,"提示"
    EXIT FUNCTION
  END IF
  exePath = EXE.PATH$
  CHDIR tmpStr

  MKDIR tmpStr & "\META-INF"

  hFile=FREEFILE
  OPEN tmpStr & "\META-INF\manifest.mf" FOR OUTPUT AS #hFile
  PRINT #hFile,"Main-Class: " & mainPath;
  CLOSE #hFile

  CONTROL GET TEXT hWnd,%IDC_ANTTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  antDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_JDKTB TO tmpStr
  tmpStr=TRIM$(tmpStr)

  cmdStr=antDir & "\bin\ant rebuild -Djavacmd=" & tmpStr
  '? cmdStr
  SHELL ENVIRON$("COMSPEC") & " /C" & cmdStr & " >compile.log"
  ? "编译完成"
  CHDIR exePath
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION RunJar(BYVAL hWnd AS DWORD)AS LONG
  LOCAL tmpStr    AS STRING
  LOCAL hFile     AS LONG
  LOCAL mainPath  AS STRING
  LOCAL antDir    AS STRING
  LOCAL cmdStr    AS STRING
  LOCAL exePath   AS STRING
  LOCAL i         AS LONG
  LOCAL argStr    AS STRING
  LOCAL argNames  AS STRING
  LOCAL argValues AS STRING

  CONTROL GET TEXT hWnd,%IDC_MAINTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  mainPath=tmpStr

  CONTROL GET TEXT hWnd,%IDC_BASEDIRTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  IF DIR$(tmpStr & "\build.xml")="" THEN
    ? "请先生成 build.xml",%MB_ICONINFORMATION,"提示"
    EXIT FUNCTION
  END IF
  exePath = EXE.PATH$
  CHDIR tmpStr

  CONTROL GET TEXT hWnd,%IDC_ANTTB TO tmpStr
  tmpStr=TRIM$(tmpStr)
  antDir=tmpStr

  CONTROL GET TEXT hWnd,%IDC_JDKTB TO tmpStr
  tmpStr=TRIM$(tmpStr)

  cmdStr=antDir & "\bin\ant run -Djavacmd=" & tmpStr

  CONTROL GET TEXT hWnd,%IDC_ARGTB TO argNames
  CONTROL GET TEXT hWnd,%IDC_ARGVALTB TO argValues
  argNames=TRIM$(argNames)
  argValues=TRIM$(argValues)

  argStr=""
  IF argNames<>"" AND argValues<>"" THEN
    FOR i=1 TO MIN(PARSECOUNT(argValues,ANY " ,"),PARSECOUNT(argNames,ANY " ,"))
      argStr=argStr & " -D" & PARSE$(argNames,ANY " ,",i) & "=""" & PARSE$(argValues,ANY " ,",i) & """"
    NEXT i
  END IF

  IF argStr<>"" THEN
    cmdStr=cmdStr & argStr
  END IF

  '? cmdStr
  SHELL ENVIRON$("COMSPEC") & " /C" & cmdStr & " >run.log"
  ? "运行完成"
  CHDIR exePath
END FUNCTION
'------------------------------------------------------------------------------

' ********************************************************************************
' Function    : IniRead ()
' Description : Reads data from the Applications INI file.
' Usage       : sText = IniRead ("INIFILE", "SECTION", "KEY", "DEFAULT")
'             : lVal  = VAL (IniRead ("INIFILE", "SECTION", "KEY", "DEFAULT"))
' ********************************************************************************
FUNCTION IniRead( BYVAL sIniFile AS STRING, _
          BYVAL sSection AS STRING, _
          BYVAL sKey AS STRING, _
          BYVAL sDefault AS STRING ) AS STRING
  LOCAL lResult AS LONG
  LOCAL szSection AS ASCIIZ * 125
  LOCAL szKey AS ASCIIZ * 125
  LOCAL szData AS ASCIIZ * 150
  LOCAL szDefault AS ASCIIZ * 150
  LOCAL szIniFile AS ASCIIZ * %MAX_PATH
  szSection = sSection
  szKey = sKey
  szIniFile = sIniFile
  szDefault = sDefault
  lResult = GETPRIVATEPROFILESTRING( szSection, szKey, szDefault, _
            szData, SIZEOF( szData ), szIniFile )
  FUNCTION = szData
END FUNCTION

' ********************************************************************************
' Function    : IniWrite ()
' Description : Saves data to the Applications INI file.
' Usage       : lResult = IniWrite ("INIFILE", "SECTION", "KEY", "VALUE")
' ********************************************************************************
FUNCTION IniWrite( BYVAL sIniFile AS STRING, _
          BYVAL sSection AS STRING, _
          BYVAL sKey AS STRING, _
          BYVAL sValue AS STRING ) AS LONG
  LOCAL szSection AS ASCIIZ * 125
  LOCAL szKey AS ASCIIZ * 125
  LOCAL szValue AS ASCIIZ * 150
  LOCAL szIniFile AS ASCIIZ * %MAX_PATH
  szSection = sSection
  szKey = sKey
  szIniFile = sIniFile
  szValue = TRIM$( sValue )
  FUNCTION = WRITEPRIVATEPROFILESTRING( szSection, szKey, szValue, szIniFile )
END FUNCTION
