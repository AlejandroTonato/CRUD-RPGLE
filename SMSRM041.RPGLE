000100121108     H DATEDIT(*YMD)
000200121105      *--------------------------------------------------------------
000300140221      * Sistema. . : SOCIOS - SMS                                   -
000400140326      * NOMBRE ....: ALEJANDRO TONATO - SONDA DEL ECUADOR           -
000500140325      * FECHA .....: 2014/03/25                                     -
000600140324      * REQUERIM...: 58340                                          -
000700140325      * OBJETIVO...: CREA, MODIFICA Y CONSULTA PREFIJOS OPERADORAS  -
000800121105      *--------------------------------------------------------------
000900000000      *  Definicion de archivos                           -
001000000000      *-----------------------------------------------------
001100140228      *  Archivo de control de cabeceras
001200140325      *ARCHIVO DE PREFIJOS OPERADORAS
001300140325     FTMSMV221  UF A E           K DISK    RENAME(TMSMV221:TMSMV221R)
001400140325      *ARCHIVO  DE TABLAS PARA OPERADORAS
001500140325     FTABEAP    IF   E           K DISK
001600140325      *ARCHIVO DE CABECERAS
001700140320     ftan6rel0  if   e           k Disk
001800000000      *- PANTALLA
001900140616     FSMSPM041  CF   E             WORKSTN INFDS(Y0SCRE)
002000000505     F                                     SFILE(RSFLMSG:Y0REGM)
002100000000      *-----------------------------------------------------
002200000000      *  Definiciòn de arreglos                            -
002300000000      *-----------------------------------------------------
002400121108     D XPTC            S             10    DIM(3) CTDATA PERRCD(3)              PETIC.PGM.
002500140708     D xmen            S             90    DIM(9) CTDATA PERRCD(1)
002600000000      *-----------------------------------------------------
002700000000      *  Definiciòn de Estructuras de datos                -
002800000000      *-----------------------------------------------------
002900000505     D* Estructuras generales
003000111122     D/COPY QRPGSRC,LGCI0001LE
003100000505     D* Estructuras para Funcion F4 = SOLICITUD
003200111122     D/COPY QRPGSRC,LGCI0004LE
003300111117      * ACCESO PRINCIPAL
003400050504     D                 DS                  INZ
003500140325     D  X0CLA                  1      6
003600140325     D  WM9W1A                 1      2  0
003700140325     D  WM9W2A                 3      4  0
003800140325     D  WM9W3A                 5      6  0
003900111122      *
004000000000      *-----------------------------------------------------
004100000000      *  Definicion de variables                           -
004200000000      *-----------------------------------------------------
004300140310     dxpgm             s             10
004400000505     C     *LIKE         DEFINE    Y0CDMG        Y0CDPR                         MSG.ERR.PRM.
004500000505     C     *LIKE         DEFINE    Y0DTMG        Y0DTPR                         DTA.ERR.PRM.
004600000000      *-----------------------------------------------------
004700111110      *  Definicion de claves                              -
004800111110      *-----------------------------------------------------
004900140321     c     KTAB          klist
005000140325     c                   kfld                    WM9W1A
005100140325     c                   kfld                    WM9W2A
005200140325     c                   kfld                    WM9W3A
005300140325     C     KOPER         klist
005400140325     c                   kfld                    x0tab
005500140325     c                   kfld                    X0ELE
005600140324     c     KTABM         klist
005700140324     c                   kfld                    N6E8XT
005800140324     c                   kfld                    N6FAXT
005900140325     c                   kfld                    WM9W2A
006000140325     c                   kfld                    WM9W3A
006100111117      *-----------------------------------------------------
006200000000      *  Paràmetros de entrada                             -
006300000000      *-----------------------------------------------------
006400000000     C     *ENTRY        PLIST
006500140325     C                   PARM                    X0CLPR            6            LLAVE 1
006600000505     C                   PARM                    X0PETI            1            PETICION
006700000505     C                   PARM                    Y0CDPR                         MSG.ERROR
006800000505     C                   PARM                    Y0DTPR                         DATOS ERROR
006900000505     C                   PARM                    X0F3              1            IND.FUNC.3
007000000505     C                   PARM                    X0F12             1            IND.FUNC.12
007100000505     C                   PARM                    X0ERR             1            IND.ERROR
007200030804      *
007300000000      *-----------------------------------------------------
007400000000      *  Inicializaciòn de variables                       -
007500000000      *-----------------------------------------------------
007600140228     c                   read      fn6reiq                                30
007700140228     c                   movel     n6f0xt        w0noem
007800121108     C                   MOVE      X0CLPR        X0CLA
007900000000     C                   EXSR      CLRMSG                                       BORRA MENSAJE
008000000402     C                   EXSR      SUB000                                       BORR.IND.ERR.
008100000505     C                   MOVEL     'DIS0000'     Y0CDMG                         COPYRIGHT
008200000505     C                   MOVE      *BLANKS       Y0DTMG                         DATOS MENSAJE
008300000000      *-----------------------------------------------------
008400000000      *  Proceso Principal                                 -
008500000000      *-----------------------------------------------------
008600000505     C                   EXSR      SUB010                                       VERIFICA REG.
008700000000     C     *IN03         DOWEQ     *OFF                                         MIENT.NO F3
008800000000     C     *IN12         ANDEQ     *OFF                                         Y NO F12
008900000000     C     *IN50         ANDEQ     *OFF                                         Y NO SALIR
009000000505     C                   EXSR      SUB030                                       ESCR.PANTALLA
009100000000     C     *IN03         IFEQ      *OFF                                         SI NO F3
009200000000     C     *IN12         ANDEQ     *OFF                                         Y NO F12
009300000000     C                   SELECT
009400121108     C     *IN04         WHENEQ    *ON
009500121108     c                   exsr      solici
009600000000     C     *IN05         WHENEQ    *ON                                          SI RENOVAR
009700000505     C                   EXSR      SUB010                                       VERIFICA REG.
009800000416     C     *IN52         WHENEQ    *ON                                          SI VISUALIZAR
009900000000     C                   SETON                                        50        SALIR
010000000000     C                   OTHER
010100140313     C                   EXSR      SUB060                                       VALIDA DATOS
010200140221     C                   IF        *in80 = '0'  and
010300140221     C                             SN = 'S' OR SN = 's'
010400140310     C                   EXSR      SUB070
010500140324     c                   else
010600140324     c                   IF        SN = 'N' OR SN = 'n'
010700140324     c                   eval      *IN12 = *ON
010800140324     C                   ENDIF
010900000000     C                   ENDIF
011000000000     C                   ENDSL
011100000000     C                   ENDIF
011200140310     c                   enddo
011300140325     C                   UNLOCK    TMSMV221                                     DESBLOQ.ARCH.
011400000505     C                   MOVEL     *IN03         X0F3                           ACTUALIZ.F3
011500000505     C                   MOVEL     *IN12         X0F12                          ACTUALIZ.F12
011600000000     C                   SETON                                        LR        FIN PROGRAMA
011700000000     C                   RETURN
011800000402      *-----------------------------------------------------
011900000402      *- BORRA INDICADORES DE ERROR                        -
012000000402      *-----------------------------------------------------
012100000402     C     SUB000        BEGSR
012200121018     C                   SETOFF                                       80        NO ERROR
012300121018     C                   MOVEA     '000'         *IN(30)
012400140306     C                   MOVEA     '000'         *IN(20)
012500140306     C                   MOVEA     '000'         *IN(22)
012600140221     C                   MOVEA     '000'         *IN(23)
012700140221     C                   MOVEA     '000'         *IN(24)
012800140221     C                   MOVEA     '000'         *IN(25)
012900140307     C                   MOVEA     '000'         *IN(26)
013000140307     C                   MOVEA     '000'         *IN(27)
013100140307     C                   MOVEA     '000'         *IN(28)
013200121018     C                   MOVEA     '000'         *IN(40)
013300000402     C                   ENDSR
013400000000      *-----------------------------------------------------
013500000000      *  INICIALIZA REGISTRO                               -
013600000000      *-----------------------------------------------------
013700000505     C     SUB010        BEGSR
013800121018     C                   SETOFF                                       5152      NO PROT.CAMPOS
013900000505     C     X0PETI        IFEQ      'A'                                          SI ES ALTA
014000121204     C                   CLEAR                   RDTO                           BORRA CAMPOS
014100140325     C                   Z-ADD     *ZEROS        WM9W1A
014200140325     C                   Z-ADD     *ZEROS        WM9W2A
014300140325     C                   Z-ADD     *ZEROS        WM9W3A
014400121108     C                   MOVEL     XPTC(1)       W1OPER                         CREAR
014500000000     C                   SETON                                        79        NO EXISTE
014600140321     C                   MOVEL     *BLANKS       KLAVE            32
014700140325     C                   MOVEL     'TMSMV221'    KLAVE
014800000000     C                   ELSE
014900000416     C                   SETON                                        51        PROTEGE LLAVE
015000140325     C     KTAB          SETLL     TMSMV221R                                    UBICAR REGISTRO
015100140325     C                   READ      TMSMV221R                            7879    LEE REGISTRO
015200000505     C     X0PETI        IFEQ      'V'                                          SI VISUALIZAR
015300121108     C                   MOVEL     XPTC(3)       W1OPER                         VISUALIZAR
015400121018     C                   SETON                                        52        PROTEGE CAMPOS
015500000000     C                   ELSE
015600121108     C                   MOVEL     XPTC(2)       W1OPER                         CAMBIAR
015700140326     c                   SETON                                        51
015800000000     C                   ENDIF
015900000000     C     *IN78         IFNE      *OFF                                         SI ESTA BLOQ.
016000000000     C                   SETON                                        50        SALIR
016100140228     C                   MOVEL     'DIS0007'     Y0CDMG                         REG.BLOQUEADO
016200000505     C                   MOVEL     X0CLA         Y0DTPR                         COD.REG.
016300000000     C                   ELSE
016400000000     C     *IN79         IFNE      *OFF                                         SI NO EXISTE
016500000000     C                   SETON                                        50        SALIR
016600000505     C                   MOVEL     'DIS0005'     Y0CDPR                         REG.NO EXIST.
016700000505     C                   MOVEL     X0CLA         Y0DTPR                         COD.REG.
016800000000     C                   ELSE
016900000505     C                   EXSR      SUB011                                       MUEVE CAMPOS
017000000000     C                   ENDIF
017100000000     C                   ENDIF
017200000000     C                   ENDIF
017300000000     C                   ENDSR
017400000000      *-----------------------------------------------------
017500000000      *- MOVER CAMPOS DE ARCHIVO A PANTALLA                -
017600000000      *-----------------------------------------------------
017700000505     C     SUB011        BEGSR
017800140228     C* CAMPOS PARA CARGAR EN LA PANTALLA
017900140325     C                   MOVEL     TM9W1A        WM9W1A
018000140325     C                   MOVEL     TM9W2A        WM9W2A
018100140325     C                   MOVEL     TM9W3A        WM9W3A
018200140325     C                   MOVEL     TM60A         WM60A
018300140325     c                   z-add     1008          x0tab             4 0
018400140325     c                   MOVEL     TM60A         X0ELE            10
018500140325     C     KOPER         chain     TABEAR                             30
018600140321     C                   If        *in30 = '0'
018700140325     C                   eval      WDESCR =      TADEEA
018800140321     C                   endif
018900140317     C                   ENDSR
019000000000      *-----------------------------------------------------
019100000000      *- ESCRIBE DATOS EN PANTALLA                         -
019200000000      *-----------------------------------------------------
019300000505     C     SUB030        BEGSR
019400000000     C                   EXSR      WRTMSG
019500000000     C                   WRITE     RCAB                                         REG.CABECERA
019600000000     C                   WRITE     RFNC                                         REG.FUNCION1
019700000000     C                   WRITE     RMSGCTL                                      REG.MENSAJES
019800000000     C                   EXFMT     RDTO                                         REG.DATOS
019900000000     C                   EXSR      CLRMSG                                       BORRA MENSAJE
020000000402     C                   EXSR      SUB000                                       BORR.IND.ERR.
020100000000     C                   ENDSR
020200000000      *-----------------------------------------------------
020300000000      *- VALIDA DATOS DEL ARCHIVO                          -
020400000000      *-----------------------------------------------------
020500140317     C     SUB060        BEGSR
020600140325      * Valida campo PREFIJO 1
020700140325     C     *IN80         IFEQ      '0'
020800140325     C     WM9W1A        IFEQ      *ZEROS
020900140320     C                   SETON                                          2580    ERROR ENTRADA
021000140317     C                   MOVEL     'DIS0312'     Y0CDMG
021100140320     c                   eval      Y0DTMG = xmen(1)
021200140317     c                   endif
021300140317     c                   ENDIF
021400140321      *VALIDA REGISTROS REPETIDO
021500140321     C     *IN80         IFEQ      *OFF                                         SI NO ERROR
021600140325     c     KTAB          chain     TMSMV221R                          30
021700140321     C                   IF        *IN30 = '0' AND X0PETI <>'C'
021800140321     C                   SETON                                          2580    ERROR ENTRADA
021900140321     C                   SETON                                          26      ERROR ENTRADA
022000140321     C                   SETON                                          27      ERROR ENTRADA
022100140708     C                   MOVEL     'DIS0312'     Y0CDMG
022200140708     C                   eval      Y0DTMG = xmen(6)
022300140321     c                   endif
022400140321     C                   ENDIF
022500140321     C
022600140325      *VALIDA CAMPO CAMPO OPERADORA
022700140325     C     *IN80         IFEQ      *OFF
022800140325     C     WM60A         IFEQ      *BLANKS
022900140325     C                   SETON                                          2880    ERROR ENTRADA
023000140325     C                   MOVEL     'DIS0312'     Y0CDMG
023100140325     c                   eval      Y0DTMG = xmen(4)
023200140325     c                   eval      WDESCR = *blanks
023300140325     c                   else
023400140325     c                   z-add     1008          x0tab             4 0
023500140325     c                   MOVEL     WM60A         X0ELE            10
023600140325     C     KOPER         chain     TABEAR                             30
023700140325     C                   If        *in30 = '1'
023800140325     C                   SETON                                          2880    ERROR ENTRADA
023900140325     C                   MOVEL     'DIS0312'     Y0CDMG
024000140325     c                   eval      Y0DTMG = xmen(5)
024100140325     c                   eval      WDESCR = *blanks
024200140325     c                   else
024300140325     c                   eval      WDESCR = TADEEA
024400140325     C                   endif
024500140325     c                   ENDIF
024600140325     c                   ENDIF
024700140325     C
024800140313      * Valida si o no
024900140313     C     *IN80         IFEQ      *OFF                                         SI NO ERROR
025000140313     C     SN            IFEQ      *BLANKS
025100140313     C                   SETON                                          2080    ERROR ENTRADA
025200140321     C                   MOVEL     'DIS0312'     Y0CDMG
025300140321     c                   eval      Y0DTMG = xmen(8)
025400140313     C                   ELSE
025500140313     C                   IF        SN <> 'S' and SN <> 'N'
025600140313     C                   SETON                                          2080    ERROR ENTRADA
025700140313     C                   MOVEL     'DIS0312'     Y0CDMG
025800140321     c                   eval      Y0DTMG = xmen(7)
025900140313     C                   ELSE
026000140313     C                   SETOFF                                         20
026100140313     C                   ENDIF
026200140313     C                   ENDIF
026300140313     C                   ENDIF
026400140313     C                   ENDSR
026500000000      *-----------------------------------------------------
026600000000      *  ACTUALIZA DATOS EN ARCHIVO                        -
026700000000      *-----------------------------------------------------
026800000505     C     SUB070        BEGSR
026900000000     C                   SETON                                        50        SALIR
027000000000     C     *IN79         IFEQ      *OFF                                         SI EXISTE REG.
027100121105     C                   EXSR      SUB072                                       ACTUAL.CAMPOS
027200140325     C                   UPDATE    TMSMV221R                                    ACT.REGISTRO
027300000000     C                   ELSE
027400140325     C                   CLEAR                   TMSMV221R                      LIMPIA REGISTRO
027500140325     C                   EXSR      SUB072
027600140325     c                   WRITE     TMSMV221R                            40      CREA REGISTRO
027700000000     C     *IN40         IFNE      *OFF                                         SI ERROR CREAR
027800000505     C                   MOVEL     'DIS0005'     Y0CDPR                         REG.YA EXIST.
027900000505     C                   MOVEL     X0CLA         Y0DTPR                         COD.REG.
028000000000     C                   ENDIF
028100000000     C                   ENDIF
028200000000     C                   ENDSR
028300121105      *-----------------------------------------------------
028400121105      *- ACTUALIZAR CAMPOS AUDITORIA MODIFICACION          -
028500121105      *-----------------------------------------------------
028600121105     C     SUB072        BEGSR
028700140325     c     X0PETI        IFEQ      'A'
028800140325     C                   MOVEL     WM9W1A        TM9W1A
028900140325     C                   MOVEL     WM9W2A        TM9W2A
029000140325     C                   MOVEL     WM9W3A        TM9W3A
029100140325     C                   MOVEL     WM60A         TM60A
029200140325     C     *ISO          MOVEL     *DATE         ES73A                          feche de creado
029300140325     C                   TIME                    ES75A                          hora de creado
029400140325     C                   MOVEL     Y0USUA        ES77A                          usuario de creado
029500140325     C     *ISO          MOVEL     *DATE         ES74A                          fecha de modificacio
029600140325     C                   TIME                    ES76A                          hora de modificacio
029700140325     C                   MOVEL     Y0USUA        ES78A                          usuario modifica
029800140325     C                   else                                                   ACT MODIFICACION
029900140325     C                   movel     WM9W1A        TM9W1A                         Código Institución
030000140325     C                   MOVEL     WM9W2A        TM9W2A                         Cód Grup Promoción
030100140325     C                   movel     WM9W3A        TM9W3A                         Porc Soc Afilizado
030200140325     C                   MOVEL     WM60A         TM60A                          Porc Soc Convertido
030300140325     C     *ISO          MOVEL     *DATE         ES74A                          fecha de modificacio
030400140325     C                   TIME                    ES76A                          hora de modificacio
030500140325     C                   MOVEL     Y0USUA        ES78A                          usuario modifica
030600140325     C                   endif
030700140325     c
030800121105     C                   ENDSR
030900121108      *---------------------------------------------------*
031000121108      *- solicitud de lista                              -*
031100121108      *---------------------------------------------------*
031200121108      *-
031300121108     c     solici        begsr
031400140221      *-
031500140313     c                   eval      *in61 = *off
031600140313     c                   eval      *in62 = *off
031700140313     c                   eval      *in63 = *off
031800140313     c                   eval      *in64 = *off
031900140313     c                   eval      *in65 = *off
032000140313     c                   eval      *in60 = *off
032100140313      *-
032200140313      *- ubica posicion del cursor
032300140325     C*- RECUPERA F4 PARA SELECCIONAR OPERADORA
032400140325     c                   IF        wf4cam ='WM60A'
032500140325     c                   z-add     1008          x0tab             4 0
032600140325     c                   MOVEL     *ZEROS        X0ELE            10
032700140411     c                   movel     *blanks       XDESC            50
032800140325     c                   eval      *in60 = *off
032900140325     c                   eval      X0F3= *off
033000140411     c                   movel     'TABRC001'    @pgm             10
033100150925     c                   call      'TABRC001'                           60
033200140325     c                   parm                    x0tab
033300140325     c                   parm                    X0ELE
033400140325     c                   parm                    XDESC
033500140325     c                   if        x0ele <> *zeros
033600140325     c                   eval      WM60A  =      X0ELE
033700140325     c                   eval      WDESCR =      XDESC
033800140325     C                   SETON                                          20
033900140325     c                   endif
034000140325     c                   endif
034100140325     c                   eval      *in61 = *OFF
034200140317      *- Cuando error en F4
034300140321     c                   if        *in60 = '1'
034400140321     c                   eval      Y0CDMG = 'dis0006'
034500140321     c                   eval      Y0DTMG = @pgm
034600140321     c                   exsr      wrtmsg
034700140321     c                   endif
034800140317      *-
034900140320     c                   endsr
035000140317      *-----------------------------------------------------
035100140317      *  LIBRERIAS UTILIZADAS                              -
035200000000      *-----------------------------------------------------
035300000505     C*  Mensajes de error
035400111122     C/COPY QRPGSRC,LGCQ0002LE
035500000000** PETICIONES DEL PROGRAMA
035600000000  Crear    Cambiar  Visualizar
035700140221** MENSAJE ERROR
035800140325Prefijo 1 es obligatorio                                                                1
035900140325Prefijo 2 es obligatorio                                                                2
036000140325Prefijo 3 es obligatorio                                                                3
036100140325Operadora es obligatorio                                                                4
036200140325Dato no válido para campo operadora                                                     5
036300140708Ya existen datos para las claves suministradas                                          6
036400140321Debe ingresar S o N                                                                     7
036500140321El campo es obligatorio                                                                 8