000100140317     hdatfmt(*iso)  datedit(*ymd)
000200140630     f* -------------------------------------------------------------
000300140630     f* SITEMA . . : SOCIOS - SMS                                   -
000400140630     f* APLICACION : SMS                                            -
000500140630     f* DECRIPCION : Trabajar con Prefijos de Operadoras            -
000600140630     f* AUTOR  ....: ALEJANDRO TONATO - SONDA DEL ECUADOR           -
000700140630     f* REQUERIM...: 58340                                          -
000800140630     f* FECHA .....: 2014/03/25                                     -
000900140630     f*--------------------------------------------------------------
001000121227     f* archivo Modelos de Scoring
001100140325     fTMSMV221  uf   e           k disk     RENAME(TMSMV221:TMSMV221R)
001200110908      *  Archivo de control de cabeceras
001300110908     ftan6rel0  if   e           k disk
001400140314      *archivo de tablas
001500140315     fTABEAP    if   e           k disk
001600110620     f* pantalla
001700140616     fSMSPT041  cf   e             workstn
001800120622     f                                     sfile(rsfl1:xreg1)                   forma reg:campo  sub
001900120622     f                                     sfile(rsfl2:xreg2)                   forma reg:campo  sub
002000120622     f                                     sfile(rsflmsg:xregm)                 forma reg:campo  sub
002100140319     d*---------------------------------------------------------------
002200140319     d*  definicion de estructuras de datos                          -
002300140319     d*---------------------------------------------------------------
002400110620     d* nombre del programa
002500110620     d                sds
002600140320     dwprogr                   1     10
002700140320     dwlibre                  81     90
002800140320     dwtermi                 244    253
002900140320     dwusuar                 254    263
003000140319     d* fecha de proceso--
003100140320     d*wfechr           DS
003200140320     d*wfepro                   1      8  0
003300140319     d* llave primaria--
003400110620     d                 ds
003500140325     d  xkey                   1      6
003600140325     D  TM9W1A                 1      2  0
003700140325     d  TM9W2A                 3      4  0
003800140325     d  TM9W3A                 5      6  0
003900140325     d  xkeymg                 1      6
004000140319     d* SITUAR--
004100110620     d                 ds
004200140325     dxsitua                   1      6
004300140325     dwsit01                   1      2  0
004400140325     dwsit02                   3      4  0
004500140325     dwsit04                   5      6  0
004600140319     d* Definicion de Variables------------
004700111227     dxpgm             s             10
004800140228     dxmaxrg           s              2  0 inz(13)
004900111227     dxfirst           s                   like(xkey)
005000111227     dxlast            s                   like(xkey)
005100111227     dxultop           s                   like(xkey)
005200111227     dxreg1            s                   like(xmaxrg)
005300111227     dxreg2            s                   like(xmaxrg)
005400111227     dxregax           s                   like(xmaxrg)
005500140320     dxrgsf1           s                   like(xmaxrg)
005600111227     dxrgsf2           s                   like(xmaxrg)
005700111227     dxopchg           s                   like(wopc) inz(2)
005800111227     dxopdlt           s                   like(wopc) inz(4)
005900111227     dxopvis           s                   like(wopc) inz(5)
006000140319      *===============================================================
006100140319      * Arreglo de Indicadores                                       =
006200140319      *===============================================================
006300110620     d indicators      s             99    based(indptr)
006400110620     d indptr          s               *   inz(%addr(*in))
006500140319     c*---------------------------------------------------------------
006600140319     c*  definicion de claves                                        -
006700140319     c*---------------------------------------------------------------
006800110620     c* llave primaria
006900111227     c     xkeyf         klist
007000140325     c                   kfld                    TM9W1A
007100140325     c                   kfld                    TM9W2A
007200140325     c                   kfld                    TM9W3A
007300140325     c     ktab          klist
007400140325     c                   kfld                    xtab
007500140325     c                   kfld                    xcon
007600140319     c*---------------------------------------------------------------
007700140319     c*  parametros de entrada                                       -
007800140319     c*---------------------------------------------------------------
007900140319     c*---------------------------------------------------------------
008000140319     c*  inicializacion de variables                                 -
008100140319     c*---------------------------------------------------------------
008200111014     c                   eval      wfepro = *date
008300140310     c                   eval(R)   xultop = *ZEROS
008400110620     c                   exsr      clrmsg
008500111227     c                   eval      xcodmg = 'dis0000'
008600111227     c                   eval(R)   xdtamg = *blanks
008700110620     c                   exsr      wrtmsg
008800110620     c                   setoff                                       kckl50
008900110908      * recupera nombre de empresa
009000110908     c                   read      fn6reiq                                30
009100111014     c                   movel     n6f0xt        wcompa
009200121114     C                   MOVEA     '10'          *IN(53)                        ORDENADO POR
009300110620     c* encera llave y situar
009400140325     c* TM9W1Ase encera porque se va a leer toda la tabla
009500120622     c* si llega con parámetros, las variables de la clave se deben cargar con éstos
009600140325     c                   eval      TM9W1A  = *ZEROS
009700140325     c                   eval      TM9W2A  = *ZEROS
009800140325     c                   eval      TM9W3A  = *zeros
009900120622     c* wsit01 se encera porque el situar debe estar en blanco al iniciar el programa
010000120622     c* se inicializan las claves para situar cuando son ocultos
010100140325     c                   eval      WSIT01 = *ZEROS
010200140311     c                   eval      WSIT02 = *ZEROS
010300140320     c                   eval      WSIT04 = *zeros
010400140319     c*---------------------------------------------------------------
010500140319     c*  proceso principal                                           -
010600140319     c*---------------------------------------------------------------
010700120622     c* Rutina donde se carga el subfile hacia delante
010800111227     c                   exsr      xsflade
010900120622     c* Rutina donde se presenta la pantalla despues de haber cargado el subfile
011000110620     c                   exsr      wrtsc1
011100120622     c* Comienza a ejecutarse el programa mientras F3 no sea presionado
011200110620     c                   dow       *inkc  =  *OFF
011300120622     c* Reinicia variable que es igual que la clave
011400111227     c                   reset                   xultop
011500120622     c* Si se presiona F5 enceramos los campos de situar y a la clave le damos el valor
011600120622     c* del primer campo que se encuentre en el subfile
011700110620     c                   if        *inke  =  *ON
011800140325     c                   eval      WSIT01 = *zeros
011900140317     c                   eval      WSIT02 = *ZEROS
012000140320     c                   eval      WSIT04 = *zeros
012100111227     c                   eval      xkey   = xfirst
012200111227     c                   exsr      xsflade
012300110620     c                   else
012400120622     c* Verifica que el número ingresado no sea distinto de los que se puede usar
012500120622     c* para modificar, borrar o vizualizar.
012600111227     c                   exsr      xvalsfl
012700110620     c                   setoff                                       12
012800110906     c                   if        *in80 = *OFF and *in95 = *off
012900110906     c                             and *in96 = *off and *in94 = *on
013000120622     c* Si no hay errores, no pulsan la tecla de avance o retroceso de pagina y
013100120622     c* no escribieron nada en los campos de Sitar en, se comienza a verificar la opcion
013200120622     c* a realizar(creaciòn, modificación o visualizacion)
013300111227     c                   exsr      xproopc
013400110620     c                   endif
013500120622     c* Si no se a pulsado F3, F12, y no hay errores se procede a verificar si se ha pulsado
013600120622     c* la tecla de avance.
013700110620     c                   if        *inkc = *OFF and *in12 = *OFF
013800110620     c                             and *in80  =  *OFF
013900120622     c* Si se pulsa la tecla de avance el indicador 95 se enciende y se ejecuta el avance de
014000120622     c* pagina
014100110620     c                   if        *in95  =  *ON
014200111227     c                   eval      xkey   = xlast
014300111227     c                   exsr      xsflade
014400110620     c                   if        *in38  =  *ON
014500111227     c                   eval      xcodmg = 'dis0002'
014600110620     c                   exsr      wrtmsg
014700110620     c                   endif
014800110620     c                   else
014900120622     c* Si se pulsa la tecla de retroceso el indicador 96 se enciende y se ejecuta el retroceso de
015000120622     c* pagina
015100110620     c                   if        *in96  =  *ON
015200111227     c                   eval      xkey   = xfirst
015300110620     c                   exsr      sflatr
015400110620     c                   if        *in39  =  *ON
015500111227     c                   eval      xcodmg = 'dis0001'
015600110620     c                   exsr      wrtmsg
015700110620     c                   endif
015800110620     c                   else
015900120622     c* Si el campo Situar en tiene datos, se apagara el indicador 94
016000110620     c                   if        not(*in94)
016100111227     c                   eval      xkey   = xsitua
016200140325     c                   eval      wsit01 = *zeros
016300140310     c                   eval      wsit02 = *ZEROS
016400140320     c                   eval      wsit04 = *zeros
016500111227     c                   exsr      xsflade
016600110620     c                   else
016700110620     c                   if        (*in51)
016800140325     c                   eval      TM9W1A = *zeros
016900140325     c                   eval      TM9W2A = *zeros
017000140325     c                   eval      TM9W3A = *zeros
017100140325     C                   EVAL      XKEY   = *ZEROS
017200140320     c                   eval      xkey   = xfirst
017300111227     c                   exsr      xsflade
017400110620     c                   endif
017500110620     c                   endif
017600110620     c                   endif
017700110620     c                   endif
017800110620     c                   endif
017900110620     c                   endif
018000110620     c                   if        *inkc = *off
018100110620     c                   exsr      wrtsc1
018200110620     c                   endif
018300110620     c                   enddo
018400140325     c                   unlock    TMSMV221
018500110620     c                   seton                                        lr
018600110620     c                   return
018700140319     c*---------------------------------------------------------------
018800140319     c*  borra registros en subarchivo 1                             -
018900140319     c*---------------------------------------------------------------
019000110620     c     clrsf1        begsr
019100110620     c                   setoff                                       91
019200110620     c                   setoff                                       92
019300110620     c                   setoff                                       93
019400110621     c                   write     rsflctl1
019500110621     c                   seton                                        92
019600111227     c                   eval      xrgsf1 = *zeros
019700111227     c                   eval      xreg1  = *zeros
019800110621     c                   endsr
019900140319     c*---------------------------------------------------------------
020000140319     c*  carga datos adelante                                        -
020100140319     c*---------------------------------------------------------------
020200111227     c     xsflade       begsr
020300110621     c                   exsr      clrsf1
020400110621     c* ubica puntero en el registro a leer o el ultimo registro.
020500140325     c     xkeyf         setll     TMSMV221R                          30
020600110621     c                   if        *in30  =  *OFF
020700140325     c                   read(n)   TMSMV221R                              38
020800110621     c                   else
020900140325     c                   read(n)   TMSMV221R                              38
021000110621     c                   endif
021100140320     c* carga dato en forma ascendente                         7
021200111227     c                   eval      xfirst = xkey
021300110621     c                   dow       *in38  =  *OFF
021400111227     c                             and xreg1 < xmaxrg
021500110621     c                   exsr      movcam
021600111227     c                   add       1             xreg1
021700110621     c                   exsr      wrtsf1
021800140325     c                   read(n)   TMSMV221R                              38
021900110621     c                   enddo
022000110621     c* asigna el ultimo registro leido
022100110621     c                   if        *in38  =  *ON
022200111227     c                   eval      xlast  = xfirst
022300110621     c                   else
022400111227     c                   eval      xlast  = xkey
022500110621     c                   endif
022600110621     c                   endsr
022700140319     c*---------------------------------------------------------------
022800140319     c*  carga datos atras                                           -
022900140319     c*---------------------------------------------------------------
023000110621     c     sflatr        begsr
023100110621     c                   exsr      clrsf1
023200140315     c                   setoff                                           38
023300111227     c                   eval      xreg1  = xmaxrg
023400110621     c* se ubica en el ultimo registro leido y carga en forma descend.
023500111227     c                   eval      xlast  = xkey
023600140325     c     xkeyf         setll     TMSMV221R                          39
023700140325     c                   readp(n)  TMSMV221R                              39
023800110621     c                   dow       *in39  =  *OFF
023900111227     c                             and xreg1 > 0
024000110621     c                   exsr      movcam
024100140220     c                   exsr      wrtsf1
024200111227     c                   eval      xfirst = xkey
024300140325     c                   readp(n)  TMSMV221R                              39
024400111227     c                   sub       1             xreg1
024500110621     c                   enddo
024600110621     c* si llego al inicio del archivo, entonces carga hacia adelante.
024700110621     c                   if        *in39  =  *ON
024800111227     c                             and xreg1 >  0
024900111227     c                   exsr      xsflade
025000110621     c                   endif
025100110621     c                   endsr
025200140319     c*---------------------------------------------------------------
025300140319     c*  mover campos de archivo a subarchivo 1                      -
025400140319     c*---------------------------------------------------------------
025500110621     c     movcam        begsr
025600110621     c                   setoff                                       8182
025700140325     C                   z-add     1008          xtab              4 0
025800140325     c                   movel     TM60A         xcon             10
025900140325     C     ktab          CHAIN     TABEAR                             32
026000140325     C                   IF        *IN32 = '0'
026100140325     C                   EVAL      WOPERA = TADEEA
026200140325     C                   ELSE
026300140325     C                   EVAL      WOPERA = *BLANKS
026400140325     c                   endif
026500111014     c                   eval      wopc   = *zeros
026600140320     c                   seton                                        97
026700110621     c                   endsr
026800140319     c*---------------------------------------------------------------
026900140319     c*  escribe datos en subarchivo 1                               -
027000140319     c*---------------------------------------------------------------
027100110621     c     wrtsf1        begsr
027200110621     c                   seton                                        91
027300110621     c                   setoff                                       97
027400111227     c     xkey          ifge      xultop
027500110621     c                   seton                                        97
027600110621     c                   endif
027700111227     c                   add       1             xrgsf1
027800110621     c                   write     rsfl1
027900110621     c                   endsr
028000140319     c*---------------------------------------------------------------
028100140319     c*  escribe datos del subarchivo 1 en pantalla       *          -
028200140319     c*---------------------------------------------------------------
028300110621     c     wrtsc1        begsr
028400140325     c                   unlock    TMSMV221
028500110621     c                   write     rcab
028600110621     c                   write     rfnc1
028700110621     c                   write     rmsgctl
028800110621     c  n91              write     rnodto
028900110621     c                   exfmt     rsflctl1
029000110621     c* verifica si se ha modificado el subarchivo 1
029100110621     c                   if        *in91  =  *ON
029200110621     c                   readc     rsfl1                                  30
029300110621     c  n30              seton                                        93
029400110621     c                   endif
029500110621     c* borra mensajes de error
029600110621     c                   exsr      clrmsg
029700110621     c                   endsr
029800140319     c*---------------------------------------------------------------
029900140319     c*  valida datos del subarchivo                                 -
030000140319     c*---------------------------------------------------------------
030100111227     c     xvalsfl       begsr
030200110621     c                   setoff                                       80
030300110621     c* recorre subarchivo 1 y verifica campos suceptibles de entrada
030400110621     c                   setoff                                       97
030500111227     c                   do        xrgsf1        xreg1
030600111227     c     xreg1         chain     rsfl1                              30
030700110621     c                   setoff                                       8182
030800111014     c                   if        wopc <> 0
030900111227     c                             and wopc <> xopchg
031000111227     c                             and wopc <> xopdlt
031100111227     c                             and wopc <> xopvis
031200110621     c                   seton                                        8180
031300111227     c                   eval      xcodmg = 'dis0003'
031400111227     c                   movel     wopc          xdtamg
031500110621     c                   exsr      wrtmsg
031600110621     c                   else
031700111014     c                   if        wopc   <> 0
031800111227     c                   eval      xultop = xkey
031900110621     c                   endif
032000110621     c                   endif
032100110621     c                   update    rsfl1
032200110621     c                   enddo
032300110621     c                   endsr
032400140319     c*---------------------------------------------------------------
032500140319     c*  procesa opciones del subarchivo 1                           -
032600140319     c*---------------------------------------------------------------
032700111227     c     xproopc       begsr
032800110621     c                   setoff                                       51
032900140224     c                   if        *inkf  =  *ON
033000140311     c                   eval      xpetic = 'A'
033100111227     c                   exsr      xcalpgm
033200110621     c                   seton                                        51
033300110621     c                   endif
033400110621     c* recorre subarchivo 1 y ejecuta opciones en orden de entrada
033500110621     c                   if        *in93  =  *ON
033600110621     c                   setoff                                       97
033700111227     c                   eval      xreg1 = 1
033800111227     c                   dow       xreg1 <= xrgsf1
033900110621     c                             and *inkc  =  *OFF
034000110621     c                             and *in12  =  *OFF
034100110621     c                             and *in80  =  *OFF
034200111227     c     xreg1         chain     rsfl1                              30
034300111014     c                   if        wopc   <> 0
034400110621     c                   seton                                        51
034500111227     c                   exsr      xejeopc
034600110621     c   12              seton                                        97
034700110621     c  nkc
034800110621     cann12
034900111014     cann80              eval      wopc   = *zeros
035000110621     c  nkc              update    rsfl1
035100110621     c                   endif
035200111227     c                   add       1             xreg1
035300110621     c                   enddo
035400110621     c                   endif
035500110621     c*
035600110621     c                   setoff                                       kl
035700110621     c                   endsr
035800140319     c*---------------------------------------------------------------
035900140319     c*  ejecuta opciones del subarchivo 1                           -
036000140319     c*---------------------------------------------------------------
036100111227     c     xejeopc       begsr
036200111227     c                   if        wopc   =  xopdlt
036300110621     c                   exsr      dltreg
036400110621     c                   else
036500140325     c     XKEYF         setll     TMSMV221R                              30
036600110621     c                   if        *in30  =  *OFF
036700110621     c                   seton                                        8180
036800111227     c                   eval      xcodmg = 'dis0005'
036900111227     c                   eval      xdtamg = xkeymg
037000110621     c                   exsr      wrtmsg
037100110621     c                   else
037200140312     c                   if        wopc   =  xopchg
037300140312     c                   eval      xpetic = 'C'
037400140312     c                   exsr      xcalpgm
037500110621     c                   else
037600111227     c                   if        wopc   =  xopvis
037700140312     c                   eval      xpetic = 'V'
037800140312     c                   exsr      xcalpgm
037900110621     c                   endif
038000110621     c                   endif
038100110621     c                   endif
038200110621     c                   endif
038300110621     c                   endsr
038400140319     c*---------------------------------------------------------------
038500140319     c*  llama a programa de mantenimiento                           -
038600140319     c*---------------------------------------------------------------
038700140310     c     xcalpgm       begsr
038800140325     c                   eval      xpgm   = 'SMSRM041'
038900150925     c                   call      'SMSRM041'                           60
039000140311     c                   parm                    xkey
039100140311     c                   parm                    xpetic            1
039200140311     c                   parm      *blanks       xcodmg
039300140311     c                   parm      *blanks       xdtamg
039400140311     c                   parm      '0'           xf3               1
039500140311     c                   parm      '0'           xf12              1
039600140311     c                   eval      *inkc  = xf3
039700140311     c                   eval      *in12  = xf12
039800110621     c* verifica si el programa se proceso satisfactoriamente
039900140311     c                   if        *in60  =  *ON
040000140311     c                   eval      xcodmg = 'dis0006'
040100140311     c                   eval      xdtamg = xpgm
040200140311     c                   endif
040300140311     c                   if        xcodmg <> *blank
040400140311     c                   seton                                        8180
040500140311     c                   exsr      wrtmsg
040600140311     c                   endif
040700140310     c                   endsr
040800140319     c*--------------------------------------------------------------
040900140319     c*  suprimir registros de archivo                              -
041000140319     c*--------------------------------------------------------------
041100111227     c     dltreg        begsr
041200111227     c                   eval      xregax = xreg1
041300110621     c                   exsr      clrsf2
041400110621     c* recorre subarchivo 1 y llena subarchivo 2 (solo suprimir)
041500111227     c     xreg1         do        xrgsf1        xreg1
041600111227     c     xreg1         chain     rsfl1                              30
041700111227     c                   if        wopc   =  xopdlt
041800111227     c                   add       1             xreg2
041900110621     c                   write     rsfl2
042000110621     c                   endif
042100110621     c                   enddo
042200110621     c* pide verificacion de supresion
042300110621     c                   exsr      wrtsc2
042400110621     c                   if        *inkc  =  *OFF
042500110621     c                             and *in12  =  *OFF
042600111227     c                   exsr      xdltarc
042700110621     c                   endif
042800110621     c* lee registro respaldado del subarchivo 1
042900111227     c     xregax        chain     rsfl1                              30
043000110621     c                   endsr
043100140319     c*---------------------------------------------------------------
043200140319     c*  borra registros en subarchivo 2                             -
043300140319     c*---------------------------------------------------------------
043400110621     c     clrsf2        begsr
043500110621     c                   setoff                                       98
043600110621     c                   write     rsflctl2
043700110621     c                   seton                                        98
043800111227     c                   eval      xreg2  = *zeros
043900110621     c                   endsr
044000140319     c*--------------------------------------------------------------
044100140319     c*  escribe datos en pantalla del subarchivo 2                 -
044200140319     c*--------------------------------------------------------------
044300110621     c     wrtsc2        begsr
044400140325     c                   unlock    TMSMV221
044500110621     c                   write     rcab
044600110621     c                   write     rfnc2
044700110621     c                   exfmt     rsflctl2
044800110621     c                   endsr
044900140319     c*---------------------------------------------------------------
045000140319     c*  borra registros en el archivo primario                      -
045100140319     c*---------------------------------------------------------------
045200111227     c     xdltarc       begsr
045300110621     c* recorre subarchivo 1 y borra del archivo primario los registros
045400111227     c                   do        xrgsf1        xreg1
045500111227     c     xreg1         chain     rsfl1                              30
045600111227     c                   if        wopc   =  xopdlt
045700111014     c                   eval      wopc   = *zeros
045800110621     c                   update    rsfl1
045900140325     C     XKEYF         CHAIN     TMSMV221R
046000140325     c                   delete    TMSMV221R                            72
046100110621     c                   if        *in72 = *off
046200111227     c                   eval      xcodmg = 'dis0008'
046300111227     c                   eval      xdtamg = xkeymg
046400110621     c                   exsr      wrtmsg
046500110621     c                   else
046600110621     c                   seton                                        8180
046700111227     c                   eval      xcodmg = 'dis0025'
046800111227     c                   eval      xdtamg = xkeymg
046900110621     c                   exsr      wrtmsg
047000110621     c                   endif
047100110621     c                   endif
047200110621     c  n80              enddo
047300110621     c                   endsr
047400140319     c*---------------------------------------------------------------
047500140319     c*  librerias utilizadas                                        -
047600140319     c*---------------------------------------------------------------
047700110621     c* libreria de mensajes
047800110621     c*copy sbsmsg
047900110621     c     clrmsg        begsr
048000110621     c                   setoff                                       89
048100110621     c                   setoff                                       90
048200110621     c                   write     rmsgctl
048300110621     c                   seton                                        90
048400111227     c                   movel     *zeros        xregm             2 0
048500110621     c* borra cola de mensajes del programa
048600111227     c                   eval(R)   xcodmg = *blanks
048700110621     c                   exsr      wrtmsg
048800110621     c                   endsr
048900140319     c*---------------------------------------------------------------
049000140319     c*  escribe los mensajes en la cola del programa                -
049100140319     c*---------------------------------------------------------------
049200110621     c     wrtmsg        begsr
049300110914     c                   call      'PROCL000'
049400111227     c                   parm                    xcodmg            7
049500111227     c                   parm                    xdtamg           50
049600111014     c                   parm                    wprogr
049700111014     c                   parm                    wmsgke            4
049800111227     c                   if        xcodmg <> *blank
049900110621     c                   seton                                        89
050000111227     c                   add       1             xregm
050100110621     c                   write     rsflmsg
050200110621     c                   endif
050300110621     c                   endsr