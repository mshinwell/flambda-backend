(* TEST
 include tool-ocaml-lib;
 flags = "-w -a";
 ocaml_script_as_argument = "true";
 setup-ocaml-build-env;
 ocaml;
*)

open Lib;;
let rec f n =
  if n <= 0 then []
  else n :: f (n-1)
in
let l = f 300 in
Gc.major ();
if List.fold_left (+) 0 l <> 301 * 150 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 746
      11 RESTART
      12 GRAB 1
      14 ACC0
      15 BRANCHIFNOT 28
      17 ACC1
      18 PUSHACC1
      19 GETFIELD1
      20 PUSHOFFSETCLOSURE0
      21 APPLY2
      22 PUSHACC1
      23 GETFIELD0
      24 MAKEBLOCK2 0
      26 RETURN 2
      28 ACC1
      29 RETURN 2
      31 RESTART
      32 GRAB 3
      34 CONST0
      35 PUSHACC4
      36 LEINT
      37 BRANCHIFNOT 42
      39 CONST0
      40 RETURN 4
      42 ACC3
      43 PUSHACC3
      44 PUSHACC3
      45 PUSHACC3
      46 C_CALL4 caml_input
      48 PUSHCONST0
      49 PUSHACC1
      50 EQ
      51 BRANCHIFNOT 58
      53 GETGLOBAL End_of_file
      55 MAKEBLOCK1 0
      57 RAISE
      58 ACC0
      59 PUSHACC5
      60 SUBINT
      61 PUSHACC1
      62 PUSHACC5
      63 ADDINT
      64 PUSHACC4
      65 PUSHACC4
      66 PUSHOFFSETCLOSURE0
      67 APPTERM 4, 9
      70 ACC0
      71 C_CALL1 caml_input_scan_line
      73 PUSHCONST0
      74 PUSHACC1
      75 EQ
      76 BRANCHIFNOT 83
      78 GETGLOBAL End_of_file
      80 MAKEBLOCK1 0
      82 RAISE
      83 CONST0
      84 PUSHACC1
      85 GTINT
      86 BRANCHIFNOT 107
      88 ACC0
      89 OFFSETINT -1
      91 C_CALL1 create_string
      93 PUSHACC1
      94 OFFSETINT -1
      96 PUSHCONST0
      97 PUSHACC2
      98 PUSHACC5
      99 C_CALL4 caml_input
     101 ACC2
     102 C_CALL1 caml_input_char
     104 ACC0
     105 RETURN 3
     107 ACC0
     108 NEGINT
     109 C_CALL1 create_string
     111 PUSHACC1
     112 NEGINT
     113 PUSHCONST0
     114 PUSHACC2
     115 PUSHACC5
     116 C_CALL4 caml_input
     118 CONST0
     119 PUSHTRAP 130
     121 ACC6
     122 PUSHOFFSETCLOSURE0
     123 APPLY1
     124 PUSHACC5
     125 PUSHENVACC1
     126 APPLY2
     127 POPTRAP
     128 RETURN 3
     130 PUSHGETGLOBAL End_of_file
     132 PUSHACC1
     133 GETFIELD0
     134 EQ
     135 BRANCHIFNOT 140
     137 ACC1
     138 RETURN 4
     140 ACC0
     141 RAISE
     142 ACC0
     143 C_CALL1 caml_flush
     145 RETURN 1
     147 RESTART
     148 GRAB 1
     150 ACC1
     151 PUSHACC1
     152 C_CALL2 caml_output_char
     154 RETURN 2
     156 RESTART
     157 GRAB 1
     159 ACC1
     160 PUSHACC1
     161 C_CALL2 caml_output_char
     163 RETURN 2
     165 RESTART
     166 GRAB 1
     168 ACC1
     169 PUSHACC1
     170 C_CALL2 caml_output_int
     172 RETURN 2
     174 RESTART
     175 GRAB 1
     177 ACC1
     178 PUSHACC1
     179 C_CALL2 caml_seek_out
     181 RETURN 2
     183 ACC0
     184 C_CALL1 caml_pos_out
     186 RETURN 1
     188 ACC0
     189 C_CALL1 caml_channel_size
     191 RETURN 1
     193 RESTART
     194 GRAB 1
     196 ACC1
     197 PUSHACC1
     198 C_CALL2 caml_set_binary_mode
     200 RETURN 2
     202 ACC0
     203 C_CALL1 caml_input_char
     205 RETURN 1
     207 ACC0
     208 C_CALL1 caml_input_char
     210 RETURN 1
     212 ACC0
     213 C_CALL1 caml_input_int
     215 RETURN 1
     217 ACC0
     218 C_CALL1 input_value
     220 RETURN 1
     222 RESTART
     223 GRAB 1
     225 ACC1
     226 PUSHACC1
     227 C_CALL2 caml_seek_in
     229 RETURN 2
     231 ACC0
     232 C_CALL1 caml_pos_in
     234 RETURN 1
     236 ACC0
     237 C_CALL1 caml_channel_size
     239 RETURN 1
     241 ACC0
     242 C_CALL1 caml_close_channel
     244 RETURN 1
     246 RESTART
     247 GRAB 1
     249 ACC1
     250 PUSHACC1
     251 C_CALL2 caml_set_binary_mode
     253 RETURN 2
     255 CONST0
     256 PUSHENVACC1
     257 APPLY1
     258 ACC0
     259 C_CALL1 sys_exit
     261 RETURN 1
     263 CONST0
     264 PUSHENVACC1
     265 GETFIELD0
     266 APPTERM1 2
     268 CONST0
     269 PUSHENVACC1
     270 APPLY1
     271 CONST0
     272 PUSHENVACC2
     273 APPTERM1 2
     275 ENVACC1
     276 GETFIELD0
     277 PUSHACC0
     278 PUSHACC2
     279 CLOSURE 2, 268
     282 PUSHENVACC1
     283 SETFIELD0
     284 RETURN 2
     286 ENVACC1
     287 C_CALL1 caml_flush
     289 ENVACC2
     290 C_CALL1 caml_flush
     292 RETURN 1
     294 CONST0
     295 PUSHENVACC1
     296 APPLY1
     297 C_CALL1 float_of_string
     299 RETURN 1
     301 CONST0
     302 PUSHENVACC1
     303 APPLY1
     304 C_CALL1 int_of_string
     306 RETURN 1
     308 ENVACC2
     309 C_CALL1 caml_flush
     311 ENVACC1
     312 PUSHENVACC3
     313 APPTERM1 2
     315 CONSTINT 13
     317 PUSHENVACC1
     318 C_CALL2 caml_output_char
     320 ENVACC1
     321 C_CALL1 caml_flush
     323 RETURN 1
     325 ACC0
     326 PUSHENVACC1
     327 PUSHENVACC2
     328 APPLY2
     329 CONSTINT 13
     331 PUSHENVACC1
     332 C_CALL2 caml_output_char
     334 ENVACC1
     335 C_CALL1 caml_flush
     337 RETURN 1
     339 ACC0
     340 PUSHENVACC1
     341 APPLY1
     342 PUSHENVACC2
     343 PUSHENVACC3
     344 APPTERM2 3
     346 ACC0
     347 PUSHENVACC1
     348 APPLY1
     349 PUSHENVACC2
     350 PUSHENVACC3
     351 APPTERM2 3
     353 ACC0
     354 PUSHENVACC1
     355 PUSHENVACC2
     356 APPTERM2 3
     358 ACC0
     359 PUSHENVACC1
     360 C_CALL2 caml_output_char
     362 RETURN 1
     364 CONSTINT 13
     366 PUSHENVACC1
     367 C_CALL2 caml_output_char
     369 ENVACC1
     370 C_CALL1 caml_flush
     372 RETURN 1
     374 ACC0
     375 PUSHENVACC1
     376 PUSHENVACC2
     377 APPLY2
     378 CONSTINT 13
     380 PUSHENVACC1
     381 C_CALL2 caml_output_char
     383 RETURN 1
     385 ACC0
     386 PUSHENVACC1
     387 APPLY1
     388 PUSHENVACC2
     389 PUSHENVACC3
     390 APPTERM2 3
     392 ACC0
     393 PUSHENVACC1
     394 APPLY1
     395 PUSHENVACC2
     396 PUSHENVACC3
     397 APPTERM2 3
     399 ACC0
     400 PUSHENVACC1
     401 PUSHENVACC2
     402 APPTERM2 3
     404 ACC0
     405 PUSHENVACC1
     406 C_CALL2 caml_output_char
     408 RETURN 1
     410 RESTART
     411 GRAB 3
     413 CONST0
     414 PUSHACC3
     415 LTINT
     416 BRANCHIF 427
     418 ACC1
     419 C_CALL1 ml_string_length
     421 PUSHACC4
     422 PUSHACC4
     423 ADDINT
     424 GTINT
     425 BRANCHIFNOT 432
     427 GETGLOBAL "really_input"
     429 PUSHENVACC1
     430 APPTERM1 5
     432 ACC3
     433 PUSHACC3
     434 PUSHACC3
     435 PUSHACC3
     436 PUSHENVACC2
     437 APPTERM 4, 8
     440 RESTART
     441 GRAB 3
     443 CONST0
     444 PUSHACC3
     445 LTINT
     446 BRANCHIF 457
     448 ACC1
     449 C_CALL1 ml_string_length
     451 PUSHACC4
     452 PUSHACC4
     453 ADDINT
     454 GTINT
     455 BRANCHIFNOT 462
     457 GETGLOBAL "input"
     459 PUSHENVACC1
     460 APPTERM1 5
     462 ACC3
     463 PUSHACC3
     464 PUSHACC3
     465 PUSHACC3
     466 C_CALL4 caml_input
     468 RETURN 4
     470 ACC0
     471 PUSHCONST0
     472 PUSHGETGLOBAL <0>(0, <0>(6, 0))
     474 PUSHENVACC1
     475 APPTERM3 4
     477 ACC0
     478 PUSHCONST0
     479 PUSHGETGLOBAL <0>(0, <0>(7, 0))
     481 PUSHENVACC1
     482 APPTERM3 4
     484 RESTART
     485 GRAB 2
     487 ACC1
     488 PUSHACC1
     489 PUSHACC4
     490 C_CALL3 sys_open
     492 C_CALL1 caml_open_descriptor
     494 RETURN 3
     496 ACC0
     497 C_CALL1 caml_flush
     499 ACC0
     500 C_CALL1 caml_close_channel
     502 RETURN 1
     504 RESTART
     505 GRAB 1
     507 CONST0
     508 PUSHACC2
     509 PUSHACC2
     510 C_CALL3 output_value
     512 RETURN 2
     514 RESTART
     515 GRAB 3
     517 CONST0
     518 PUSHACC3
     519 LTINT
     520 BRANCHIF 531
     522 ACC1
     523 C_CALL1 ml_string_length
     525 PUSHACC4
     526 PUSHACC4
     527 ADDINT
     528 GTINT
     529 BRANCHIFNOT 536
     531 GETGLOBAL "output"
     533 PUSHENVACC1
     534 APPTERM1 5
     536 ACC3
     537 PUSHACC3
     538 PUSHACC3
     539 PUSHACC3
     540 C_CALL4 caml_output
     542 RETURN 4
     544 RESTART
     545 GRAB 1
     547 ACC1
     548 C_CALL1 ml_string_length
     550 PUSHCONST0
     551 PUSHACC3
     552 PUSHACC3
     553 C_CALL4 caml_output
     555 RETURN 2
     557 ACC0
     558 PUSHCONSTINT 438
     560 PUSHGETGLOBAL <0>(1, <0>(3, <0>(4, <0>(6, 0))))
     562 PUSHENVACC1
     563 APPTERM3 4
     565 ACC0
     566 PUSHCONSTINT 438
     568 PUSHGETGLOBAL <0>(1, <0>(3, <0>(4, <0>(7, 0))))
     570 PUSHENVACC1
     571 APPTERM3 4
     573 RESTART
     574 GRAB 2
     576 ACC1
     577 PUSHACC1
     578 PUSHACC4
     579 C_CALL3 sys_open
     581 C_CALL1 caml_open_descriptor
     583 RETURN 3
     585 ACC0
     586 PUSHGETGLOBAL "%.12g"
     588 C_CALL2 format_float
     590 RETURN 1
     592 ACC0
     593 PUSHGETGLOBAL "%d"
     595 C_CALL2 format_int
     597 RETURN 1
     599 GETGLOBAL "false"
     601 PUSHACC1
     602 C_CALL2 string_equal
     604 BRANCHIFNOT 609
     606 CONST0
     607 RETURN 1
     609 GETGLOBAL "true"
     611 PUSHACC1
     612 C_CALL2 string_equal
     614 BRANCHIFNOT 619
     616 CONST1
     617 RETURN 1
     619 GETGLOBAL "bool_of_string"
     621 PUSHENVACC1
     622 APPTERM1 2
     624 ACC0
     625 BRANCHIFNOT 631
     627 GETGLOBAL "true"
     629 RETURN 1
     631 GETGLOBAL "false"
     633 RETURN 1
     635 CONST0
     636 PUSHACC1
     637 LTINT
     638 BRANCHIF 646
     640 CONSTINT 255
     642 PUSHACC1
     643 GTINT
     644 BRANCHIFNOT 651
     646 GETGLOBAL "char_of_int"
     648 PUSHENVACC1
     649 APPTERM1 2
     651 ACC0
     652 RETURN 1
     654 RESTART
     655 GRAB 1
     657 ACC0
     658 C_CALL1 ml_string_length
     660 PUSHACC2
     661 C_CALL1 ml_string_length
     663 PUSHACC0
     664 PUSHACC2
     665 ADDINT
     666 C_CALL1 create_string
     668 PUSHACC2
     669 PUSHCONST0
     670 PUSHACC2
     671 PUSHCONST0
     672 PUSHACC7
     673 C_CALL5 blit_string
     675 ACC1
     676 PUSHACC3
     677 PUSHACC2
     678 PUSHCONST0
     679 PUSHACC 8
     681 C_CALL5 blit_string
     683 ACC0
     684 RETURN 5
     686 CONSTINT -1
     688 PUSHACC1
     689 XORINT
     690 RETURN 1
     692 CONST0
     693 PUSHACC1
     694 GEINT
     695 BRANCHIFNOT 700
     697 ACC0
     698 RETURN 1
     700 ACC0
     701 NEGINT
     702 RETURN 1
     704 RESTART
     705 GRAB 1
     707 ACC1
     708 PUSHACC1
     709 C_CALL2 greaterequal
     711 BRANCHIFNOT 716
     713 ACC0
     714 RETURN 2
     716 ACC1
     717 RETURN 2
     719 RESTART
     720 GRAB 1
     722 ACC1
     723 PUSHACC1
     724 C_CALL2 lessequal
     726 BRANCHIFNOT 731
     728 ACC0
     729 RETURN 2
     731 ACC1
     732 RETURN 2
     734 ACC0
     735 PUSHGETGLOBAL Invalid_argument
     737 MAKEBLOCK2 0
     739 RAISE
     740 ACC0
     741 PUSHGETGLOBAL Failure
     743 MAKEBLOCK2 0
     745 RAISE
     746 CLOSURE 0, 740
     749 PUSH
     750 CLOSURE 0, 734
     753 PUSHGETGLOBAL "Pervasives.Exit"
     755 MAKEBLOCK1 0
     757 PUSHGETGLOBAL "Pervasives.Assert_failure"
     759 MAKEBLOCK1 0
     761 PUSH
     762 CLOSURE 0, 720
     765 PUSH
     766 CLOSURE 0, 705
     769 PUSH
     770 CLOSURE 0, 692
     773 PUSH
     774 CLOSURE 0, 686
     777 PUSHCONST0
     778 PUSHCONSTINT 31
     780 PUSHCONST1
     781 LSLINT
     782 EQ
     783 BRANCHIFNOT 789
     785 CONSTINT 30
     787 BRANCH 791
     789 CONSTINT 62
     791 PUSHCONST1
     792 LSLINT
     793 PUSHACC0
     794 OFFSETINT -1
     796 PUSH
     797 CLOSURE 0, 655
     800 PUSHACC 9
     802 CLOSURE 1, 635
     805 PUSH
     806 CLOSURE 0, 624
     809 PUSHACC 11
     811 CLOSURE 1, 599
     814 PUSH
     815 CLOSURE 0, 592
     818 PUSH
     819 CLOSURE 0, 585
     822 PUSH
     823 CLOSUREREC 0, 12
     827 CONST0
     828 C_CALL1 caml_open_descriptor
     830 PUSHCONST1
     831 C_CALL1 caml_open_descriptor
     833 PUSHCONST2
     834 C_CALL1 caml_open_descriptor
     836 PUSH
     837 CLOSURE 0, 574
     840 PUSHACC0
     841 CLOSURE 1, 565
     844 PUSHACC1
     845 CLOSURE 1, 557
     848 PUSH
     849 CLOSURE 0, 545
     852 PUSHACC 22
     854 CLOSURE 1, 515
     857 PUSH
     858 CLOSURE 0, 505
     861 PUSH
     862 CLOSURE 0, 496
     865 PUSH
     866 CLOSURE 0, 485
     869 PUSHACC0
     870 CLOSURE 1, 477
     873 PUSHACC1
     874 CLOSURE 1, 470
     877 PUSHACC 28
     879 CLOSURE 1, 441
     882 PUSH
     883 CLOSUREREC 0, 32
     887 ACC0
     888 PUSHACC 31
     890 CLOSURE 2, 411
     893 PUSHACC 22
     895 CLOSUREREC 1, 70
     899 ACC 15
     901 CLOSURE 1, 404
     904 PUSHACC 11
     906 PUSHACC 17
     908 CLOSURE 2, 399
     911 PUSHACC 12
     913 PUSHACC 18
     915 PUSHACC 23
     917 CLOSURE 3, 392
     920 PUSHACC 13
     922 PUSHACC 19
     924 PUSHACC 23
     926 CLOSURE 3, 385
     929 PUSHACC 14
     931 PUSHACC 20
     933 CLOSURE 2, 374
     936 PUSHACC 20
     938 CLOSURE 1, 364
     941 PUSHACC 20
     943 CLOSURE 1, 358
     946 PUSHACC 17
     948 PUSHACC 22
     950 CLOSURE 2, 353
     953 PUSHACC 18
     955 PUSHACC 23
     957 PUSHACC 29
     959 CLOSURE 3, 346
     962 PUSHACC 19
     964 PUSHACC 24
     966 PUSHACC 29
     968 CLOSURE 3, 339
     971 PUSHACC 20
     973 PUSHACC 25
     975 CLOSURE 2, 325
     978 PUSHACC 25
     980 CLOSURE 1, 315
     983 PUSHACC 12
     985 PUSHACC 28
     987 PUSHACC 30
     989 CLOSURE 3, 308
     992 PUSHACC0
     993 CLOSURE 1, 301
     996 PUSHACC1
     997 CLOSURE 1, 294
    1000 PUSHACC 29
    1002 PUSHACC 31
    1004 CLOSURE 2, 286
    1007 MAKEBLOCK1 0
    1009 PUSHACC0
    1010 CLOSURE 1, 275
    1013 PUSHACC1
    1014 CLOSURE 1, 263
    1017 PUSHACC0
    1018 CLOSURE 1, 255
    1021 PUSHACC1
    1022 PUSHACC 22
    1024 PUSHACC4
    1025 PUSHACC3
    1026 PUSH
    1027 CLOSURE 0, 247
    1030 PUSH
    1031 CLOSURE 0, 241
    1034 PUSH
    1035 CLOSURE 0, 236
    1038 PUSH
    1039 CLOSURE 0, 231
    1042 PUSH
    1043 CLOSURE 0, 223
    1046 PUSH
    1047 CLOSURE 0, 217
    1050 PUSH
    1051 CLOSURE 0, 212
    1054 PUSH
    1055 CLOSURE 0, 207
    1058 PUSHACC 32
    1060 PUSHACC 35
    1062 PUSHACC 33
    1064 PUSH
    1065 CLOSURE 0, 202
    1068 PUSHACC 41
    1070 PUSHACC 40
    1072 PUSHACC 42
    1074 PUSH
    1075 CLOSURE 0, 194
    1078 PUSHACC 46
    1080 PUSH
    1081 CLOSURE 0, 188
    1084 PUSH
    1085 CLOSURE 0, 183
    1088 PUSH
    1089 CLOSURE 0, 175
    1092 PUSHACC 51
    1094 PUSH
    1095 CLOSURE 0, 166
    1098 PUSH
    1099 CLOSURE 0, 157
    1102 PUSHACC 55
    1104 PUSHACC 57
    1106 PUSH
    1107 CLOSURE 0, 148
    1110 PUSH
    1111 CLOSURE 0, 142
    1114 PUSHACC 63
    1116 PUSHACC 62
    1118 PUSHACC 64
    1120 PUSHACC 38
    1122 PUSHACC 40
    1124 PUSHACC 42
    1126 PUSHACC 44
    1128 PUSHACC 46
    1130 PUSHACC 48
    1132 PUSHACC 50
    1134 PUSHACC 52
    1136 PUSHACC 54
    1138 PUSHACC 56
    1140 PUSHACC 58
    1142 PUSHACC 60
    1144 PUSHACC 62
    1146 PUSHACC 64
    1148 PUSHACC 66
    1150 PUSHACC 82
    1152 PUSHACC 84
    1154 PUSHACC 86
    1156 PUSHACC 88
    1158 PUSHACC 90
    1160 PUSHACC 92
    1162 PUSHACC 94
    1164 PUSHACC 96
    1166 PUSHACC 98
    1168 PUSHACC 100
    1170 PUSHACC 104
    1172 PUSHACC 104
    1174 PUSHACC 104
    1176 PUSHACC 108
    1178 PUSHACC 110
    1180 PUSHACC 112
    1182 PUSHACC 117
    1184 PUSHACC 117
    1186 PUSHACC 117
    1188 PUSHACC 117
    1190 MAKEBLOCK 69, 0
    1193 POP 53
    1195 SETGLOBAL Pervasives
    1197 BRANCH 2177
    1199 RESTART
    1200 GRAB 1
    1202 ACC1
    1203 BRANCHIFNOT 1213
    1205 ACC1
    1206 GETFIELD1
    1207 PUSHACC1
    1208 OFFSETINT 1
    1210 PUSHOFFSETCLOSURE0
    1211 APPTERM2 4
    1213 ACC0
    1214 RETURN 2
    1216 RESTART
    1217 GRAB 1
    1219 ACC0
    1220 BRANCHIFNOT 1251
    1222 CONST0
    1223 PUSHACC2
    1224 EQ
    1225 BRANCHIFNOT 1231
    1227 ACC0
    1228 GETFIELD0
    1229 RETURN 2
    1231 CONST0
    1232 PUSHACC2
    1233 GTINT
    1234 BRANCHIFNOT 1244
    1236 ACC1
    1237 OFFSETINT -1
    1239 PUSHACC1
    1240 GETFIELD1
    1241 PUSHOFFSETCLOSURE0
    1242 APPTERM2 4
    1244 GETGLOBAL "List.nth"
    1246 PUSHGETGLOBALFIELD Pervasives, 2
    1249 APPTERM1 3
    1251 GETGLOBAL "nth"
    1253 PUSHGETGLOBALFIELD Pervasives, 3
    1256 APPTERM1 3
    1258 RESTART
    1259 GRAB 1
    1261 ACC0
    1262 BRANCHIFNOT 1274
    1264 ACC1
    1265 PUSHACC1
    1266 GETFIELD0
    1267 MAKEBLOCK2 0
    1269 PUSHACC1
    1270 GETFIELD1
    1271 PUSHOFFSETCLOSURE0
    1272 APPTERM2 4
    1274 ACC1
    1275 RETURN 2
    1277 ACC0
    1278 BRANCHIFNOT 1291
    1280 ACC0
    1281 GETFIELD1
    1282 PUSHOFFSETCLOSURE0
    1283 APPLY1
    1284 PUSHACC1
    1285 GETFIELD0
    1286 PUSHGETGLOBALFIELD Pervasives, 16
    1289 APPTERM2 3
    1291 RETURN 1
    1293 RESTART
    1294 GRAB 1
    1296 ACC1
    1297 BRANCHIFNOT 1313
    1299 ACC1
    1300 GETFIELD0
    1301 PUSHACC1
    1302 APPLY1
    1303 PUSHACC2
    1304 GETFIELD1
    1305 PUSHACC2
    1306 PUSHOFFSETCLOSURE0
    1307 APPLY2
    1308 PUSHACC1
    1309 MAKEBLOCK2 0
    1311 POP 1
    1313 RETURN 2
    1315 RESTART
    1316 GRAB 1
    1318 ACC1
    1319 BRANCHIFNOT 1331
    1321 ACC1
    1322 GETFIELD0
    1323 PUSHACC1
    1324 APPLY1
    1325 ACC1
    1326 GETFIELD1
    1327 PUSHACC1
    1328 PUSHOFFSETCLOSURE0
    1329 APPTERM2 4
    1331 RETURN 2
    1333 RESTART
    1334 GRAB 2
    1336 ACC2
    1337 BRANCHIFNOT 1350
    1339 ACC2
    1340 GETFIELD1
    1341 PUSHACC3
    1342 GETFIELD0
    1343 PUSHACC3
    1344 PUSHACC3
    1345 APPLY2
    1346 PUSHACC2
    1347 PUSHOFFSETCLOSURE0
    1348 APPTERM3 6
    1350 ACC1
    1351 RETURN 3
    1353 RESTART
    1354 GRAB 2
    1356 ACC1
    1357 BRANCHIFNOT 1370
    1359 ACC2
    1360 PUSHACC2
    1361 GETFIELD1
    1362 PUSHACC2
    1363 PUSHOFFSETCLOSURE0
    1364 APPLY3
    1365 PUSHACC2
    1366 GETFIELD0
    1367 PUSHACC2
    1368 APPTERM2 5
    1370 ACC2
    1371 RETURN 3
    1373 RESTART
    1374 GRAB 2
    1376 ACC1
    1377 BRANCHIFNOT 1400
    1379 ACC2
    1380 BRANCHIFNOT 1407
    1382 ACC2
    1383 GETFIELD0
    1384 PUSHACC2
    1385 GETFIELD0
    1386 PUSHACC2
    1387 APPLY2
    1388 PUSHACC3
    1389 GETFIELD1
    1390 PUSHACC3
    1391 GETFIELD1
    1392 PUSHACC3
    1393 PUSHOFFSETCLOSURE0
    1394 APPLY3
    1395 PUSHACC1
    1396 MAKEBLOCK2 0
    1398 RETURN 4
    1400 ACC2
    1401 BRANCHIFNOT 1405
    1403 BRANCH 1407
    1405 RETURN 3
    1407 GETGLOBAL "List.map2"
    1409 PUSHGETGLOBALFIELD Pervasives, 2
    1412 APPTERM1 4
    1414 RESTART
    1415 GRAB 2
    1417 ACC1
    1418 BRANCHIFNOT 1437
    1420 ACC2
    1421 BRANCHIFNOT 1444
    1423 ACC2
    1424 GETFIELD0
    1425 PUSHACC2
    1426 GETFIELD0
    1427 PUSHACC2
    1428 APPLY2
    1429 ACC2
    1430 GETFIELD1
    1431 PUSHACC2
    1432 GETFIELD1
    1433 PUSHACC2
    1434 PUSHOFFSETCLOSURE0
    1435 APPTERM3 6
    1437 ACC2
    1438 BRANCHIFNOT 1442
    1440 BRANCH 1444
    1442 RETURN 3
    1444 GETGLOBAL "List.iter2"
    1446 PUSHGETGLOBALFIELD Pervasives, 2
    1449 APPTERM1 4
    1451 RESTART
    1452 GRAB 3
    1454 ACC2
    1455 BRANCHIFNOT 1476
    1457 ACC3
    1458 BRANCHIFNOT 1482
    1460 ACC3
    1461 GETFIELD1
    1462 PUSHACC3
    1463 GETFIELD1
    1464 PUSHACC5
    1465 GETFIELD0
    1466 PUSHACC5
    1467 GETFIELD0
    1468 PUSHACC5
    1469 PUSHACC5
    1470 APPLY3
    1471 PUSHACC3
    1472 PUSHOFFSETCLOSURE0
    1473 APPTERM 4, 8
    1476 ACC3
    1477 BRANCHIF 1482
    1479 ACC1
    1480 RETURN 4
    1482 GETGLOBAL "List.fold_left2"
    1484 PUSHGETGLOBALFIELD Pervasives, 2
    1487 APPTERM1 5
    1489 RESTART
    1490 GRAB 3
    1492 ACC1
    1493 BRANCHIFNOT 1516
    1495 ACC2
    1496 BRANCHIFNOT 1522
    1498 PUSH_RETADDR 1509
    1500 ACC6
    1501 PUSHACC6
    1502 GETFIELD1
    1503 PUSHACC6
    1504 GETFIELD1
    1505 PUSHACC6
    1506 PUSHOFFSETCLOSURE0
    1507 APPLY 4
    1509 PUSHACC3
    1510 GETFIELD0
    1511 PUSHACC3
    1512 GETFIELD0
    1513 PUSHACC3
    1514 APPTERM3 7
    1516 ACC2
    1517 BRANCHIF 1522
    1519 ACC3
    1520 RETURN 4
    1522 GETGLOBAL "List.fold_right2"
    1524 PUSHGETGLOBALFIELD Pervasives, 2
    1527 APPTERM1 5
    1529 RESTART
    1530 GRAB 1
    1532 ACC1
    1533 BRANCHIFNOT 1549
    1535 ACC1
    1536 GETFIELD0
    1537 PUSHACC1
    1538 APPLY1
    1539 BRANCHIFNOT 1547
    1541 ACC1
    1542 GETFIELD1
    1543 PUSHACC1
    1544 PUSHOFFSETCLOSURE0
    1545 APPTERM2 4
    1547 RETURN 2
    1549 CONST1
    1550 RETURN 2
    1552 RESTART
    1553 GRAB 1
    1555 ACC1
    1556 BRANCHIFNOT 1570
    1558 ACC1
    1559 GETFIELD0
    1560 PUSHACC1
    1561 APPLY1
    1562 BRANCHIF 1570
    1564 ACC1
    1565 GETFIELD1
    1566 PUSHACC1
    1567 PUSHOFFSETCLOSURE0
    1568 APPTERM2 4
    1570 RETURN 2
    1572 RESTART
    1573 GRAB 2
    1575 ACC1
    1576 BRANCHIFNOT 1599
    1578 ACC2
    1579 BRANCHIFNOT 1605
    1581 ACC2
    1582 GETFIELD0
    1583 PUSHACC2
    1584 GETFIELD0
    1585 PUSHACC2
    1586 APPLY2
    1587 BRANCHIFNOT 1597
    1589 ACC2
    1590 GETFIELD1
    1591 PUSHACC2
    1592 GETFIELD1
    1593 PUSHACC2
    1594 PUSHOFFSETCLOSURE0
    1595 APPTERM3 6
    1597 RETURN 3
    1599 ACC2
    1600 BRANCHIF 1605
    1602 CONST1
    1603 RETURN 3
    1605 GETGLOBAL "List.for_all2"
    1607 PUSHGETGLOBALFIELD Pervasives, 2
    1610 APPTERM1 4
    1612 RESTART
    1613 GRAB 2
    1615 ACC1
    1616 BRANCHIFNOT 1639
    1618 ACC2
    1619 BRANCHIFNOT 1646
    1621 ACC2
    1622 GETFIELD0
    1623 PUSHACC2
    1624 GETFIELD0
    1625 PUSHACC2
    1626 APPLY2
    1627 BRANCHIF 1637
    1629 ACC2
    1630 GETFIELD1
    1631 PUSHACC2
    1632 GETFIELD1
    1633 PUSHACC2
    1634 PUSHOFFSETCLOSURE0
    1635 APPTERM3 6
    1637 RETURN 3
    1639 ACC2
    1640 BRANCHIFNOT 1644
    1642 BRANCH 1646
    1644 RETURN 3
    1646 GETGLOBAL "List.exists2"
    1648 PUSHGETGLOBALFIELD Pervasives, 2
    1651 APPTERM1 4
    1653 RESTART
    1654 GRAB 1
    1656 ACC1
    1657 BRANCHIFNOT 1672
    1659 ACC0
    1660 PUSHACC2
    1661 GETFIELD0
    1662 C_CALL2 equal
    1664 BRANCHIF 1672
    1666 ACC1
    1667 GETFIELD1
    1668 PUSHACC1
    1669 PUSHOFFSETCLOSURE0
    1670 APPTERM2 4
    1672 RETURN 2
    1674 RESTART
    1675 GRAB 1
    1677 ACC1
    1678 BRANCHIFNOT 1692
    1680 ACC0
    1681 PUSHACC2
    1682 GETFIELD0
    1683 EQ
    1684 BRANCHIF 1692
    1686 ACC1
    1687 GETFIELD1
    1688 PUSHACC1
    1689 PUSHOFFSETCLOSURE0
    1690 APPTERM2 4
    1692 RETURN 2
    1694 RESTART
    1695 GRAB 1
    1697 ACC1
    1698 BRANCHIFNOT 1719
    1700 ACC1
    1701 GETFIELD0
    1702 PUSHACC1
    1703 PUSHACC1
    1704 GETFIELD0
    1705 C_CALL2 equal
    1707 BRANCHIFNOT 1713
    1709 ACC0
    1710 GETFIELD1
    1711 RETURN 3
    1713 ACC2
    1714 GETFIELD1
    1715 PUSHACC2
    1716 PUSHOFFSETCLOSURE0
    1717 APPTERM2 5
    1719 GETGLOBAL Not_found
    1721 MAKEBLOCK1 0
    1723 RAISE
    1724 RESTART
    1725 GRAB 1
    1727 ACC1
    1728 BRANCHIFNOT 1748
    1730 ACC1
    1731 GETFIELD0
    1732 PUSHACC1
    1733 PUSHACC1
    1734 GETFIELD0
    1735 EQ
    1736 BRANCHIFNOT 1742
    1738 ACC0
    1739 GETFIELD1
    1740 RETURN 3
    1742 ACC2
    1743 GETFIELD1
    1744 PUSHACC2
    1745 PUSHOFFSETCLOSURE0
    1746 APPTERM2 5
    1748 GETGLOBAL Not_found
    1750 MAKEBLOCK1 0
    1752 RAISE
    1753 RESTART
    1754 GRAB 1
    1756 ACC1
    1757 BRANCHIFNOT 1773
    1759 ACC0
    1760 PUSHACC2
    1761 GETFIELD0
    1762 GETFIELD0
    1763 C_CALL2 equal
    1765 BRANCHIF 1773
    1767 ACC1
    1768 GETFIELD1
    1769 PUSHACC1
    1770 PUSHOFFSETCLOSURE0
    1771 APPTERM2 4
    1773 RETURN 2
    1775 RESTART
    1776 GRAB 1
    1778 ACC1
    1779 BRANCHIFNOT 1794
    1781 ACC0
    1782 PUSHACC2
    1783 GETFIELD0
    1784 GETFIELD0
    1785 EQ
    1786 BRANCHIF 1794
    1788 ACC1
    1789 GETFIELD1
    1790 PUSHACC1
    1791 PUSHOFFSETCLOSURE0
    1792 APPTERM2 4
    1794 RETURN 2
    1796 RESTART
    1797 GRAB 1
    1799 ACC1
    1800 BRANCHIFNOT 1825
    1802 ACC1
    1803 GETFIELD0
    1804 PUSHACC2
    1805 GETFIELD1
    1806 PUSHACC2
    1807 PUSHACC2
    1808 GETFIELD0
    1809 C_CALL2 equal
    1811 BRANCHIFNOT 1816
    1813 ACC0
    1814 RETURN 4
    1816 ACC0
    1817 PUSHACC3
    1818 PUSHOFFSETCLOSURE0
    1819 APPLY2
    1820 PUSHACC2
    1821 MAKEBLOCK2 0
    1823 POP 2
    1825 RETURN 2
    1827 RESTART
    1828 GRAB 1
    1830 ACC1
    1831 BRANCHIFNOT 1855
    1833 ACC1
    1834 GETFIELD0
    1835 PUSHACC2
    1836 GETFIELD1
    1837 PUSHACC2
    1838 PUSHACC2
    1839 GETFIELD0
    1840 EQ
    1841 BRANCHIFNOT 1846
    1843 ACC0
    1844 RETURN 4
    1846 ACC0
    1847 PUSHACC3
    1848 PUSHOFFSETCLOSURE0
    1849 APPLY2
    1850 PUSHACC2
    1851 MAKEBLOCK2 0
    1853 POP 2
    1855 RETURN 2
    1857 RESTART
    1858 GRAB 1
    1860 ACC1
    1861 BRANCHIFNOT 1879
    1863 ACC1
    1864 GETFIELD0
    1865 PUSHACC0
    1866 PUSHACC2
    1867 APPLY1
    1868 BRANCHIFNOT 1873
    1870 ACC0
    1871 RETURN 3
    1873 ACC2
    1874 GETFIELD1
    1875 PUSHACC2
    1876 PUSHOFFSETCLOSURE0
    1877 APPTERM2 5
    1879 GETGLOBAL Not_found
    1881 MAKEBLOCK1 0
    1883 RAISE
    1884 RESTART
    1885 GRAB 2
    1887 ACC2
    1888 BRANCHIFNOT 1917
    1890 ACC2
    1891 GETFIELD0
    1892 PUSHACC3
    1893 GETFIELD1
    1894 PUSHACC1
    1895 PUSHENVACC2
    1896 APPLY1
    1897 BRANCHIFNOT 1908
    1899 ACC0
    1900 PUSHACC4
    1901 PUSHACC4
    1902 PUSHACC4
    1903 MAKEBLOCK2 0
    1905 PUSHOFFSETCLOSURE0
    1906 APPTERM3 8
    1908 ACC0
    1909 PUSHACC4
    1910 PUSHACC3
    1911 MAKEBLOCK2 0
    1913 PUSHACC4
    1914 PUSHOFFSETCLOSURE0
    1915 APPTERM3 8
    1917 ACC1
    1918 PUSHENVACC1
    1919 APPLY1
    1920 PUSHACC1
    1921 PUSHENVACC1
    1922 APPLY1
    1923 MAKEBLOCK2 0
    1925 RETURN 3
    1927 RESTART
    1928 GRAB 1
    1930 ACC0
    1931 PUSHENVACC1
    1932 CLOSUREREC 2, 1885
    1936 ACC2
    1937 PUSHCONST0
    1938 PUSHCONST0
    1939 PUSHACC3
    1940 APPTERM3 6
    1942 ACC0
    1943 BRANCHIFNOT 1967
    1945 ACC0
    1946 GETFIELD0
    1947 PUSHACC1
    1948 GETFIELD1
    1949 PUSHOFFSETCLOSURE0
    1950 APPLY1
    1951 PUSHACC0
    1952 GETFIELD1
    1953 PUSHACC2
    1954 GETFIELD1
    1955 MAKEBLOCK2 0
    1957 PUSHACC1
    1958 GETFIELD0
    1959 PUSHACC3
    1960 GETFIELD0
    1961 MAKEBLOCK2 0
    1963 MAKEBLOCK2 0
    1965 RETURN 3
    1967 GETGLOBAL <0>(0, 0)
    1969 RETURN 1
    1971 RESTART
    1972 GRAB 1
    1974 ACC0
    1975 BRANCHIFNOT 1996
    1977 ACC1
    1978 BRANCHIFNOT 2003
    1980 ACC1
    1981 GETFIELD1
    1982 PUSHACC1
    1983 GETFIELD1
    1984 PUSHOFFSETCLOSURE0
    1985 APPLY2
    1986 PUSHACC2
    1987 GETFIELD0
    1988 PUSHACC2
    1989 GETFIELD0
    1990 MAKEBLOCK2 0
    1992 MAKEBLOCK2 0
    1994 RETURN 2
    1996 ACC1
    1997 BRANCHIFNOT 2001
    1999 BRANCH 2003
    2001 RETURN 2
    2003 GETGLOBAL "List.combine"
    2005 PUSHGETGLOBALFIELD Pervasives, 2
    2008 APPTERM1 3
    2010 RESTART
    2011 GRAB 1
    2013 ACC1
    2014 BRANCHIFNOT 2038
    2016 ACC1
    2017 GETFIELD0
    2018 PUSHACC2
    2019 GETFIELD1
    2020 PUSHACC1
    2021 PUSHENVACC2
    2022 APPLY1
    2023 BRANCHIFNOT 2033
    2025 ACC0
    2026 PUSHACC3
    2027 PUSHACC3
    2028 MAKEBLOCK2 0
    2030 PUSHOFFSETCLOSURE0
    2031 APPTERM2 6
    2033 ACC0
    2034 PUSHACC3
    2035 PUSHOFFSETCLOSURE0
    2036 APPTERM2 6
    2038 ACC0
    2039 PUSHENVACC1
    2040 APPTERM1 3
    2042 ACC0
    2043 PUSHENVACC1
    2044 CLOSUREREC 2, 2011
    2048 CONST0
    2049 PUSHACC1
    2050 APPTERM1 3
    2052 RESTART
    2053 GRAB 2
    2055 ACC1
    2056 BRANCHIFNOT 2077
    2058 ACC2
    2059 BRANCHIFNOT 2084
    2061 ACC2
    2062 GETFIELD1
    2063 PUSHACC2
    2064 GETFIELD1
    2065 PUSHACC2
    2066 PUSHACC5
    2067 GETFIELD0
    2068 PUSHACC5
    2069 GETFIELD0
    2070 PUSHENVACC1
    2071 APPLY2
    2072 MAKEBLOCK2 0
    2074 PUSHOFFSETCLOSURE0
    2075 APPTERM3 6
    2077 ACC2
    2078 BRANCHIFNOT 2082
    2080 BRANCH 2084
    2082 RETURN 3
    2084 GETGLOBAL "List.rev_map2"
    2086 PUSHGETGLOBALFIELD Pervasives, 2
    2089 APPTERM1 4
    2091 RESTART
    2092 GRAB 2
    2094 ACC0
    2095 CLOSUREREC 1, 2053
    2099 ACC3
    2100 PUSHACC3
    2101 PUSHCONST0
    2102 PUSHACC3
    2103 APPTERM3 7
    2105 RESTART
    2106 GRAB 1
    2108 ACC1
    2109 BRANCHIFNOT 2123
    2111 ACC1
    2112 GETFIELD1
    2113 PUSHACC1
    2114 PUSHACC3
    2115 GETFIELD0
    2116 PUSHENVACC1
    2117 APPLY1
    2118 MAKEBLOCK2 0
    2120 PUSHOFFSETCLOSURE0
    2121 APPTERM2 4
    2123 ACC0
    2124 RETURN 2
    2126 RESTART
    2127 GRAB 1
    2129 ACC0
    2130 CLOSUREREC 1, 2106
    2134 ACC2
    2135 PUSHCONST0
    2136 PUSHACC2
    2137 APPTERM2 5
    2139 CONST0
    2140 PUSHACC1
    2141 PUSHENVACC1
    2142 APPTERM2 3
    2144 ACC0
    2145 BRANCHIFNOT 2151
    2147 ACC0
    2148 GETFIELD1
    2149 RETURN 1
    2151 GETGLOBAL "tl"
    2153 PUSHGETGLOBALFIELD Pervasives, 3
    2156 APPTERM1 2
    2158 ACC0
    2159 BRANCHIFNOT 2165
    2161 ACC0
    2162 GETFIELD0
    2163 RETURN 1
    2165 GETGLOBAL "hd"
    2167 PUSHGETGLOBALFIELD Pervasives, 3
    2170 APPTERM1 2
    2172 ACC0
    2173 PUSHCONST0
    2174 PUSHENVACC1
    2175 APPTERM2 3
    2177 CLOSUREREC 0, 1200
    2181 ACC0
    2182 CLOSURE 1, 2172
    2185 PUSH
    2186 CLOSURE 0, 2158
    2189 PUSH
    2190 CLOSURE 0, 2144
    2193 PUSH
    2194 CLOSUREREC 0, 1217
    2198 GETGLOBALFIELD Pervasives, 16
    2201 PUSH
    2202 CLOSUREREC 0, 1259
    2206 ACC0
    2207 CLOSURE 1, 2139
    2210 PUSH
    2211 CLOSUREREC 0, 1277
    2215 CLOSUREREC 0, 1294
    2219 CLOSURE 0, 2127
    2222 PUSH
    2223 CLOSUREREC 0, 1316
    2227 CLOSUREREC 0, 1334
    2231 CLOSUREREC 0, 1354
    2235 CLOSUREREC 0, 1374
    2239 CLOSURE 0, 2092
    2242 PUSH
    2243 CLOSUREREC 0, 1415
    2247 CLOSUREREC 0, 1452
    2251 CLOSUREREC 0, 1490
    2255 CLOSUREREC 0, 1530
    2259 CLOSUREREC 0, 1553
    2263 CLOSUREREC 0, 1573
    2267 CLOSUREREC 0, 1613
    2271 CLOSUREREC 0, 1654
    2275 CLOSUREREC 0, 1675
    2279 CLOSUREREC 0, 1695
    2283 CLOSUREREC 0, 1725
    2287 CLOSUREREC 0, 1754
    2291 CLOSUREREC 0, 1776
    2295 CLOSUREREC 0, 1797
    2299 CLOSUREREC 0, 1828
    2303 CLOSUREREC 0, 1858
    2307 ACC 24
    2309 CLOSURE 1, 2042
    2312 PUSHACC 25
    2314 CLOSUREREC 1, 1928
    2318 CLOSUREREC 0, 1942
    2322 CLOSUREREC 0, 1972
    2326 ACC0
    2327 PUSHACC2
    2328 PUSHACC7
    2329 PUSHACC 9
    2331 PUSHACC 11
    2333 PUSHACC 13
    2335 PUSHACC 15
    2337 PUSHACC 17
    2339 PUSHACC 10
    2341 PUSHACC 12
    2343 PUSHACC 13
    2345 PUSHACC 15
    2347 PUSHACC 23
    2349 PUSHACC 25
    2351 PUSHACC 27
    2353 PUSHACC 29
    2355 PUSHACC 31
    2357 PUSHACC 33
    2359 PUSHACC 35
    2361 PUSHACC 37
    2363 PUSHACC 40
    2365 PUSHACC 42
    2367 PUSHACC 41
    2369 PUSHACC 45
    2371 PUSHACC 47
    2373 PUSHACC 50
    2375 PUSHACC 52
    2377 PUSHACC 51
    2379 PUSHACC 55
    2381 PUSHACC 56
    2383 PUSHACC 59
    2385 PUSHACC 61
    2387 PUSHACC 60
    2389 PUSHACC 64
    2391 PUSHACC 66
    2393 PUSHACC 68
    2395 PUSHACC 70
    2397 MAKEBLOCK 37, 0
    2400 POP 36
    2402 SETGLOBAL List
    2404 BRANCH 2432
    2406 CONST0
    2407 PUSHACC1
    2408 LEINT
    2409 BRANCHIFNOT 2414
    2411 CONST0
    2412 RETURN 1
    2414 ACC0
    2415 OFFSETINT -1
    2417 PUSHOFFSETCLOSURE0
    2418 APPLY1
    2419 PUSHACC1
    2420 MAKEBLOCK2 0
    2422 RETURN 1
    2424 RESTART
    2425 GRAB 1
    2427 ACC1
    2428 PUSHACC1
    2429 ADDINT
    2430 RETURN 2
    2432 CLOSUREREC 0, 2406
    2436 CONSTINT 300
    2438 PUSHACC1
    2439 APPLY1
    2440 PUSHCONST0
    2441 C_CALL1 gc_major
    2443 CONSTINT 150
    2445 PUSHCONSTINT 301
    2447 MULINT
    2448 PUSHACC1
    2449 PUSHCONST0
    2450 PUSH
    2451 CLOSURE 0, 2425
    2454 PUSHGETGLOBALFIELD List, 12
    2457 APPLY3
    2458 NEQ
    2459 BRANCHIFNOT 2466
    2461 GETGLOBAL Not_found
    2463 MAKEBLOCK1 0
    2465 RAISE
    2466 POP 2
    2468 ATOM0
    2469 SETGLOBAL T320-gc-2
    2471 STOP
**)
