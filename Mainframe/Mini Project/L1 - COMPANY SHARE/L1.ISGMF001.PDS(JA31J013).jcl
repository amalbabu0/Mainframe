//TECN013A JOB NOTIFY=&SYSUID                                           00010000
//STEP001  EXEC PGM=IEFBR14                                             00020000
//DD1      DD DSN=TECN013.L1J.SHARE.ACC,DISP=(MOD,DELETE,DELETE),       00030000
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00040000
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00050000
//DD2      DD DSN=TECN013.L1J.SHARE.INFY,DISP=(MOD,DELETE,DELETE),      00051001
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00052000
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00053000
//DD3      DD DSN=TECN013.L1J.SHARE.PS3,DISP=(MOD,DELETE,DELETE),       00054001
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00055001
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00056001
//STEP002  EXEC PGM=IEFBR14                                             00060000
//DD4      DD DSN=TECN013.L1J.SHARE.ACC,DISP=(NEW,CATLG,DELETE),        00070001
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00080000
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00090000
//DD5      DD DSN=TECN013.L1J.SHARE.INFY,DISP=(NEW,CATLG,DELETE),       00091001
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00092001
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00093001
//DD6      DD DSN=TECN013.L1J.SHARE.PS3,DISP=(NEW,CATLG,DELETE),        00094001
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(10,10)),                    00095001
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              00096001
//STEP003  EXEC PGM=SORT                                                00100001
//SYSPRINT DD SYSOUT=*                                                  00110000
//SYSOUT   DD SYSOUT=*                                                  00120000
//SORTIN   DD DSN=TECN013.L1J.SHARE.PS2,DISP=SHR                        00130001
//SORTOUT1 DD DSN=TECN013.L1J.SHARE.ACC,DISP=SHR                        00140001
//SORTOUT2 DD DSN=TECN013.L1J.SHARE.INFY,DISP=SHR                       00141001
//SYSIN    DD *                                                         00150000
  SORT FIELDS=(1,5,CH,A)                                                00160001
  OUTFIL FNAMES=SORTOUT1,INCLUDE=(14,5,CH,EQ,C'ACC  ')                  00170001
  OUTFIL FNAMES=SORTOUT2,INCLUDE=(14,5,CH,EQ,C'INFY ')                  00171001
  SUM FIELDS=NONE                                                       00180000
/*                                                                      00190000
//STEP004  EXEC PGM=SORT                                                00200001
//SYSPRINT DD SYSOUT=*                                                  00210001
//SYSOUT   DD SYSOUT=*                                                  00220001
//SORTIN01 DD DSN=TECN013.L1J.SHARE.ACC,DISP=SHR                        00230001
//SORTIN02 DD DSN=TECN013.L1J.SHARE.INFY,DISP=SHR                       00240001
//SORTOUT  DD DSN=TECN013.L1J.SHARE.PS3,DISP=SHR                        00250001
//SYSIN    DD *                                                         00260001
  MERGE FIELDS=(1,5,CH,A)                                               00272001
/*                                                                      00280001
