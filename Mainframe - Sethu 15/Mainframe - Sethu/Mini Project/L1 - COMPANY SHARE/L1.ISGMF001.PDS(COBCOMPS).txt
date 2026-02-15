//TECN013A JOB NOTIFY=&SYSUID                                           00010000
//JOBPROC JCLLIB ORDER=IBMUSER.ALL1                                     00020000
//COBCL EXEC IGYWCL,                                                    00030000
//          PGMLIB=TECN013.L1.ISGMF001.LOADLIB,                         00040000
//*         COPYLIB=TXSAD01.COPYLIB.PDS,                                00050000
//          GOPGM=CA21G013                                              00060000
//COBOL.SYSIN  DD DSN=TECN013.L1.ISGMF001.PDS(CA21G013),DISP=SHR        00070000
//*LKED.SYSLIB  DD DSN=TECN013.L1.ISGMF001.LOADLIB(CA21G013),DISP=SHR   00080000
