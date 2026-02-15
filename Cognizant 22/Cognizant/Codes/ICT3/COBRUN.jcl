//TECN116A JOB NOTIFY=&SYSUID                                          
//RUN EXEC PGM=ICT03CBM                                
//STEPLIB DD DSN=TECN116.MYLIB.COBOL.LOADLIB,DISP=SHR 
//SYSPRINT DD SYSOUT=*                                                 
//INPKSDS DD DSN=TECN116.MYDATA.ICT03.BOOK.KSDS,DISP=SHR               
//OUTKSDS DD DSN=TECN116.MYDATA.ICT03.OUTPUT.KSDS,DISP=SHR             
//SYSIN DD DUMMY                                                       