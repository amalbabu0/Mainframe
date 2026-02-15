       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CA21G013.                                            00020000
       DATA DIVISION.                                                   00030000
       LINKAGE SECTION.                                                 00040000
       77 LK77-OUT-SHR-PRICE PIC 9(05)V9(03).                           00050000
       77 LK77-NEW-PRICE     PIC 9(03)V9(02).                           00060000
       77 LK77-NO-OF-SHARES  PIC 9(03).                                 00070000
       77 LK77-INC-TAX       PIC 9(04)V9(03).                           00080000
       77 LK77-PER-HELD      PIC 9(02).                                 00090001
       PROCEDURE DIVISION USING LK77-OUT-SHR-PRICE LK77-NEW-PRICE       00100001
                                LK77-INC-TAX                            00110003
                                LK77-NO-OF-SHARES LK77-PER-HELD.        00111003
           COMPUTE LK77-OUT-SHR-PRICE =                                 00130002
                                LK77-NEW-PRICE * LK77-NO-OF-SHARES.     00131002
           COMPUTE LK77-INC-TAX =                                       00140002
                              ( LK77-OUT-SHR-PRICE / LK77-NEW-PRICE )   00150002
                              * LK77-PER-HELD.                          00151002
                                                                        00152002
           EXIT.                                                        00160002
           GOBACK.                                                      00170002
