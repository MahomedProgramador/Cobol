$set sourceformat(free)

FD VENDOR-FILE.
01 VENDOR-RECORD.
  05 VENDOR-NUMBER      PIC 9(5).
  05 FILLER             PIC X VALUE SPACE.
  05 VENDOR-NAME        PIC X(30).
  05 FILLER PIC X VALUE SPACE .
  05 FIM-FICHEIRO-P     PIC X VALUES 'N'.


  05 VENDOR-ADDRESS-1   PIC X(30).
  05 FILLER PIC X VALUE SPACES.
  05 VENDOR-ADDRESS-2   PIC X(30).
  05 FILLER PIC X VALUE SPACES.
  05 VENDOR-CITY        PIC X(20).
  05 FILLER PIC X VALUE SPACES.
  05 VENDOR-STATE        PIC X(2).
  05 FILLER PIC X VALUE SPACES.
  05 VENDOR-ZIP         PIC X VALUE SPACE.
  05 FILLER             PIC X VALUE SPACES.
  05 VENDOR-CONTACT     PIC X(30).
  05 FILLER             PIC X VALUE SPACES.
  05 VENDOR-PHONE       PIC X(15).
