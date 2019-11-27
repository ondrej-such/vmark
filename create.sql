-- Table of files

CREATE TABLE IF NOT EXISTS WavFiles (
    OriginalName TEXT NOT NULL,
    Email TEXT NOT NULL,
    NickName TEXT NOT NULL,
    Frequency INTEGER NOT NULL,
    Duration REAL NOT NULL,
    Words TEXT
);

CREATE TABLE IF NOT EXISTS Spectrograms (
    WavId INTEGER  REFERENCES WavFiles(rowid),
    windowLength REAL NOT NULL, 
    DeviceXmin INTEGER NOT NULL,
    DeviceXmax INTEGER NOT NULL,
    UserXmin REAL NOT NULL,
    UserXmax REAL NOT NULL,
    UserPlotXmax REAL NOT NULL,
    UserPlotXmin REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS Segments (
  WavFile INTEGER  REFERENCES WavFiles(rowid),
  WordPosition INTEGER NOT NULL,
  VocalPosition INTEGER NOT NULL,
  PreVocal TEXT NOT NULL,
  Vocal TEXT NOT NULL,
  PostVocal TEXT NOT NULL,
  Start REAL,
  Finish REAL
);

CREATE TABLE IF NOT EXISTS Spectra (
  SegId INTEGER REFERENCES Segments(rowid),
  Energy REAL NOT NULL,
  Spectrum TEXT NOT NULL,
  SeqNumber INTEGER NOT NULL,
  WindowLength INTEGER NOT NULL
);

CREATE TRIGGER IF NOT EXISTS SEG_TRIG
 UPDATE OF Start ON Segments FOR EACH ROW
BEGIN
	DELETE FROM Spectra WHERE SegId = NEW.rowid;
END;
