--USERS
CREATE TABLE IF NOT EXISTS Users (
    Id INTEGER PRIMARY KEY,
    Email TEXT NOT NULL,
    NickName TEXT NOT NULL
);

INSERT INTO Users(Email, NickName)
  SELECT DISTINCT Email, NickName FROM WavFiles;

ALTER TABLE WavFiles ADD User INTEGER REFERENCES Users(Id);

UPDATE WavFiles
SET User = (
  SELECT Id FROM Users WHERE Email = WavFiles.Email AND NickName = WavFiles.NickName
);

CREATE TABLE IF NOT EXISTS WavFiles2 (
  Id INTEGER PRIMARY KEY,
  OriginalName TEXT NOT NULL,
  Frequency INTEGER NOT NULL,
  Duration REAL NOT NULL,
  Words TEXT,
  User REFERENCES Users(Id)
);

INSERT INTO WavFiles2
  SELECT rowid, OriginalName, Frequency, Duration, Words, User FROM WavFiles;

DROP TABLE WavFiles;
ALTER TABLE WavFiles2 RENAME TO WavFiles;

--PROJECTS
CREATE TABLE IF NOT EXISTS Pojects (
  Id INTEGER PRIMARY KEY,
  Name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS Words (
  Id INTEGER PRIMARY KEY,
  Word TEXT NOT NULL,
  Project INTEGER  REFERENCES Projects(Id)
);

CREATE TABLE IF NOT EXISTS ProjectOwners (
  Id INTEGER PRIMARY KEY,
  Project INTEGER REFERENCES Projects(Id),
  User INTEGER REFERENCES Users(Id)
);

ALTER TABLE WavFiles ADD Project INTEGER REFERENCES Projects(Id);

--SEGMENTS
CREATE TABLE IF NOT EXISTS SegmentPositions (
  Id INTEGER PRIMARY KEY,
  Segment INTEGER  REFERENCES Segments(Id),
  Start REAL NOT NULL,
  Finish REAL NOT NULL,
  User REFERENCES Users(Id)
);

INSERT INTO SegmentPositions(Segment, Start, Finish, User)
  SELECT rowid, Start, Finish, NULL FROM Segments WHERE Start IS NOT NULL AND Finish IS NOT NULL;

CREATE TABLE IF NOT EXISTS OwnerValidation (
  Id INTEGER PRIMARY KEY,
  User REFERENCES Users(Id),
  SegmentPosition REFERENCES SegmentPositions(Id),
  Valid INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS HearingValidation (
  Id INTEGER PRIMARY KEY,
  User REFERENCES Users(Id),
  SegmentPosition REFERENCES SegmentPositions(Id),
  Word REFERENCES Words(Id),
  WrittenWord TEXT
);

--REMOVE UNNEEDED VALUES
CREATE TABLE IF NOT EXISTS Segments2 (
  Id INTEGER PRIMARY KEY,
  WavFile INTEGER  REFERENCES WavFiles(rowid),
  WordPosition INTEGER NOT NULL,
  VocalPosition INTEGER NOT NULL,
  PreVocal TEXT NOT NULL,
  Vocal TEXT NOT NULL,
  PostVocal TEXT NOT NULL
);

INSERT INTO Segments2
  SELECT rowid, WavFile, WordPosition, VocalPosition, PreVocal, Vocal, PostVocal FROM Segments;

DROP TABLE Segments;
ALTER TABLE Segments2 RENAME TO Segments;

