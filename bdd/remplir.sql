DROP TABLE axiomes;
DROP TABLE theoremes;
DROP TABLE definitions;
DROP TABLE parameters;


CREATE TABLE language
( id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255)
);
CREATE TABLE axiomes
( id INT UNSIGNED AUTO_INCREMENT UNIQUE,
  md VARCHAR(255),
  idName VARCHAR(255),
  statement VARCHAR(255),
  langID INT UNSIGNED NOT NULL,
  PRIMARY KEY (md,idName,langID),
  CONSTRAINT fk_langID_axiomes
    FOREIGN KEY (langID)
    REFERENCES language(id)
);
CREATE TABLE theoremes
( id INT UNSIGNED AUTO_INCREMENT UNIQUE,
  md VARCHAR(255),
  idName VARCHAR(255),
  type VARCHAR(255),
  statement VARCHAR(255),
  langID INT UNSIGNED NOT NULL,
  PRIMARY KEY (md,idName,langID),
  CONSTRAINT fk_langID_theoremes
    FOREIGN KEY (langID)
    REFERENCES language(id)
);
CREATE TABLE definitions
( id INT UNSIGNED AUTO_INCREMENT UNIQUE,
  md VARCHAR(255),
  idName VARCHAR(255),
  type VARCHAR(255),
  statement VARCHAR(255),
  langID INT UNSIGNED NOT NULL,
  PRIMARY KEY (md,idName,langID),
  CONSTRAINT fk_langID_definitions
    FOREIGN KEY (langID)
    REFERENCES language(id)
);
CREATE TABLE parameters
( id INT UNSIGNED AUTO_INCREMENT UNIQUE,
  md VARCHAR(255),
  idName VARCHAR(255),
  type VARCHAR(255),
  langID INT UNSIGNED NOT NULL,
  PRIMARY KEY (md,idName,langID),
  CONSTRAINT fk_langID_parameters
    FOREIGN KEY (langID)
    REFERENCES language(id)
);
