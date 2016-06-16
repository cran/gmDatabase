SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=`TRADITIONAL`;

CREATE SCHEMA IF NOT EXISTS `gmDatabase` DEFAULT CHARACTER SET latin1 ;
USE `gmDatabase` ;
-- -----------------------------------------------------
-- Table `gmDatabase`.`gmVarType`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmVarType` (
  `gmVarTypeID` VARCHAR(10) NOT NULL,
  `gmVarTypeDescription` VARCHAR(255) NULL DEFAULT NULL,
  `gmTable` VARCHAR(45) NULL DEFAULT NULL,
  PRIMARY KEY (`gmVarTypeID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmVar`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmVar` (
  `gmVarID` BIGINT(19) UNSIGNED NOT NULL AUTO_INCREMENT,
  `gmVarName` VARCHAR(128) NOT NULL,
  `gmVarTypeID` VARCHAR(10) NOT NULL,
  `gmVarDescription` VARCHAR(255) NULL DEFAULT NULL,
  PRIMARY KEY (`gmVarID`),
  INDEX `fk_gmVar_gmVarType_idx` (`gmVarTypeID` ASC),
  CONSTRAINT `fk_gmVar_gmVarType`
    FOREIGN KEY (`gmVarTypeID`)
    REFERENCES `gmVarType` (`gmVarTypeID`)
    ON DELETE CASCADE
	  ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmInherits`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmInherits` (
  `gmVarID` BIGINT(19) UNSIGNED,
  `child` BIGINT(19) UNSIGNED NOT NULL,
  `remark` VARCHAR(80), 
  INDEX `fk_gmInherits_gmVarID_idx` (`gmVarID` ASC),
  INDEX `fk_gmInherits_child_idx` (`child` ASC),
  CONSTRAINT `fk_gmInherits_gmVar`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmInherits_child`
    FOREIGN KEY (`child`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmGrandChilds`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmGrandChilds` (
  `gmVarID` BIGINT(19) UNSIGNED,
  `grandchild` BIGINT(19) UNSIGNED NOT NULL,
  `generation` BIGINT(5) UNSIGNED ,
  INDEX `uk_gmGrandChilds_idx` (`gmVarID` ASC,`grandchild` ASC,`generation` ASC),
  INDEX `fk_gmGrandChilds_gmVarID_idx` (`gmVarID` ASC),
  INDEX `fk_gmGrandChilds_grandchild_idx` (`grandchild` ASC),
  CONSTRAINT `fk_gmGrandChilds_gmVar`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmGrandChilds_grandchild`
    FOREIGN KEY (`grandchild`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `uk_gmGrandChilds`	
    UNIQUE KEY 	(`gmVarID`,`grandchild`,`generation`)     
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmElements`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmElements` (
  `gmVarID` BIGINT(19) UNSIGNED,
  `member` BIGINT(19) UNSIGNED NOT NULL,
  `required` BOOL NOT NULL,
  `remark` VARCHAR(80), 
  INDEX `fk_gmElements_gmVar_idx` (`gmVarID` ASC),
  INDEX `fk_gmElements_member_idx` (`member` ASC),
  CONSTRAINT `fk_gmElements_gmVar`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmElements_member`
    FOREIGN KEY (`member`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;



-- -----------------------------------------------------
-- Table `gmDatabase`.`gmObject`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmObject` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL AUTO_INCREMENT,
  `gmCreateTime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  `gmRightsGroup` BIGINT(19) UNSIGNED,
  CONSTRAINT `fk_gmObject_gmObject`
    FOREIGN KEY (`gmRightsGroup`)
	  REFERENCES `gmObject` (`gmID`)
	  ON DELETE CASCADE
	  ON UPDATE CASCADE,
  PRIMARY KEY (`gmID`)
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmInteger`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmInteger` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` INT(10) UNSIGNED NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmInteger_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmInteger_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
    ON DELETE CASCADE
	ON UPDATE CASCADE,
  CONSTRAINT `fk_gmInteger_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
	ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmNumeric`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmNumeric` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmNumeric_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmNumeric_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmNumeric_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmRef`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmRef` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `gmRefID` BIGINT(19) UNSIGNED NOT NULL,
  PRIMARY KEY (`gmID`,`gmVarID`,`gmRefID`),
  INDEX `sk_gmIDVar_idx` (gmVarID ASC, `gmID` ASC),
  INDEX `sk_gmID_idx` (`gmID` ASC),
  INDEX `sk_gmVar_idx` (gmVarID ASC),
  CONSTRAINT `fk_gmRefID_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmRefRef_gmObject`
    FOREIGN KEY (`gmRefID`)
    REFERENCES `gmObject` (`gmID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmRef_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmString`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmString` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` VARCHAR(255) NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmString_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmString_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmString_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
    ON DELETE CASCADE
	ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmText`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmText` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` LONGTEXT NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmText_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmText_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmText_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmBlob`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmBlob` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` LONGBLOB NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmBlob_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmBlob_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmBlob_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `gmDatabase`.`gmBoolean`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmBoolean` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` BOOL NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmBoolean_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmBoolean_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmBoolean_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;

-- -----------------------------------------------------
-- Table `gmDatabase`.`gmDate`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `gmDate` (
  `gmID` BIGINT(19) UNSIGNED NOT NULL,
  `gmVarID` BIGINT(19) UNSIGNED,
  `x` TIMESTAMP NULL DEFAULT NULL,
  PRIMARY KEY (`gmID`, `gmVarID`),
  INDEX `fk_gmDate_gmObject_idx` (`gmID` ASC),
  CONSTRAINT `fk_gmDate_gmObject`
    FOREIGN KEY (`gmID`)
    REFERENCES `gmObject` (`gmID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_gmDate_gmVarID`
    FOREIGN KEY (`gmVarID`)
    REFERENCES `gmVar` (`gmVarID`)
	ON DELETE CASCADE
    ON UPDATE CASCADE
)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;

SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
