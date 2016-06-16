INSERT INTO gmVarType (gmVarTypeID, gmVarTypeDescription, gmTable)
VALUES
('set','a set of objects','gmRef'),
('real','a real number','gmNumeric'),
('int','an integer','gmInteger'),
('string','a string value','gmString'),
('text','a text value','gmText'),
('blob','a longblob','gmBlob'),
('boolean','a boolean value', 'gmBoolean'),
('date','a timestamp value','gmDate');

INSERT INTO gmVar (gmVarID, gmVarName, gmVarTypeID, gmVarDescription)
VALUES
(1,'gmObject','set','a general object'),
(2,'user','set','a person (as list in root) with access to db'),
(3,'gmName','string','the name of the object'),
(4,'userName','string','the login name of the user'),
(5,'password','string','the password of the user'),
(6,'gmCreator','set','the person who created the object'),
(7,'gmDescription','text','a description'),
(8,'gmCreationTime','date','the creation time in yyyy-mm-dd hh:mm:ss format'),
(9,'gmRoot','set','the root object'),
(10,'userGroup','set','a group of users'),
(11,'rightsGroup','set','a group defining the access rights for a user group'),
(12,'readers','set','defining the user groups being able to read objects belonging to a rights group'),
(13,'writers','set','defining the user groups being able to write objects belonging to a rights group');

INSERT INTO gmElements (gmVarID, member, required, remark)
VALUES
(1,3,FALSE,'gmObject.gmName'),
(1,6,FALSE,'gmObject.gmCreator'),
(1,7,FALSE,'gmObject.gmDescription'),
(1,8,FALSE,'gmObject.gmCreationTime'),
(2,4,TRUE,'user.userName'),
(2,5,TRUE,'user.password'),
(9,2,TRUE,'gmRoot.user'),
(9,10,TRUE,'gmRoot.userGroup'),
(10,2,TRUE,'userGroup.user'),
(9,11,TRUE,'gmRoot.rightsGroup'),
(11,12,TRUE,'rightsGroup.readers'),
(11,13,TRUE,'rightsGroup.writers'),
(12,10,TRUE,'readers.userGroup'),
(13,10,TRUE,'writers.userGroup');

INSERT INTO gmInherits (gmVarID, child, remark)
VALUES
(1,2,'user extends gmObject'),
(2,6,'gmCreator extends user'),
(1,9,'gmRoot extends gmObject'),
(1,10,'userGroup extends gmObject'),
(1,11,'rightsGroup extends gmObject'),
(1,12,'readers extends gmObject'),
(1,13,'writers extends gmObject');

INSERT INTO gmObject (gmID)					VALUES (1);
INSERT INTO gmRef (gmID, gmVarID, gmRefID)	VALUES (1,9,1);
INSERT INTO gmString (gmID, gmVarID, x)		VALUES (1,3,'root');
