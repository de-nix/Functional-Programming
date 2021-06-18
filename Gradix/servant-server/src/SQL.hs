{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs#-}
module SQL where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Types
import Data.List
instance FromRow Student where
  fromRow = Student <$> field <*> field <*> field <*>field
instance FromRow Attendance where
  fromRow = Attendance <$> field <*> field <*> field <*> field <*> field
instance ToRow Student where
	toRow (Student sid sname sgroup scode) = toRow(sid,sname,sgroup,scode)
instance ToRow Attendance where
	toRow (Attendance a b c d e) = toRow (a,b,c,d,e)

data SQLConnection = SQLConnection{location ::String} deriving (Show)

instance ModelAPI SQLConnection IO where
  createStudent :: Student   -> SQLConnection -> IO ()
  createStudent student conn = do
    db <- open (location conn)
    execute db "INSERT INTO student(_sId,_sName,_sGroup,_sCode) VALUES (?,?,?,?)" student
    close db
  removeStudent :: StudentId -> SQLConnection -> IO ()
  removeStudent studentId conn = do
    db <- open (location conn)
    execute db "DELETE from student WHERE _sId =  (?)" (Only studentId)
    close db
  updateStudent :: Student   -> SQLConnection -> IO()
  updateStudent (Student sid sname sgroup scode) conn = do
    db <- open (location conn)
    execute db "UPDATE student set _sName = (?), _sGroup = (?), _sCode = (?) WHERE _sId = (?)" (sname, sgroup, scode, sid)
    close db
  findStudent   :: StudentId -> SQLConnection ->IO(Maybe Student)
  findStudent studentID conn= do
    db <- open (location conn)
    result <-query db "SELECT * from student where _sid = (?)" (Only studentID) :: IO [Student]
    close db
    return $ firstOrNothing result
  getAllStudents :: SQLConnection -> IO([Student])
  getAllStudents conn = do
    db <- open (location conn)
    result <- query_ db "SELECT * from student" :: IO [Student]
    close db
    return result
  createAttendance :: Attendance -> SQLConnection -> IO()
  createAttendance attendance conn= do
    db <- open (location conn)
    execute db "INSERT into attendance (_aId, _aStudentId, _aSeminar,_aGroup,_aActivity) VALUES (?,?,?,?,?)" attendance
    close db
  removeAttendance :: AttendanceId -> SQLConnection -> IO()
  removeAttendance attendanceID conn= do
    db <- open (location conn)
    execute db "DELETE from attendance WHERE _aId = (?)" (Only attendanceID)
    close db
  updateAttendance :: Attendance -> SQLConnection -> IO()
  updateAttendance Attendance{_aId = aid, _aSeminar = sem, _aGroup = gr,_aActivity = act} conn= do
    db <- open (location conn)
    execute db "UPDATE attendance set  _aSeminar = (?), _aGroup = (?), _aActivity = (?) WHERE _aId = (?)" (sem, gr, act, aid)
    close db
  findAttendance   :: AttendanceId -> SQLConnection -> IO(Maybe Attendance)
  findAttendance attendanceID conn = do
    db <- open (location conn)
    result <-query db "SELECT * from attendance where _aId = (?)" (Only attendanceID) :: IO [Attendance]
    close db
    return $ firstOrNothing result
  getAllAttendances :: SQLConnection -> IO ([Attendance])
  getAllAttendances conn= do
    db <- open (location conn)
    query_ db "SELECT * from attendance" :: IO [Attendance]

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x
