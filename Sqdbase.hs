module Sqdbase (
  putIntoDB
, createTableQ
, prepExec
) where

import Busmtc
import Database.HDBC

createTableQ = "CREATE TABLE IF NOT EXISTS busdetails(Route TEXT, Servtype TEXT, Origin TEXT, Dest TEXT, Jtime INTEGER);"
insertBusQ = "INSERT INTO busdetails VALUES(?, ?, ?, ?, ?);"

pathTablePreQ = "CREATE TABLE IF NOT EXISTS "
pathTablePosQ = " (Path TEXT);"
pathInsPreQ = "INSERT INTO " 
pathInsPosQ = " VALUES(?);"


putIntoDB c b = do
    d <- getBusDetails b
    let (sqld, sqlp) = bdbpToSql d
    prepExec c insertBusQ sqld
    let t = p:(fromSql (sqld !! 0))
    prepExec c (pathTablePreQ ++ t ++ pathTablePosQ) []
    let help l = prepExec c (pathInsPreQ ++ t ++ pathInsPosQ) [l]
    mapM_ help sqlp
    putStrLn "done"

bdbpToSql (bd, p) = (help bd, help p) where
  help lst = map toSql lst

prepExec c q lst = do
    s <- prepare c q
    if null lst 
        then executeRaw s
        else (execute s lst) >>= (\_ -> return ())